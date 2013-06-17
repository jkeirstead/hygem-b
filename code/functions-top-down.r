## Functions for the top down model
fillcolour <- "#CCCCCC"
pointsize <- 2
## Get the regions.  These are defined in functions.r
regions <- define_regions()
last_year <- 2010

## Loads the data used by the top down model
##
## @return a data frame containing GDP (USD), population (000s), urbanization (%), building energy consumption by fuel (EJ), by region and year
load_top_down_inputs <- function() {

  ## If the clean input data exists, then just load it in
  clean_file <- "../data/clean-input-data.csv"
  if (file.exists(clean_file)) {
    data <- read.csv(clean_file)
  } else {

    ## Check if you have an internet connection
    library(RCurl)
    out <- is.character(getURL("www.google.com"))
    if (!out) {
      stop("Internet connection needed to download data.  Please connect and try again, or use run_hygem(TRUE)")
    } else {
  
      ## Create a dummy data frame to hold the full data
      data <- expand.grid(region=levels(regions$region), year=1971:2050)
      
      ## Load the GDP data
      gdp <- load_gdp()
      population <- load_population()
      urban <- load_urbanization()
      energy <- load_energy()
      
      ## Merge them all together
      data <- merge(data, gdp)
      data <- merge(data, population)
      data <- merge(data, urban)
      data <- merge(data, energy)

      ## Tidy up the factoring
      data <- mutate(data, region=factor(as.character(region)))
      
      ## Save the results for later
      write.csv(data, clean_file, row.names=FALSE)
    }
  }
  
  return(data)
}

## Generated summary plot for input data
##
## @param a data frame containing the input data
show_top_down_inputs <- function(data) {

  ## Rearrange and plot
  data <- subset(data, select=c(year, region, population, urban, gdp, energy))
  data <- mutate(data, gdp=gdp/1e12, population=population/1e6, energy=energy/1e6)

  ## Load required library for using melt function
  require(reshape2)
  data.m <- melt(data, id=c("region", "year"))

  ## Drop future energy values since they should be empty
  last_year <- 2010

  future_energy_ids <- with(data.m, which(year> last_year & variable=="energy"))
  data.m[future_energy_ids, ]$value <- NA
  data.m <- mutate(data.m, variable=factor(variable, lev=c("gdp", "population", "urban", "energy"),
                                 lab=c("GDP (constant 2000 USD, tns)", 
                                   "Population (bns)",
                                   "Urban fraction (%)",
                                   "Building energy consumption (EJ)")))

  ## Load required library for using ggplot function
  require(ggplot2)

  gg <- ggplot(data.m, aes(x=year, y=value, colour=region)) + 
    geom_line(aes(linetype=(year>2010))) + 
      facet_wrap(~variable, ncol=1, scale="free") + 
        theme_bw() +
          scale_linetype(guide="none") +
            scale_color_discrete(name="Region") + 
              labs(x="", y="Value")
  print(gg)
  
}


## Runs a regression model of global energy consumption
##
## Forecasts global building energy consumption to 2050
##
## @param data the input data frame
## @return a list containing the model, the model predictions, and a plot
run_global_regression <- function(data) {

  ## Reorganize the data
  global_data <- ddply(data, .(year), summarize, 
                       gdp=sum(gdp)/1e12,
                       urban=weighted.mean(urban, population, na.rm=TRUE),
                       population=sum(population)/1e6,
                       energy=sum(energy, na.rm=TRUE)/1e6)  

  ## Define the training data set
  train_data <- subset(global_data, energy!=0)

  ## Define the model
  G <- lm(energy ~ population + log(gdp) + logit(urban/100), train_data)

  ## Do the prediction
  set.seed(123)
  interval_type <- "prediction"
  global_pred <- predict(G, global_data, interval=interval_type)
  global_pred <- as.data.frame(cbind(year=1971:2050, global_pred))
 
  gg.global <- ggplot(global_pred, aes(x=year)) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), fill=fillcolour) + 
      geom_line(aes(y=fit)) +
        geom_point(data=train_data, aes(x=year, y=energy), size=pointsize) +
          theme_bw(11) +
            labs(y="Global building energy demand (EJ)")

  global_result <- list(model=G, pred=global_pred, plot=gg.global)
  save(global_result, file="../data/global-model.Rda")
  return(global_result)
}

## Runs a regression model of regional energy consumption
##
## Forecasts building energy consumption to 2050 by region
##
## @param data the input data frame
## @return a list containing the model, the model predictions, and a plot
run_regional_regression <- function(data) {
  regional_data <- mutate(data, 
                     gdp=gdp/1e12,
                     population=population/1e6,
                     energy=energy/1e6)

  ## Define the training data set
  train_data <- subset(regional_data, energy!=0)

  ## Need lme4 for multi-level models
  ## Loaded in first chunk with arm
  require(lme4)

  ## The model
  R <- lmer(energy ~ 1 + population + log(gdp) + logit(urban/100) + 
           (1 + population + log(gdp) + logit(urban/100) | region), 
           train_data, control=list(maxIter=1000))

  regional_pred <- predict_regional_model(R, regional_data, n.sims=1000)

  ## Make a plot
  gg.regional <- ggplot(regional_pred, aes(x=year)) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), fill=fillcolour) + 
      geom_line(aes(y=fit)) +
        geom_point(data=subset(regional_data, energy!=0), aes(x=year, y=energy), size=pointsize) +
          facet_wrap( ~ region, ncol=2) +
            theme_bw(11) +
              labs(y="Regional building energy demand (EJ)")

  regional_result <- list(model=R, pred=regional_pred, plot=gg.regional)
  save(regional_result, file="../data/regional-model.Rda")
  return(regional_result)

}

## Runs a regression model of energy consumption by fuel and region
##
## Forecasts building energy consumption to 2050 by region and fuel
##
## @param data the input data frame
## @return a list containing the model, the model predictions, and a plot
run_fuel_regression <- function(data) {

  ## Build the sub-data sets
  fuels <- data[,-ncol(data)]
  fuels <- melt(fuels, id=c("region", "year", "gdp", "population", "urban"), var="fuel", value.name="energy")
  fuels <- mutate(fuels, gdp=gdp/1e12, population=population/1e6, energy=energy/1e6)
  y.max <- 2010
  train.data.fuel <- subset(fuels, year<=y.max)
  fuel.names <- levels(fuels$fuel)

  ## Define the model
  F <- lmer(energy ~ 1 + population + log(gdp) + logit(urban/100) + 
            (1 + population + log(gdp) + logit(urban/100) | fuel:region), 
            data=train.data.fuel, control=list(maxIter=1000))

  fuels_pred <- predict_fuel_model(F, fuels, n.sims=1000)
  ## Trim out any negative values
  fuels_pred <- mutate(fuels_pred, fit=pmax(fit, 0), 
                       lwr=pmax(lwr, 0), upr=pmax(upr, 0))

  ## Save this to a file for the bottom-up model
  write.csv(fuels_pred, "../data/fuel-predictions.csv", row.names=FALSE)
  
  ## Make a plot
  gg.fuels <- ggplot(fuels_pred, aes(x=year)) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), fill=fillcolour) +
      geom_line(aes(y=fit)) + 
        geom_point(data=train.data.fuel, aes(x=year, y=energy), size=pointsize) + 
          facet_grid(fuel ~ region) + 
            theme_bw(11) + 
              labs(y="Energy demand (EJ)") +
                theme(axis.text.x=element_text(angle=90, hjust=0))
  
  fuel_result <- list(model=F, pred=fuels_pred, plot=gg.fuels)
  save(fuel_result, file="../data/fuel-model.Rda")
  return(fuel_result)

}

## Makes forecasts for the fuels model.  Note this is hard-coded based
## on the LMER formulation above.
predict_fuel_model <- function(mod, dat, n.sims=100, ci=0.95) {
  
  # Calculate the vector components
  fix <- fixef(mod)
  ran <- ranef(mod)
  fix.se <- se.fixef(mod)
  ran.se <- se.ranef(mod)
   
  # Calculate the slopes
  tmp <- rownames(ran$`fuel:region`)
  tmp <- colsplit(tmp, ":", names=c("fuel", "region"))
  ran.tmp <- cbind(ran$`fuel:region`, tmp)
  ran.se.tmp <- cbind(ran.se$`fuel:region`, tmp)
  
  # Simulate the model
  results <- data.frame()
  fuel.names <- as.character(levels(dat$fuel))
  region.names <- as.character(levels(dat$region))
  for (f in fuel.names) {
    for (r in region.names) {
      row <- with(ran.tmp, which(region==r & fuel==f))

      # Calculate intercept values
      i.fix <- fix[1]
      i.ran <- ran.tmp[row,1]
      i.val <- i.ran + i.fix
      
      ## Calculate slopes
      beta.pop <- ran.tmp[row, 2] + fix[2]
      beta.gdp <- ran.tmp[row, 3] + fix[3]
      beta.urban <- ran.tmp[row, 4] + fix[4]

      # Calculate the prediction
      df <- subset(dat, region==r & fuel==f)
      y.hat <- i.val + beta.pop%*%t(df$population) + beta.gdp%*%t(log(df$gdp)) + 
        beta.urban%*%t(logit(df$urban/100))
      y <- apply(y.hat, 2, function(x) rnorm(n.sims, x, sd(mod@resid)))
      
      # Calculate the summary statistics 
      alpha <- 1-ci
      tmp.result <- adply(y, 2, function(x) data.frame(lwr=quantile(x, alpha/2),
                                       fit=mean(x),
                                       upr=quantile(x, 1-alpha/2)))
      tmp.result <- cbind(tmp.result, df)
      results <- rbind(results, tmp.result)
    }
  }
  results <- results[,c("year", "region", "fuel", "fit","lwr","upr")]
  return(results)
}

## Makes forecasts for the regional model.  Note this is hard-coded based
## on the LMER formulation above.
predict_regional_model <- function(mod, dat, n.sims=100, ci=0.95) {
  
  # Calculate the vector components
  fix <- fixef(mod)
  ran <- ranef(mod)
  fix.se <- se.fixef(mod)
  ran.se <- se.ranef(mod)
  
  # Simulate the model
  results <- data.frame()

  region.names <- as.character(levels(dat$region))

  for (r in region.names) {
    row <- which(row.names(ran$region)==r)
    
    # Calculate intercept values
    i.ran <- ran$region[row,1]
    i.fix <- fix[1]
    i.val <- i.ran + i.fix
    
    # Calculate slopes
    beta.pop <- ran$region[row, 2] + fix[2]
    beta.gdp <- ran$region[row, 3] + fix[3]
    beta.urban <- ran$region[row, 4] + fix[4]
    
    # Calculate the prediction
    df <- subset(dat, region==r)
    y.hat <- i.val + beta.pop%*%t(df$population) + beta.gdp%*%t(log(df$gdp)) + 
      beta.urban%*%t(logit(df$urban/100))
      
    y <- apply(y.hat, 2, function(x) rnorm(n.sims, x, sd(mod@resid)))
    
    # Calculate the summary statistics 
    alpha <- 1-ci
    tmp.result <- adply(y, 2, function(x) data.frame(lwr=quantile(x, alpha/2),
                                       fit=mean(x),
                                       upr=quantile(x, 1-alpha/2)))
    tmp.result <- cbind(tmp.result, df)
    results <- rbind(results, tmp.result)
  }
  results <- results[,c("year", "region", "fit","lwr","upr")]

  return(results)
}


## Gets pre-saved model results
##
## @return a list
load_top_down_results <- function() {
  G <- load("../data/global-model.Rda")
  R <- load("../data/regional-model.Rda")
  F <- load("../data/fuel-model.Rda")
  models <- list(global=get(G), regional=get(R), fuels=get(F))
  return(models)
}

## Shows a summary of the models
##
## Generates tables showing estimates of global energy consumption,
## regional energy consumption, and regional emissions.
##
## @param a list containing the models and their predictions
show_top_down_results <- function(models) {
  require(xtable)

  ## Identify each model for easy use below
  G <- models$global
  R <- models$regional
  F <- models$fuels

  ## Set table options
  tblOptions <- getOption("xtable.html.table.attributes",
                          "border=1 width=400")
  
  ## Table 1
  ## ====================================
  R.global <- ddply(R$pred, .(year), summarize, fit=sum(fit), lwr=sum(lwr), upr=sum(upr))
  F.global <- ddply(F$pred, .(year), summarize, fit=sum(fit), lwr=sum(lwr), upr=sum(upr))
  df <- data.frame(Model=c("Global", "Regional", "Fuels", "IEA Technology Roadmap"),
                   Estimate=c(predict_to_string(G$pred),
                     predict_to_string(R.global),
                     predict_to_string(F.global),
                     184))
  tbl1 <- xtable(df,
                 caption="Summary of global building energy demand prediction (in EJ, with 95% confidence intervals)",
                 align="llr")
  print(tbl1, include.rownames=FALSE, type='html', html.table.attributes=tblOptions)

  ## Table 2
  ## ====================================
  R.region <- ddply(R$pred, .(region), predict_to_string)
  F.tmp <- ddply(F$pred, .(region, year), summarize, fit=sum(fit), lwr=sum(lwr), upr=sum(upr))
  F.region <- ddply(F.tmp, .(region), predict_to_string)

  regions <- merge(R.region, F.region, by="region")
  names(regions) <- c("Region", "Regional model", "Fuels model")
  tbl2 <- xtable(regions,
                 caption="Summary of regional building energy predictions. Values are central estimates
for 2050 including 95% confidence intervals.",
                 align="llrr")
  print(tbl2, include.rownames=FALSE, type='html', html.table.attributes=tblOptions)
  
  ## Table 3
  ## ====================================
  ## Calculate the emissions
  efs <- read.csv("../data/emission-factors.csv")
  names(efs) <- c("region", "fuel", "ef")
  tmp <- melt(F$pred, id=c("year", "region", "fuel"), variable="prediction", value.name="energy")
  tmp <- merge(tmp, efs)
  tmp <- mutate(tmp, emissions=ef*energy)
  tmp2 <- dcast(tmp, year + region + fuel ~  prediction, value.var="emissions")
  tmp2 <- ddply(tmp2, .(year, region), summarize, fit=sum(fit)/1000, lwr=sum(lwr)/1000, upr=sum(upr)/1000)

  total <- ddply(tmp2, .(year), summarize, fit=sum(fit), lwr=sum(lwr), upr=sum(upr))

  ## The regional totals
  regional_emissions <- ddply(tmp2, .(region), predict_to_string, digits=2)
  ## The overall totals
  total_emissions <- data.frame(region="Total", V1=predict_to_string(total, digits=2))
  ## The IEA prediction
  iea_emissions <- data.frame(region="IEA Technology Roadmap", V1=15.2)
  
  tbl3.data <- list(regional_emissions, total_emissions, iea_emissions)
  tbl3.data <- do.call("rbind", tbl3.data)
  names(tbl3.data) <- c("Region", "Emissions (Gt CO2)")

  ## Make the final table
  tbl3 <- xtable(tbl3.data,
                 caption="Predicted 2050 emissions from the buildings sector (by region and with global
total and reference value).",
                 align="llr")
  print(tbl3, include.rownames=FALSE, type='html', html.table.attributes=tblOptions)
  
}
