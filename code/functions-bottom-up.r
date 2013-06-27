## Functions for the bottom-up model


## Calculates residential space heating demands
##
## Calculates residential space heating demand by region and year, as the product of activity drivers similar to the ASIF framework for greenhouse gas emissions (Schipper 1999, Isaac 2009)
## @param data a data frame giving the properties of each region
## @param fuel a data frame giving the existing fuel consumption in each region
## @return a data frame with the space heat demand in EJ by region and global total
calculate_residential_space_heat <- function(data, fuel) {

  ## Units in data are k people (pop), m^2/person (floorcap), hdd (hdd), kJ/m^2.hdd (intensity), and % (eff)
  ## Calculate the space heating demand in 2010, 2050 (LMS), and 2050 (LCS)
  df <- mutate(data, sh.energy=1000*pop*floorcap*hdd*intensity/(eff/100)/1e15)

  ## Add a global total column for each year and scenario
  df <- ddply(df, .(year, scenario), transform, global.sh.energy=sum(sh.energy))

  ## Calculate the fuel share in each region for the LCS scenario
  shares <- ddply(fuel, .(year, region), summarize, fuel=fuel, fuel_share=fit/sum(fit))

  ## Calculate the resulting energy consumption
  tmp2 <- merge(df, shares)
  tmp2 <- mutate(tmp2, sh.fuel=sh.energy*fuel_share)

  ## Cast the data to calculate the savings
  tmp2 <- dcast(subset(tmp2, year==2050), region + year + fuel ~ scenario, value.var="sh.fuel")
  names(tmp2) <- c("region", "year", "fuel", "LMS", "LCS")
  tmp2 <- mutate(tmp2, savings=LMS-LCS)

  ## Return the result
  return(tmp2)
}

  
## Calculates the heat production from GSHPs
##
## @param df a dataframe giving the ground-source heat pump parameters
## @param fuel a dataframe giving the fuel share by region
## @return a dataframe giving the change in energy consumption by region and fuel type
calculate_GSHP_heat <- function(df, fuel) {
  ## First calculate the total amount of energy delivered and the required electricity
  tmp <- mutate(df, energy=households*penetration*size*cap.factor*8760*3.6/1e12,
                elec=-energy/COP)

  ## Then work out the savings
  tmp <- subset(tmp, year==2050)

  ## Calculate the saving in each region
  tmp <- transform(tmp, scenario=ifelse(scenario=="", "LMS", "lowC"))
  tmp <- tmp[,c("year", "scenario", "region", "energy", "elec")]
  tmp.m <- melt(tmp, id=c("year", "scenario", "region"))
  savings <- ddply(tmp.m, .(year, region, variable), function(df) {
    dcast(df, year + region ~ scenario, value.var="value")})
  savings <- transform(savings, saving=lowC-LMS)
  
  ## Calculate the fuel share in each region for the LCS scenario
  shares <- ddply(fuel, .(year, region), summarize, fuel=fuel, fuel_share=fit/sum(fit))
  ## Adjust shares removing the electricity component
  tmp2 <- ddply(shares, .(year, region), summarize, fuel=fuel, fuel_share=ifelse(fuel=="elec", 0, fuel_share))
  shares <- ddply(tmp2, .(year, region), summarize, fuel=fuel, fuel_share=fuel_share/sum(fuel_share))
  shares <- transform(shares, variable=ifelse(fuel=="elec", "elec", "energy"))
  ## Calculate the resulting energy consumption
  tmp2 <- merge(savings, shares)
  tmp2 <- mutate(tmp2, gshp.energy=ifelse(fuel=="elec", saving, saving*fuel_share))

  ## Drop some unloved columns
  tmp2 <- tmp2[,c("year", "region", "fuel", "gshp.energy")]

  ## Return the result
  return(tmp2)
}
  
## Calculates the savings from increased electricity efficiency
##
## Calculates the savings from assumed increases in efficiency of electrical appliances and lighting.
## @param df a dataframe giving the share of electricity use in each region
## @return a dataframe giving the electrical efficiency savings in EJ
calculate_electrical_savings <- function(df) {
  ## Have to add back the heat electricity to the saved electricity
  df <- mutate(df, elec_eff=elec_comm*comm_nonheatelec*eff + elec_resi*res_nonheatelec*eff +
               elec_comm*(1-comm_nonheatelec) + elec_resi*(1 - res_nonheatelec))

  ## Remove unused columns and years
  df <- df[,c("year", "region", "fit", "fuel", "elec_eff")]
  df <- subset(df, year==2050)
  
  ## Calculate the savings  
  result <- transform(df, saving=fit - elec_eff)
  return(result)
}

## Calculates fuel switching savings
##
## Calculates the change in fuel demands for given assumptions about switching away from fossil fuels for space and water heating.  Currently assumes that 30% of total demand is for heat and that 50% of fossil fuel demand is displaced to electricity, solar, and biomass heating (a 3:1:1 ratio).
## @param df a dataframe giving the existing heat fuel mixes
## @return a dataframe giving the changes in demands after switching by region, year and fuel type (EJ)
calculate_fuel_switching <- function(df) {
  ## Label by fossil and nonfossil fuel type
  df <- mutate(df, fuel_type=ifelse(fuel %in% c("coal", "gas", "oil"), "fossil",
                     ifelse(fuel=="heat", "heat", "nonfossil")))
  
  ## Calculate the relative transfers
  ## Only transfer from fossil to non-fossil
  tmp <- ddply(df, .(region, fuel, year, scenario), transform,
               change=ifelse(fuel_type=="fossil", -1, 0)*displaced*heat)
  
  ## This amount now needs to be redistributed to each of the nonfossil sources
  ## Assume the following redistribution weights
  weights <- data.frame(fuel=c("biomass", "coal", "elec", "gas", "heat", "oil", "renew"),
                        weight=c(0.2, 0, 0.6, 0, 0, 0, 0.2))
  tmp <- merge(tmp, weights)

  ## Then do the calculation
  tmp2 <- ddply(tmp, .(region, year, scenario), transform, fossil_change=-sum(change))
  tmp3 <- mutate(tmp2, change=ifelse(fuel_type=="nonfossil", weight*fossil_change, change))

  ## Calculate the new amount of heat for each fuel type
  tmp3 <- mutate(tmp3, new_heat=heat + change)

  ## Sanity check
  cat(sprintf("Original heat demand = %.2f EJ\n", sum(df$fit)))
  cat("Summary of change in fuel allocations:\n")
  print(ddply(tmp3, .(fuel_type, fuel), summarize, change=sum(change)))

  ## Return just the changes in heat demand for each region
  result <- tmp3[,c("year", "scenario", "region", "fuel", "change")]
  return(result)
}

## Calculates a summary of the LMS and LCS scenarios
##
## Calculates the energy demand from the global buildings sector by region, for the year 2050, under the low mitigation scenario (LMS) and low carbon scenario (LCS).
## @param fuel a dataframe giving the energy consumption by fuel type from the top-down model
## @param savings a dataframe giving the savings in energy consumption by fuel type and intervention
## @return a dataframe giving the energy consumption by fuel in the LMS and LCS
calculate_summary <- function(fuel, savings) {

  ## First calculate the demand in the LMS case
  LMS <- subset(fuel, year==2050)

  ## Then create a copy for the LCS by merging the LMS with the interventions
  savings <- mutate(savings, efficiency=replace(efficiency, is.na(efficiency), 0))
  LCS <- merge(LMS, savings)

  ## Show total savings by intervention
  tmp <- melt(savings, id=c("year", "region", "fuel"))
  total_saving <- ddply(tmp, .(year, variable), summarize, saving=sum(value))
  total_saving <- mutate(total_saving, saving=round(saving, 3))
  print(ddply(tmp, .(year, variable), summarize, saving=round(sum(value),3)))
  
  ## Calculate the resulting savings
  LCS <- transform(LCS, new_energy=fit - space_heat - gshp - efficiency - shifting - carbon)

  ## Tidy the labels
  names(LCS) <- c("region", "fuel", "year", "LMS", "space_heat", "gshp",
                  "efficiency", "shifting", "carbon", "LCS")
  return(LCS)
}

## Calculates the emissions associated with 2050 energy demands
##
## Calculates global CO2 emissions for global energy consumption.  Emissions factors are based on a separate optimization model of the global power sector, see Shah et al (2013).
## @param df a dataframe with the energy demands (in EJ) by region, fuel and scenario
## @return a dataframe with the emissions (in GtCO2) by region, fuel, and scenario
calculate_emissions <- function(df) {

  ## Load the emission factors and melt by scenario
  efs <- read.csv("../data/LCS_emission-factors.csv")
  efs <- melt(efs, id=c("region", "fuel"), variable.name="scenario",
              value.name="ef")

  ## Rearrange the energy demands to match
  ## Note that all of the interventions should be allocated LMS emission factors
  df2 <- melt(df, id=c("year", "region", "fuel"), variable.name="intervention",
              value.name="energy")
  df2 <- transform(df2, scenario=ifelse(intervention=="LCS", "LCS", "LMS"))
    
  ## Merge these with the energy demands
  tmp <- merge(df2, efs)

  ## Calculate the emissions (in Gt CO2)
  tmp <- transform(tmp, emissions=energy*ef/1000)

  return(tmp)
}

## Create a waterfall plot
##
## Creates a waterfall plot.  Assumes that the data is in a dataframe according to LMS - space_heat - gshp - efficiency - carbon - shifting = LCS
## @return a ggplot object
make_waterfall_plot <- function(df) {

  ## First we want to do this globally so
  df.m <- melt(df, id=c("region", "fuel", "year"))
  df.m <- ddply(df.m, .(year, variable), summarize, total=round(sum(value), 3))

  ## Drop the LMS and LCS
  test <- subset(df.m, !is.element(variable, c("LMS", "LCS")))
  
  ## Add in the residential/commercial splits
  test <- transform(test, commercial=0, residential=1)
  elec_data <- subset(fuel_data, fuel=="elec")
  elec_share <- load_electricity_share_data(elec_data)
  tmp <- ddply(elec_share, .(year), summarize,
               comm=sum(elec_comm*comm_nonheatelec),
               resi=sum(elec_resi*res_nonheatelec))
  tmp.m <- melt(tmp, id="year")
  tmp.m <- subset(tmp.m, year==2050)
  tmp.m <- transform(tmp.m, value=value/sum(value))
  elec.tmp <- dcast(tmp.m, year ~ variable, value.var="value")
  test <- transform(test,
                    commercial=ifelse(variable=="efficiency", elec.tmp$comm, commercial),
                    residential=ifelse(variable=="efficiency", elec.tmp$resi, residential))

  ## Calculate total savings by sector
  test.m <- melt(test, id=c("year", "variable", "total"), variable.name="sector")
  test <- dcast(test.m, year + variable ~ sector, value.var="value")
  save <- ddply(test.m, .(sector), summarize, savings=sum(total*value), share=weighted.mean(value, total))

  ## So these are the middle value
  df2 <- merge(df.m, test)

  ## Then the LMS value
  sector_share <- load_sector_share_data()
  df.lms <- cbind(subset(df.m, variable=="LMS"), sector_share)

  ## Then the LCS value
  lcs.comm <- with(df.lms, total*commercial) - subset(save, sector=="commercial")$savings
  lcs.res <- with(df.lms, total*residential) - subset(save, sector=="residential")$savings  
  df.lcs <- cbind(subset(df.m, variable=="LCS"), commercial=lcs.comm/(lcs.comm+lcs.res), residential=lcs.res/(lcs.comm + lcs.res))
  
  ## Put it all together
  l <- list(df.lms, df2, df.lcs)
  df.m <- do.call("rbind", l)

  min.total <- with(df.m, c(0, head(total, 1) - cumsum(tail(total, -1))))
  max.total <- with(df.m, c(head(total, 1), rev(head(cumsum(rev(total)), -1))))

  waterfall.df <- cbind(df.m, min=min.total, max=max.total)
  
  ## Tidy up the factors
  cats <- c("LMS", "Space heating", "GSHPs", "Elec efficiency",
            "Fuel switching", "Decarbonizing grid", "LCS")
  waterfall.df <- mutate(waterfall.df, variable=factor(variable,
                                         labels=cats,
                                         lev=c("LMS", "space_heat", "gshp",
                                           "efficiency", "shifting",
                                           "carbon", "LCS")))
  

  waterfall.m <- melt(waterfall.df, id=c("year", "variable", "total", "min", "max"), variable.name="sector")
  waterfall.m <- mutate(waterfall.m, energy=total*value)

  ## STOPPED HERE
  ## The problem is that I have to correct the max and min values for
  ## each sector, i.e. LMS resi = (min, 143), LMS comm = (143, max).
  ## It might help to keep the connecting lines in a separate data frame
  ## Also strip out the data frame manipulation from this method.
  ## Should be able to pass it something like:
  ## order, category, fill, value
  ## Write this function first
  offset <- 0.3
  gg <- ggplot(waterfall.df) + 
    geom_rect(aes(ymin=min, ymax=max, 
                  xmin=as.numeric(variable) - offset,
                  xmax=as.numeric(variable) + offset)) +
                    geom_segment(data=tail(waterfall.df, n=nrow(waterfall.df)-1),
                                 aes(x=as.numeric(variable) + offset - 1,
                                     xend=as.numeric(variable) + 1 - offset - 1,
                                     y=max,
                                     yend=max), linetype="dashed") +
                                       scale_x_continuous(breaks=1:length(cats), labels=cats) +
                                         theme_bw() +
                                           labs(x="", y="Value")
  return(gg)
}
