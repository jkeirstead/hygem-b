## General functions
## ----------------------------------------

## Make sure the output directory exists
outdir <- "../output"
if (!file.exists(outdir)) dir.create(outdir)

## Defines the regions for later analysis.

## This functions retrieves a sample data set from the World Bank data
## bank and uses the country and regional definitions contained
## therein to define the regions for the present analysis.  There are
## some minor changes to the default World Bank regions to reflect the
## availability of statistics from other data sources, most notably
## the IEA Energy Technology Perspective report.
##
## Some duplicates of the names are manually added to capture differences
## between the World Bank and UN naming conventions.
##
## Params:
##	debug	a boolean, if TRUE prints a summary of the countries not assigned to a region
## Returns:
## 	a data frame containing "region" and "country" columns
define_regions <- function(debug=FALSE) {
  
  ## Load required library for World Bank data access
  require(WDI)

  ## Select the GDP indicator as a high-coverage indicator
  gdp_code <- "NY.GDP.MKTP.CD"
  gdp_data <- WDI(ind=gdp_code, extra=TRUE)
  
  ## Load required library for plyr
  require(plyr)
  
  ## Split the data by region and come up with a list for all of the countries
  tmp <- dlply(gdp_data, .(region, income), summarize, country=unique(country))
  
  ## Define regions
  china <- list(region="China", country=c("China", "Hong Kong SAR", "Macao SAR", "China, Hong Kong SAR",
                                  "China, Macao SAR", "Hong Kong"))

  india <- list(region="India", country="India")

  oecd_europe <- list(region="OECD Europe", 
                    country=c(tmp['Europe & Central Asia (all income levels).High income: OECD'][[1]]$country, 
                      "Turkey", "Israel", "Slovakia", "Channel Islands", "Faeroe Islands", "Isle of Man", "Andorra", "Gibraltar", "Holy See", "San Marino", "Liechtenstein", "Monaco"))

  oecd_asia_oceania <- list(region="OECD Asia Oceania",
                          country=c(tmp['East Asia & Pacific (all income levels).High income: OECD'][[1]]$country,
                            "Republic of Korea", "Korea"))

  oecd_north_america <- list(region="OECD North America",
                             country=c(tmp['North America.High income: OECD'][[1]]$country,
                               "United States of America"))

  latin_america <- list(region="Latin America",
                      country=c(
                        tmp['Latin America & Caribbean (all income levels).High income: nonOECD'][[1]]$country, 
                        tmp['Latin America & Caribbean (all income levels).Low income'][[1]]$country, 
                        tmp['Latin America & Caribbean (all income levels).Lower middle income'][[1]]$country, 
                        tmp['Latin America & Caribbean (all income levels).Upper middle income'][[1]]$country,
                        "Bermuda", "Anguilla", "Bahamas", "British Virgin Islands", "Guadeloupe", "Martinique",
                        "Montserrat", "Netherlands Antilles", "Saint Kitts and Nevis", "Saint Lucia",
                        "Saint Vincent and the Grenadines", "United States Virgin Islands",
                        "Bolivia (Plurinational State of)", "Falkland Islands (Malvinas)", "French Guiana",
                        "Venezuela (Bolivarian Republic of)", "Venezuela"))
  
  eastern_europe <- list(region="Eastern Europe",
                       country=c(tmp['Europe & Central Asia (all income levels).Low income'][[1]]$country,
                         tmp['Europe & Central Asia (all income levels).Lower middle income'][[1]]$country,
                         tmp['Europe & Central Asia (all income levels).Upper middle income'][[1]]$country,
                         "Croatia", "Cyprus",
                         "Former Soviet Union (If no detail)",
                         "Former Yugoslavia (If no detail)",
                         "Kyrgyzstan", "Republic of Moldova", "TFYR Macedonia",
                         "Former Yugoslav Republic of Macedonia"))
  ## Remove Turkey
  eastern_europe$country <- with(eastern_europe, country[!is.element(country, c("Turkey"))])
  
  non_oecd_asia <- list(region="Non-OECD Asia",
                       country=c(tmp['East Asia & Pacific (all income levels).Low income'][[1]]$country,
                         tmp['East Asia & Pacific (all income levels).Lower middle income'][[1]]$country,
                         tmp['East Asia & Pacific (all income levels).Upper middle income'][[1]]$country,
                         tmp['South Asia.Low income'][[1]]$country,
                         tmp['South Asia.Lower middle income'][[1]]$country,
                         "Brunei Darussalam", "Singapore",
                         "Dem. People's Republic of Korea", "Maldives", "Lao People's Democratic Republic",
                         "Viet Nam", "Melanesia", "New Caledonia", "Micronesia", "Guam",
                         "Micronesia (Fed. States of)", "Nauru", "Northern Mariana Islands", "Polynesia",
                         "Cook Islands", "French Polynesia", "Niue", "Tokelau", "Wallis and Futuna Islands",
                         "Chinese Taipei", "Korea, DPR"))
  ## Remove China and India from this
  non_oecd_asia$country <- with(non_oecd_asia, country[!is.element(country, c("China", "India"))])

  mena <- list(region="MENA",
             country=c(tmp['Middle East & North Africa (all income levels).High income: nonOECD'][[1]]$country,
               tmp['Middle East & North Africa (all income levels).High income: OECD'][[1]]$country,
               tmp['Middle East & North Africa (all income levels).Lower middle income'][[1]]$country,
               tmp['Middle East & North Africa (all income levels).Upper middle income'][[1]]$country,
               "Egypt", "Libyan Arab Jamahiriya", "Western Sahara",
               "Iran (Islamic Republic of)", "Occupied Palestinian Territory", "Yemen",
               "Islamic Republic of Iran"))
  
  ## Remove Israel
  mena$country <- with(mena, country[!is.element(country, c("Israel"))])

  ss_africa <- list(region="Sub-Saharan Africa",
                  country=c(tmp['Sub-Saharan Africa (all income levels).Low income'][[1]]$country,
                    tmp['Sub-Saharan Africa (all income levels).Lower middle income'][[1]]$country,
                    tmp['Sub-Saharan Africa (all income levels).Upper middle income'][[1]]$country,
                    tmp['Sub-Saharan Africa (all income levels).Not classified'][[1]]$country,
                    "Mayotte", "Réunion", "United Republic of Tanzania", "Congo",
                    "Democratic Republic of the Congo", "Democratic Republic of Congo", "Equatorial Guinea",
                    "Côte d'Ivoire", "Gambia"))


  ## Assemble all of these sub-regions into a single list
  all_regions <- list(china, india, oecd_europe, oecd_asia_oceania, oecd_north_america, latin_america,
                      eastern_europe, non_oecd_asia, mena, ss_africa)

  ## If the user wants it, print a summary of the countries without a matching region
  if (debug) {

    ## Create vectors of selected and all countries
    selected_countries <- ldply(all_regions, summarize, country=country)$country
    all_countries <- ldply(tmp, summarize, country=country)$country

    ## Calculate which ones are missing
    missing_countries <- setdiff(all_countries, selected_countries)
    n_missing_countries <- length(missing_countries)
    missing_countries <- paste(missing_countries, collapse=", ")
    
    ## Print the results
    if (length(missing_countries)>0) {
      warning(paste("The following", n_missing_countries,
                    "countries are not assigned to a region:\n", missing_countries))
    } else {
      print("All countries assigned to a region.")
    }
  }

  ## Compress the all_regions list into a data frame and return
  countries <- ldply(all_regions, function(l) data.frame(region=l$region, country=l$country))
  return(countries)
  
}

## Forecast a value based on exponential growth.  The function allows for different growth rates in each period.
##
## Params:
## 	value   	the values to predict.  NA values will be filled in
##	growth_rate	the percent growth (i.e. 8% passed as 8.0)
forecast_value <- function(values, growth_rate) {
  for (i in 2:length(values)) {
    if (is.na(values[i]) ) {
      values[i] <- values[i-1]*(1 + growth_rate[i]/100)
    }
  }
  return (values)
}

## Converts predicted regression results to a formatted string
## Assumes an additional column containing the year of the prediction
##
## Params
##	df 	the prediction data frame
##	y	the target prediction year
##	digits  the number of digits in the string format
##
## Returns:
## 	a formatted string of the form fit +/- 95% PI
predict_to_string <- function(df, y=2050, digits=1) {
  tmp <- subset(df, year==y)
  result <- paste(round(tmp$fit, digits),
                  round((tmp$upr-tmp$lwr)/2, digits), 
                  sep=" +/- ")
  return(result)
}

## The logit function
##
## Logistic transformation to convert values between 0 and 1 to
## values between -Infinity and Infinity.
## Param:
##	a vector of values between 0 and 1
## Return:
## 	a vector of values between -Inf and +Inf
logit <- function(x) {
  return(log(x/(1-x)))
}

## Generates a waterfall plot
##
## @param df the original dataframe
## @return a ggplot object
make_waterfall <- function(df) {
  cats <- c("LMS", "Space Heat", "GSHP", "Electrical", "LCS")
  df <- data.frame(cat=factor(
                     c("LMS", "Space Heat", "GSHP", "Electrical", "LCS"),
                     lev=cats),
                   min=c(0, 150, 140, 130, 0),
                   max=c(200, 200, 150, 140, 130))
  
  offset <- 0.3
  gg <- ggplot(df) + 
    geom_rect(aes(ymin=min, ymax=max,
                  xmin=as.numeric(cat) - offset,
                  xmax=as.numeric(cat) + offset)) +
                    geom_segment(data=tail(df, n=nrow(df)-1),
                                 aes(x=as.numeric(cat) + offset - 1,
                                     xend=as.numeric(cat) + 1 - offset - 1,
                                     y=max,
                                     yend=max), linetype="dashed") +
                                       scale_x_continuous(breaks=1:5,labels=cats) +
                                         theme_bw() +
                                           labs(x="", y="Value")

  return(gg)
}
