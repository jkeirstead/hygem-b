## Functions for loading the data needed by the bottom-up model

## Loads heating degree day data 
##
## Loads population-weighted heating degree days for world regions in 2010, based on a 18 deg Celsius base temperature

## @return a data frame with the number of heating degree days per year by region
## @source Baumert and Selman, WRI 2003.  Note: Heating and cooling degree days, United Nations, Department of Economic and Social Affairs, Population Division (2011).
load_hdd_data <- function() {
  return(read.csv("../data/hdd.csv"))
}

## Loads space heating intensity values for different scenarios
##
## Gets the space heating intensity values for 2010, 2050 low mitigation (LMS) and low carbon (LCS) scenarios.
##
## @detail Data are calibrated such that residential space heating comprises 54-59 % of total final demand in the residential sector). In the IEA 15, 54% of end use was space heating in 2004, compared with 59% in 1990. Residential sector represents 75% of total building sector demand in 2009.
## @return a data frame with the year, scenario, region, and heating intensity in kJ useful energy per square meter per heating degree day.
## @source IEA (2007) Energy use in the new millenium; Shen (2006) 2030 Energy demand scenarios and biomass CHP options for rural households in Beijing [M.Sc. thesis], Zhou et al LBNL (2008) Current status and future scenarios of residentiaL energy consumption in China.  \link{http://www.esds.ac.uk/findingData/snDescription.asp?sn=6301&key=}
load_space_heat_intensity_data <- function() {
  df <- read.csv("../data/spaceheat_intensity.csv")
  df <- cbind(df, colsplit(df$year, "\\.", c("year", "scenario")))
  df <- df[,-1]
  df <- df[,c("year", "scenario", "region", "heating.intensity")]
  return(df)
}

## Load the efficiency of residential heating by fuel
##
## @return a data frame giving heating efficiency (%) by year and fuel type
## @source Isaac and Vurren (2009) Modeling global residential sector energy demand for heating and air conditioning, Energy Policy, Table 1: 2005 and 2050 values for efficiency
load_space_heat_efficiency_data <- function() {
  return(read.csv("../data/spaceheat_efficiency.csv"))
}

## Loads the estimated fuel consumption in each region
##
## Loads the estimated fuel consumption in each region for 2010 (observed data) and 2050 (simulated by top-down model).  
load_fuel_data <- function() {
  file <- "../data/fuel-predictions.csv"
  if (file.exists(file)) {
    fuel_data <- read.csv(file)
  } else {
    file <- "../data/archived-fuel-predictions.csv"
    fuel_data <- read.csv(file)
  }

  ## Tidy up the fuel data 
  fuel_data <- subset(fuel_data, year %in% c(2010,2050))
  fuel_data <- fuel_data[,c("fit","region","fuel","year")]

  ## Change the fuels to all lower case so that we can later merge
  ## with efficiency data
  levels(fuel_data$fuel) <- c("biomass","coal","elec","gas",
                              "heat","oil","other","renew","uranium")
  
  ## Omit "other" and "Uranium"
  fuel_data <- subset(fuel_data, !is.element(fuel, c("other", "uranium")))
  return(fuel_data)
}

## Calculates GDP per capita in each region
##
## Calculates GDP per capita in each region in 2010 and 2050.  Uses the same underlying data as the top-down model.
## @return a data frame with GDP per capita (USD 2010) and population (000s) by region and year
## @seealso the data functions of the top down model
load_gdp_per_capita_data <- function() {

  input_data <- read.csv("../data/un-input-data.csv")
  tmp <- subset(input_data, year %in% c(2010, 2050),
                 select=c(region, year, gdp, population))
  tmp <- mutate(tmp, gdpcapita=gdp/(population*1000))
  tmp <- subset(tmp, select=c(region, year, population, gdpcapita))
  return(tmp)
}

## Calculates floor area
##
## Calculates the amount of floor space per capita based on empirical function of GDP per capita
## @param gdp a vector giving GDP per capita values
## @return a vector giving corresponding floor area in metres squared per capita
## @detail See Isaac and Vurren (2009) Modeling global residential sector energy demand for heating and air conditioning, Energy Policy for the method.
calculate_floor_area <- function(gdp) {
  floor.area <- 6.33*log(gdp)-28.95
  return(floor.area)
}

## Load ground-source heat pump data
##
## Loads in data on the estimated penetration of ground-source heat pumps in different regions.  For all regions, it is assumed that the rated capacity is 12 kW, with an annual capacity factor of 18%, and a coefficient of performance of 2.4 (Bertrani 2009, Staffell et al 2012).
## @detail 2050 values are based on a linear extrapolation of 2010--2030 trend in households
## @return a data frame with estimates of heat pump penetration and specs by region and year
## @source Household numbers from United Nations 2009 Global report on human settlements Table A.4/B.9.  Other sources: Bertrani (2009) Long-term projections of geothermal-electric development in the world \link{http://www.iea-gia.org/documents/LongTermGeothermElecDevelopWorldBertanioffenburg23Feb09.pdf}.  Staffell et al 2012 EES.
load_gshp_data <- function() {
  df <- read.csv("../data/gshp.csv")
  df <- cbind(df, colsplit(df$year, "\\.", c("year", "scenario")))
  df <- df[,-1]
  df <- df[,c("year", "scenario", "region", "households", "penetration", "size", "cap.factor", "COP")]
  return(df)
}

## Calculate the relative share of demand from commercial and residential sectors
##
## Loads historical shares of commercial and public services energy demand in the building sector.
## @return a data frame giving the global average share of building energy demand in the commercial and residential sectors (fraction)
## @source IEA \link{http://www.esds.ac.uk/findingData/snDescription.asp?sn=6301&key=}
load_sector_share_data <- function() {
  df <- read.csv("../data/comm_share.csv")
  df <- df[,c("year","Global")]
  df <- subset(df, year==2009)
  return(data.frame(commercial=df$Global, residential=1-df$Global))
}

## Calculate the relative share of electricity demand from commercial and residential sectors
##
## Loads historical shares of commercial and residential electricity demand in the building sector.
## @param elec_data a data frame giving electricity demand by region and year
## @return a data frame giving the global average share of building electricity demand in the commercial and residential sectors (fraction)
## @source IEA \link{http://www.esds.ac.uk/findingData/snDescription.asp?sn=6301&key=}
load_electricity_share_data <- function(elec_data) {
  ## Load in the commercial share data from the IEA
  df <- read.csv("../data/comm_share.csv")
  df <- df[,-12]
  df <- subset(df, year==2009)

  library(reshape2)
  df2 <- melt(df, id="year", variable.name="region", value.name="comm_share")
  df2 <- mutate(df2, res_share=1-comm_share)

  ## Pretend the year is actually 2010 to merge with electricity data
  df2$year <- 2010

  ## Convert region names to match standard ones
  library(stringr)
  df2$region <- str_replace_all(df2$region, "\\.", " ")
  df2$region <- str_replace(df2$region, "Non OECD", "Non-OECD")
  df2$region <- str_replace(df2$region, "Sub Saharan", "Sub-Saharan")

  ## Make a copy of the data for 2050
  tmp <- df2
  tmp$year <- 2050
  shares <- rbind(df2, tmp)

  ## Merge the shares with the electricity data
  tmp <- merge(elec_data, shares)
  tmp <- mutate(tmp, elec_comm=fit*comm_share, elec_resi=fit*res_share)

  ## Calculate the non-heat share of electricity
  ## Source: DOE 2011 Buildings energy data book
  ## 30% of residential building sector elec demand for heating, 10% of commercial
  tmp <- cbind(tmp, comm_nonheatelec=0.9, res_nonheatelec=0.7)
  ## Assume a 30% savings available from energy efficiency in lights and appliances
  tmp <- cbind(tmp, eff=0.7)
  return(tmp)
}


## Loads fuel demands for space and water heating
##
## @param fuel_data the data frame containing the fuel data by region and year
## @detail Assume that the share of demand for space and water heating is 30% overall (DOE 2011).  Estimate a 30% reduction in coal, gas, and oil by increased electricity use, 10% use of solar heating, and 10% biomass.
## @source DOE. Buildings Energy Data Book. U.S. Department of Energy, 2011. \link{http://buildingsdatabook.eren.doe.gov/}
load_heat_fuel_data <- function(fuel_data) {
  df <- subset(fuel_data, year==2050)

  ## Add new parameters
  df <- transform(df, heat_share=0.3, displaced=0.5, scenario="lowC")
  ## Calculate heat fuel demand
  df <- transform(df, heat=fit*heat_share)
  return(df)
}
