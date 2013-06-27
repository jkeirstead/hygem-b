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
  ## This calculate only works for the LMS and LCS scenarios
  df <- df[,c("region", "fuel", "year", "LMS", "LCS")]
  df <- melt(df, id=c("year", "region", "fuel"), variable.name="scenario",
              value.name="energy")
    
  ## Merge these with the energy demands
  tmp <- merge(df, efs)

  ## Calculate the emissions (in Gt CO2)
  tmp <- transform(tmp, emissions=energy*ef/1000)

  return(tmp)
}

## Splits final energy demands by commercial and domestic sectors
##
## @param df a dataframe containing the results by intervention
split_by_sector <- function(df) {

  ## First tabulate the commercial and residential splits by intervention
  LMS.share <- load_sector_share_data()

  ## Calculate the change in demand in the electrical efficiency measure
  fuel_data <- load_fuel_data()
  elec_data <- subset(fuel_data, fuel=="elec")
  elec_share <- load_electricity_share_data(elec_data)
  tmp <- ddply(elec_share, .(year), summarize,
               comm=sum(elec_comm*comm_nonheatelec),
               resi=sum(elec_resi*res_nonheatelec))
  tmp.m <- melt(tmp, id="year")
  tmp.m <- subset(tmp.m, year==2050)
  tmp.m <- transform(tmp.m, value=value/sum(value))
  elec.share <- dcast(tmp.m, year ~ variable, value.var="value")

  ## Calculate the resulting LCS shares
  init_splits <- data.frame(measure=c("LMS", "space_heat", "gshp", "efficiency",
                              "shifting", "carbon", "LCS"),
                            commercial=c(LMS.share$commercial, 0, 0, elec.share$comm, 0, 0, 1),
                            residential=c(LMS.share$residential, 1, 1, elec.share$resi, 1, 1, 1))
  
  tmp <- melt(df, id=c("year", "region", "fuel"), variable.name="measure")
  tmp <- merge(tmp, init_splits)
  tmp <- subset(tmp, !is.element(measure, c("LMS", "LCS")))
  tmp.m <- melt(tmp, id=c("year", "region", "fuel", "measure", "value"),
              variable.name="sector", value.name="share")
  savings <- ddply(tmp.m, .(sector), summarize, ints=sum(value*share))

  ## Calculate LMS demands
  LMS.demand <- data.frame(LMS=as.numeric(LMS.share*sum(df$LMS)),
                           sector=names(LMS.share))

  tmp <- merge(LMS.demand, savings)
  tmp <- transform(tmp, LCS=LMS-ints)
  tmp <- transform(tmp, share=LCS/sum(LCS))

  id <- which(init_splits$measure=="LCS")
  splits <- init_splits
  splits[id, 2:3] <- tmp$share
  
  ## Then put it all together
  df.m <- melt(df, id=c("region", "fuel", "year"),
               variable.name="measure", value.name="energy")
  tmp <- merge(df.m, splits)
  tmp.m <- melt(tmp, id=c("measure", "region", "fuel", "year", "energy"),
                variable.name="sector", value.name="share")
  tmp.m <- transform(tmp.m, value=energy*share)

  final <- dcast(tmp.m, region + fuel + year + sector ~ measure, value.var="value")
  
  return(final)
}
