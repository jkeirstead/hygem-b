## @knitr load-bottom-up-data
hdd <- load_hdd_data()
space_heat_intensity <- load_space_heat_intensity_data()
space_heat_efficiency <- load_space_heat_efficiency_data()
fuel_data <- load_fuel_data()

## Calculate consumption weighted efficiency in each region
fuel_efficiency <- merge(fuel_data, space_heat_efficiency)
region_efficiency <- ddply(fuel_efficiency, .(region,year), summarize,
                    eff=sum(fit*efficiency)/sum(fit))

## Then we need GDP, population, and GDP/cap for each region and year
gdp_capita <- load_gdp_per_capita_data()
floor_area <- summarize(gdp_capita, region=region, year=year, floorcap=calculate_floor_area(gdpcapita))

## Merge all residential space heating data together
pop <- gdp_capita[,c("region", "year", "population")]
resi.sh.data <- merge(merge(merge(merge(pop, floor_area), hdd),
                            space_heat_intensity), region_efficiency)
names(resi.sh.data) <- c("region","year","pop","floorcap","hdd",
                         "scenario", "intensity","eff")

## Get estimates of residential ground source heat pumps
gshp <- load_gshp_data()

## Get the share of energy in the commercial and residential sectors
sector_share <- load_sector_share_data()

## Determine the shares of electricity demand for residential and commercial.
elec_data <- subset(fuel_data, fuel=="elec")
elec_share <- load_electricity_share_data(elec_data)

## Finally get space and water heating fuel transfer data
heat_fuel_data <- load_heat_fuel_data(fuel_data)

## @knitr init-models
## Make sure the output directory exists
outdir <- "../output"
if (!file.exists(outdir)) dir.create(outdir)
source("functions-bottom-up.r")

## @knitr run-space-heat-model
## Each of these models calculates the change in energy consumption
## by region and fuel.
## Start with the residential space heating model
resi.sh.results <- calculate_residential_space_heat(resi.sh.data, fuel_data)
sprintf("Global savings from improved residential space heating = %.2f EJ", sum(resi.sh.results$savings))

## @knitr run-gshp-model
gshp.model <- calculate_GSHP_heat(gshp, fuel_data)
sprintf("Increased GSHP use = %.2f EJ less fossil fuel demand", sum(subset(gshp.model, fuel!="elec")$gshp.energy))
sprintf("Increased GSHP use = %.2f EJ more electricity demand", -sum(subset(gshp.model, fuel=="elec")$gshp.energy))
sprintf("Net savings = %.2f EJ", sum(gshp.model$gshp.energy))

## @knitr run-electrical-model
elec.eff <- calculate_electrical_savings(elec_share)
sprintf("Total savings from improved electrical appliance and lighting efficiency = %.2f EJ", sum(elec.eff$saving))

## @knitr run-fuel-switch-model
fuel_transfer <- calculate_fuel_switching(heat_fuel_data)
sprintf("Total savings from fuel switching = %.2f EJ", sum(fuel_transfer$change))

## @knitr bottom-up-summary
## Create a dataframe summarizing all of the interventions
space_heat <- subset(resi.sh.results, year==2050,
                     select=c(year, region, fuel, savings))
heat_pumps <- subset(gshp.model, year==2050,
                     select=c(year, region, fuel, gshp.energy))
efficiency <- subset(elec.eff, year==2050,
                     select=c(year, region, fuel, saving))
shifting <- subset(fuel_transfer, year==2050 & scenario=="lowC",
                    select=c(year, region, fuel, change))
carbon <- cbind(shifting[,1:3], carbon=0)
interventions <- merge(merge(merge(merge(space_heat, heat_pumps), efficiency, all.x=TRUE),
                       shifting), carbon)

names(interventions) <- c("year", "region", "fuel",
                          "space_heat", "gshp", "efficiency", "shifting", "carbon")
## Calculate the summary
results <- calculate_summary(fuel_data, interventions)
if (nrow(subset(results, LCS<0))) {
  tmp <- sum(subset(results, LCS<0)$LCS)
  warning(sprintf("There is %.2f EJ of negative demand.  Converting this to 0.", tmp))
  results <- transform(results, LCS=replace(LCS, LCS<0, 0))
}
sprintf("Total LMS demand = %.2f EJ", sum(results$LMS))
sprintf("Total LCS demand = %.2f EJ", sum(results$LCS))
gg <- make_waterfall_plot(results)
print(gg + labs(y="Building energy demand (EJ)"))

## @knitr emissions-calculation
emissions <- calculate_emissions(results)
em <- dcast(emissions, year + region + fuel ~ scenario, value.var="emissions")
sprintf("Total LMS emissions = %.2f Gt CO2", sum(em$LMS))
sprintf("Total LCS emissions = %.2f Gt CO2", sum(em$LCS))
