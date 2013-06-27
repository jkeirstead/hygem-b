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

## @knitr run-space-heat-model
## Each of these models calculates the change in energy consumption
## by region and fuel.
## Start with the residential space heating model
resi.sh.results <- calculate_residential_space_heat(resi.sh.data, fuel_data)
sprintf("Global savings from improved residential space heating = %.2f EJ", sum(resi.sh.results$savings))
with(resi.sh.results, sprintf("Equivalent to a %.1f%% saving on a 2050 LMS heating demand of %.2f EJ", 100*sum(savings)/sum(LMS), sum(LMS)))

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
## Note that because of aggregation in earlier calculations,
## the per fuel totals are only estimates and only totals by
## region and year should be used.

sprintf("Total LMS demand = %.2f EJ", sum(results$LMS))
sprintf("Total LCS demand = %.2f EJ", sum(results$LCS))

## Calculate shares by sector

tmp <- melt(results, id=c("year", "region", "fuel"))
tmp <- ddply(tmp, .(year, variable), summarize, sum=round(sum(value),3))
tmp <- mutate(tmp, category=factor(variable,
                     levels=c("LMS", "space_heat", "gshp", "efficiency", "shifting", "carbon", "LCS"),
                     labels=c("LMS", "Space heating", "Ground HPs", "Electrical\nEfficiency", "Fuel switching", "Decarbonization", "LCS")),
              value=ifelse(variable %in% c("LMS", "LCS"), sum, -sum))
source("waterfall.r")
gg <- waterfall(tmp)
print(gg +
      theme_bw() +      
      labs(x="", y="Global building energy demand (EJ)"))

## @knitr emissions-calculation
emissions <- calculate_emissions(results)
sprintf("WARNING: The emissions calculation still needs some work")

em <- subset(emissions, intervention %in% c("LMS", "LCS"))
em <- ddply(em, .(intervention), summarize, emissions=sum(emissions))
em <- dcast(cbind(em, dummy=1), dummy ~ intervention, value.var="emissions")
sprintf("Total LMS emissions = %.2f Gt CO2", sum(em$LMS))
sprintf("Total LCS emissions = %.2f Gt CO2", sum(em$LCS))

tmp <- ddply(emissions, .(year, intervention), summarize, emissions=sum(emissions))
names(tmp) <- c("year", "category", "value")
tmp <- transform(tmp, category=factor(category,
                     levels=c("LMS", "space_heat", "gshp", "efficiency", "shifting", "carbon", "LCS"),
                     labels=c("LMS", "Space heating", "Ground HPs", "Electrical\nEfficiency", "Fuel switching", "Decarbonization", "LCS")),
                 value=ifelse(category %in% c("LMS", "LCS"), value, -value))

gg <- waterfall(tmp)
print(gg +
      theme_bw() +
      labs(x="", y="Emissions from global buildings sector (Gt CO2)"))
