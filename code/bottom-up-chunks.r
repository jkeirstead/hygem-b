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
results.split <- split_by_sector(results)
tmp <- melt(results.split, id=c("year", "region", "fuel", "sector"))
tmp <- ddply(tmp, .(year, variable, sector), summarize, sum=round(sum(value),3))
tmp <- mutate(tmp, category=factor_interventions(variable),
              value=ifelse(variable %in% c("LMS", "LCS"), sum, -sum))

## Make the plot
source("waterfall.r")
gg <- waterfall(tmp)
print(gg +
      theme_bw() +      
      labs(x="", y="Global building energy demand (EJ)"))

## @knitr emissions-calculation
emissions <- calculate_emissions(results)

em <- ddply(emissions, .(scenario), summarize, emissions=sum(emissions))
em <- dcast(cbind(em, dummy=1), dummy ~ scenario, value.var="emissions")
sprintf("Total LMS emissions = %.2f Gt CO2", sum(em$LMS))
sprintf("Total LCS emissions = %.2f Gt CO2", sum(em$LCS))

## Try to do the waterfall plot
em.waterfall <- calculate_emissions_detail(results.split)
## Lump decarbon and extra into one
df <- dcast(em.waterfall, sector ~ intervention, value.var="emissions")
df <- transform(df, carbon=carbon+extra)
df <- melt(df, id="sector", variable.name="intervention", value.var="value")
df <- subset(df, intervention!="extra")

tmp <- transform(df,
                 category=factor_interventions(intervention))
gg.em <- waterfall(tmp)
print(gg.em +
      theme_bw() +      
      labs(x="", y="Emissions from global buildings sector (Gt CO2)"))

## @knitr emissions-table
## Prep a table with the results by region
tmp2 <- ddply(emissions, .(region, scenario), summarize, total=sum(emissions))
tmp2 <- dcast(tmp2, region ~ scenario, value.var="total")
require(xtable)
tmp2.xt <- xtable(tmp2,
                  align="llcc",
                  digits=c(0,0,3,3),
                  caption="Summary of emissions by region under the Low Mitigation and Low Carbon 2050 scenarios")
print(tmp2.xt, type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))

