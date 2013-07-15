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

## @knitr run-space-heat-model
## Each of these models calculates the change in energy consumption
## by region and fuel.
## Start with the residential space heating model
resi.sh.results <- calculate_residential_space_heat(resi.sh.data, fuel_data)

sprintf("Global savings from improved residential space heating = %.2f EJ", sum(resi.sh.results$savings))
with(resi.sh.results, sprintf("Equivalent to a %.1f%% saving on a 2050 LMS heating demand of %.2f EJ", 100*sum(savings)/sum(LMS), sum(LMS)))

## @knitr space-heat-tables
## Make some useful tables
resi.in <- subset(resi.sh.data, year==2050)
resi.in <- transform(resi.in, scenario=ifelse(scenario=="lowC", "LCS", "LMS"), pop=pop/1e6)
resi.in <- transform(resi.in, scenario=factor(scenario, levels=c("LMS", "LCS")))
resi.in <- dcast(resi.in, region + pop + floorcap + hdd + eff ~ scenario, value.var="intensity")
resi.out <- resi.sh.results[,c("region", "fuel", "LMS", "LCS")]
resi.out <- ddply(resi.out, .(region), summarize, LMS=sum(LMS), LCS=sum(LCS))
resi.table.data <- merge(resi.in, resi.out, by="region")
names(resi.table.data) <- c("Region", "P", "FA", "HDD", "Eff", "UE_LMS", "UE_LCS", "E_LMS", "E_LCS")

resi.tbl <- xtable(resi.table.data,
                   caption="Residential space heating calculation by region and scenario. P = population (billions), FA = floor area (m2/cap), HDD = heating degree days (deg C), Eff = space heating efficiency (%), UE = useful energy intensity (kJ/m2 HDD), E = energy demand (EJ). LMS = Low mitigation scenario, LCS = low carbon scenario.",
                   align="llcccccccc",
                   digits=c(0,0,2,1,0,1,0,0,1,1),
                   label="tbl:space_heat_calcs")
## Print these out
tblOptions <- getOption("xtable.html.table.attributes",
                        "border=1 width=600")
print(resi.tbl, include.rownames=FALSE, type="html", html.table.attributes=tblOptions)
print(resi.tbl, file=file.path(outdir, "table-4-space-heat-calcs.tex"), include.rownames=FALSE)

## @knitr run-gshp-model
gshp.list <- calculate_GSHP_heat(gshp, fuel_data)
gshp.model <- gshp.list$savings
sprintf("Increased GSHP use = %.2f EJ less fossil fuel demand", sum(subset(gshp.model, fuel!="elec")$gshp.energy))
sprintf("Increased GSHP use = %.2f EJ more electricity demand", -sum(subset(gshp.model, fuel=="elec")$gshp.energy))
sprintf("Net savings = %.2f EJ", sum(gshp.model$gshp.energy))

## @knitr gshp-tables
## Make a tidy version of the main parameters
gshp.in <- subset(gshp, year==2050)
gshp.in <- transform(gshp.in, scenario=ifelse(scenario=="lowC", "LCS", "LMS"), households=households/1e6)
gshp.in <- transform(gshp.in, scenario=factor(scenario, levels=c("LMS", "LCS")), penetration=penetration*100)
gshp.in <- dcast(gshp.in, region + households ~ scenario, value.var="penetration")
CF <- unique(gshp$cap.factor)
COP <- unique(gshp$COP)
size <- unique(gshp$size)

gshp.out <- gshp.list$energy[,c("region", "scenario", "energy", "elec")]
gshp.out <- transform(gshp.out, elec=-elec, scenario=ifelse(scenario=="lowC", "LCS", "LMS"))
gshp.out <- transform(gshp.out, scenario=factor(scenario, levels=c("LMS", "LCS")))
gshp.out.energy <- dcast(gshp.out, region ~ scenario, value.var="energy")
gshp.out.elec <- dcast(gshp.out, region ~ scenario, value.var="elec")
gshp.out <- merge(gshp.out.energy, gshp.out.elec, by="region")

gshp.table.data <- merge(gshp.in, gshp.out, by="region")
names(gshp.table.data) <- c("Region", "Households", "Penetration_LMS", "Penetration_LCS", "Heat_out_LMS", "Heat_out_LCS", "Elec_in_LMS", "Elec_in_LCS")
gshp.tbl <- xtable(gshp.table.data,
                   caption=sprintf("Residential ground source heat pump (GSHP) calculation by region and scenario. phi = penetration rate. For all regions, heat pumps are assumed to be %d kW, with a capacity factor of %d and a COP of %.1f. LMS = Low mitigation scenario, LCS = low carbon scenario.", size, CF*100, COP),
                   align="llccccccc",
                   digits=c(0,0,0,1,1,3,3,3,3),
                   label="tbl:GSHP_calcs")

## Print these out
tblOptions <- getOption("xtable.html.table.attributes",
                        "border=1 width=800")
print(gshp.tbl, include.rownames=FALSE, type="html", html.table.attributes=tblOptions)
print(gshp.tbl, file=file.path(outdir, "table-5-gshp-calcs.tex"), include.rownames=FALSE)

## @knitr grid-decarbonization
## Print a table of emissions figures
efs <- read.csv("../data/emission-factors.csv")
efs <- subset(efs, fuel=="elec")[,-2]
efs.tbl <- xtable(efs,
                   caption="Electricity emission factors by region and scenario (Mt CO2 per EJ).  LMS emission factors based upon Defra (2012) and LCS factors from a least-cost optimization model of the power sector (Shah et al, 2012). LMS = Low mitigation scenario, LCS = low carbon scenario.",
                   align="llcc",
                   digits=c(0,0,1,2),
                   label="tbl:elec_emission-factors")

## Print these out
tblOptions <- getOption("xtable.html.table.attributes",
                        "border=1 width=600")
print(efs.tbl, include.rownames=FALSE, type="html", html.table.attributes=tblOptions)
print(efs.tbl, file=file.path(outdir, "table-6-elec-emissions-factors.tex"), include.rownames=FALSE)

## @knitr run-electrical-model
elec.eff <- calculate_electrical_savings(elec_share)
sprintf("Total savings from improved electrical appliance and lighting efficiency = %.2f EJ", sum(elec.eff$saving))
message('Use the following table on the "R input" tab of the spreadsheet model')
## Calculate the residential and commercial shares needed by the costing model
tmp <- subset(elec_share, year==2050)
tmp <- transform(tmp, region=region, residential=elec_resi*res_nonheatelec*eff + elec_resi*(1-res_nonheatelec),
                 commercial=elec_comm*comm_nonheatelec*eff + elec_comm*(1-comm_nonheatelec))
tmp <- summarize(tmp, region=region, residential=elec_resi-residential, commercial=elec_comm-commercial)
require(xtable)
tmp.xt <- xtable(tmp,
                  align="llrr",
                  digits=c(0,0,3,3),
                  caption="Summary of electrical efficiency savings by region (EJ)")
print(tmp.xt, type="html", include.rownames=FALSE,
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))


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

