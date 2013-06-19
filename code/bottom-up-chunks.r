## @knitr load-bottom-up-data
source("functions-bottom-up-data.r")
hdd <- load_hdd_data()
sheat.intens <- load_space_heat_intensity_data()
sheat.eff <- load_space_heat_efficiency_data()
fuel_data <- load_fuel_data()

## Calculate consumption weighted efficiency in each region
fuel.eff <- merge(fuel_data, sheat.eff)
region.eff <- ddply(fuel.eff, .(region,year), summarize,
                    eff=sum(fit*efficiency)/sum(fit))

## Then we need GDP, population, and GDP/cap for each region and year
gdp_capita <- load_gdp_per_capita_data()
floor_area <- summarize(gdp_capita, region=region, year=year, floorcap=calculate_floor_area(gdpcapita))

## Merge all residential space heating data together
pop <- gdp_capita[,c("region", "year", "population")]
resi.sh.data <- merge(merge(merge(merge(pop, floor_area), hdd), sheat.intens), region.eff)
names(resi.sh.data) <- c("region","year","pop","floorcap","hdd", "scenario", "intensity","eff")

## Get estimates of residential ground source heat pumps
gshp <- load_gshp_data()

## Get the share of energy in the commercial and residential sectors
sector_share <- load_sector_share_data()

## Determine the shares of electricity demand for residential and commercial.
elec_data <- subset(fuel_data, fuel=="elec")
elec_share <- load_electricity_share_data(elec_data)

## Finally get space and water heating fuel transfer data
heat_fuel_data <- load_heat_fuel_data(fuel_data)


## @knitr stopped-here
x <- 1
