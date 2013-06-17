## Loads GDP data from the World Bank
##
## Loads historical GDP data (constant 2000 dollars) from the World Bank.  Estimates future real GDP growth rates for each region from PriceWaterhouse Cooper's The World in 2050 report (http://www.pwc.com/en_GX/gx/world-2050/pdf/world-in-2050-jan-2011.pdf, Table 5).
##
## @return a data frame containing GDP in constant 2000 USD by region and year
load_gdp <- function() {
   require(WDI)
   gdp_code <- "NY.GDP.MKTP.KD"	# GDP (constant 2000 USD)
   gdp_data <- WDI(ind=gdp_code, start=1970, end=last_year)
   names(gdp_data)[3] <- "gdp"
 
   ## Create a temporary data frame to store the regional totals
   tmp <- merge(regions, gdp_data, all.x=TRUE)
   df <- expand.grid(region=levels(regions$region), year=1971:2050)
   tmp <- merge(tmp, df, all.y=TRUE)
   tmp <- ddply(tmp, .(region, year), summarize, GDP=sum(gdp, na.rm=TRUE))
   ## Insert NAs again
   tmp <- mutate(tmp, GDP=replace(GDP, GDP==0, NA))
 
   ## Need to make future projections
   gdp_proj_params <- data.frame(region=levels(regions$region),
                                 rate=c(5.9, 8.1, mean(c(5.1,2.3,1.9,1.7,1.4,1.3)), mean(c(3.1,2.4,1.0)),
                                   mean(c(2.4,2.2)), mean(c(4.9,4.7,4.4)), 4.0, mean(c(8.8,5.8)),5.0, mean(c(7.9,5.0))))
 
   ## Make a convenient data frame for the analysis
   tmp2 <- merge(tmp, gdp_proj_params)
   tmp2 <- mutate(tmp2, rate=ifelse(year<=last_year, 0, rate))
 
   ## Store the results back in the summary data frame
   tmp3 <- ddply(tmp2, .(region), summarize, year=year, gdp=forecast_value(GDP, rate))
   return(tmp3)
}

## Loads population data from the UN
##
## Gets historic and future projections of global population under the 'Medium' scenario to 2050.
##
## @return a data frame with the population in thousands by year and region
load_population <- function() {
  ## Download the UN file to a temporary location
  un_url <- "http://esa.un.org/unpd/wpp/Excel-Data/EXCEL_FILES/1_Population/WPP2012_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS"

  pop_filename <- tempfile()
  if (!file.exists(pop_filename)) download.file(un_url, pop_filename, mode="wb")

  ## Open the file with XLConnect
  ## Users may have issues if the jvm.dll folder is not in the system PATH.
  ## See http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r
  ## See http://stackoverflow.com/questions/171205/java-maximum-memory-on-windows-xp/497757#497757
  options(java.parameters = "-Xmx150m") # Decrease the available memory for JVM, otherwise may be a problem with providing sufficient contiguous memory
  require(XLConnect)
  wb <- loadWorkbook(pop_filename)

  ## Historical estimates
  pop_data_historical <- readWorksheet(wb, "ESTIMATES", 17, 2, 282, 66)
  pop_data_historical <- tidy_pop_data(pop_data_historical)

  ## Future projections (medium fertility variant)
  pop_data_future <- readWorksheet(wb, "MEDIUM FERTILITY", 17, 2, 282, 46)
  pop_data_future <- tidy_pop_data(pop_data_future)

  ## I've manually checked and 2010 is the same in both, so drop it from the future one
  pop_data_future <- subset(pop_data_future, year!=last_year)

  ## Bind them together and drop the variant column
  pop_data <- rbind(pop_data_historical, pop_data_future)
  pop_data <- pop_data[,-which(names(pop_data)=="Variant")]

  ## Check which countries don't match.  Should only return the names of regions or other aggregate totals
  ## Run this manually
  ## countries.pop <- pop_data$country
  ## countries.regions <- regions$country
  ## unique(countries.pop[!is.element(countries.pop, countries.regions)])

  ## Merge this with the main data set above
  tmp <- merge(regions, pop_data, all.x=TRUE)
  df <- expand.grid(region=levels(regions$region), year=1971:2050)
  tmp <- merge(tmp, df, all.y=TRUE)

  ## Save this to a temporary file
  tmp_pop <- write.csv(tmp, "tmp-population.csv", row.names=FALSE)

  ## Then return the regional totals
  tmp2 <- ddply(tmp, .(region, year), summarize, population=sum(population, na.rm=TRUE))
  return(tmp2)
}

## Loads urbanization statistics from the UN website
##
## Gets the urban proportion of each world region.  Uses the regional population totals
## to create a population weighted urbanization average for each region.
##
## @pop_data a data frame containing the population data 
## @return a data frame
load_urbanization <- function() {
  ## Download the file to a temporary location
  un_url <- "http://esa.un.org/unpd/wup/CD-ROM/WUP2011-F02-Proportion_Urban.xls"
  pop_filename <- tempfile()

  if (!file.exists(pop_filename)) download.file(un_url, pop_filename, mode="wb")

  ## Read the data
  wb <- loadWorkbook(pop_filename)
  urban_data <- readWorksheet(wb, "PROPORTION-URBAN", 13, 2, 279, 25)

  ## Tidy things up
  urban_data <- tidy_urban_data(urban_data)

  ## Manully check which countries don't match.  This should only return
  ## aggregate regions
  ## countries.pop <- urban_data$country
  ## countries.regions <- regions$country
  ## unique(countries.pop[!is.element(countries.pop, countries.regions)])

  ## Merge with population data so we can create the correct regional totals
  pop_data <- read.csv("tmp-population.csv")
  file.remove("tmp-population.csv")
  tmp <- merge(pop_data, urban_data)
  tmp <- merge(regions, tmp, all.x=TRUE)
  tmp <- ddply(tmp, .(region, year), summarize, urban=weighted.mean(urban, population, na.rm=TRUE))

  df <- expand.grid(region=levels(regions$region), year=1971:2050)
  tmp2 <- merge(df, tmp, all.x=TRUE)
  
  return(tmp2)
}

## Loads building energy consumption data from IEA
##
## Loads building energy consumption data from the IEA's Extended Energy Balances.  Owing to the data licensing conditions, users will need to download this data themselves and save it into the \code{data} folder in a comma-separated value file called \code{iea-buildings-data.csv}.  For UK academic users, the data can be accessed via the ESDS data service by going to \link{http://www.esds.ac.uk/findingData/snDescription.asp?sn=6301&key=}.  Other users will have to find their own access.  The data set can be extracted by selecting:
## \itemize{
## \item Extended Energy Balances
## \item Time = 1970 to 2010
## \item Flow = Residential, Commercial and public services
## \item Product = All fuels
## }
## We recommend using the ``Set dimension order \ldots'' tool, choosing product as the columns and flow, country, and time as the rows.  The data can then be saved as comma-delimited values.
##
## @return a data frame containing the energy consumption in EJ
load_energy <- function() {

  ## Load the raw data
  iea_file <- "../data/iea-buildings-data.csv"
  if (!file.exists(iea_file)) {
    stop("IEA data file not found.  Please download the file and save it in `data/iea-buildings-data.csv'")
  }

  ## If we've got the data file, then proceed
  iea_data <- read.csv(iea_file, skip=5)

  ## Tidy things up
  iea_data <- tidy_iea_data(iea_data)

  ## Manually check which countries don't match.  This should only return
  ## regionally aggregates
  ## countries.energy <- iea_data$country
  ## countries.regions <- regions$country
  ## unique(countries.energy[!is.element(countries.energy, countries.regions)])

  ## Merge as before
  tmp <- merge(regions, iea_data, all.x=TRUE)
  df <- expand.grid(region=levels(regions$region), year=1971:2050)
  tmp <- merge(df, tmp, all.x=TRUE)
  
  ## Might be multiple countries per region so need to aggregate here too
  tmp2 <- melt(tmp, id=c("region", "year", "country"))
  tmp2 <- ddply(tmp2, .(region, year, variable), summarize, value=sum(value, na.rm=TRUE))
  tmp3 <- dcast(tmp2, region + year ~ variable, value.var="value")

  return(tmp3)
}

## Transforms UN population data into a sensible shape
##
## Params:
## 	df	the raw population data frame from the UN spreadsheet
##
## Returns:
## 	a melted data from with country, year, and population
tidy_pop_data <- function(df) {
  ## Load the required library
  require(stringr)
  
  ## Drop the notes and country code
  df <- df[,-which(names(df) %in% c("Notes", "Country.code"))]
  
  ## Melt the data  
  df <- melt(df, id=c("Variant", "Major.area..region..country.or.area"), variable.name="year")
  
  ## Tidy up the year variable
  df <- mutate(df, year=as.numeric(str_replace(year,"X","")))
  
  ## Tidy names and return
  names(df) <- c("Variant", "country", "year", "population")
  
  return(df)
}

## Transforms UN urbanization data into a sensible shape.  It also
## interpolates the points so there are defined values for every year
##
## Params:
## 	df	the raw urbanization data frame from the UN spreadsheet
##
## Returns:
## 	a melted data from with country, year, and urbanization
tidy_urban_data <- function(df) {
  require(plyr)
  require(reshape2)
  
  ## Drop the notes and country code
  df <- df[,-which(names(df) %in% c("Note", "Country.code"))]

  ## Set the names (which for some reason got screwed up in the download)
  names(df) <- c("country", seq(1950, 2050, 5))
  
  ## Melt the data  
  df <- melt(df, id=c("country"), variable.name="year")
  
  ## Tidy up the year variable
  df <- mutate(df, year=as.numeric(as.character(year)))
  
  ## Tidy names and return
  names(df) <- c("country", "year", "urban")

  ## Do the interpolation
  tmp <- ddply(df, .(country), summarize, year=min(year):max(year))
  tmp <- merge(tmp, df, all.x=TRUE)

  ## Create a function to do the interpolation
  test <- function(df) {
    df <- subset(df, !is.na(urban))
    fun <- approxfun(df$year, df$urban)
    return(fun)
  }
  tmp <- ddply(tmp, .(country), function(df) {return(data.frame(year=df$year, urban=test(df)(df$year)))})
  
  return(tmp)
}


## Transforms the IEA energy demand data into a sensible shape.  
##
## Params:
## 	df	the raw urbanization data frame from the IEA spreadsheet
##
## Returns:
## 	a melted data from with country, year, and energy demand (TJ)
tidy_iea_data <- function(df) {

  ## Load required libraries
  require(stringr)
  
  ## Drop all of the rows that start "Table title" or "Bibliographic"
  ## There's also a problem with blank lines to be sorted out
  bad_starts <- "^(Table title|Bibliographic)"
  df <- df[-grep(bad_starts, df[,1]),]
  df <- subset(df, X!="")

  ## Convert all of the columns into numerical values  
  empty_vals <- "\\.\\.|x"
  for (i in 4:ncol(df)) {
    df[,i] <- as.numeric(str_replace(as.character(df[,i]), empty_vals, NA))
  }

  ## Drop the first row as irrelevant
  df <- df[-1,]

  ## Set the third column (TIME) as numeric
  df[,3] <- as.numeric(as.character(df[,3]))

  ## Get the names of the fuels and tidy initial names
  fuel_names <- names(df)[4:ncol(df)]
  names(df)[1:3] <- c("sector", "country", "year")

  ## Set the proper label for China
  df <- mutate(df, country=str_replace(country, "People's Republic of China", "China"))
  df <- mutate(df, country=str_replace(country, "Hong Kong, China", "Hong Kong"))
  ## Capture the end of communism
  ## Yugoslavia obviously not now Russia, but still within Eastern Europe
  df <- mutate(df, country=str_replace(country, "Former Soviet Union \\(If no detail\\)", "Russian Federation"))
  df <- mutate(df, country=str_replace(country, "Former Yugoslavia \\(If no detail\\)", "Russian Federation"))

  ## We only want to deal with a select number of fuels
  ## Manually create a data frame that lines up fuel names to desired aggregate categories
  ## These mysterious codes line up to an old Excel spreadsheet
  line_offset <- 136
  elec_cat <- 198
  heat_cat <- c(199, 154, 189)
  coal_cat <- c(137:143, 145:149)
  gas_cat <- c(150:153, 165, 166, 168, 172:174)
  oil_cat <- c(167, 169:171, 175:188)
  biomass_cat <- c(144, 155:164)
  uranium_cat <- 190
  renewables_cat <- 191:196
  other_cat <- 197
  cats <- list(list(name="Electricity", id=elec_cat),
               list(name="Heat", id=heat_cat),
               list(name="Coal", id=coal_cat),
               list(name="Gas", id=gas_cat),
               list(name="Oil", id=oil_cat),
               list(name="Biomass", id=biomass_cat),
               list(name="Uranium", id=uranium_cat),
               list(name="Renewables", id=renewables_cat),
               list(name="Other", id=other_cat))
  fuel_cats <- ldply(cats, function(l) return(data.frame(name=l$name, label=fuel_names[l$id - line_offset])))

  # For debugging
  # fuel_names[which(!is.element(fuel_names, fuel_cats$label))]
  
  ## Melt the data frame
  df_melt <- melt(df, id=c("sector", "country", "year"), variable.name="fuel")

  ## Merge it with the fuel categories
  df_melt <- merge(df_melt, fuel_cats, by.x="fuel", by.y="label")

  ## We only care about the aggregate categories so sum sectors and fuels
  ## Strictly speaking I didn't need to do the above labelling but we may change
  ## our minds and want fuel-wise breakdowns later  
  tmp <- ddply(df_melt, .(country, year, name), summarize, value=sum(value, na.rm=TRUE))
  tmp <- dcast(tmp, country + year ~ name)
  tmp <- mutate(tmp, energy=Electricity + Heat + Coal + Gas + Oil + Biomass + Uranium + Renewables + Other)
  
  ## Return the result
  return(tmp)
}
