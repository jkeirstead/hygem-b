## Retrofit optimization model
## Based on Mark Jenning's code

## Calculates the space heating retrofit costs using an optimization model
##
## @param debug   If TRUE, will use demands from earlier GAMS model implementation
calculate_retrofit_costs <- function(debug=FALSE) {
  
  ## First define the sets
  regions <- c("China", "Eastern Europe", "India", "Latin America",
               "MENA", "Non-OECD Asia", "OECD Asia Oceania", "OECD Europe",
               "OECD North America", "Sub-Saharan Africa")
  scenario <- c("LMS", "LCS")
  technologies <- data.frame(tech=c("triple_glazing", "loft_insul",
                               "door_insul", "floor_insul", "cavity_wall_insul"),
                             element=c("glazing", "roof", "doors", "floor", "walls"))
  
  ## Then modify the regional data
  ## UN 2009 Global report on human settlements Table A.4/B.9.
  ## 2050 figure extrapolated from 2010 and 2030 using
  ## (1 + (2030-2010)/2010)*2030
  ## Measured in houses
  ##
  ## Baumert and Selman WRI 2003 Heating and cooling degree days
  regions <- data.frame(region=regions,
                        nhouses=1e9*c(0.961, 0.149, 0.413, 0.319, 0.177, 0.423,
                          0.102, 0.208, 0.220, 0.547),
                        hdd=1000*c(2.158, 4.258, 0.080, 0.372, 0.865,
                          0.297, 1.915, 2.642, 2.390, 0.081))

  ## Then modify the technology data
  ##
  ## Energy Savings Trust 2010 CE330 House Types
  ## Worst case difference between initial and post-retrofit U-values employed in this model
  ## measured in W/m2 C
  ## Cost estimates from DECC 2050 calculator
  ## European central bank statistical data warehouse:
  ## British pound sterling to US dollar exchange rate, average 1999-2012 = 1.67
  ## Measured in dollars per house
  technologies <- cbind(technologies, 
                        u=c(1.6, 2.2, 2.7, 0.44, 1.92),
                        cost=c(4330, 213, 167, 403, 1651))

  ## Constant
  seconds_per_day <- 24*60*60

  ## Residential space heating demand (in EJ)
  if (debug) {
    resi <- data.frame(region=rep(regions$region, 2), 
                       scenario=rep(scenario, each=length(regions$region)),
                       demand=c(16.32, 9.65, 0.47, 2.26, 2.78, 1.44, 1.18, 13.7, 5.88, 1.72,
                         6.74, 4.24, 0.45, 0.81, 0.86, 1.37, 0.89, 3.73, 2.43, 0.56))
  } else {
    ## TODO Load this in from the bottom-up model
    stop("TODO bottom-up model results not yet available")
  }

  ## Assumptions Greater London Authority.
  ## London Datastore: Generalize Land Use Database 2005. ONS 2012 Neighbourhood statistics
  ## Measured in m2 per house
  area <- data.frame(region=regions$region,
                     walls=65,
                     floor=65,
                     roof=45,
                     glazing=30,
                     doors=5)
  area <- melt(area, id="region", variable.name="element", value.name="area")

  ## Calculate the required saving by region
  resi.m <- dcast(resi, region ~ scenario, value.var="demand")
  resi.m <- transform(resi.m, saving=LMS-LCS)
  saving <- resi.m[,c("region", "saving")]
  
  ## The decision variables are then
  ## P is the penetration rate (max 1.0)
  ##    (m2/house) * (houses) * (W/m2 C) * (C) * (s) = J, needs to be in EJ
  tmp <- merge(merge(technologies, area), regions)
  tmp <- transform(tmp, energy=area*nhouses*u*hdd*seconds_per_day/1e18)
  tmp <- ddply(tmp, .(region, tech), summarize, max_energy=sum(energy))
  
  ## Calculate the cost matrix
  tmp2 <- merge(technologies, regions)
  tmp2 <- transform(tmp2, total_cost=nhouses*cost) 
  tmp2 <- tmp2[,c("region", "tech", "total_cost")]

  ## Now to solve this we have
  require(lpSolve)
  tmp.lp <- merge(merge(tmp, tmp2), saving)

  ## Solve the model for each region
  ## Don't forget to constrain the P values between 0 and 1
  models <- dlply(tmp.lp, .(region), function(df) {
    f.obj <- df$total_cost
    f.con <- rbind(matrix(df$max_energy, nrow=1), diag(nrow(df)))
    f.dir <- c(">", rep("<=", nrow(df)))
    f.rhs <- c(unique(df$saving), rep(1, nrow(df)))
    
    model <- lp("min", f.obj, f.con, f.dir, f.rhs)
    return(model)
  })

  ## Summarize the results
  penetrations <- ldply(models, function(l) return(data.frame(penetration=l$solution, tech=levels(tmp.lp$tech))))
  ## print(dcast(penetrations, region ~ tech, value.var="penetration"))

  ## Then calculate the final costs
  g <- 0.035  	## discount rate
  n <- 40	## annuity period

  ## Calculate the total cost and add the annuity factors
  tmp <- ldply(models, function(l) data.frame(capital=l$objval))
  tmp <- cbind(tmp, discount=g, years=n)

  return(tmp)
}

