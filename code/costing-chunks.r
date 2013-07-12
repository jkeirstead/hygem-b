## Code chunks for costing models

## @knitr space-heat-costs
source('retrofit-optimization.r')
space_costs <- calculate_retrofit_costs(TRUE)
tmp <- transform(space_costs, capital=capital/1e9)
names(tmp) <- c("Region", "Capital cost")
tmp.xt <- xtable(tmp[,1:2],
                 digits=c(0,0,2),
                 align="llr",
                 caption="Capital cost of space-heating retrofit measures (billion USD)")
print(tmp.xt, include.rownames=FALSE,  type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))

## @knitr heat-pump-costs
## From the bottom-up chunks, the basic penetration data is in gshp
## Rearrange first
tmp <- subset(gshp, year==2050)
tmp <- transform(tmp, scenario=ifelse(scenario=="lowC", "LCS", "LMS"))
## Calculate how many GSHPs are needed.
## http://2050-wiki.greenonblack.com/cost_categories/84 estimates £990 per kW for GSHP
## Assume 1.671 USD to GBP, British pound sterling to US dollar exchange rate, average 1999 - 2012
tmp <- mutate(tmp, number=households*penetration, cost=number*size*990*1.671)
tmp.mt <- dcast(tmp, year + region ~ scenario, value.var="cost")
tmp.mt <- transform(tmp.mt, cost=LCS-LMS, discount=0.035, years=25)
gshp_costs <- tmp.mt[,c("region", "cost", "discount", "years")]

tmp <- transform(gshp_costs, cost=cost/1e9)
names(tmp) <- c("Region", "Capital cost")
tmp.xt <- xtable(tmp[,1:2],
                 digits=c(0,0,2),
                 align="llr",
                 caption="Capital cost of ground-source heat pumps (billion USD)")
print(tmp.xt, include.rownames=FALSE,  type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))


## @knitr efficiency-costs
message('Note these results are calculated in the supplementary spreadsheet efficiency_costs.xlsx and hard-coded here for convenience.  Future versions will port this to R')
eff_costs <- data.frame(region=space_costs$region,
                        cost=c(25441583799.8172, 13659324918.2762, 18678417577.204,
                          9416235099.30109, 10713407116.5336, 22166510116.8187,
                          10117046204.5931, 80653591310.8488, 117181800698.091, 11780127109.4934),
                        discount=0.035, years=15)
tmp <- transform(eff_costs, cost=cost/1e9)[,1:2]
names(tmp) <- c("Region", "Capital cost")

tmp.xt <- xtable(tmp,
                 digits=c(0,0,2),
                 align="llr",
                 caption="Capital cost of lighting and appliance efficiency investments (billion USD)")
print(tmp.xt, include.rownames=FALSE,  type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))


## @knitr other-costs
other_costs <- data.frame(region=space_costs$region, cost=0, discount=0, years=0)

## @knitr heat-pump-operating
## From the bottom-up chunks, the basic penetration data is in gshp
## Rearrange first
tmp <- subset(gshp, year==2050)
tmp <- transform(tmp, scenario=ifelse(scenario=="lowC", "LCS", "LMS"))
## Calculate how many GSHPs are needed.
tmp <- mutate(tmp, number=households*penetration)
## Calculate the operating and maintenance costs
## Rawlings and Sykulski 1999 Ground source heat pumps gives annual GSHP maintenance costs (commercial) = $1.72/m2
## CIBSE presentation July 20, 2006 Viessmann UK, Heat supply per m2 (depends on soil type) = 25 W/m2
tmp <- transform(tmp, opex=number*size*1000/25*1.72)

tmp.mt <- dcast(tmp, year + region ~ scenario, value.var="opex")
tmp.mt <- transform(tmp.mt, cost=LCS-LMS)
gshp_op_costs <- tmp.mt[,c("region", "cost")]

tmp <- transform(gshp_op_costs, cost=cost/1e9)
names(tmp) <- c("Region", "Capital cost")
tmp.xt <- xtable(tmp[,1:2],
                 digits=c(0,0,2),
                 align="llr",
                 caption="Annual operating and maintenance costs of ground-source heat pumps (billion USD)")
print(tmp.xt, include.rownames=FALSE,  type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))




## @knitr space-heating-fuels
## These costs are measured in $ per GJ
fuel_costs <- read.csv("../data/fuel-costs-space-heat.csv", skip=3)
fuel_costs <- ddply(fuel_costs, .(region, scenario), summarize, price=mean(price))

## Total demands are given in resi.sh.results
tmp <- ddply(resi.sh.results, .(region), summarize, LMS=sum(LMS), LCS=sum(LCS))
tmp <- melt(tmp, id="region", variable.name="scenario", value.name="energy")
tmp <- merge(tmp, fuel_costs)
## Calculate the total cost and saving (converting between EJ and GJ)
tmp2 <- transform(tmp, cost=energy*1e9*price)
tmp2 <- dcast(tmp2, region ~ scenario, value.var="cost")
tmp2 <- transform(tmp2, cost=LCS-LMS)
space_fuels <- tmp2[,c("region", "cost")] ## in USD

## @knitr gshp-fuels
## These costs are measured in $ per GJ
fuel_costs <- read.csv("../data/fuel-costs-heat-pumps.csv", skip=3)
## Energy demands are in gshp.model
tmp <- ddply(gshp.model, .(region, fuel=="elec"), summarize, energy=sum(gshp.energy))
names(tmp)[2] <- "fuel"
tmp <- transform(tmp, fuel=ifelse(fuel, "elec", "fossil"))
## Calculate the cost delta
tmp2 <- merge(tmp, fuel_costs)
tmp2 <- transform(tmp2, total=cost*1e9*energy)
tmp2 <- dcast(tmp2, region ~ scenario, value.var="total")
gshp_fuels <- summarize(tmp2, region=region, cost=(-LCS-LMS)/1e9)

## @knitr fuel-switch-costs
## These are fossil fuel costs measured in $ per GJ
fuel_costs <- read.csv("../data/fuel-costs-switching.csv", skip=3)
## Calculate the amount of fossil fuel switched
tmp <- intermediate_switch(heat_fuel_data)
tmp <- ddply(tmp, .(region), summarize, total_switched=-sum(heat_displ))
tmp <- merge(tmp, fuel_costs)
tmp <- transform(tmp, total=cost*total_switched*1e9)
tmp2 <- dcast(tmp, region ~ scenario, value.var="total")
tmp2 <- transform(tmp2, cost=LCS-LMS)
transfer_fuels <- tmp2[,c("region", "cost")]

## @knitr efficiency-fuels
message('Note these results are calculated in the supplementary spreadsheet efficiency_costs.xlsx and hard-coded here for convenience.  Future versions will port this to R')
eff_fuels <- data.frame(region=space_costs$region,
                        cost=c(-13075229954, -16307591875, -9623777736,
                          -16575673119, -15281020652, -23190595282,
                          -10048852710, -32393345429, -17252313055, -14650561536))

## @knitr cost-summary
regional_factors <- data.frame(region=space_costs$region,
                               capital=c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 1, 1, 1, 0.9),
                               operating=c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 1, 1, 1, 0.7))

## Create a summary of the capital costs
names(space_costs)[2] <- "cost"
capex <- list(space=space_costs, gshp=gshp_costs, eff=eff_costs, switch=other_costs, grid=other_costs)
capex <- ldply(capex, rbind)
names(capex)[3] <- "capex"

## Create a summary of the operating costs
empty <- data.frame(region=gshp_op_costs$region, cost=0)
opex <- list(space=empty, gshp=gshp_op_costs, eff=empty, switch=empty, grid=empty)
opex <- ldply(opex, rbind)
names(opex)[3] <- "opex"

## Create a summary of the fuel costs
fuelex <- list(space=space_fuels, gshp=gshp_fuels, eff=eff_fuels, switch=transfer_fuels, grid=empty)
fuelex <- ldply(fuelex, rbind)
names(fuelex)[3] <- "fuel"

## Now merge these into a single data frame
tmp <- merge(merge(capex, opex), fuelex)
## Calculate the annuity factor and total cost
tmp <- mutate(tmp, AF=discount/(1-((1+discount)**(-years))), annual_capex=capex*AF)
tmp[is.na(tmp)] <- 0
## Merge with regional factors
tmp <- merge(tmp, regional_factors)

## Calculate the totals and tidy
totals <- summarize(tmp, intervention=.id, region=region, total=annual_capex*capital + (opex+fuel)*operating)
totals <- transform(totals, intervention=factor(intervention, levels=c("space", "gshp", "eff", "switch", "grid"),
                              labels=c("Space heating", "Heat pumps", "Electrical efficiency",
                                "Fuel switching", "Grid decarbonization")))

## Finally add in the household numbers
hh <- read.csv("../data/gshp.csv")
hh <- subset(hh, year==2050)
hh <- hh[,c("region", "households")]

## Calculate per household values
totals <- merge(totals, hh)
totals <- transform(totals, per_hh=total/households)

## Now arrange this to match table 11
per_hh <- ddply(totals, .(region), summarize, hh=sum(per_hh))
per_hh <- merge(per_hh, hh)
hh_total <- with(per_hh, weighted.mean(hh, households))

tmp <- transform(totals, total=total/1e9)
raw <- dcast(tmp, region ~ intervention, value.var="total")
tmp <- apply(raw[,-1], 1, sum)
raw <- cbind(raw, Total=tmp)

cost_results <- merge(raw, per_hh[,-3])
names(cost_results)[ncol(cost_results)] <- "Cost per hh"

bottom <- cbind(region="Global total", as.data.frame(t(apply(cost_results[,-1], 2, sum))))
## Correct with population weighted total
bottom[length(bottom)] <- hh_total

## Final results
results <- rbind(cost_results, bottom)
results <- results[, -which(names(results)=="Grid decarbonization")]

## Present the table
tmp.xt <- xtable(results,
                 digits=c(0,0,2,2,2,2,2,2),
                 align="llrrrrrr",
                 caption="Summary of cost difference between 2050 LMS and LCS scenarios including annualized capital, operating, and fuel (billion USD).  Cost per household in USD per household.")
print(tmp.xt, include.rownames=FALSE,  type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=600"))


gg <- ggplot(totals, aes(x=region, y=total/1e9)) +
  geom_bar(aes(fill=intervention), stat="identity", position="dodge") +
  geom_segment(data=raw, aes(x=as.numeric(region)-0.45, xend=as.numeric(region)+0.45, y=Total, yend=Total), size=1) +
  theme_bw() +
  labs(x="Region", y="Annualized cost difference (billion USD)") +
  scale_fill_discrete(name="Intervention")
print(gg)
