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
other_costs <- data.frame(space_costs$region, fuel=0, grid=0, discount=0, years=0)
tmp <- other_costs[,1:3]
names(tmp) <- c("Region", "Fuel switching", "Grid decarbonization")
tmp.xt <- xtable(tmp,
                 digits=c(0,0,2, 2),
                 align="llrr",
                 caption="Capital cost of other energy-saving measures (billion USD)")
print(tmp.xt, include.rownames=FALSE,  type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))

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


## @knitr fuel-costs
## These costs are measured in $ per GJ
fuel_costs <- read.csv("../data/fuel-costs.csv", skip=3)
fuel_costs <- ddply(fuel_costs, .(region, scenario), summarize, price=mean(price))

## @knitr space-heating-fuels
## Total demands are given in resi.sh.results
tmp <- ddply(resi.sh.results, .(region), summarize, LMS=sum(LMS), LCS=sum(LCS))
tmp <- melt(tmp, id="region", variable.name="scenario", value.name="energy")
tmp <- merge(tmp, fuel_costs)
## Calculate the total cost and saving (converting between EJ and GJ)
tmp2 <- transform(tmp, cost=energy*1e9*price)
tmp2 <- dcast(tmp2, region ~ scenario, value.var="cost")
tmp2 <- transform(tmp2, saving=LCS-LMS)
space_fuels <- tmp2[,c("region", "saving")] ## in USD


## @knitr summary-something
#regional_factors <- data.frame(region=regions$region,
#                               capital=c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 1, 1, 1, 0.9),
#                               operating=c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 1, 1, 1, 0.7))
