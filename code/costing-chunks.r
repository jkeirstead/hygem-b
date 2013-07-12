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
tmp.mt <- transform(tmp.mt, cost=LCS-LMS)
heat_pumps <- tmp.mt[,c("region", "cost")]

tmp <- transform(heat_pumps, cost=cost/1e9)
names(tmp) <- c("Region", "Capital cost")
tmp.xt <- xtable(tmp[,1:2],
                 digits=c(0,0,2),
                 align="llr",
                 caption="Capital cost of ground-source heat pumps (billion USD)")
print(tmp.xt, include.rownames=FALSE,  type="html",
      html.table.attributes=getOption("xtable.html.table.attributes",
                          "border=1 width=400"))


## @knitr efficiency-costs
message('Not yet implemented')

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

## @knitr summary-something
#regional_factors <- data.frame(region=regions$region,
#                               capital=c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 1, 1, 1, 0.9),
#                               operating=c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 1, 1, 1, 0.7))
