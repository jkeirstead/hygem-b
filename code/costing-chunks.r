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
message('Not yet implemented')

## @knitr efficiency-costs
message('Not yet implemented')

## @knitr other-costs
message('Not yet implemented')

#regional_factors <- data.frame(region=regions$region,
#                               capital=c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 1, 1, 1, 0.9),
#                               operating=c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 1, 1, 1, 0.7))
