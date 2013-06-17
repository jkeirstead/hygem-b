## Code chunks for the top-down regression model

## @knitr load-functions
source('functions.r')
source('functions-top-down.r')
source('functions-top-down-data.r')

## @knitr top-down-inputs
data <- load_top_down_inputs()
show_top_down_inputs(data)

## @knitr top-down-global
G <- run_global_regression(data)
print(summary(G$model))
print(G$plot)

## @knitr top-down-regional
R <- run_regional_regression(data)
print(summary(R$model))
print(R$plot)

## @knitr top-down-fuels
F <- run_fuel_regression(data)
print(summary(F$model))
print(F$plot)

## @knitr top-down-results
models <- load_top_down_results()
show_top_down_results(models)

