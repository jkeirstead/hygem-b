# Hybrid Model of the Global Building Energy sector

This is an R script for calculating the energy consumption of the global buildings sector in 2050, as well as the estimated cost of an aggressive carbon mitigation strategy.  

## Running the model

The HyGEM model is a hybrid model, so-called because it combines a top-down prediction model of global building energy demand and a bottom-up technical accounting model.  The default configuration is to run these two components together, i.e.

``` source('hygem-b.r')
``` run_hygem()

However this will first check that the necessary input data are available.  Since one of the input data sets must be downloaded manually from the IEA, the user has a second option to use pre-calculated results from the top-down model, i.e.

``` source('hygem-b.r')
``` run_hygem(run_top_down=FALSE)

## Viewing the results

The model results are calculated and presented in an HTML file `hygem-b.html` located in the root directory.


## Further information

A manuscript based on this model has been submitted to _Energy and Buildings_.  For more information, please contact [j.keirstead@imperial.ac.uk](mailto:j.keirstead@imperial.ac.uk).
