## The Hybrid Global Energy Model of the Buildings Sector (HyGEM-B)
## James Keirstead and Mark Jennings
## Imperial College London
## 13 June 2013
##
## This file contains the high-level script for running the HyGEM-B model.

## Run the HyGEM model
##
## Runs the HyGEM model of energy demand in the global buildings sector.  The model consists of two components: a top-down regression model which predicts energy consumption in ten world regions by fuel type; and a bottom-up technical model which calculates the possible energy and emission savings from five key interventions: improved space heating efficiency, increased use of ground-source heat pumps, improved efficiency of electrical appliances and lighting, fuel transfer (from fossil fuels to other low carbon vectors), and decarbonization of the electricity grid.
##
## @param run_top_down a Boolean indicating whether the top-down model should be run.  By default = TRUE, but this requires users to manually download a data set from the IEA.  By setting the option to a FALSE a set of precompiled results will be used.
run_hygem <- function(run_top_down=FALSE) {

  ## Load required library
  library(knitr)

  ## Save the archive flag
  e1 <- new.env(parent=environment())
  assign("run_top_down", run_top_down, envir=e1)
  
  ## Run the model
  knit2html("code/hygem-b.Rmd", envir=e1)
}
