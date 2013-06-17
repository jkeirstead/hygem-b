## The Hybrid Global Energy Model of the Buildings Sector (HyGEM-B)
## James Keirstead and Mark Jennings
## Imperial College London
## 13 June 2013
##
## This file contains the high-level script for running the HyGEM-B model.

## Run the HyGEM model
##
## Runs the HyGEM model of energy demand in the global buildings sector.  The model consists of two components: a top-down regression model which predicts energy consumption in ten world regions by fuel type; and a bottom-up technical model which calculates the likely mix of technologies that meet this target.  
##
## @param use_TD_archive a Boolean indicating whether pre-compiled top-down model results should be used, default = FALSE.  Users must manually download a data set from the IEA to use the top-down model and so this option allows users to skip directly to the bottom-up component.
run_hygem <- function(use_TD_archive=FALSE) {

  ## Load required library
  library(knitr)

  ## Save the archive flag
  e1 <- new.env(parent=environment())
  assign("use_TD_archive", use_TD_archive, envir=e1)
  
  ## Run the model
  knit2html("code/hygem-b.Rmd", envir=e1)
}
