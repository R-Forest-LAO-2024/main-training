
## Libraries ######

## + Create function to install package if not already done ####
load_pkg <- function(.pkg){
  pkg_name <- as.character(substitute(.pkg))
  if (!require(pkg_name, character.only = TRUE, quietly = TRUE)) install.packages(pkg_name, dep =TRUE)
  library(pkg_name, character.only = TRUE, quietly = TRUE)
}

## + Run function on required packages for the analysis ####

load_pkg(tictoc)
load_pkg(readxl)
load_pkg(tidyverse)
load_pkg(sf)
#load_pkg(terra)


## Create folders ######
dir.create("data", showWarnings = FALSE)
dir.create("report", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

## paths ######
path_datafile <- "data-source/NFI_Pilot_2024_20240510.xlsx"
