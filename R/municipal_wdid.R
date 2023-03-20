library(nhdplusTools)
library(cdssr)
library(dplyr)
library(sf)
library(mapview)
library(dataRetrieval)

agg <- cdssr::get_water_classes(county = "Larimer", wc_identifier = "U:1")

muni2 <- cdssr::get_water_classes(
  county        = "Larimer",
  wc_identifier = "U:2",
  timestep      = "month"
  )
