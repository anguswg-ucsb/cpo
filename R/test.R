library(cdssr)
library(dplyr)
library(AOI)
library(sf)
library(terra)


counties  <- AOI::aoi_get(state = "CO", county = "all")

