# Angus Watters
# get all the climate data from gridMET for modeling
# Data output: Weekly average climate variables within each polygon area

library(climateR)
library(terra)
library(dplyr)
library(sf)

source("R/utils.R")

# start and end dates
start_date <- "1980-01-01"
end_date   <- "2023-01-01"

# local path to waterdistricts shape
district_path <- "data/water_districts_simple.geojson"

# save path
climate_path <- "data/weekly_climate.rds"

# gridmet variable names
climateR::params %>%
  dplyr::filter(id == "gridmet") %>%
  .$variable

# climate variables to get
varname <- c("pr", "tmmn", "tmmx", "pdsi", "pet",
             "spi14d", "spi30d", "spi90d", "spi1y",
             "spei14d", "spei30d", "spei90d", "spei1y",
             "eddi14d", "eddi30d", "eddi90d", "eddi1y", "z")

# climate variables to get
varname <- c("pr", "tmmn", "tmmx", "pdsi", "pet",
             "spi14d", "spi30d", "spi90d", "spi1y",
             "spei14d", "spei30d", "spei90d", "spei1y",
             "eddi14d", "eddi30d", "eddi90d", "eddi1y", "z")

# climate variables to get
varname <- c("eddi180d", "eddi270d", "eddi1y", "eddi2y", "eddi5y" )

# read in water districts polygon
aoi <- sf::read_sf(district_path)

if(file.exists(climate_path)) {

  message(paste0(
    "Reading climate data: ",
    "\n---> ", climate_path
  ))

  # get climate gridMET
  clim_ts <- readRDS(climate_path)

} else {

  # get climate gridMET
  clim_ts <- get_gridmet(
    aoi        = aoi,
    varname    = varname,
    start_date = start_date,
    end_date   = end_date,
    name_col   = "district",
    wide       = TRUE,
    verbose    = TRUE
  )

  message(paste0(
    "Saving climate data: ",
    "\n---> ", climate_path
  ))

  # save path
  saveRDS(clim_ts, climate_path)

}
