# Angus Watters
# load in NRCS Dataset from SOAP API (Python code to be translated into R)
# NRCS_URL = 'https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL

# Libraries
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# NRCS Streamflow forecasts
nrcs_path     <- "data/nrcs/nrcs_forecasts.csv"

# district shape path
district_path <- "data/water_districts_simple.geojson"

# path to Water right points of interest
wr_path <-   "data/rights_by_gnis_id.rds"
nrcs <- readr::read_csv(nrcs_path)

# get points of NRCS points
nrcs_pts <-
  nrcs %>%
  dplyr::group_by(stationTriplet) %>%
  dplyr::slice(1) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    near_id = 1:n()
  )

# read in water rights points
wr_pts <- readRDS(wr_path)

# nearest forecast points to each water right point
near_pts <- sf::st_nearest_feature(wr_pts, nrcs_pts)
near_pts
nrcs_pts$near_id
# tmp_wr <- wr_pts[1:3, ]
# tmp_for <- nrcs_pts[20, ]

# add forecast point indexes as column
wr_pts$near_id <- near_pts


nearest_fx <-
  wr_pts %>%
  dplyr::left_join(
    dplyr::select(sf::st_drop_geometry(nrcs_pts), station_code, near_id),
    by = c("near_id")
  )

nearest_fx %>%
  sf::st_drop_geometry() %>%
  dplyr::select(district, wdid, gnis_id, seniority, station_code)


# add
nrcs_pts <-
  nrcs_pts %>%
  dplyr::mutate(
    near_pts = 1:dplyr::row_number()
  )
mapview::mapview(tmp_wr) + tmp_for
wr_pts[near_pts, ]



nrcs_pts <-
  nrcs %>%
  dplyr::group_by(stationTriplet) %>%
  dplyr::slice(1) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::ungroup()

nrcs_pts %>%
  mapview::mapview()
elevatr::get_elev_raster()
