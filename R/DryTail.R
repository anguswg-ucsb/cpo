# Exploring Streamflow vs Call Year at Dry Tails

library(cdssr)
library(dplyr)
library(AOI)
library(nhdplusTools)
library(sf)
library(nngeo)
library(terra)

# load in utils.R functions
source("R/utils.R")

# local path to waterdistricts shape
districts_path <- "data/water_districts_simple.geojson"

# annual data
annual_path  <-  "data/annual_model_data.csv"

# path to reference table
ref_tbl_path <- "data/district_lookup_table.csv"

# reference table with USGS and Snotel IDs for each district
ref_tbl <- readr::read_csv(ref_tbl_path)

# # dates to get data for
start_date = "1970-01-01"
end_date   = Sys.Date()

# basin to get data for
basins <- c("South Platte")


# subset reference table
#sites_df <-
  #ref_tbl %>%
  #dplyr::filter(district == ifelse(
    #dist_shp$DISTRICT[i] < 10,
    #paste0("0", dist_shp$DISTRICT[i]),
    #paste0(dist_shp$DISTRICT[i])
 # )
 # )


# Load in observed streamflow (cfs) from CDSSR
site_id <- unique(ref_tbl$usgs_id)

streamflow_df <- lapply(1:nrow(ref_tbl), function(i) {

  logger::log_info("Getting district {unique(ref_tbl[i, ]$district)} streamflow data...")

  # unique(site_df[i, ]$basin)
  ids <- tidyr::separate_rows(ref_tbl[i, ], site_id, sep = ", ")$usgs_id # extracting comma-separated IDs associated with district

  streamflow <- cdssr:get_sw_ts(site_id = ids) %>% # do inner loop to get each ID in list
    dplyr::mutate(
      district = unique(ref_tbl[i, ]$district),
      #abbrev     = "",
      usgs_id = unique(ref_tbl[i, ]$usgs_id),
      start_date = "1970-01-01",
      end_date   = "2023-06-01",
      timescale  = "year"
    )

  streamflow

})

  # obs_flow <- cdssr::get_sw_ts(
  # abbrev     = "",
  # start_date = "1970-01-01",
  # end_date   = "2023-06-01",
  # timescale  = "year"
  # )


# Aggregate with call data based on usgs site IDs

# Order streamflow small-->large


# Slice off values above (1) top 2/3 and (2) median
