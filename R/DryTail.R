# Exploring Streamflow vs Call Year at Dry Tails
#install.packages("cdssr")
library(cdssr)
library(dplyr)
library(AOI)
library(nhdplusTools)
library(sf)
library(nngeo)
library(terra)

# load in utils.R functions
source("R/utils.R")

# path to reference table
ref_tbl_path <- "data/district_lookup_table.csv"

# reference table
ref_tbl <- readr::read_csv(ref_tbl_path)


# Load in observed streamflow (cfs) from CDSSR
streamflow_df <- lapply(1:length(ref_tbl), function(i) {

  logger::log_info("Getting district {unique(ref_tbl[i, ]$district)} streamflow data...")

  ids <- tidyr::separate_rows(ref_tbl[i, ], usgs_id, sep = ", ")$usgs_id

  streamflow_list <- lapply(ids, function(id) {
    streamflow <- cdssr::get_sw_ts(
      usgs_id = id,
      start_date = "1970-01-01",
      end_date   = "2023-06-01",
      timescale  = "year"
    )
    return(streamflow)
  })

  streamflow <- do.call(rbind, streamflow_list)
})

################################################################################
#Next Steps

# Aggregate with call data based on usgs site IDs

# Order streamflow small-->large

# Slice off values above (1) top 2/3 and (2) median
