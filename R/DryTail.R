# Exploring Streamflow vs Call Year at Dry Tails
# Emma Golub
# 6/7/23

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
#ref_tbl <- readr::read_csv(ref_tbl_path)
ref_tbl <- read.csv(ref_tbl_path)


##### Load in observed streamflow (cfs) from CDSSR #####
streamflow_df <- lapply(1:length(ref_tbl), function(i) {

  logger::log_info("Getting district {unique(ref_tbl[i, ]$district)} streamflow data...")

  #ids <- tidyr::separate_rows(ref_tbl[i, ], usgs_id, sep = ", ")$usgs_id
  ids <- lapply(tidyr::separate_rows(ref_tbl[i, ], usgs_id, sep = ", ")$usgs_id, as.character)

  streamflow_list <- lapply(ids, function(id) {
    if (!grepl("^0", id)) {
      id <- paste0("0", id)
    }
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

##### Data wrangling #####
unpacked_list <- unlist(streamflow_df, recursive = FALSE)

# Access individual data frames from the unpacked list. Each list is a district (d)
df_d80 <- unpacked_list[[1]]
df_d64 <- unpacked_list[[2]]
df_d23 <- unpacked_list[[3]]
df_d9 <- unpacked_list[[4]]
df_d8 <- unpacked_list[[5]]
df_d7 <- unpacked_list[[6]]

# in the process of fixing this section

df_agg <- bind_rows(unpacked_list, .id = "district")

# Manually change each element in the ids list to districts
df_agg <- lapply(df_agg, function(d) {
  if (d == 1) {
    df_agg <- 80
  } else if (d == 2) {
    df_agg <- 64
  } else if (id == 3) {
    df_agg <- 23
  } else if (d == 4) {
    df_agg <- 8
  } else if (d == 5) {
    df_agg <- 8
  } else if (d == 6) {
    df_agg <- 7
  } else {
    df_agg <- d
  }
  return(df_agg)
})

print(df_agg)

################################################################################
#Next Steps

# Aggregate with call data based on usgs site IDs

# Order streamflow small-->large

# Slice off values above (1) top 2/3 and (2) median
