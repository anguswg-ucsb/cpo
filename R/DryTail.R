# Exploring Streamflow vs Call Year at Dry Tails
# Emma Golub
# 6/7/23

#install.packages("cdssr")
library(cdssr)
library(dplyr)
library(tidyverse)

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


# FIGURE OUT WHY SOME DISTRICTS ARE MISSING


##### Data wrangling #####
unpacked_list <- unlist(streamflow_df, recursive = FALSE)

# Access individual data frames from the unpacked list. Each list is a district (d)

df_all <- list2env(setNames(streamflow_df, paste0("df", 1:6)), envir = .GlobalEnv)

# Add a column representing the original data frame district
df1 <- df1 %>% mutate(district = 80)
df2 <- df2 %>% mutate(district = 64)
df3 <- df3 %>% mutate(district = 23)
df4 <- df4 %>% mutate(district = 9)
df5 <- df5 %>% mutate(district = 8)
df6 <- df6 %>% mutate(district = 7)

# Stack the data frames
stacked_df <- bind_rows(df1, df2, df3, df4, df5, df6)
flow_df <- select(stacked_df, -meas_type, -meas_count, -data_source, -modified)
flow_df <- rename(flow_df, year = water_year)

################################################################################
#Next Steps

# Aggregate with call data based on usgs site IDs
call_model_df <- read.csv("./data/annual_model_data.csv")

# Grouping by year, district. Sum qaf based on certain sites in each water district
flow_df <- flow_df %>%
            group_by(district, year) %>%
            summarize(total_qaf = sum(total_qaf))
# This wrong, might be overcounting some flow at sites that are adjacent to each other... follow up with Angus here


merged_df <- call_model_df %>%
              left_join(flow_df, by = c("district", "year"))



# Order streamflow small-->large

# Slice off values above (1) top 2/3 and (2) median
