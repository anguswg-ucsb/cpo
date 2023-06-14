# Exploring Streamflow vs Call Year at Dry Tails
# Emma Golub
# 6/7/23

# Install.packages("cdssr")
library(cdssr)
library(dplyr)
library(tidyverse)
library(magrittr)

output_data_dir <- "./data"

# Load in utils.R functions
source("R/utils.R")

# Path to reference table
ref_tbl_path <- "data/district_lookup_table.csv"
ref_tbl <- read.csv(ref_tbl_path)
ref_tbl <- ref_tbl[, -c(3:6)]
ref_tbl <- ref_tbl[1:16, ]

# Load in observed streamflow (cfs) from CDSSR
streamflow_df <- lapply(1:nrow(ref_tbl), function(d) { # for each district (row) in the table

  logger::log_info("Getting district {ref_tbl[d, ]$district} streamflow data...")

  ids <- sapply(tidyr::separate_rows(ref_tbl[d, ], usgs_id, sep = ", ")$usgs_id, as.character)

  streamflow_list <- lapply(ids, function(id) { # for each id in the ids list
    if (!grepl("^0", id)) {
      id <- paste0("0", id)
    } else if (TRUE) {
    }

    streamflow <- cdssr::get_sw_ts(
      usgs_id = id,
      start_date = "1970-01-01",
      end_date   = "2023-06-01",
      timescale  = "year"
    )
    return(streamflow)
  })  %>%

  bind_rows() %>%
    mutate(district = ref_tbl[d, ]$district)

  streamflow_list
})

# Bind and rename/remove columns
flow_df <- streamflow_df %>%
  bind_rows() %>%
  select(-station_num, -abbrev, -meas_type, -meas_count, -data_source, -modified) %>%
  rename(year = water_year, usgs_id = usgs_site_id)

# Build crosswalk and make sure id's all have "0" as first character to avoid double-counting the same IDs
crosswalk <- ref_tbl %>%
 separate_rows(usgs_id, sep = ", ")

for (id in 1:nrow(crosswalk)) {
  usgs_id <- crosswalk$usgs_id[id] # Get the value of "usgs_id" for the current row

  if (!grepl("^0", usgs_id)) {
    crosswalk$usgs_id[id] <- paste0("0", usgs_id) # Add "0" as the first character if it's missing
  }
}

# Yay! Now crosswalk and flow_df both have 8 unique usgs_ids.

# Initialize an empty list to store the final dataframes
df_list <- list()

# Loop through each unique district in flow_df
for (i in 1:length(unique(flow_df$district))) { # for i in unique districts

  condition <- crosswalk$usgs_id[crosswalk$district == unique(flow_df$district)[i]] # watch the parentheses here!

  # filter the flow_df by both the condition vector AND the relevant district in the loop
  temp <- flow_df %>%
    filter(usgs_id %in% condition,
      district == unique(flow_df$district)[i]) %>%
    group_by(district, year) %>%
    summarise(tot_qaf = sum(total_qaf))

  # Add the temporary dataframe to the list
  df_list[[i]] <- temp
}

# Combine all dataframes in the list into one dataframe
final_df <- bind_rows(df_list)

# Yay! All the districts are there now. This just yields a weird "cachekey" error, but it doesn't affect the final dataframe


################################################################################
# Find dry tail years.
################################################################################

# Initialize an empty list to store the results
drytail_lower_half <- list()

# Loop through each unique district
for (d in 1:length(unique(final_df$district))) {
  # Calculate the median tot_qaf value for the current district
  median_value <- median(final_df$tot_qaf[final_df$district == unique(final_df$district)[d]])

  # Filter the final_df based on the condition and district
  condition <- final_df$year[final_df$tot_qaf < median_value & final_df$district == unique(final_df$district)[d]]

  # Filter the final_df by both the condition vector and the relevant district in the loop
  drytail_lower_half[[d]] <- final_df[final_df$year %in% condition & final_df$district == unique(final_df$district)[d], ]
}

# Combine all dataframes in the list into one dataframe
drytail_lower_half <- do.call(rbind, drytail_lower_half)
write_csv(drytail_lower_half, "drytail_lower_half.csv")


# Determine the years with the bottom tercile driest streamflow for each district
# Initialize an empty list to store the results
drytail_lower_tercile <- list()

# Loop through each unique district
for (d in 1:length(unique(final_df$district))) {
  # Calculate the tercile tot_qaf value for the current district
  tercile_value <- quantile(final_df$tot_qaf[final_df$district == unique(final_df$district)[d]], 0.33)

  # Filter the final_df based on the condition and district
  condition <- final_df$year[final_df$tot_qaf < tercile_value & final_df$district == unique(final_df$district)[d]]

  # Filter the final_df by both the condition vector and the relevant district in the loop
  drytail_lower_tercile[[d]] <- final_df[final_df$year %in% condition & final_df$district == unique(final_df$district)[d], ]
}

# Combine all dataframes in the list into one dataframe
drytail_lower_tercile <- do.call(rbind, drytail_lower_tercile)
write_csv(drytail_lower_tercile, "drytail_lower_tercile.csv")



###############################################################################
# Plotting for Dry Tail Years
###############################################################################

# Filter dry tail years for each district in annual model data and save as a new csv.
# Then call it in as the new df to run plots from Exploratory.R
call_model <- read.csv("./data/annual_model_data.csv")

#drytail_call_model <- call_model[call_model$year %in% drytail_lower_half$year & call_model$district %in% drytail_lower_half$district, ]

# Initialize an empty list to store the filtered dataframes
filtered_dfs <- list()

# Loop through each unique value in district of call_model
for (value1 in 1:length(unique(call_model$district))) {
  # Loop through each unique value in district of drytail_lower_half
  for (value2 in 1:length(unique(drytail_lower_half$district))) {
    # Check if the values in column1 of both dataframes match
    if (value1 == value2) {
      # Filter call_model based on the current value of district
      filtered_df <- call_model[call_model$district == unique(call_model$district) &
                                  call_model$year %in% drytail_lower_half$year, ]
      # Add the filtered dataframe to the list
      filtered_dfs[[length(filtered_dfs) + 1]] <- filtered_df
      # Break the inner loop to move to the next value in dataframe1
      break
    }
  }
}

# Combine all filtered dataframes in the list into one dataframe
result_df <- do.call(rbind, filtered_dfs)

# RIGHT NOW, YEAR ORDER IS OFF, AND I THINK SOME YEARS ARE MISSING. BUT ALL DISTRICTS ARE THERE.


#df <-

#source("R/Exploratory.R")
