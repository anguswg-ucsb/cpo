###############################################################################
# Comparing Observed Streamflow and NRCS Forecast Trends

# For District 1: USGS Flow Gage South Platte @ Denver , 06714000
#                 USGS FLow Gage South Platte @ South Platte 06707500

# Emma Golub
# 7/24/23
###############################################################################
library(cdssr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(ggplot2)

#output_data_dir <- "./data"

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
      end_date   = "2023-07-01",
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

# Initialize an empty list to store the final dataframes
df_list <- list()

# Loop through each unique district in flow_df
for (i in 1:length(unique(flow_df$district))) { # for i in unique districts

  condition <- crosswalk$usgs_id[crosswalk$district == unique(flow_df$district)[i]] # watch the parentheses here!

  # Filter the flow_df by both the condition vector AND the relevant district in the loop
  temp <- flow_df %>%
    filter(usgs_id %in% condition,
           district == unique(flow_df$district)[i]) %>%
    group_by(district, year) %>%
    summarise(tot_qaf = sum(total_qaf)) # really in cf?

  # Add the temporary dataframe to the list
  df_list[[i]] <- temp
}

# Combine all dataframes in the list into one dataframe
obs_flow_df <- bind_rows(df_list)

# Convert from cf into af
obs_flow_df <- obs_flow_df %>%
  mutate(obs_af = tot_qaf * 2.2965774533658E-5)

# Isolate district 1
obs_flow_df_d1 <- obs_flow_df[obs_flow_df$district == '1',]






# Bring in NRCS Forecast data
data <- read_csv('data/annual_model_v07202023.csv')

fx_flow_df <- data %>%
  select(district, year, wdid, gnis_id, apr_fx_apr_to_sep
         )

# Isolate district 1 (has only one unique USGS_ID anyway... 201759)
fx_flow_df_d1 <- fx_flow_df[fx_flow_df$district == '01',] %>%
  select(district, year, apr_fx_apr_to_sep
  )




# Plot against each other
plot(fx_flow_df_d1$year, fx_flow_df_d1$apr_fx_apr_to_sep, type = "l", col = "blue", xlab = "Year", ylab = "Value")

# Add the second dataset to the same plot
lines(fx_flow_df_d1$year[1:length(obs_flow_df_d1$year)], obs_flow_df_d1$obs_af, col = "red")

# Add a legend to differentiate the datasets
legend("topleft", legend = c("forecast (af)", "observed (af)"), col = c("blue", "red"), lty = 1)

