# Exploring Streamflow vs Call Year at Dry Tails
# Emma Golub
# 6/7/23

#install.packages("cdssr")
library(cdssr)
library(dplyr)
library(tidyverse)
library(magrittr)

# load in utils.R functions
source("R/utils.R")

# path to reference table
ref_tbl_path <- "data/district_lookup_table.csv"

# reference table
#ref_tbl <- readr::read_csv(ref_tbl_path)
ref_tbl <- read.csv(ref_tbl_path)
# Remove the 3rd through 6th columns
ref_tbl <- ref_tbl[, -c(3:6)]
# Remove rows after the 16th row
ref_tbl <- ref_tbl[1:16, ]

##### Load in observed streamflow (cfs) from CDSSR #####
print(seq_len(nrow(ref_tbl)))

streamflow_df <- lapply(1:nrow(ref_tbl), function(d) { # for each district (row) in the table

  logger::log_info("Getting district {ref_tbl[d, ]$district} streamflow data...")


  ids <- sapply(tidyr::separate_rows(ref_tbl[d, ], usgs_id, sep = ", ")$usgs_id, as.character)
  #lapply over every row in reg_tbl and separate attributes in column usgs_id

  streamflow_list <- lapply(ids, function(id) { # for each id in the ids list
    if (!grepl("^0", id)) {
      id <- paste0("0", id)
    } else if (TRUE) {
      # Continue loop or perform specific operations
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

# Expand the list of usgs site ids in each district into their own rows
crosswalk <- ref_tbl %>%
  separate_rows(usgs_id, sep = ",") # this correctly has all districts

# Initialize an empty list to store the final dataframes
df_list <- list()

# Loop through each unique district in flow_df
for (i in 1:length(unique(flow_df$district))) { # for i in unique districts
  # Filter the crosswalk based on the current district
  condition <- crosswalk$usgs_id[crosswalk$district == unique(flow_df$district)[i]] # watch the parentheses here!

  # Filter the flow_df based on the condition
  # Here will be some usgs_ids that appear multiple times in the crosswalk because
  #  they are relevant for multiple districts. Because of this, we need to make sure to
  #  filter the flow_df by both the condition vector AND the relevant district in the loop
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

# something is still not right with this final df... missing districts!


################################################################################
# Next, find the dry tail years.
# From average flow data in each year by district, determine which years are DRY (i.e. the driest 50% years, get a list of years for each district that will be half the number of years in each district)

  # Determine the years with the bottom 50% driest streamflow for each district
  # result <- aggregate(tot_qaf ~ district, data = final_df, FUN = function(x) {
  #   years <- final_df$year[x <= median(x)]
  #   paste(years, collapse = ", ")
  # })

# Calculate the median streamflow for each district
median_streamflow <- aggregate(tot_qaf ~ district, data = final_df, FUN = median)

# Create an empty dataframe to store the results
result_df <- data.frame(district = character(), year = I(list()), stringsAsFactors = FALSE)

# Iterate over each district
for (i in 1:nrow(median_streamflow)) {
  # Get the current district
  d <- median_streamflow$district[i]

  # Get the median streamflow for the current district
  med_qaf <- median_streamflow$tot_qaf[i]

  # Subset the dataframe for the current district
  district_data <- subset(final_df, district == d)

  # Subset the dataframe to include only the years where streamflow is below the median
  below_median <- subset(district_data, tot_qaf < med_qaf)

  # Get the unique years below the median
  unique_years <- unique(below_median$year)

  # Append the unique years to the result dataframe
  result_df <- rbind(result_df, data.frame(district = d, year = I(list(unique_years))))
}

write_csv(result_df, "drytailyears_bydistrict.csv")





# Determine the years with the bottom tercile driest streamflow for each district

