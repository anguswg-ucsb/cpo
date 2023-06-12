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

streamflow_df <- lapply(1:nrow(ref_tbl), function(d) {

  logger::log_info("Getting district {ref_tbl[d, ]$district} streamflow data...")


  ids <- sapply(tidyr::separate_rows(ref_tbl[d, ], usgs_id, sep = ", ")$usgs_id, as.character)
  #lapply over every row in reg_tbl and separate attributes in column usgs_id
  # why is it stopping at district 7 halfway throught the dataframe?

  streamflow_list <- lapply(ids, function(id) {
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

#flow_df <- bind_rows(streamflow_df)

flow_df <- streamflow_df %>%
  bind_rows() %>%
  select(-station_num, -abbrev, -meas_type, -meas_count, -data_source, -modified) %>%
  rename(year = water_year, usgs_id = usgs_site_id)


# reformat reference table
crosswalk <- ref_tbl %>%
              separate_rows(usgs_id, sep = ",")

# create empty list to add data into each time you loop through
df_list <- list()

# loop through each unique district number in the flow_df df
for (i in 1:length(unique(flow_df$district))) {

  # show vector of usgs_ids that should be summed for each district
  #create vector from usgs_id col where district column equals i in vector of unique district names
  condition <- crosswalk$usgs_id[crosswalk$district == unique(flow_df$district)[i]]


  # create temporary df that sums up the total_qaf col by year
  temp <- flow_df %>%
    dplyr::filter(usgs_id %in% condition) %>%
    dplyr::group_by(district, year) %>%
    dplyr::summarise(tot_qaf = sum(total_qaf)
    )
  # add temporary df to the list df
  df_list[[i]] <- temp

}

#bind all of the items in the list into one df
final_df <- df_list %>% #1686 obs, years start at 1983
  bind_rows()



# # Grouping by year, district. Sum qaf based on certain sites in each water district
# flow_df <- flow_df %>%
#             group_by(district, year) %>%
#             summarize(total_qaf = sum(total_qaf))



# Aggregate with call data based on usgs site IDs
call_model_df <- read.csv("./data/annual_model_data.csv") #2322 obs, years start at 1970
call_model_df_1983on <- subset(call_model_df, !(year >= 1970 & year <= 1982)) #1763 obs


################################################################################
# TO FIX FROM HERE ON

# right now, call data has different WDIDs but flow data only has different usgs_ids. how to associate these two...
# sum up wdids into each usgs_id ? clarify what wdids are.

merged_df <- call_model_df_1983on %>%
              left_join(final_df, by = c("district","year"))
# something's not quite right. There should be gaps in flow column for years 1970-1982 because flow data was only available for 1983 and onward...




# Order streamflow small-->large

# Slice off values above (1) top 2/3 and (2) median
