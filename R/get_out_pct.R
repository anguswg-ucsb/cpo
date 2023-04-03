# Angus Watters
# Collect water right call analysis data for WDIDs along major GNIS ID stream segments
# imports data and libraries from get_gnis_flines.R and get_netamounts.R and uses output data from these scripts to run call analysis

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)
library(AOI)
library(nhdplusTools)
library(sf)

source("R/get_gnis_flines.R")
source("R/utils.R")

# path to save call analysis data
save_path <- "data/wdid_call_analysis.rds"

# start and end dates
start_date = "1980-01-01"
end_date   = "2023-01-01"

# API token for CDSS rest services, provide token if expecting to go over guest API limits
api_key = "2fx+0sUzKbpOWeqkWzbU4BIIOtpwoVyE"

# Call function to get call data
call_df <- get_call_data(
  wdid_df    = wr_gnis,
  start_date = start_date,
  end_date   = end_date,
  api_key    = api_key,
  save_path  = save_path
  )

