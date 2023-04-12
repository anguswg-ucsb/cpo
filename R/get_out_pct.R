# Angus Watters
# Collect water right call analysis data for WDIDs along major GNIS ID stream segments
# imports data and libraries from get_gnis_flines.R and get_netamounts.R and uses output data from these scripts to run call analysis

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)
library(AOI)
library(sf)

source("R/get_gnis_flines.R")
source("R/utils.R")

# path to save call analysis data
call_save_path   <- "data/wdid_call_analysis.rds"
weekly_call_path <- "data/wdid_call_analysis_weekly.rds"

# start and end dates
start_date = "1980-01-01"
end_date   = "2023-01-01"

# API token for CDSS rest services, provide token if expecting to go over guest API limits
api_key = NULL

# load and go get call analysis data
if(file.exists(weekly_call_path)) {

  message(paste0(
    "Reading weekly call data: ",
    "\n---> ", weekly_call_path
  ))

  # read in call analysis data
  call_df <- readRDS(weekly_call_path)

} else {
  # load and go get call analysis data
  if(file.exists(call_save_path)) {

    message(paste0(
      "Reading daily call data: ",
      "\n---> ", call_save_path
    ))

    # read in call analysis data
    call_df <- readRDS(call_save_path)

    message(paste0("Calculating weekly call data..."))

    # calculate weekly average calls data
    weekly_calls <- aggreg_calls(df = call_df)

    message(paste0(
      "Saving weekly call data: ",
      "\n---> ", weekly_call_path
    ))

    saveRDS(weekly_calls, weekly_call_path)

    rm(call_df)

  }

  call_df <- get_call_data(
    wdid_df    = wr_gnis,
    start_date = start_date,
    end_date   = end_date,
    api_key    = NULL
  )

  message(paste0(
    "Saving daily call data: ",
    "\n---> ", call_save_path,
  ))

  # save daily call data
  saveRDS(call_df, call_save_path)

  message(paste0("Calculating weekly call data..."))

  # calculate weekly average calls data
  weekly_calls <- aggreg_calls(df = call_df)

  message(paste0(
    "Saving weekly call data: ",
    "\n---> ", weekly_call_path
  ))

  saveRDS(weekly_calls, weekly_call_path)

  rm(call_df)

}

