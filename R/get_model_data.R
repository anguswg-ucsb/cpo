# Angus Watters
# Script that sources get_climate.R and get_out_pct.R scripts which will pull together all the climate and call analysis data used for modeling
# Timescale: Weekly averages
# Spatial res of outcome dataset: Max/Min water rights selected for each major GNIS ID flowline in each water district
# Spatial res of predictor dataset: Weekly climate data rasters are averaged across entire water districts. Climate variables are assigned to WDID/GNIS IDs according to water district (i.e. a WDID/GNIS ID within district 6 is assigned the weekly average climate variables across all of district 6)

# load libraries
library(climateR)
library(terra)
library(dplyr)
library(sf)

# source climate/call analysis data and/or go get the data if its not there
source("R/get_climate.R")
source("R/get_out_pct.R")

# save path
model_data_path <- "data/model_data.rds"

if(file.exists(model_data_path)) {

    message(paste0(
        "Reading model data: ",
        "\n---> ", model_data_path
    ))

    # get climate gridMET
    mod_df <- readRDS(model_data_path)

} else {

    message(paste0("Joining climate and call analysis data"))

    mod_df <-
        call_df %>%
        dplyr::left_join(
            clim_ts,
            by = c("district", "date")
        )

    message(paste0(
        "Saving model data: ",
        "\n---> ", model_data_path
    ))

    # save path
    saveRDS(mod_df, model_data_path)

}

rm(call_df, clim_ts, wr_net, dist_shp)
