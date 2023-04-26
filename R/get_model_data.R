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
source("R/get_snotel.R")
source("R/get_out_pct.R")

# save path
model_data_path <- "data/model_data.rds"

# district shape path
district_path <- "data/water_districts_simple.geojson"

if(file.exists(model_data_path)) {

    message(paste0(
        "Reading model data: ",
        "\n---> ", model_data_path
    ))

    # get climate gridMET
    mod_df <- readRDS(model_data_path)

} else {

    message(paste0("Joining climate and call analysis data"))

    # join climate, snotel, and out of priority data
    mod_df <-
        week_calls %>%
        dplyr::left_join(
            clim_ts,
            by = c("district", "date")
        ) %>%
        dplyr::left_join(
            dplyr::mutate(
                dplyr::select(
                    sf::st_drop_geometry(sf::read_sf(district_path)), district = DISTRICT, basin = BASIN
                ),
                district = dplyr::case_when(
                    as.numeric(district) < 10 ~ paste0("0", district),
                    TRUE                      ~ paste0(district)
                )
            ),
            by = c("district")
        ) %>%
        dplyr::relocate(basin, district) %>%
        dplyr::left_join(
            snotel_df,
            by = c("basin", "date")
        ) %>%
        dplyr::mutate(
            month = lubridate::month(date)
        ) %>%
        dplyr::group_by(month, basin) %>%
        dplyr::mutate(
            swe = impute_mean(swe)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-month)

    message(paste0(
        "Saving model data: ",
        "\n---> ", model_data_path
    ))

    # save path
    saveRDS(mod_df, model_data_path)

}

rm(clim_ts, wr_net, dist_shp, wr_pts, week_calls)

#  =======================================
#  =======================================



















