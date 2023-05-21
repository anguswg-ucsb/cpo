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
source("R/get_nrcs_forecasts.R")

# weekly model data save path
model_data_path <- "data/model_data.rds"

# monthly model data save path
month_data_path <- "data/model_month_data.rds"

# district shape path
district_path <- "data/water_districts_simple.geojson"

# forecasts_df$wdid %>% unique() %>% length()
# week_calls$wdid %>% unique() %>% length()

# forecasts_df %>%
#     dplyr::filter(wdid %in% unique(week_calls$wdid)) %>%
#     .$wdid %>%
#     unique() %>%
#     length()


if(file.exists(model_data_path) & file.exists(month_data_path)) {

    message(paste0(
        "Reading model data: ",
        "\n---> ", model_data_path,
        "\n---> ", month_data_path
    ))

    # read in weekly model data
    mod_df <- readRDS(model_data_path)

    # read in monthly model data
    mod_month <- readRDS(month_data_path)

} else {

    message(paste0("Joining climate and call analysis data"))

    # join weekly climate, SNOTEL, out of priority calls, and NRCS forecasts data
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
        dplyr::filter(date %in% unique(snotel_df$date)) %>%
        dplyr::left_join(
            dplyr::select(snotel_df, -basin),
            by = c("district", "date")
        ) %>%
        dplyr::filter(!is.na(swe), !is.na(mar_swe), !is.na(apr_swe), !is.na(may_swe)) %>% # remove missing SWE values
        dplyr::ungroup() %>%
        dplyr::mutate(
            month_date = as.Date(paste0(format(date, "%Y-%m"), "-01"))
        ) %>%
        dplyr::left_join(
            dplyr::select(forecasts_df, district, wdid, station_code, month_date = date, ep_50, ep_90),
            by = c("district", "wdid", "month_date")
        ) %>%
        dplyr::relocate(basin, district, date, wdid, gnis_id, station_code)

    # Join monthly calls, monthly SWE, and monthly forecasts data into single dataset
    mod_month <-
        aggreg_calls_month(week_calls) %>%
        dplyr::left_join(
                na.omit(
                    dplyr::mutate(
                        dplyr::ungroup(
                            dplyr::summarise(
                                dplyr::group_by(
                                    dplyr::mutate(
                                        snotel_df,
                                        date = paste0(format(date, "%Y-%m"), "-01")
                                    ),
                                    basin, district, date
                                ),
                                dplyr::across(where(is.numeric), \(x) mean(x, na.rm  = TRUE))
                            )
                        ),
                        date = as.Date(date)
                    )
                ),
            by = c("district", "date")
        ) %>%
        # dplyr::filter(date %in% unique(forecasts_df$date)) %>%
        dplyr::left_join(
            dplyr::select(forecasts_df, district, wdid, station_code, date, ep_50, ep_90),
            by = c("district", "wdid", "date")
        ) %>%
        dplyr::relocate(basin, district, date, wdid, gnis_id, station_code)

    # # calculate monthly SWE data
    # month_snow <- na.omit(
    #                 dplyr::mutate(
    #                     dplyr::ungroup(
    #                         dplyr::summarise(
    #                             dplyr::group_by(
    #                                 dplyr::mutate(
    #                                     snotel_df,
    #                                     date = paste0(format(date, "%Y-%m"), "-01")
    #                                     ),
    #                                 basin, district, date
    #                                 ),
    #                             dplyr::across(where(is.numeric), \(x) mean(x, na.rm  = TRUE))
    #                             )
    #                         ),
    #                     date = as.Date(date)
    #                     )
    #                 )
    #
    # # calculate monthly calls data
    # month_calls <- aggreg_calls_month(week_calls)
    #
    # # Join monthly calls, monthly SWE, and monthly forecasts data into single dataset
    # mod_month <-
    #     month_calls %>%
    #     dplyr::left_join(
    #         dplyr::select(month_snow, -basin),
    #         by = c("district", "date")
    #     ) %>%
    #     dplyr::filter(date %in% unique(forecasts_df$date)) %>%
    #     dplyr::left_join(
    #         dplyr::select(forecasts_df, district, wdid, station_code, date, ep_50, ep_90),
    #         by = c("district", "wdid", "date")
    #     )

    message(paste0(
        "Saving model data: ",
        "\n---> ", model_data_path,
        "\n---> ", month_data_path
    ))

    # save path
    saveRDS(mod_df, model_data_path)
    saveRDS(mod_month, month_data_path)

}

rm(clim_ts, wr_net, dist_shp, wr_pts, week_calls, forecasts_df, gnis_flines, snotel_df,
   wr_gnis, districts_path, end_date, start_date, site_path, gnis_path,
   api_key, weekly_call_path, wr_net_path, wr_pts_path, swe_path, uwdids_path,
   climate_path, call_save_path, wr_gnis_path)

#  =======================================
#  =======================================
#
#
# modsdf <-
#     week_calls %>%
#     dplyr::left_join(
#         clim_ts,
#         by = c("district", "date")
#     ) %>%
#     dplyr::left_join(
#         dplyr::mutate(
#             dplyr::select(
#                 sf::st_drop_geometry(sf::read_sf(district_path)), district = DISTRICT, basin = BASIN
#             ),
#             district = dplyr::case_when(
#                 as.numeric(district) < 10 ~ paste0("0", district),
#                 TRUE                      ~ paste0(district)
#             )
#         ),
#         by = c("district")
#     ) %>%
#     dplyr::relocate(basin, district) %>%
#     dplyr::filter(date %in% unique(snotel_df$date)) %>%
#     dplyr::mutate(
#         year     = lubridate::year(date),
#         week_num = strftime(date, format = "%V")
#     ) %>%
#     dplyr::left_join(
#         dplyr::select(
#             dplyr::mutate(
#                 expanded_df,
#                 year     = lubridate::year(date),
#                 week_num = strftime(date, format = "%V"),
#             ),
#             -date, -basin
#         ),
#         # by = c("basin", "district", "date")
#         relationship = "many-to-many",
#         by           = c("district", "year", "week_num")
#     )
#
# modsdf %>% dplyr::filter(is.na(swe))
# dplyr::mutate(
#     month = lubridate::month(date)
# ) %>%
#     dplyr::group_by(month, basin) %>%
#     dplyr::mutate(
#         swe = impute_mean(swe)
#     ) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-month) %>%
#     dplyr::mutate(
#         out = dplyr::case_when(
#             out_pct > 0 ~ "1",
#             TRUE        ~ "0"
#         ),
#         out = factor(out, levels = c("1", "0"))
#     ) %>%
#     dplyr::relocate(out, .after = out_pct)
















