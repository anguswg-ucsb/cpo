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

# save path
model_data_path <- "data/model_data.rds"

# district shape path
district_path <- "data/water_districts_simple.geojson"

# forecasts_df$wdid %>% unique() %>% length()
# week_calls$wdid %>% unique() %>% length()

# forecasts_df %>%
#     dplyr::filter(wdid %in% unique(week_calls$wdid)) %>%
#     .$wdid %>%
#     unique() %>%
#     length()


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
        dplyr::filter(date %in% unique(snotel_df$date)) %>%
        dplyr::left_join(
            dplyr::select(snotel_df, -basin),
            by = c("district", "date")
        ) %>%
        dplyr::filter(!is.na(swe), !is.na(mar_swe), !is.na(apr_swe), !is.na(may_swe)) %>% # remove missing SWE values
        # dplyr::mutate(
        #     month = lubridate::month(date)
        # ) %>%
        # dplyr::group_by(month, basin, district) %>%
        # dplyr::mutate(
        #     dplyr::across(contains("swe"), impute_mean)
        #     # swe = impute_mean(swe)
        # ) %>%
        dplyr::ungroup() %>%
        # dplyr::select(-month) %>%
        dplyr::mutate(
            out = dplyr::case_when(
                out_pct > 0 ~ "1",
                TRUE        ~ "0"
            ),
            out = factor(out, levels = c("1", "0"))
        ) %>%
        dplyr::relocate(out, .after = out_pct)



        dplyr::select(
            dplyr::mutate(
                forecasts_df,
                mon_year  = paste0(lubridate::month(date, label = T), "_",  lubridate::year(date))
                ),
        district, wdid, mon_year, ep_50, ep_90
        )

    mod_df2 <-
        mod_df %>%
        dplyr::mutate(
            mon_year  = paste0(lubridate::month(date, label = T), "_",  lubridate::year(date))
        ) %>%
        dplyr::left_join(
            dplyr::select(
                dplyr::mutate(
                    forecasts_df,
                    mon_year  = paste0(lubridate::month(date, label = T), "_",  lubridate::year(date))
                ),
                district, wdid, mon_year, ep_50, ep_90
            ),
            by = c("district", "wdid", "mon_year")
        ) %>%
        dplyr::relocate(mon_year, ep_50, ep_90)
    max(forecasts_df$date)

    tmp <-
        mod_df2 %>%
        dplyr::filter(!is.na(ep_50)) %>%
        dplyr::select(basin, district, gnis_id, wdid, approp_date, seniority, date, out_pct, out, swe, ep_50, ep_90)

    tmp %>%
        dplyr::group_by(district, seniority, date) %>%
        dplyr::summarise(
            dplyr::across(c(out_pct, swe, ep_50, ep_90), \(x) mean(x, na.rm = TRUE))
        )

    daily_calls$datetime[1]
    month_count <-
        daily_calls %>%
        dplyr::slice(40000:50000) %>%
        dplyr::mutate(
            # date = as.Date(analysis_date),
            mon_year  = paste0(lubridate::month(datetime, label = T), "_",  lubridate::year(datetime))
        ) %>%
        dplyr::mutate(
            out = dplyr::case_when(
                analysis_out_of_priority_percent_of_day > 0 ~ 1,
                TRUE                                        ~ 0
                ),
            month = format(datetime, "%Y-%m")
            ) %>%
        dplyr::select(district,wdid = analysis_wdid, seniority, mon_year, month, out_pct = analysis_out_of_priority_percent_of_day, out) %>%
        # dplyr::group_by(district, wdid, seniority, mon_year) %>%
        dplyr::group_by(district, wdid, seniority, month) %>%
        dplyr::summarise(total_occurrences = sum(out))
        # dplyr::add_count()
        dplyr::summarise(
            out_pct   = mean(out_pct, na.rm = T),
            out_count = sum(out, na.rm = T)
        )
        dplyr::add_count() %>%
        dplyr::select(district, seniority, mon_year, out_pct = analysis_out_of_priority_percent_of_day, out, n)
        dplyr::group_by(district, seniority, mon_year) %>%
        dplyr::add_count()
        dplyr::summarise(
            out_pct   = mean(out_pct, na.rm = T),
            out_count =
        )
    message(paste0(
        "Saving model data: ",
        "\n---> ", model_data_path
    ))

    # save path
    saveRDS(mod_df, model_data_path)

}

rm(clim_ts, wr_net, dist_shp, wr_pts, week_calls, gnis_flines, snotel_df,
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
















