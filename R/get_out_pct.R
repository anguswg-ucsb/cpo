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
call_save_path    <- "data/wdid_call_analysis.rds"
# weekly_call_path  <- "data/wdid_call_analysis_weekly.rds"
weekly_call_path  <- "data/wdid_call_analysis_week.rds"
monthly_call_path <- "data/wdid_call_analysis_month.rds"

# start and end dates
start_date = "1980-01-01"
end_date   = "2023-01-01"

# API token for CDSS rest services, provide token if expecting to go over guest API limits
api_key = NULL

# load and go get call analysis data
if(file.exists(weekly_call_path) & file.exists(monthly_call_path)) {

  message(paste0(
    "Reading weekly call data: ",
    "\n---> ", weekly_call_path
  ))

  # read in call analysis data
  week_calls <- readRDS(weekly_call_path)
  # call_df <- readRDS(weekly_call_path)

  # read in call analysis data
  month_calls <- readRDS(monthly_call_path)

  # # read in daily calls data
  # daily_calls <- readRDS(call_save_path)


} else {

  # load and go get call analysis data
  if(file.exists(call_save_path)) {

    message(paste0(
      "Reading daily call data: ",
      "\n---> ", call_save_path
    ))

    # read in call analysis data
    daily_calls <- readRDS(call_save_path)

    message(paste0("Calculating weekly call data..."))

    # calculate weekly average calls data
    week_calls <- aggreg_calls2(df = daily_calls)

    # calculate weekly average calls data
    month_calls <- aggreg_calls_month(df = week_calls)

    message(paste0(
      "Saving weekly/monthly call data: ",
      "\n---> ", weekly_call_path,
      "\n---> ", monthly_call_path
    ))

    saveRDS(week_calls, weekly_call_path)
    saveRDS(month_calls, monthly_call_path)

    rm(daily_calls)

  } else {


    daily_calls <- get_call_data(
      wdid_df    = wr_gnis,
      start_date = start_date,
      end_date   = end_date,
      api_key    = api_key
    )

    message(paste0(
      "Saving daily call data: ",
      "\n---> ", call_save_path,
    ))

    # save daily call data
    saveRDS(daily_calls, call_save_path)

    message(paste0("Calculating weekly call data..."))

    # calculate weekly average calls data
    week_calls <- aggreg_calls2(df = daily_calls)

    # calculate weekly average calls data
    month_calls <- aggreg_calls_month(df = week_calls)

    message(paste0(
      "Saving weekly/monthly call data: ",
      "\n---> ", weekly_call_path,
      "\n---> ",  gsub(".rds", ".csv", weekly_call_path),
      "\n---> ", monthly_call_path
    ))

    # weekly save
    saveRDS(week_calls, weekly_call_path)
    readr::write_csv(week_calls,    gsub(".rds", ".csv", weekly_call_path))

    # monthly save
    saveRDS(month_calls, monthly_call_path)


    rm(daily_calls)

    }

}




# # ######################################################################
# # ######################################################################
# # ######################################################################

# wr_gnis
# aoi <- aoi_get()
# cdssr::get_sw_stations(dist_shp[1, ], radius = 15)
#
# nrcs %>%
#   dplyr::group_by(station_code) %>%
#   dplyr::slice(1) %>%
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   mapview::mapview() + dist_shp
# nrcs$station_code %>% unique()
# dist_shp[1, ]
# dataRetrieval::findNLDI(comid = coms$comid[9])

# rm( wr_net, wr_gnis, wr_pts, gnis_flines)
# # ######################################################################
# # ######################################################################
#
# # back calculate out_pct given WDID admin numbers compared to water right from other WDID that calls out 99999.00000 admin number
#
# week_calls <- readRDS(weekly_call_path)
# # calls_df <- readRDS(call_save_path)
#
# week_calls <- aggreg_calls(calls_df)
# out <-
#   calls_df %>%
#   dplyr::filter(district == "02") %>%
#   dplyr::mutate(
#     year = lubridate::year(datetime)
#   ) %>%
#   dplyr::filter(year %in% c(2002, 2003)) %>%
#   dplyr::select(-year)
#
# out %>%
#   # rect_out_pct() %>%
#   # tidyr::pivot_longer(cols = c(out_pct, out_pct2)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(x = datetime, y = analysis_out_of_priority_percent_of_day), size = 1) +
#   # ggplot2::geom_line(ggplot2::aes(x = datetime, y = value, color = name), size = 1.5) +
#   # ggplot2::facet_wrap(~seniority, nrow = 3)
#   ggplot2::facet_grid(seniority~wdid_gnis_id)
#
# tmp <-
#   out %>%
#   # rect_out_pct() %>%
#   aggreg_calls()
#
# tmp %>%
#   # tidyr::pivot_longer(cols = c(out_pct, out_pct2)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct), size = 1) +
#   # ggplot2::geom_line(ggplot2::aes(x = datetime, y = value, color = name), size = 1.5) +
#   # ggplot2::facet_wrap(~seniority, nrow = 3)
#   ggplot2::facet_grid(seniority~gnis_id)
# # aggregate the output dataframe from get_call_data from daily timesteps to a weekly timestep to align with average weekly climate data
# calc_out_pct <- function(df) {
#
#   # # Convert the sequence to a data frame
#   # date_df <-
#   #   data.frame(
#   #     # date = seq(as.Date("1979-12-31"), as.Date("2022-12-26"), by = "7 days")
#   #     date = seq(as.Date("1979-12-31"), Sys.Date(), by = "7 days")
#   #   ) %>%
#   #   dplyr::mutate(
#   #     year     = lubridate::year(date),
#   #     week_num = strftime(date, format = "%V")
#   #   )
#
#   out <-
#       # df %>%
#     calls_df %>%
#     # dplyr::filter(wdid_gnis_id == "1385432") %>%
#     dplyr::filter(district == "05") %>%
#     dplyr::mutate(
#       year = lubridate::year(datetime)
#     ) %>%
#     dplyr::filter(year %in% c(2002, 2003)) %>%
#     dplyr::select(datetime, district,
#                   gnis_id = wdid_gnis_id,
#                   seniority,
#                   wdid = analysis_wdid,
#                   priority_wdid,
#                   wdid_priority_date = wdid_approp_date,
#                   priority_date = priority_date,
#                   # wdid_approp_date,
#                   # priority_date,
#                   out_pct = analysis_out_of_priority_percent_of_day
#                   # priority_wdid,
#                   # priority_date,
#                   # wdid_approp_date,
#                   # wdid_structure_name, wdid_structure_type, seniority
#     ) %>%
#     dplyr::mutate(
#       priority_date = dplyr::case_when(
#         is.na(priority_date) ~ "2099-01-01",
#         TRUE                 ~ priority_date
#       ),
#       out_pct = dplyr::case_when(
#         wdid_priority_date <= priority_date ~ 0,
#         TRUE ~ out_pct
#       )
#     )
#   out %>%
#     dplyr::group_by(gnis_id)
#
#   out <- calls_df %>%
#   dplyr::filter(district == "05") %>%
#     dplyr::mutate(
#       year = lubridate::year(datetime)
#     ) %>%
#     dplyr::filter(year %in% c(2002, 2003)) %>%
#     dplyr::select(-year)
#   week_calls <- aggreg_calls(df = out)
#
#
#   out %>%
#     # tidyr::pivot_longer(cols = c(out_pct, out_pct2)) %>%
#     ggplot2::ggplot() +
#     ggplot2::geom_line(ggplot2::aes(x = datetime, y = out_pct), size = 1.5) +
#     # ggplot2::geom_line(ggplot2::aes(x = datetime, y = value, color = name), size = 1.5) +
#     # ggplot2::facet_wrap(~seniority, nrow = 3)
#     ggplot2::facet_grid(seniority~gnis_id)
#     # dplyr::select(datetime, district,
#     #               wdid = analysis_wdid,
#     #               wdid_gnis_id,
#     #               out_pct = analysis_out_of_priority_percent_of_day,
#     #               priority_wdid,
#     #               priority_date,
#     #               wdid_approp_date,
#     #               wdid_structure_name, wdid_structure_type, seniority
#     # )
#
# }
# ######################################################################
# ######################################################################

