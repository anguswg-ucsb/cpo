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
library(cdssr)

source("R/get_gnis_flines.R")
source("R/utils.R")

# # source climate/call analysis data and/or go get the data if its not there
# source("R/get_climate.R")
# source("R/get_snotel.R")
# source("R/get_out_pct.R")
# source("R/get_nrcs_forecasts.R")

# flines <-
#   gnis_flines %>%
#   dplyr::group_by(district) %>%
#   dplyr::summarise()
gnis_flines %>%
  dplyr::filter(district == "6") %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf( ggplot2::aes(color = factor(district)))

mapview::mapview(flines)
gnis_wr <- cdssr::get_call_analysis_gnisid(
  gnis_id     = "00201759",
  admin_no    = "99999.00000",
  stream_mile = "166.09",
  start_date  = "2000-01-01",
  end_date    = "2004-01-01"
  )

# Angus Watters
# Collect water right call analysis data for WDIDs along major GNIS ID stream segments

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)
library(AOI)
library(nhdplusTools)
library(sf)

source("R/utils.R")
# source("R/get_netamounts.R")

# local path to waterdistricts shape
# districts_path <- "data/water_districts_simple.geojson"
# gnis_path      <- "data/nhd_gnis_id_flines.rds"
# wr_gnis_path   <- "data/rights_by_gnis_id.rds"
# gnis_lst_path <- "data/district_gnisids.rds"

# # path to district snotel
# snotel_path <- "data/district_snotel_table.csv"

# local path to waterdistricts shape
districts_path <- "data/water_districts_simple.geojson"

# annual data
annual_path  <-  "data/annual_model_data.rds"

# path to reference table
ref_tbl_path <- "data/reference_tables/district_lookup_table.csv"

# reference table with USGS and Snotel IDs for each district
ref_tbl <- readr::read_csv(ref_tbl_path)

# dates to get data for
start_date = "1980-01-01"
end_date   = Sys.Date()
# start_date = "2010-01-01"
# end_date   = "2012-01-01"

# basin to get data for
basins <- c("South Platte")

# ***************************
# ---- get GNIS ID lines ----
# ***************************

# check if mainstem data path exists
if(file.exists(annual_path)) {

  message(paste0("Reading data from ---> ", annual_path))

  annual_data <- readRDS(annual_path)

  if(file.exists(districts_path)) {

    message(paste0("Reading data from ---> ", districts_path))

    dist_shp <-
      districts_path %>%
      sf::read_sf() %>%
      dplyr::filter(BASIN %in% basins) %>%
      dplyr::arrange(DISTRICT)

  } else {

    stop(paste0("Data not found at path ---> ", districts_path))

  }

} else {

  if(file.exists(districts_path)) {

    message(paste0("Reading data from ---> ", districts_path))

    dist_shp <-
      districts_path %>%
      sf::read_sf() %>%
      dplyr::filter(BASIN %in% basins) %>%
      dplyr::arrange(DISTRICT)


  } else {

    stop(paste0("Data not found at path ---> ", districts_path))

  }

  # loop over each huc4 and get mainstem of the river
  annual_data <- lapply(1:nrow(dist_shp), function(i) {

    message(paste0("District: ", dist_shp[i, ]$DISTRICT, " - (", i, "/", nrow(dist_shp), ")"))
    message(paste0("Pulling NHDPlus network data..."))

    # pull GNIS ID data
    gnis <- nhdplusTools::get_nhdplus(
      AOI         = dist_shp[i, ],
      realization = "flowline"
    )

    tryCatch({

      message(paste0("Locating mainstem rivers..."))

      # get lowest stream levels in AOI
      tops <- sort(unique(dplyr::filter(gnis, streamcalc != 0)$streamleve))[1:2]

      # lowest hydrologic point in district
      downstream_fline <-
        gnis %>%
        dplyr::filter(streamcalc != 0) %>%
        dplyr::filter(streamleve %in% tops) %>%
        # dplyr::filter(streamorde >= 2) %>%
        dplyr::mutate(dplyr::across(c(-geometry), as.character)) %>%
        # dplyr::group_by(terminalpa) %>%
        dplyr::group_by(streamleve) %>%
        dplyr::slice_min(hydroseq)

      # get upstream mainstem network from lowest hydrologic point in district
      um_net <-  lapply(1:length(unique(downstream_fline$comid)), function(y) {

        net <- nhdplusTools::navigate_network(
          start    = as.integer(unique(downstream_fline$comid)[y]),
          mode     = "UM",
          distance = 300
        ) %>%
          dplyr::mutate(origin_comid = as.character(unique(downstream_fline$comid)[y])) %>%
          dplyr::mutate(dplyr::across(c(-geometry), as.character))

        net

      }) %>%
        dplyr::bind_rows()

      # max flow lines for each streamlevel
      max_fline <-
        um_net %>%
        sf::st_filter(
          sf::st_transform(dist_shp[i, ], 4269),
          .predicate = st_within
          ) %>%
        dplyr::group_by(origin_comid) %>%
        dplyr::slice_max(hydroseq)

      message("Mainstem rivers:\n", paste0(unique(max_fline$gnis_name), sep = "\n"))

      # mapview::mapview(um_net, color = "green") + mapview::mapview(max_fline, color = "red") + dist_shp[i, ]

      # # get upstream GNIS IDs of the longest GNIS ID
      # upstreams <-
      #   gnis %>%
      #   dplyr::filter(streamcalc != 0) %>%
      #   dplyr::filter(streamorde >= 3) %>%
      #   dplyr::mutate(dplyr::across(c(-geometry), as.character)) %>%
      #   dplyr::group_by(gnis_id, gnis_name, streamorde) %>%
      #   dplyr::summarise() %>%
      #   dplyr::mutate(
      #     gnis_id    = dplyr::case_when(
      #       gnis_id == " " & gnis_name == " " ~ "no_gnis_id",
      #       gnis_id == " " & gnis_name != " " ~ gnis_name,
      #       TRUE                              ~ gnis_id
      #     ),
      #     gnis_name    = dplyr::case_when(
      #       gnis_name == " " & gnis_id %in%  c(" ", "no_gnis_id")  ~ "no_gnis_name",
      #       gnis_name == " " & !gnis_id %in%  c(" ", "no_gnis_id") ~ gnis_id,
      #       TRUE                                                   ~ gnis_name
      #     ),
      #     district   = dist_shp$DISTRICT[i],
      #     len        = units::drop_units(sf::st_length(geometry)),
      #     unit       = "meters"
      #   ) %>%
      #   dplyr::arrange(-len) %>%
      #   dplyr::ungroup() %>%
      #   # dplyr::group_by(district, gnis_id, gnis_name) %>%
      #   dplyr::group_by(district, gnis_id, gnis_name, streamorde) %>%
      #   dplyr::summarise(
      #     len = sum(len, na.rm = T)
      #   ) %>%
      #   # dplyr::ungroup() %>%
      #   dplyr::arrange(-len) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::filter(gnis_id != "no_gnis_id") %>%
      #   dplyr::mutate(streamorde = as.numeric(streamorde)) %>%
      #   dplyr::arrange(-streamorde, -len) %>%
      #   dplyr::slice_max(len) %>%
      #   dplyr::mutate(uid = paste0(district, "_", gnis_id))
      #
      # message("Mainstem rivers:\n", paste0( unique(upstreams$gnis_name, sep = "\n")))
      #
      # # most upstream flowline of district mainstem
      # max_fline <-
      #   gnis %>%
      #   dplyr::filter(gnis_id %in% c(upstreams$gnis_id)) %>%
      #   dplyr::slice_max(hydroseq)

      # get water rights information around most upstream of mainstems
      wr_net <- cdssr::get_water_rights_netamount(
        water_district    = dist_shp$DISTRICT[i]
        # aoi    =  sf::st_centroid(max_fline),
        # radius = 5
      )

      message(paste0("Determing most upstream water right on rivers"))

      # make points out of water rights transactions tdata
      pts <-
        wr_net %>%
        dplyr::tibble() %>%
        dplyr::mutate(
          lon     = longitude,
          lat     = latitude,
          gnis_id = sub("^0+", "", gnis_id)
        ) %>%
        dplyr::filter(!is.na(longitude) | !is.na(latitude)) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

      # if max_fline has a gnis_id then filter points down to that GNIS ID
      if (all(max_fline$gnis_id != " ")) {

        if(all(max_fline$gnis_id %in% pts$gnis_id)) {

          # Filter points down to GNIS ID of interest
          # pts <- dplyr::filter(pts, gnis_id %in% max_fline$gnis_id)
          pts <- dplyr::filter(pts, gnis_id %in% max_fline$gnis_id,  !grepl("GROUNDWATER", water_source))
        }

      }

      # find feature nearest to uppermost flow line of GNIS ID in each district
      pts <- pts[sf::st_nearest_feature(sf::st_transform(max_fline, 4326), pts), ]

      # plot(dist_shp[i, ]$geometry)
      # plot(pts$geometry, add = T)
      # plot(um_net$geometry, add = T)
      # plot(max_fline$geometry, lwd = 3, col = "red", add = T)
      # mapview::mapview(pts) + max_fline + um_net + dist_shp[i, ]

      # drop point geometry and just keep WDID info
      pts <-
        pts %>%
        sf::st_drop_geometry() %>%
        dplyr::select(
           district = water_district,
           wdid, gnis_id, water_source,
           appropriation_date, admin_number, lon, lat
           ) %>%
        dplyr::mutate(
          district = ifelse(district < 10, paste0("0", district), district)
        )

      message("Upstream water right WDID:\n", paste0(unique(pts$wdid), sep = "\n"))

       calls <- lapply(1:nrow(pts), function(y) {
         message("Pausing iteration for 2.5 minutes...")

         # add pause in loop as to not overwhelm CDSS resources
         Sys.sleep(150)

         message("Iteration resuming...")

         # go get Out of Priority percent data
         req_data <- get_call_data2(
           wdid_df    = pts[y, ],
           start_date = start_date,
           end_date   = end_date
           # start_date = "1980-01-01",
           # end_date   = Sys.Date()
         )

         req_data

       }) %>%
         dplyr::bind_rows()

     # get average call year data
     calls <-
       calls %>%
       dplyr::tibble() %>%
       dplyr::select(
         district, datetime,
         wdid        = analysis_wdid,
         gnis_id     = wdid_gnis_id,
         water_source,
         approp_date = wdid_approp_date,
         priority_date,
         out_pct     = analysis_out_of_priority_percent_of_day
         ) %>%
       dplyr::mutate(
         year      = lubridate::year(datetime),
         month     = lubridate::month(datetime),
         call_year = lubridate::year(priority_date)
         # call_year = ifelse(is.na(call_year), 2099, call_year)
       )

     # calculate average call year across entire year
     avg_year <-
       calls %>%
       dplyr::group_by(wdid, year) %>%
       dplyr::mutate(
         avg_call_year = mean(call_year, na.rm = T)
       ) %>%
       dplyr::ungroup() %>%
       dplyr::group_by(wdid, year) %>%
       dplyr::slice(1) %>%
       dplyr::ungroup()
       # names()

     # avg_year %>%
     #   ggplot2::ggplot() +
     #   ggplot2::geom_line(ggplot2::aes(x = datetime, y = avg_call_year))

     # subset reference table
     sites_df <-
       ref_tbl %>%
        dplyr::filter(district == ifelse(
                                  dist_shp$DISTRICT[i] < 10,
                                  paste0("0", dist_shp$DISTRICT[i]),
                                  paste0(dist_shp$DISTRICT[i])
                                  )
                                )

     # get snotel data from snotel_id in sites_df
     snotel_df <- get_snotel_peaks(
                         sites_df,
                         id_col = "snotel_id"
                         )


     # get NRCS forecasts data
     nrcs_df <- batch_get_forecasts(
       station_df      = sites_df,
       # station_df      =   ref_tbl[1, ],
       element_cd      = "SRVO",
       forecast_period = "APR-JUL",
       state           = "CO",
       network         = "USGS"
     )

     # calculate sum across all forecasts point for the given district for each month
     nrcs <-
       nrcs_df %>%
       dplyr::mutate(
         year  = as.character(lubridate::year(date)),
         month = as.character(lubridate::month(date, label = T)),
         year_mon = paste0(as.character(lubridate::year(date)),
                           "_", as.character(lubridate::month(date, label = T)))
       ) %>%
       dplyr::filter(!month %in% c("Dec", "Jun")) %>%
       # dplyr::group_by(basin, district, date, exceedance_prob) %>%
       dplyr::group_by(basin, district, year_mon, exceedance_prob) %>%
       dplyr::summarise(
         exceedance_vals = sum(exceedance_vals, na.rm = T)
       ) %>%
       dplyr::ungroup() %>%
       dplyr::filter(exceedance_prob == "50") %>%
       tidyr::separate(year_mon, into = c("year", "month"), sep = "_") %>%
       dplyr::select(-exceedance_prob) %>%
       tidyr::complete(month, year, basin, district) %>%
       dplyr::group_by(month) %>%
       dplyr::mutate(
         exceedance_vals = ifelse(is.na(exceedance_vals) |is.nan(exceedance_vals),
                       mean(exceedance_vals, na.rm = TRUE), exceedance_vals)
       ) %>%
       dplyr::ungroup() %>%
       tidyr::pivot_wider(
         id_cols     = c(year),
         names_from  = month,
         values_from = exceedance_vals
       ) %>%
       stats::setNames(c("year", paste0(tolower(names(.)[!grepl("year", names(.))]),
                                        "_exceed_val")))

     # nrcs %>% stats::setNames(c("year", paste0(tolower(names(nrcs)[!grepl("year", names(nrcs))]),
     #                                    "_exceed_val")))


     # final join of all data
     final <-
       avg_year %>%
       dplyr::mutate(district = as.character(district)) %>%
       dplyr::select(district, wdid, gnis_id, water_source, approp_date, year, avg_call_year) %>%
       dplyr::mutate(
         year = as.character(year)
       ) %>%
       dplyr::left_join(
         snotel_df,
         by = c("year", "district")
       ) %>%
       dplyr::relocate(basin, district) %>%
       dplyr::left_join(
         nrcs,
         by = "year"
       )

     # annual_data %>%
     # ggplot2::ggplot() +
     #   ggplot2::geom_point(ggplot2::aes(x = may_swe, y = avg_call_year, color = district))

     final

     }, error = function(e) {

      message(paste0("Skipping iteration: ", i, "Error:\n", e))

      NULL

    })


  }) %>%
    dplyr::bind_rows()

  # sprintf("%.5s", sub_flines$gnis_id[1])
  message(paste0("Saving data to path ---> ", annual_path))

  # save rds
  saveRDS(annual_data, annual_path)

}

