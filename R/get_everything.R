# Angus Watters
# Script that sources get_climate.R and get_out_pct.R scripts which will pull together all the climate and call analysis data used for modeling
# Timescale: Weekly averages
# Spatial res of outcome dataset: Max/Min water rights selected for each major GNIS ID flowline in each water district
# Spatial res of predictor dataset: Weekly climate data rasters are averaged across entire water districts. Climate variables are assigned to WDID/GNIS IDs according to water district (i.e. a WDID/GNIS ID within district 6 is assigned the weekly average climate variables across all of district 6)

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

# # dates to get data for
start_date = "1980-01-01"
end_date   = Sys.Date()
# start_date = "2010-01-01"
# end_date   = "2015-01-01"

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
  # annual_data <- lapply(1:nrow(dist_shp), function(i) {
  annual_data <- lapply(1:4, function(i) {

    # i = 3

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
      # tops <- sort(unique(dplyr::filter(gnis, streamcalc != 0)$streamleve))[2]
      # tops <- sort(unique(dplyr::filter(gnis, streamcalc != 0)$streamleve))[1]

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

      # mapview::mapview(dist_shp[i, ])
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
        # dplyr::filter(wdid == "0604255") %>%
        dplyr::filter(!is.na(longitude) | !is.na(latitude)) %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

      # mapview::mapview(dist_shp[i, ]) + pts
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
      # pts <- pts[1, ]
      message("Upstream water right WDID:\n", paste0(unique(pts$wdid), sep = "\n"))

       calls <- lapply(1:nrow(pts), function(y) {
         message("Pausing iteration for 2.5 minutes...")

         # add pause in loop as to not overwhelm CDSS resources
         # Sys.sleep(150)

         message("Iteration resuming...")

         # go get Out of Priority percent data
         req_data <- get_call_data2(
           wdid_df    = pts[y, ],
           start_date = start_date,
           end_date   = end_date
           # start_date = "2000-01-01",
           # end_date   = "2015-12-31"
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

     call_windows <- list(
       "may_sep"  = c(5, 6, 7, 8, 9),
       "june_sep" = c(6, 7, 8, 9),
       "july_sep" = c(7, 8, 9) ,
       "aug_sep"  = c(8, 9)
     )

     avg_call_windows <- lapply(1:length(call_windows), function(z) {

      message("Calculating average and minimum calls for ",  names(call_windows)[z], "...")

       # z = 1
        tmp <-
          calls %>%
          dplyr::filter(
            month %in% call_windows[[z]]
          ) %>%
          dplyr::mutate(
            avg_call_group = names(call_windows)[z]

          ) %>%
          dplyr::group_by(year, wdid) %>%
          dplyr::mutate(
            avg_call_year = mean(call_year, na.rm = T),
            min_call_year = lubridate::year(min(priority_date, na.rm = T))
          ) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::select(district, wdid, gnis_id, year, avg_call_group, avg_call_year, min_call_year) %>%
          tidyr::pivot_longer(
            cols = c(avg_call_year, min_call_year)
          ) %>%
          dplyr::mutate(
            name = paste0(gsub("_year", "", name), "_", avg_call_group)
          ) %>%
          dplyr::select(-avg_call_group, -district)
          # stats::setNames(c("district", "wdid", "gnis_id", "year", "avg_call_group",
          #                   paste0("avg_call_", names(call_windows)[z]),
          #                   paste0("min_call_", names(call_windows)[z])
          #                   )
          #                 ) %>%
          # dplyr::select(-avg_call_group)

        tmp
     }) %>%
       dplyr::bind_rows()%>%
       tidyr::pivot_wider(
         names_from  = name,
         values_from = value
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
       dplyr::ungroup() %>%
       dplyr::select(district, wdid, water_source, approp_date, year, avg_call_year) %>%
       dplyr::left_join(
         avg_call_windows,
         by = c("year", "wdid")
       ) %>%
       dplyr::relocate(district, wdid,gnis_id, water_source, approp_date,)
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

     # fx_periods <- c("MAR-JUL", "APR-JUN", "APR-JUL", "APR-SEP", "MAY-JUL", "MAY-SEP")
     fx_periods <- c("APR-JUL", "APR-SEP", "MAY-JUL", "MAY-SEP", "JUN-SEP")
      # j = 1
     # forecast periods to go get
     # fx_periods <- c("APR-JUL", "MAY-JUL")

     fxs <- lapply(1:length(fx_periods), function(j) {

       message(j, "/", length(fx_periods))
       message("Getting data for ", fx_periods[j])

       tryCatch({

         # get NRCS forecasts data
         nrcs_df <- batch_get_forecasts_pubdate(
           station_df      = sites_df,
           element_cd      = "SRVO",
           forecast_period = fx_periods[j],
           begin_date      = "1970-01-01",
           end_date        = "2023-01-01",
           state           = "CO",
           network         = "USGS",
           id_col          = "usgs_id"
         ) %>%
           dplyr::mutate(
             forecast_period = fx_periods[j]
           )

         nrcs_df

       }, error = function(e) {

         message("No data found for forecast period ", fx_periods[j])

         NULL

       })

     }) %>%
       dplyr::bind_rows()

     fxs <-
       fxs %>%
       dplyr::mutate(
         date = as.Date(date),
         pub_month = tolower(lubridate::month(pub_date, label = T)),
         pub_year  = lubridate::year(pub_date),
         # col_name  = paste0(tolower(gsub("-.*", "", forecast_period)), "_fx_", tolower(gsub("-", "_", forecast_period)))
         col_name  = paste0(pub_month, "_fx_", tolower(gsub("-", "_", forecast_period)))
       ) %>%
       dplyr::filter(exceedance_prob == "50", pub_month %in% c("apr", "may")) %>%
       dplyr::group_by(pub_date, col_name, forecast_period) %>%
         # dplyr::group_by(col_name, forecast_period) %>%
       dplyr::arrange(pub_month) %>%
       dplyr::mutate(
         exceedance_vals = sum(exceedance_vals, na.rm = T)
       ) %>%
       dplyr::slice(1) %>%
       dplyr::ungroup() %>%
       dplyr::select(basin, district, year = pub_year, col_name, exceedance_vals) %>%
       tidyr::pivot_wider(
         # id_cols = c(-year, -basin, -district),
         names_from  = col_name,
         values_from = exceedance_vals
       ) %>%
       dplyr::relocate(sort(names(.))) %>%
       dplyr::relocate(basin, district, year)

    # fxs3$pub_date %>% unique()
     # # get NRCS forecasts data
     # nrcs_df <- batch_get_forecasts(
     #   station_df      = sites_df,
     #   # station_df      =   ref_tbl[1, ],
     #   element_cd      = "SRVO",
     #   forecast_period = "APR-JUL",
     #   state           = "CO",
     #   network         = "USGS"
     # )

     # # calculate sum across all forecasts point for the given district for each month
     # nrcs <-
     #   nrcs_df %>%
     #   dplyr::mutate(
     #     year  = as.character(lubridate::year(date)),
     #     month = as.character(lubridate::month(date, label = T)),
     #     year_mon = paste0(as.character(lubridate::year(date)),
     #                       "_", as.character(lubridate::month(date, label = T)))
     #   ) %>%
     #   dplyr::filter(!month %in% c("Dec", "Jun")) %>%
     #   # dplyr::group_by(basin, district, date, exceedance_prob) %>%
     #   dplyr::group_by(basin, district, year_mon, exceedance_prob) %>%
     #   dplyr::summarise(
     #     exceedance_vals = sum(exceedance_vals, na.rm = T)
     #   ) %>%
     #   dplyr::ungroup() %>%
     #   dplyr::filter(exceedance_prob == "50") %>%
     #   tidyr::separate(year_mon, into = c("year", "month"), sep = "_") %>%
     #   dplyr::select(-exceedance_prob) %>%
     #   tidyr::complete(month, year, basin, district) %>%
     #   dplyr::group_by(month) %>%
     #   dplyr::mutate(
     #     exceedance_vals = ifelse(is.na(exceedance_vals) |is.nan(exceedance_vals),
     #                   mean(exceedance_vals, na.rm = TRUE), exceedance_vals)
     #   ) %>%
     #   dplyr::ungroup() %>%
     #   tidyr::pivot_wider(
     #     id_cols     = c(year),
     #     names_from  = month,
     #     values_from = exceedance_vals
     #   ) %>%
     #   stats::setNames(c("year", paste0(tolower(names(.)[!grepl("year", names(.))]),
     #                                    "_exceed_val")))

     # nrcs %>% stats::setNames(c("year", paste0(tolower(names(nrcs)[!grepl("year", names(nrcs))]),
     #                                    "_exceed_val")))


     # final join of all data
     final <-
       avg_year %>%
       dplyr::mutate(district = as.character(district)) %>%
       # dplyr::select(district, wdid, gnis_id, water_source, approp_date, year, contains("_call")) %>%
       dplyr::mutate(
         year = as.character(year)
       ) %>%
       dplyr::left_join(
         snotel_df,
         by = c("year", "district")
       ) %>%
       dplyr::relocate(basin, district) %>%
       dplyr::left_join(
         dplyr::select(
           dplyr::mutate(
            fxs,
            year = as.character(year)
         ),
         -basin, -district
         ),
         by = "year"
       ) %>%
      tidyr::fill(basin, .direction = "updown")

      # final %>%
     # ggplot2::ggplot() +
     #   ggplot2::geom_point(ggplot2::aes(x = apr_exceed_val, y = avg_call_year, color = district))

     final

     }, error = function(e) {

      message(paste0("Skipping iteration: ", i, "Error:\n", e))

      NULL

    })


  })

  annual_data2 <-
    annual_data %>%
    dplyr::bind_rows()

  # sprintf("%.5s", sub_flines$gnis_id[1])
  message(paste0("Saving data to path ---> ", annual_path))

  # save rds
  saveRDS(annual_data, annual_path)

}

