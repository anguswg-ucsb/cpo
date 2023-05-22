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

source("R/get_netamounts.R")

# local path to waterdistricts shape
districts_path <- "data/water_districts_simple.geojson"
gnis_path      <- "data/nhd_gnis_id_flines.rds"
# wr_gnis_path   <- "data/rights_by_gnis_id.rds"
gnis_lst_path <- "data/district_gnisids.rds"

# path to district snotel
snotel_path <- "data/district_snotel_table.csv"

# ***************************
# ---- get GNIS ID lines ----
# ***************************

# check if mainstem data path exists
if(file.exists(gnis_lst_path)) {

  message(paste0("Reading data from ---> ", gnis_path))

  gnisid_df <- readRDS(gnis_lst_path)

  if(file.exists(districts_path)) {

    message(paste0("Reading data from ---> ", districts_path))

    dist_shp <- sf::read_sf(districts_path)

  } else {
    stop(paste0("Data not found at path ---> ", districts_path))
  }

} else {

  message(paste0("Data not found at path ---> ", gnis_lst_path))

  if(file.exists(districts_path)) {

    message(paste0("Reading data from ---> ", districts_path))

    dist_shp <- sf::read_sf(districts_path)

  } else {
    stop(paste0("Data not found at path ---> ", districts_path))
  }

  i = 1

  # loop over each huc4 and get mainstem of the river
  gnisid_df <- lapply(1:nrow(dist_shp), function(i) {
  # gnisids <- lapply(1:3, function(i) {

    message(paste0(i, "/", nrow(dist_shp)))

    # mapview::mapview(gnis)
    # i = 5
    # dist_shp$DISTRICT
    # nhdplusTools::get_nwis(sf::st_buffer(gnis, 5))
    gnis <- nhdplusTools::get_nhdplus(
      AOI         = dist_shp[i, ],
      # AOI = dist_shp %>% dplyr::filter(DISTRICT == 1),
      realization = "flowline"
    )

    tryCatch({

      # gnis <-
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
      #   dplyr::filter(gnis_id != "no_gnis_id") %>%
      #   dplyr::slice_max(len) %>%
      #   dplyr::mutate(uid = paste0(district, "_", gnis_id)) %>%
      #   dplyr::select(uid, district, gnis_id, gnis_name, streamorde, len, unit, geometry)

      # rm(upstream, gnisid_df, tmp)
      upstreams <-
        gnis %>%
        dplyr::filter(streamcalc != 0) %>%
        dplyr::filter(streamorde >= 3) %>%
        dplyr::mutate(dplyr::across(c(-geometry), as.character)) %>%
        dplyr::group_by(gnis_id, gnis_name, streamorde) %>%
        dplyr::summarise() %>%
        dplyr::mutate(
          gnis_id    = dplyr::case_when(
            gnis_id == " " & gnis_name == " " ~ "no_gnis_id",
            gnis_id == " " & gnis_name != " " ~ gnis_name,
            TRUE                              ~ gnis_id
          ),
          gnis_name    = dplyr::case_when(
            gnis_name == " " & gnis_id %in%  c(" ", "no_gnis_id")  ~ "no_gnis_name",
            gnis_name == " " & !gnis_id %in%  c(" ", "no_gnis_id") ~ gnis_id,
            TRUE                                                   ~ gnis_name
          ),
          district   = dist_shp$DISTRICT[i],
          len        = units::drop_units(sf::st_length(geometry)),
          unit       = "meters"
        ) %>%
        dplyr::arrange(-len) %>%
        dplyr::ungroup() %>%
        dplyr::filter(gnis_id != "no_gnis_id") %>%
        dplyr::slice_max(len) %>%
        dplyr::mutate(uid = paste0(district, "_", gnis_id)) %>%
        structure_type
        dplyr::select(uid, district, gnis_id, gnis_name, structure_name, structure_type, streamorde, len, unit, geometry)


      max_fline <-
        gnis %>%
        dplyr::filter(gnis_id %in% c(upstreams$gnis_id)) %>%
        dplyr::slice_max(hydroseq)

   sf::st_centroid(max_fline)

      wr_net <- cdssr::get_water_rights_netamount(
        # water_district    = dist_shp$DISTRICT[i]
        aoi    =  sf::st_centroid(max_fline),
        radius = 5
      )

    # make points out of water rights transactions tdata
     pts <-
       wr_net %>%
       dplyr::mutate(
         lon = longitude,
         lat = latitude
       ) %>%
       sf::st_as_sf( coords = c("longitude", "latitude"), crs = 4326)

     # find feature nearest to uppermost flow line of GNIS ID in each district
     pts <- pts[sf::st_nearest_feature(max_fline, pts), ]

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

     message("Pausing iteration for 2 minutes...")

     # add pause in loop as to not overwhelm CDSS resources
     Sys.sleep(180)

     message("Iteration resuming...")

     calls <- get_call_data2(
       wdid_df    = pts,
       start_date = "1980-01-01",
       end_date   = Sys.Date()
     )

     calls2 <-
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

     avg_year <-
       calls2 %>%
       dplyr::group_by(year) %>%
       dplyr::mutate(
         avg_call_year = mean(call_year, na.rm = T)
       ) %>%
       dplyr::ungroup() %>%
       dplyr::group_by(year) %>%
       dplyr::slice(1)
       # names()

     avg_year %>%
       ggplot2::ggplot() +
       ggplot2::geom_line(ggplot2::aes(x = datetime, y = avg_call_year))

     # go get the snotel data
     snotel_sites <- readr::read_csv(snotel_path, show_col_types = FALSE)


     aggreg_snotel(

     )
     # mapview::mapview(max_fline) + pts2 + pts
     }, error = function(e) {

      message(paste0("Skipping iteration: ", i, "Error:\n", e))

      NULL

    })


  }) %>%
    dplyr::bind_rows()
  # sprintf("%.5s", sub_flines$gnis_id[1])
  message(paste0("Saving data to path ---> ", gnis_lst_path))

  # save rds
  saveRDS(gnisid_df, gnis_lst_path)

}


wr_net
gnisid_df %>%
  dplyr::mutate(uid = paste0(district, "_", gnis_id))
gnisid_df$gnis_id
# remove leading 0s

wr_net <-
  wr_net %>%
  dplyr::mutate(
    district = ifelse(water_district < 10, paste0("0", water_district), water_district),
    gnis_id = sub("^0+", "", gnis_id),
    uid = paste0(district, "_", gnis_id)
    )

wr_net$gnis_id <- sub("^0+", "", wr_net$gnis_id)
tmp <-
  wr_net %>%
  dplyr::filter(uid %in% unique(gnisids$uid)) %>%
  dplyr::group_by(uid)
tmp

if(file.exists(wr_gnis_path)) {

  message(paste0("Reading data from ---> ", wr_gnis_path))

  wr_gnis <- readRDS(wr_gnis_path)

} else {
  # gnis_flines <- gnis
  # filter GNIS IDs to streamorders greater than or equal to 4
  # filter gnis IDs with missing names
  # filter out stream flines less than 10,000 meters
  gnis_trim <-
    gnis_flines %>%
    dplyr::filter(streamorde >= 4) %>%
    dplyr::filter(len > 10000, gnis_id != "no_gnis_id")

  # remove leading 0s
  wr_pts$gnis_id <- sub("^0+", "", wr_pts$gnis_id)

  wr_gnis <-
    wr_pts %>%
    dplyr::filter(gnis_id %in% gnis_trim$gnis_id) %>%
    dplyr::group_by(gnis_id) %>%
    dplyr::slice(
      which.min(as.Date(appropriation_date)),
      which.max(as.Date(appropriation_date)),
      which(as.Date(appropriation_date) == median(as.Date(appropriation_date)))[1]
      # which(as.Date(appropriation_date) == median(as.numeric(as.Date(appropriation_date))))[1]
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(gnis_id) %>%
    dplyr::mutate(
      seniority = dplyr::case_when(
        as.Date(appropriation_date) == min(as.Date(appropriation_date)) ~ "senior",
        as.Date(appropriation_date) == max(as.Date(appropriation_date)) ~ "junior",
        TRUE                                                            ~ "median"
      )
    ) %>%
    dplyr::ungroup()

  message(paste0("Saving data to path ---> ", wr_gnis_path))

  # save rds
  saveRDS(wr_gnis, wr_gnis_path)

}
