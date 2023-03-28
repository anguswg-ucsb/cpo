# Angus Watters
# Collect data from CDSS to use in Shiny App

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)
library(climateR)

# ***********************
# ---- Paths to data ----
# ***********************

# TODO: just added these manually as i went, got to find a better place/method for keeping track and storing shiny req data

# Paths, manually added these as I went.
wd_shp_path       <- "data/water_districts_simple.geojson"
wr_net_path       <- "data/water_right_netamounts.rds"
wr_pts_path       <- "data/water_right_netamounts_pts.rds"
uwdids_path       <- "data/unique_wdids.rds"
districts_path    <- "data/water_districts_tbl.rds"
ms_path           <- "data/nhd_mainstems.rds"
msu_path          <- "data/nhd_mainstems_union.rds"
huc_path          <- "data/huc4.rds"
end_pts_path      <- "data/upstream_pts.rds"
call_path         <- "data/upstream_call_analysis.rds"
dist_ms_path      <- "data/nhd_mainstems_district.rds"
dist_msu_path     <- "data/nhd_mainstems_union_district.rds"
dist_end_pts_path <- "data/upstream_pts_district.rds"
dist_call_path    <- "data/upstream_call_analysis_district.rds"
gnis_path         <- "data/nhd_gnis_id_flines.rds"

# ***********************************
# ---- get water districts table ----
# ***********************************

if(file.exists(districts_path)) {

  message(paste0("Reading data from ---> ", districts_path))

  water_dists <- readRDS(districts_path)

} else {

  message(paste0("Data not found at path ---> ", districts_path))

  # water districts reference table
  water_dists <- cdssr::get_reference_tbl("waterdistricts")

  message(paste0("Saving data to path ---> ", districts_path))

  # save water districts reference table data
  saveRDS(water_dists, districts_path)

}

# ***************************************************
# ---- Water rights netamounts by water district ----
# ***************************************************
# pull all water rights net amounts for each water district

if(file.exists(wr_net_path)) {

  message(paste0("Reading data from ---> ", wr_net_path))

  wr_net <- readRDS(wr_net_path)

  if(file.exists(wr_pts_path)) {

    message(paste0("Reading data from ---> ", wr_pts_path))

    wr_pts <- readRDS(wr_pts_path)

  } else {

    # colorado state geometry
    co <- AOI::aoi_get(state = "CO")

    # HUC8s
    hucs <- nhdplusTools::get_huc8(co)

    # convert HUC8s to HUC4
    huc4s <-
      hucs %>%
      dplyr::mutate(
        huc4 = substr(huc8, 1, 4)
      ) %>%
      dplyr::group_by(huc4) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      sf::st_crop(co) %>%
      sf::st_simplify(dTolerance = 200)

    wr_pts <-
      wr_net %>%
      dplyr::filter(!is.na(longitude) | !is.na(latitude)) %>%
      dplyr::filter(!is.na(stream_mile)) %>%
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs    = 4326
      ) %>%
      dplyr::select(wdid, structure_name, structure_type, gnis_id,
                    appropriation_date, admin_number, geometry) %>%
      sf::st_join(huc4s) %>%
      dplyr::mutate(
        district = substr(wdid, 1, 2)
      ) %>%
      dplyr::relocate(district)

    message(paste0("Saving spatial data to path ---> ", wr_pts_path))

    # save water rights netamount spatial data
    saveRDS(wr_pts, wr_pts_path)

    message(paste0("Saving unique WDID data to path ---> ", uwdids_path))

    uwdids = data.frame(wdid = unique(wr_pts$wdid)) %>%
      dplyr::mutate(
        water_district = substr(wdid, 1, 2)
      )

    # save water rights netamount spatial data
    saveRDS(uwdids, uwdids_path)
  }

} else {

  message(paste0("Data not found at path ---> ", wr_net_path))

  wr_net <- lapply(1:nrow(water_dists), function(i) {

    message(paste0("District: ", water_dists$water_district[i], " - (", i, "/", nrow(water_dists), ")"))

    # GET request to CDSS API
    tryCatch({

      wr_net <- cdssr::get_water_rights_netamount(
        water_district    = water_dists$water_district[i]
      )
      wr_net
    },
    error = function(e) {

      NULL

    })

  }) %>%
    dplyr::bind_rows()

  message(paste0("Saving data to path ---> ", wr_net_path))

  # save water rights netamount data
  saveRDS(wr_net, wr_net_path)

  if(!file.exists(wr_pts_path)) {

    # colorado state geometry
    co <- AOI::aoi_get(state = "CO")

    # HUC8s
    hucs <- nhdplusTools::get_huc8(co)

    # convert HUC8s to HUC4
    huc4s <-
      hucs %>%
      dplyr::mutate(
        huc4 = substr(huc8, 1, 4)
      ) %>%
      dplyr::group_by(huc4) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      sf::st_crop(co) %>%
      sf::st_simplify(dTolerance = 200)

    wr_pts <-
      wr_net %>%
      dplyr::filter(!is.na(longitude) | !is.na(latitude)) %>%
      dplyr::filter(!is.na(stream_mile)) %>%
      sf::st_as_sf(
        coords = c("longitude", "latitude"),
        crs    = 4326
      )  %>%
      sf::st_join(huc4s)

  message(paste0("Saving spatial data to path ---> ", wr_pts_path))

  # save water rights netamount spatial data
  saveRDS(wr_pts, wr_pts_path)

  message(paste0("Saving unique WDID data to path ---> ", uwdids_path))

  uwdids = data.frame(wdid = unique(wr_pts$wdid)) %>%
    dplyr::mutate(
      water_district = substr(wdid, 1, 2)
    )

  # save water rights netamount spatial data
  saveRDS(uwdids, uwdids_path)

  }

}

# ***********************
# ---- get mainstems ----
# ***********************

# check if mainstem data path exists
if(file.exists(ms_path)) {

  message(paste0("Reading data from ---> ", ms_path))

  main_stem <- readRDS(ms_path)

  # if union mainstem lines file exists, read it in
  if(file.exists(msu_path)) {

    message(paste0("Reading data from ---> ", msu_path))

    ms_union <- readRDS(msu_path)

  } else {

    # union linestrings by HUC4
    ms_union <-
      main_stem %>%
      dplyr::group_by(huc4) %>%
      dplyr::summarise() %>%
      dplyr::ungroup()

    message(paste0("Saving data to path ---> ", msu_path))

    # save rds
    saveRDS(ms_union, msu_path)

  }

  # if huc4 file exists, read it in
  if(file.exists(huc_path)) {

  message(paste0("Reading data from ---> ", huc_path))

  # read in HUC4s shapes
  huc4s <- readRDS(huc_path)

  } else {

    # colorado state geometry
    co <- AOI::aoi_get(state = "CO")

    # HUC8s
    hucs <- nhdplusTools::get_huc8(co)

    # convert HUC8s to HUC4
    huc4s <-
      hucs %>%
      dplyr::mutate(
        huc4 = substr(huc8, 1, 4)
      ) %>%
      dplyr::group_by(huc4) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      sf::st_crop(co) %>%
      sf::st_simplify(dTolerance = 200)

    message(paste0("Saving data to path ---> ", huc_path))

    # save HUC4s data
    saveRDS(huc4s, huc_path)

    }

  } else {

  message(paste0("Data not found at path ---> ", ms_path))

  if(file.exists(huc_path)) {

    # read in HUC4s shapes
    huc4s <- readRDS(huc_path)

  } else {

    # colorado state geometry
    co <- AOI::aoi_get(state = "CO")

    # HUC8s
    hucs <- nhdplusTools::get_huc8(co)

    # convert HUC8s to HUC4
    huc4s <-
      hucs %>%
      dplyr::mutate(
        huc4 = substr(huc8, 1, 4)
      ) %>%
      dplyr::group_by(huc4) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      sf::st_crop(co) %>%
      sf::st_simplify(dTolerance = 200)

    message(paste0("Saving data to path ---> ", huc_path))

    # save HUC4s data
    saveRDS(huc4s, huc_path)

  }

  # loop over each huc4 and get mainstem of the river
  main_stem <- lapply(1:nrow(huc4s), function(i) {

    message(paste0(i, "/", nrow(huc4s)))

    huc_net <- nhdplusTools::get_nhdplus(
      AOI = huc4s[i, ],
      realization = "flowline"
    )

    tryCatch({

      # minimum stream level of mainstem
      min_lvl <-
        huc_net %>%
        # huc_outs %>%
        dplyr::filter(streamcalc != 0) %>%
        dplyr::group_by(terminalpa) %>%
        dplyr::filter(streamleve == min(streamleve)) %>%
        dplyr::filter(streamorde >= 3) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(huc4 = huc4s$huc4[i]) %>%
        dplyr::mutate(dplyr::across(c(-geometry), as.character))

      # longest levelpath will be the main flowline/mainstem
      main_lvlpath <-
        min_lvl %>%
        dplyr::group_by(levelpathi) %>%
        dplyr::summarise() %>%
        dplyr::mutate(
          lengths = sf::st_length(geometry)
        ) %>%
        dplyr::slice(which.max(lengths)) %>%
        .$levelpathi

      # filter to the main levelpathi
      min_lvl <-
        min_lvl %>%
        dplyr::filter(levelpathi == main_lvlpath)

    min_lvl

    }, error = function(e) {

      message(paste0("Skipping iteration: ", i, "Error:\n", e))

      NULL

      })

    # plot(huc_net$geometry)
    # plot(min_lvl$geometry, lwd = 2, color = "red", add = T)
    # plot(um_net$geometry, color = "red", add = T)

    # min_lvl %>% dplyr::mutate(streamleve = as.character(streamleve),
    #            streamorde = as.character(streamorde)) %>%
    #   ggplot2::ggplot() + ggplot2::geom_sf(ggplot2::aes(color = hydroseq))

    # mapview::mapview(ends, color = "green") +
    #   mapview::mapview(um_net, color = "blue") +
    #   mapview::mapview(min_lvl, color = "red")

  }) %>%
    dplyr::bind_rows()

  message(paste0("Saving data to path ---> ", ms_path))

  # save rds
  saveRDS(main_stem, ms_path)


  # if union mainstem lines file doesn't exist
  if(!file.exists(msu_path)) {

    # union linestrings by HUC4
    ms_union <-
      main_stem %>%
      dplyr::group_by(huc4) %>%
      dplyr::summarise() %>%
      dplyr::ungroup()

    message(paste0("Saving data to path ---> ", msu_path))

    # save rds
    saveRDS(ms_union, msu_path)

  }

  }

# ************************
# ---- get end points ----
# ************************

# locate WDIDs closest to most upstream point of main stem

if(file.exists(end_pts_path)) {

  message(paste0("Reading data from ---> ", end_pts_path))

  end_pts <- readRDS(end_pts_path)

} else {

  message(paste0("Data not found at path ---> ", end_pts_path))

  # most upstream flowlines
  us_start <-
    main_stem %>%
    dplyr::group_by(huc4) %>%
    dplyr::filter(hydroseq == max(hydroseq))

  # list of HUC4s
  huc_lst <- us_start$huc4

  us_start <-
    us_start %>%
    nhdplusTools::get_node(position = "start")

  # nearest point index
  near_idx <- sf::st_nearest_feature(us_start, wr_pts)

  # most upstream WDID by HUC4
  end_pts <- wr_pts[near_idx, ]

  # add huc4 columns
  end_pts$huc4 <- huc_lst

  message(paste0("Saving data to path ---> ", end_pts_path))

  # save water rights netamount data
  saveRDS(end_pts, end_pts_path)

}


# ***************************
# ---- get call_analysis ----
# ***************************

# extract call analysis data for the most upstream WDID for each HUC4/River mainstem (end_pts.rds)

if(file.exists(call_path)) {

  message(paste0("Reading data from ---> ", call_path))

  # read in call dataframe
  call_df <- readRDS(call_path)

} else {

  message(paste0("Data not found at path ---> ", call_path))

  # get call analysis data for each point of interest
  call_lst <- lapply(1:nrow(end_pts), function(i) {

    message(paste0("WDID: ",  end_pts$wdid[i], " - (", i, "/", nrow(end_pts), ")"))

    # GET request to CDSS API
    tryCatch({
        ca <- cdssr::get_call_analysis_wdid(
                  wdid       =  end_pts$wdid[i],
                  admin_no   = "99999.00000",
                  start_date = "1990-01-01",
                  end_date   = "2023-01-01"
                )
        ca

    }, error = function(e) {

        NULL
    })

  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      year  = lubridate::year(datetime),
      month = lubridate::month(datetime)
      ) %>%
    dplyr::select(datetime, year, month,
                  wdid = analysis_wdid, priority_wdid,
                  admin_no = analysis_wr_admin_no,
                  priority_admin_no, priority_date,
                  out_pct = analysis_out_of_priority_percent_of_day
                  ) %>%
    dplyr::group_by(wdid) %>%
    dplyr::mutate(
      priority_date = dplyr::case_when(
        is.na(priority_date) ~ Sys.Date(),
        TRUE                 ~ as.Date(priority_date)
      )
    ) %>%
    dplyr::ungroup()

  message(paste0("Saving data to path ---> ", call_path))

  # save call analysis data
  saveRDS(call_lst, call_path)

}

# ********************************
# ---- get mainstems district ----
# ********************************

# check if mainstem data path exists
if(file.exists(dist_ms_path)) {

  message(paste0("Reading data from ---> ", dist_ms_path))

  dist_ms <- readRDS(dist_ms_path)

  # if union mainstem lines file exists, read it in
  if(file.exists(dist_msu_path)) {

    message(paste0("Reading data from ---> ", dist_msu_path))

    dist_msu <- readRDS(dist_msu_path)

  } else {

    # union linestrings by HUC4
    dist_msu <-
      dist_ms %>%
      dplyr::group_by(district) %>%
      dplyr::summarise() %>%
      dplyr::ungroup()

    message(paste0("Saving data to path ---> ", dist_msu_path))

    # save rds
    saveRDS(dist_msu, dist_msu_path)

  }

  if(file.exists(wd_shp_path)) {

    message(paste0("Reading data from ---> ", wd_shp_path))

    dist_shp <- sf::read_sf(wd_shp_path)

  } else {
    stop(paste0("Data not found at path ---> ", wd_shp_path))
  }

} else {

  message(paste0("Data not found at path ---> ", dist_ms_path))

  if(file.exists(wd_shp_path)) {

    message(paste0("Reading data from ---> ", wd_shp_path))

    dist_shp <- sf::read_sf(wd_shp_path)

  } else {
    stop(paste0("Data not found at path ---> ", wd_shp_path))
  }


  # loop over each huc4 and get mainstem of the river
  main_stem <- lapply(1:nrow(dist_shp), function(i) {

    message(paste0(i, "/", nrow(dist_shp)))

    dist_net <- nhdplusTools::get_nhdplus(
      AOI         = dist_shp[i, ],
      realization = "flowline"
    )

    tryCatch({

      # minimum stream level of mainstem
      min_lvl <-
        dist_net %>%
        # huc_outs %>%
        dplyr::filter(streamcalc != 0) %>%
        dplyr::group_by(terminalpa) %>%
        dplyr::filter(streamleve == min(streamleve)) %>%
        dplyr::filter(streamorde >= 3) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(district = dist_shp$DISTRICT[i]) %>%
        dplyr::mutate(dplyr::across(c(-geometry), as.character))

      # longest levelpath will be the main flowline/mainstem
      main_lvlpath <-
        min_lvl %>%
        dplyr::group_by(levelpathi) %>%
        dplyr::summarise() %>%
        dplyr::mutate(
          lengths = sf::st_length(geometry)
        ) %>%
        dplyr::slice(which.max(lengths)) %>%
        .$levelpathi

      # filter to the main levelpathi
      min_lvl <-
        min_lvl %>%
        dplyr::filter(levelpathi == main_lvlpath)

      min_lvl
      # mapview::mapview(dist_shp) + min_lvl
    }, error = function(e) {

      message(paste0("Skipping iteration: ", i, "Error:\n", e))

      NULL

    })


  }) %>%
    dplyr::bind_rows()

  message(paste0("Saving data to path ---> ", dist_ms_path))

  # save rds
  saveRDS(main_stem, dist_ms_path)


  # if union mainstem lines file doesn't exist
  if(!file.exists(dist_msu_path)) {

    # union linestrings by HUC4
    ms_union <-
      main_stem %>%
      dplyr::group_by(district) %>%
      dplyr::summarise() %>%
      dplyr::ungroup()

    message(paste0("Saving data to path ---> ", dist_msu_path))

    # save rds
    saveRDS(ms_union, dist_msu_path)

  }

}

# ***************************
# ---- get GNIS ID lines ----
# ***************************

# check if mainstem data path exists
if(file.exists(gnis_path)) {

  message(paste0("Reading data from ---> ", gnis_path))

  gnis_flines <- readRDS(gnis_path)

  if(file.exists(wd_shp_path)) {

    message(paste0("Reading data from ---> ", wd_shp_path))

    dist_shp <- sf::read_sf(wd_shp_path)

  } else {
    stop(paste0("Data not found at path ---> ", wd_shp_path))
  }

} else {

  message(paste0("Data not found at path ---> ", gnis_path))

  if(file.exists(wd_shp_path)) {

    message(paste0("Reading data from ---> ", wd_shp_path))

    dist_shp <- sf::read_sf(wd_shp_path)

  } else {
    stop(paste0("Data not found at path ---> ", wd_shp_path))
  }

  # i = 1

  # loop over each huc4 and get mainstem of the river
  gnis_flines <- lapply(1:nrow(dist_shp), function(i) {

    message(paste0(i, "/", nrow(dist_shp)))

    gnis <- nhdplusTools::get_nhdplus(
      AOI         = dist_shp[i, ],
      realization = "flowline"
    )

    # gnis %>%
    #   dplyr::mutate(gnis_order = paste0(gnis_id, "_", streamorde)) %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(ggplot2::aes(color = gnis_order))

    # gnis %>%
    #   dplyr::filter(streamcalc != 0) %>%
    #   dplyr::filter(streamorde >= 3) %>%
    #   dplyr::mutate(gnis_order = paste0(gnis_id, "_", streamorde)) %>%
    #   # dplyr::group_by(gnis_order)
    # # dplyr::group_by(levelpathi)
    #   # dplyr::filter(!gnis_order %in% c(" _4", " _5", " _3")) %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(ggplot2::aes(color = factor(gnis_order)))
    #   # ggplot2::geom_sf(ggplot2::aes(color = factor(levelpathi)))

    tryCatch({

      gnis <-
        gnis %>%
        dplyr::filter(streamcalc != 0) %>%
        dplyr::filter(streamorde >= 3) %>%
        dplyr::mutate(dplyr::across(c(-geometry), as.character)) %>%
        dplyr::group_by(gnis_id, gnis_name, streamorde) %>%
        # dplyr::group_by(gnis_id, gnis_name, streamorde, levelpathi) %>%
        dplyr::summarise() %>%
        dplyr::mutate(
          # gnis_order = paste0(gnis_id, "_", streamorde),
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
        dplyr::select(district, gnis_id, gnis_name, streamorde, len, unit, geometry)

      gnis

    }, error = function(e) {

      message(paste0("Skipping iteration: ", i, "Error:\n", e))

      NULL

    })


  }) %>%
    dplyr::bind_rows()
  # sprintf("%.5s", sub_flines$gnis_id[1])
  message(paste0("Saving data to path ---> ", gnis_path))

  # save rds
  saveRDS(gnis_flines, gnis_path)

}

300 * 2

600 * (20*365)

length(unique(gnis_flines$gnis_id))

unique(gnis_flines$streamorde)

gnis_flines$len %>% hist(breaks = 50)
gnis_flines$len %>% mean()
gnis_flines
gnis_flines %>%
  dplyr::filter(streamorde >= 4) %>%
  # mapview::mapview()
  # dplyr::group_by(gnis_id)
  # dplyr::filter(gnis_id != "no_gnis_id") %>%
  dplyr::filter(len > 10000, gnis_id != "no_gnis_id") %>%
  # dplyr::group_by(gnis_id)
  ggplot2::ggplot() +
  # ggplot2::geom_sf()
  ggplot2::geom_sf(ggplot2::aes(color = streamorde))

gnis_flines  %>%
  dplyr::filter(streamorde >= 4) %>%
  dplyr::group_by(gnis_id) %>%
  dplyr::summarise()  %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf()
  # ggplot2::geom_sf(ggplot2::aes(color = streamorde))


gnis_trim <-
  gnis_flines %>%
  dplyr::filter(streamorde >= 4) %>%
  dplyr::filter(len > 10000, gnis_id != "no_gnis_id")


wr_pts$gnis_id <- sub("^0+", "", wr_pts$gnis_id)

# wr_gnis <-
#   wr_pts %>%
#   dplyr::filter(gnis_id %in% gnis_trim$gnis_id)
#
# wr_pts$gnis_id

length(unique(wr_gnis$gnis_id))
length(unique(wr_gnis$wdid))

wr_gnis <-
  wr_pts %>%
  dplyr::filter(gnis_id %in% gnis_trim$gnis_id) %>%
  dplyr::group_by(gnis_id) %>%
  dplyr::slice(
    # which(as.Date(appropriation_date) == mean(as.Date(appropriation_date))),
    which.min(as.Date(appropriation_date)),
    which.max(as.Date(appropriation_date))
    ) %>%
  dplyr::group_by(gnis_id) %>%
  dplyr::mutate(
    seniority = dplyr::case_when(
      as.Date(appropriation_date) == min(as.Date(appropriation_date)) ~ "senior",
      TRUE                                                            ~ "junior"
    )
  ) %>%
  dplyr::ungroup()

# wr_gnis %>%
#   dplyr::left_join(
#                    dplyr::filter(
#                      dplyr::select(
#                      sf::st_drop_geometry(gnis_flines), gnis_id, gnis_name
#                      ),
#                      gnis_id %in% wr_gnis$gnis_id
#                    ),
#                    by = "gnis_id"
#                    )
# wr_gnis

365*30
11000*364

uwr = unique(wr_gnis$wdid)
length(uwr)

gnis_ca <- lapply(1:nrow(wr_gnis), function(i) {
  message(paste0(i, "/", nrow(wr_gnis)))

  # GET request to CDSS API
  tryCatch({
    calls <- cdssr::get_call_analysis_wdid(
      wdid       = wr_gnis$wdid[i],
      admin_no   = "99999.00000",
      # start_date = "2022-12-01",
      # end_date   = "2023-01-01"
      start_date = "1993-01-01",
      end_date   = "2023-01-01",
      api_key = "2fx+0sUzKbpOWeqkWzbU4BIIOtpwoVyE"
    ) %>%
      dplyr::mutate(
        district            = wr_gnis$district[i],
        wdid_gnis_id        = wr_gnis$gnis_id[i],
        wdid_approp_date    = wr_gnis$appropriation_date[i],
        wdid_structure_name = wr_gnis$structure_name[i],
        wdid_structure_type = wr_gnis$structure_type[i],
        seniority           = wr_gnis$seniority[i]
      )
    calls
  }, error = function(e) {

    NULL
  })

})

call_df <- gnis_ca %>% dplyr::bind_rows()
saveRDS(call_df, "data/gnis_call_analysis.rds")

# *******************************************
# ---- get % of out priority per GNIS ID ----
# *******************************************

length(unique(wr_pts$gnis_id))
length(unique(wr_pts$gnis_id))
unique(dist_ms$gnis_id)
length(unique(dist_ms$gnis_id))

dist_ms %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = gnis_id))

mapview::mapview(dist_msu)

3628*1*(20*365)
# *********************************
# ---- get end points district ----
# *********************************

# locate WDIDs closest to most upstream point of main stem

if(file.exists(dist_end_pts_path)) {

  message(paste0("Reading data from ---> ", dist_end_pts_path))

  dist_ends <- readRDS(dist_end_pts_path)

} else {

  message(paste0("Data not found at path ---> ", dist_end_pts_path))

  # most upstream flowlines
  us_start <-
    main_stem %>%
    dplyr::group_by(district) %>%
    dplyr::filter(hydroseq == max(hydroseq))

  # list of HUC4s
  dist_lst <- us_start$district

  us_start <-
    us_start %>%
    nhdplusTools::get_node(position = "start")

  # nearest point index
  near_idx <- sf::st_nearest_feature(us_start, wr_pts)

  # most upstream WDID by HUC4
  dist_ends <- wr_pts[near_idx, ]

  # add huc4 columns
  dist_ends$dist_lst <- dist_lst

  message(paste0("Saving data to path ---> ", dist_end_pts_path))

  # save water rights netamount data
  saveRDS(dist_ends, dist_end_pts_path)

}

# ************************************
# ---- get call_analysis district ----
# ************************************

# extract call analysis data for the most upstream WDID for each HUC4/River mainstem (end_pts.rds)

if(file.exists(dist_call_path)) {

  message(paste0("Reading data from ---> ", dist_call_path))

  # read in call dataframe
  dist_call_df <- readRDS(dist_call_path)

} else {

  message(paste0("Data not found at path ---> ", dist_call_path))

  # get call analysis data for each point of interest
  call_lst <- lapply(1:nrow(dist_ends), function(i) {

    message(paste0("WDID: ",  dist_ends$wdid[i], " - (", i, "/", nrow(dist_ends), ")"))

    # GET request to CDSS API
    tryCatch({
      ca <- cdssr::get_call_analysis_wdid(
        wdid       =  dist_ends$wdid[i],
        admin_no   = "99999.00000",
        start_date = "1990-01-01",
        end_date   = "2023-01-01"
      )
      ca

    }, error = function(e) {

      NULL
    })

  })

  call_lst2 <-
    call_lst %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      year  = lubridate::year(datetime),
      month = lubridate::month(datetime)
    ) %>%
    dplyr::select(datetime, year, month,
                  wdid = analysis_wdid, priority_wdid,
                  admin_no = analysis_wr_admin_no,
                  priority_admin_no, priority_date,
                  out_pct = analysis_out_of_priority_percent_of_day
    ) %>%
    dplyr::group_by(wdid) %>%
    dplyr::mutate(
      priority_date = dplyr::case_when(
        is.na(priority_date) ~ Sys.Date(),
        TRUE                 ~ as.Date(priority_date)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      district = substr(wdid, 1, 2)
    ) %>%
    dplyr::relocate(district)

  message(paste0("Saving data to path ---> ", dist_call_path))

  # save call analysis data
  saveRDS(call_lst2, dist_call_path)

}
