# Get water rights net amounts data

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)
library(AOI)
library(nhdplusTools)
library(sf)

# save paths
wr_net_path       <- "data/water_right_netamounts.rds"
wr_pts_path       <- "data/water_right_netamounts_pts.rds"
uwdids_path       <- "data/unique_wdids.rds"

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
