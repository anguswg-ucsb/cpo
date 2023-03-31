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
wr_gnis_path   <- "data/rights_by_gnis_id.rds"

# ***************************
# ---- get GNIS ID lines ----
# ***************************

# check if mainstem data path exists
if(file.exists(gnis_path)) {

  message(paste0("Reading data from ---> ", gnis_path))

  gnis_flines <- readRDS(gnis_path)

  if(file.exists(districts_path)) {

    message(paste0("Reading data from ---> ", districts_path))

    dist_shp <- sf::read_sf(districts_path)

  } else {
    stop(paste0("Data not found at path ---> ", districts_path))
  }

} else {

  message(paste0("Data not found at path ---> ", gnis_path))

  if(file.exists(districts_path)) {

    message(paste0("Reading data from ---> ", districts_path))

    dist_shp <- sf::read_sf(districts_path)

  } else {
    stop(paste0("Data not found at path ---> ", districts_path))
  }

  # i = 1

  # loop over each huc4 and get mainstem of the river
  gnis_flines <- lapply(1:nrow(dist_shp), function(i) {

    message(paste0(i, "/", nrow(dist_shp)))

    gnis <- nhdplusTools::get_nhdplus(
      AOI         = dist_shp[i, ],
      realization = "flowline"
    )

    tryCatch({

      gnis <-
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

if(file.exists(wr_gnis_path)) {

  message(paste0("Reading data from ---> ", wr_gnis_path))

  wr_gnis <- readRDS(wr_gnis_path)

} else {

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

  message(paste0("Saving data to path ---> ", wr_gnis_path))

  # save rds
  saveRDS(wr_gnis, wr_gnis_path)

}
