# Angus Watters
# Script that sources get_climate.R and joins with snotel site data

# load libraries
library(climateR)
library(terra)
library(dplyr)
library(sf)
library(nhdplusTools)
library(snotelr)

# install.packages("snotelr")
# source climate/call analysis data and/or go get the data if its not there
source("R/get_climate.R")

# --------------------
# ---- Data paths ----
# --------------------

# path to snotel data
snotel_path <- "data/all_snotel_co.rds"

# district shape path
district_path <- "data/water_districts_simple.geojson"

site_path <- "data/snotel_sites.csv"

# ----------------------
# ---- Read in data ----
# ----------------------

# read in snotel data
snotel <- readRDS(snotel_path)

# site_df <-
#   snotel %>%
#   dplyr::group_by(site_id) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(site_name, site_id, lat, lon)
#
# readr::write_csv(site_df, "data/snotel_sites.csv")
# water districts shape
dists   <- sf::read_sf(district_path)

# ------------------------------
# ---- join snotel by basin ----
# ------------------------------

go_get_snotel_data <- function(site_ids) {

  site_lst <- lapply(1:length(site_ids), function(i) {

    message(paste0("Site: ", i, "/", length(site_ids)))
   snotelr::snotel_download(site_id = site_ids[i], internal = TRUE)

  }) %>%
    dplyr::bind_rows()

  return(site_lst)

}



# create sf points for snotel sites
snotel_pts <-
  snotel %>%
  dplyr::group_by(site_id) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(
    huc12 = gsub(".*\\((.*)\\).*", "\\1", huc),
    huc4  = substr(huc12, 1, 4)
  ) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_join(
    dplyr::summarise(dplyr::group_by(dists, BASIN))
  ) %>%
  dplyr::relocate(basin = BASIN)

snotel <-
  snotel %>%
  dplyr::left_join(
    dplyr::select(sf::st_drop_geometry(snotel_pts), basin, site_id),
    by = "site_id"
  ) %>%
  # dplyr::mutate(
  #   elev_bin = dplyr::case_when(
  #     elev <= 10000 ~ "low_elev",
  #     TRUE          ~ "high_elev"
  #   )
  # ) %>%
  # dplyr::group_by(date, elev_bin, basin) %>%
  dplyr::group_by(date, basin) %>%
  dplyr::summarise(
    sno_dpth_mm = mean(sno_dpth_mm, na.rm =T),
    swe_mm      = mean(swe_mm, na.rm =T)
  ) %>%
  dplyr::ungroup()

library(imputeTS)

get_snotel <- function(site_path, snotel_path, district_path) {
  # path to snotel data
  snotel_path <- "data/all_snotel_co.rds"

  # district shape path
  district_path <- "data/water_districts_simple.geojson"

  site_path <- "data/snotel_sites.csv"

  # read in snotel data
  site_df <- readr::read_csv(site_path)

  site_ids <- unique(site_df$site_id)

  snotel_df <- go_get_snotel_data(site_ids = site_ids)

  # snotel_lst <-
  #   snotel_df %>%
  #   dplyr::group_by(site_id) %>%
  #   dplyr::group_split()

  # snotel_lst <- lapply(1:length(snotel_lst), function(i) {
  #   # message(paste0(i, "/", length(snotel_split)))
  #   snotel_lst[[i]] %>%
  #     dplyr::mutate(
  #       snow_water_equivalent = imputeTS::na_seadec(snow_water_equivalent, find_frequency = T)
  #       ) %>%
  #     dplyr::mutate(
  #       snow_water_equivalent = dplyr::case_when(
  #         snow_water_equivalent < 0 ~ 0,
  #         TRUE                      ~ snow_water_equivalent
  #       )
  #     )
  # }) %>%
  #   dplyr::bind_rows()

  # read in snotel data
  # snotel2 <- readRDS(snotel_path)

  # water districts shape
  dists   <- sf::read_sf(district_path)

  # Convert the sequence to a data frame
  date_df <-
    data.frame(
      # date = seq(as.Date("1979-12-31"), as.Date("2022-12-26"), by = "7 days")
      date = seq(as.Date("1979-12-31"), Sys.Date(), by = "7 days")
      # date = seq(as.Date(min(out$analysis_date)), Sys.Date(), by = "7 days")
    ) %>%
    dplyr::mutate(
      year     = lubridate::year(date),
      week_num = strftime(date, format = "%V")
    )

  # create sf points for snotel sites
  snotel_pts <-
    snotel_df %>%
    dplyr::group_by(site_id) %>%
    dplyr::slice(1) %>%
    # dplyr::mutate(
    #   huc12 = gsub(".*\\((.*)\\).*", "\\1", huc),
    #   huc4  = substr(huc12, 1, 4)
    # ) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    sf::st_join(
      dplyr::summarise(dplyr::group_by(dists, BASIN))
    ) %>%
    dplyr::relocate(basin = BASIN)

  snotel_basin <-
    snotel_df %>%
    dplyr::left_join(
      dplyr::select(sf::st_drop_geometry(snotel_pts), basin, site_id),
      by = "site_id"
    ) %>%
    # dplyr::mutate(
    #   elev_bin = dplyr::case_when(
    #     elev <= 3000 ~ "low_elev",
    #     TRUE          ~ "high_elev"
    #   )
    # ) %>%
    # dplyr::group_by(date, elev_bin, basin) %>%
    dplyr::group_by(date, basin) %>%
    dplyr::summarise(
      swe = mean(snow_water_equivalent, na.rm =T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(datetime = date) %>%
    dplyr::mutate(
      year     = lubridate::year(datetime),
      week_num = strftime(datetime, format = "%V")
    ) %>%
    dplyr::left_join(
      date_df,
      by = c("year", "week_num")
    ) %>%
    dplyr::group_by(basin, date) %>%
    dplyr::summarise(
      swe = mean(swe, na.rm =T)
    )


}
snotel_basin %>%
  # dplyr::group_by(date, elev_bin, basin) %>%
  # dplyr::summarise(
  #   sno_dpth_mm = mean(sno_dpth_mm, na.rm =T),
  #   swe_mm      = mean(swe_mm, na.rm =T)
  # ) %>%
  # dplyr::ungroup() %>%
  # dplyr::mutate(
  #   year = lubridate::year(datetime),
  #   datetime = as.Date(datetime)
  # ) %>%
  # dplyr::filter(year %in% c(1985)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = swe, color = basin)) +
  ggplot2::facet_wrap(~basin)


hist(snotel2$elev)
clim_ts2 <-
  clim_ts %>%
  dplyr::left_join(
    dplyr::mutate(
      dplyr::select(sf::st_drop_geometry(dists), district = DISTRICT, basin = BASIN),
      district = dplyr::if_else(district < 10, paste0("0", district), as.character(district))
      ),
    by = "district"
  )

snotel2 %>%
  dplyr::group_by(basin) %>%
  dplyr::mutate(
    year = lubridate::year(date)
  ) %>%
  dplyr::filter(basin == "South Platte") %>%
  dplyr::mutate(
    elev_bin = dplyr::case_when(
      elev <= 9000 ~ "bin1",
      elev > 9000 & elev <= 10500 ~ "bin2",
      elev > 10500  ~ "bin3",
    )
  )

# snowtmp <-
  snotel2 %>%
  dplyr::group_by(basin) %>%
    # dplyr::mutate(
    #   year = lubridate::year(date)
    # ) %>%
    dplyr::filter(basin == "South Platte") %>%
    dplyr::mutate(
      elev_bin = dplyr::case_when(
        elev <= 9000 ~ "bin1",
        TRUE ~ "bin2"
        # elev <= 9000 ~ "bin1",
        # elev > 9000 & elev <= 10000 ~ "bin2",
        # elev > 10000  ~ "bin3",
      )
    ) %>%
    dplyr::group_by(date, elev_bin) %>%
    dplyr::summarise(
      sno_dpth_mm = mean(sno_dpth_mm, na.rm =T),
      swe_mm      = mean(swe_mm, na.rm =T)
      ) %>%
    dplyr::mutate(
      year = lubridate::year(date)
    ) %>%
    dplyr::filter(year == 2010) %>%
    dplyr::ungroup() %>%
    # tidyr::pivot_longer(cols = c(sno_dpth_mm, swe_mm))
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(x = date, y = sno_dpth_mm, color = elev_bin)) +
    ggplot2::facet_wrap(~elev_bin)
    # dplyr::group_by(site_id)

#   # extract the text between the parentheses using regular expressions and gsub
#   df$new_column <- gsub(".*\\((.*)\\).*", "\\1", df$huc)
# nchar("110200060406")
mapview::mapview(snotel_pts)

# water districts shape
dists   <- sf::read_sf(district_path)

# basins <-
#   dists %>%
#   dplyr::group_by(dists, BASIN) %>%
  dplyr::summarise(  dplyr::group_by(dists, BASIN))

pts <-
  sf::st_join(
  snotel_pts,
  dplyr::summarise(  dplyr::group_by(dists, BASIN))
  )

rm(basin_shp)
  # row = 6
# i = 4
dists_hucs <- lapply(1:nrow(dists), function(i) {

  hucs <- nhdplusTools::get_huc(AOI = dists[i, ], type = "huc04")

  # calculate the amount of overlap using st_intersection
  overlapping_polygons <- sf::st_intersection(dists[i, ], hucs)
  plot(overlapping_polygons$geometry)
  mapview::mapview(snotel_pts) + hucs + dists[i, ] + overlapping_polygons
  # calculate the area of overlap for each polygon
  overlap_areas <- st_area(overlapping_polygons)
  mapview::mapview(snotel_pts) + hucs + dists[i, ] + basins + basin_shp
})
# get district HUCs
hucs <- nhdplusTools::get_huc(AOI = dists[1:2, ], type = "huc04")
mapview::mapview(snotel_pts) + hucs + dists[row, ]

# # create sf points for snowtel sites
# snotel_pts <-
#   snowtel %>%
#   dplyr::group_by(site_id) %>%
#   dplyr::slice(1) %>%
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# nchar("110200060406")
# mapview::mapview(snotel_pts)

# unique snotel site IDs
snowtel_sites <- unique(snowtel$site_id)
#
site <- snotel_download(site_id = 303, internal = TRUE)

# create sample dataframes
polygon1 <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1.25), c(0,0))))
polygon2 <- st_polygon(list(rbind(c(0.5,0), c(1.5,0), c(1.5,1), c(0.5,1), c(0.5,0))))
polygon3 <- st_polygon(list(rbind(c(1.5,0), c(2.5,0), c(2.5,1), c(1.5,1), c(1.5,0))))
polygons_df <- st_sf(geometry = st_sfc(polygon2, polygon1, polygon3))
single_polygon_df <- st_sf(geometry = st_sfc(polygon1))

# calculate the amount of overlap using st_intersection
overlapping_polygons <- st_intersection(polygons_df, single_polygon_df)
plot(polygons_df$geometry,  col = "red")
plot(single_polygon_df$geometry, col = "blue", add = T)
plot(overlapping_polygons$geometry, add = T)
plot(polygons_df$geometry)
plot(overlapping_polygons$geometry)
# calculate the area of overlap for each polygon
overlap_areas <- st_area(overlapping_polygons)












