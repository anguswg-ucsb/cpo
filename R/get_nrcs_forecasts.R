# Angus Watters
# load in NRCS Dataset from SOAP API (Python code to be translated into R)
# NRCS_URL = 'https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL

# Libraries
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# --------------------
# ---- Data paths ----
# --------------------

# NRCS Streamflow forecasts
nrcs_path     <- "data/nrcs/nrcs_forecasts.csv"

# district shape path
district_path <- "data/water_districts_simple.geojson"

# path to Water right points of interest
wr_path <-   "data/rights_by_gnis_id.rds"

# path to save output to
forecasts_path <- "data/wdid_nrcs_forecasts.rds"
# nrcs <- readr::read_csv("data/nrcs/nrcs_forecasts.csv")

# --------------------
# ---- Read data -----
# --------------------
# load and go get call analysis data
if(file.exists(forecasts_path)) {

  message(paste0(
    "Reading WDID NRCS Forecasts data: ",
    "\n---> ", forecasts_path
  ))

  # read in call analysis data
  forecasts_df <- readRDS(forecasts_path)

} else {

  # get WDID level NRCS monthly forecasts data
  forecasts_df <- process_forecasts(
    pts_path  = wr_path,
    nrcs_path = nrcs_path
  )

  # snotel_df <- get_snotel(
  # site_path     = site_path,
  # district_path = district_path
  # )
  message(paste0(
    "Saving WDID NRCS Forecasts data: ",
    "\n---> ",  forecasts_path
  ))

  saveRDS(forecasts_df, forecasts_path)
  # saveRDS(snotel_df, paste0(swe_path, ".rds"))
  # readr::write_csv(snotel_df, paste0(swe_path, ".csv"))

}

# --------------------
# ---- SOAP data -----
# --------------------

library(httr)
library(xml2)

# Example usage
forecast_period <- "APR-JUL"
station_code <- "09361500"
element_cd <- "SRVO"
network <- "USGS"
state <- "CO"

casts <- get_forecast_data(
  forecast_period = "APR-JUL",
  station_code    = "09361500",
  element_cd      = "SRVO",
  network         = "USGS",
  state           = "CO"
)

# snotel_df <- lapply(1:nrow(site_df), function(i) {
#
#   logger::log_info("Getting district {unique(site_df[i, ]$district)} snotel data...")
#
#   # unique(site_df[i, ]$basin)
#   ids <- tidyr::separate_rows(site_df[i, ], site_id, sep = ", ")$site_id
#
#   snotel <- go_get_snotel_data(site_ids = ids) %>%
#     dplyr::mutate(
#       basin    = unique(site_df[i, ]$basin),
#       district = unique(site_df[i, ]$district)
#     )
#
#   snotel
#
# })


# make_soap_body <- function(forecast_period, station_code, element_cd, network, state) {
#   # Create SOAP request body
#   body <- paste0(
#             '<?xml version="1.0" encoding="UTF-8"?>
#                  <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:q0="http://www.wcc.nrcs.usda.gov/ns/awdbWebService"
#                        xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
#                         <SOAP-ENV:Body>
#                           <q0:getForecasts>',
#                              '<stationTriplet>', station_code, ':', state, ':', network, '</stationTriplet>',
#                              '<elementCd>', element_cd, '</elementCd>',
#                              '<forecastPeriod>', forecast_period, '</forecastPeriod>',
#                          '</q0:getForecasts>
#                         </SOAP-ENV:Body>
#                       </SOAP-ENV:Envelope>'
#             )
#
#
#   return(xml2::read_xml(body))
#
# }

# get_forecast_data <- function(
#     forecast_period,
#     station_code,
#     element_cd,
#     network,
#     state
#     ) {
#
#   # Create SOAP request body
#   body <- make_soap_body(forecast_period, station_code, element_cd, network, state)
#
#   # Save body as temporary XML file
#   temp_file <- tempfile(fileext = ".xml")
#
#   xml2::write_xml(body, temp_file)
#
#   # NRCS SOAP API Client
#   NRCS_URL = 'https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL'
#
#   # NRCS_URL = 'http://www.wcc.nrcs.usda.gov/ns/awdbWebService'
#   r <- httr::POST(NRCS_URL, body = httr::upload_file(temp_file))
#
#   resp_data <-
#     r %>%
#     httr::content() %>%
#     xml2::as_list()
#
#   # Delete the temporary XML file
#   file.remove(temp_file)
#
#   message("Getting forecast data - Station code: ", station_code)
#
#   fx <- lapply(1:length(resp_data$Envelope$Body$getForecastsResponse), function(i) {
#
#     rows <- resp_data$Envelope$Body$getForecastsResponse[[i]]
#
#     row_lst <- lapply(rows, unlist)
#
#     final <- data.frame(
#       exceedence_prob = unlist(row_lst[names(row_lst) == "exceedenceProbabilities"]),
#       exceedence_vals = unlist(purrr::map(row_lst[names(row_lst) == "exceedenceValues"], ~if (is.null(.x)) NA else .x))
#     )
#
#     if (all(is.na(final$exceedence_vals))) {
#
#       NULL
#
#     } else {
#
#       final <-
#         final %>%
#         dplyr::tibble() %>%
#         dplyr::mutate(
#           element_cd      = row_lst$elementCd,
#           station_triplet = row_lst$stationTriplet,
#           date            = row_lst$calculationDate,
#           pub_date        = row_lst$publicationDate
#         ) %>%
#         tidyr::separate(station_triplet, into = c("usgs_id", "state", "network"), sep = ":",  remove = FALSE) %>%
#         dplyr::select(date, pub_date, usgs_id, station_triplet, element_cd, exceedence_prob, exceedence_vals)
#
#       final
#     }
#
#   }) %>%
#     dplyr::bind_rows()
#
#   return(fx)
#
# }

# # Create SOAP request body
# body <- make_soap_body(forecast_period, station_code, element_cd, network, state)
#
# xml2::write_xml(body, "soap_test.xm")
#
# # NRCS SOAP API Client
# NRCS_URL = 'https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL'
#
# # NRCS_URL = 'http://www.wcc.nrcs.usda.gov/ns/awdbWebService'
# r <- httr::POST(NRCS_URL, body = upload_file("soap_test.xml"))
#
# resp_data <-
#   r %>%
#   httr::content() %>%
#   xml2::as_list()

# fx <- lapply(1:length(resp_data$Envelope$Body$getForecastsResponse), function(i) {
#   # i = 162
#   message(i, "/", length(resp_data$Envelope$Body$getForecastsResponse))
#
#   rows <- resp_data$Envelope$Body$getForecastsResponse[[i]]
#
#   row_lst <- lapply(rows, unlist)
#
#   final <- data.frame(
#     exceedence_prob = unlist(row_lst[names(row_lst) == "exceedenceProbabilities"]),
#     exceedence_vals = unlist(purrr::map(row_lst[names(row_lst) == "exceedenceValues"], ~if (is.null(.x)) NA else .x))
#     )
#
#   if (all(is.na(final$exceedence_vals))) {
#
#     NULL
#
#   } else {
#
#     final <-
#       final %>%
#       dplyr::tibble() %>%
#       dplyr::mutate(
#         element_cd      = row_lst$elementCd,
#         station_triplet = row_lst$stationTriplet,
#         date            = row_lst$calculationDate,
#         pub_date        = row_lst$publicationDate
#       ) %>%
#       tidyr::separate(station_triplet, into = c("usgs_id", "state", "network"), sep = ":",  remove = FALSE) %>%
#       dplyr::select(date, pub_date, usgs_id, station_triplet, element_cd, exceedence_prob, exceedence_vals)
#
#     final
#   }
#
#   }) %>%
#   dplyr::bind_rows()
  # names(tmp)

  # unique_names <- unique(names(lst))
 # tmp2 <-  tmp[names(tmp) == "exceedenceProbabilities"] %>%
 #   cbind() %>%
 #    as.data.frame()
 #
 #  tmp[names(tmp) == "exceedenceValues"] %>%
 #    cbind() %>%
 #    as.data.frame()

  # data.frame(
  #   exceedence_prob = unlist(cbind(row_lst[names(row_lst) == "exceedenceProbabilities"])),
  #   # exceedence_vals = unlist(cbind(row_lst[names(row_lst) == "exceedenceValues"]))
  #   exceedence_vals =  unlist(purrr::map(row_lst[names(row_lst) == "exceedenceValues"], ~if (is.null(.x)) NA else .x))
  # )

   # unlist(purrr::map(row_lst[names(row_lst) == "exceedenceValues"], ~if (is.null(.x)) NA else .x))

#   row_lst[names(row_lst) == "exceedenceValues"]
#
#   ex_probs <- stats::setNames(as.data.frame(cbind(row_lst[names(row_lst) == "exceedenceProbabilities"])), "exceedenceProbabilities")
#   rownames(ex_probs) <- NULL
#
#   unlist(ex_probs$exceedenceProbabilities)
#
#   ex_vals <- stats::setNames(as.data.frame(cbind(row_lst[names(row_lst) == "exceedenceValues"])), "exceedenceValues")
#
#   rownames(ex_vals) <- NULL
#
#   final <- cbind(ex_probs, ex_vals)
# # unlist(row_lst[names(row_lst) == "exceedenceProbabilities"])
# # unlist(cbind(row_lst[names(row_lst) == "exceedenceProbabilities"]))
#
#   final <-   data.frame(
#                 # exceedence_prob = unlist(cbind(row_lst[names(row_lst) == "exceedenceProbabilities"])),
#                 # exceedence_vals = unlist(cbind(row_lst[names(row_lst) == "exceedenceValues"]))
#                 exceedence_prob = unlist(row_lst[names(row_lst) == "exceedenceProbabilities"]),
#                 exceedence_vals =  unlist(purrr::map(row_lst[names(row_lst) == "exceedenceValues"], ~if (is.null(.x)) NA else .x))
#               )
#
#   final <-
#     final %>%
#     dplyr::tibble() %>%
#     dplyr::mutate(
#       element_cd      = row_lst$elementCd,
#       station_triplet = row_lst$stationTriplet,
#       date            = row_lst$calculationDate,
#       pub_date        = row_lst$publicationDate
#     ) %>%
#     tidyr::separate(station_triplet, into = c("usgs_id", "state", "network"), sep = ":",  remove = FALSE) %>%
#     dplyr::select(date, pub_date, usgs_id, station_triplet, element_cd, exceedence_prob, exceedence_vals)
    # dplyr::relocate(calc_date, station_triplet, element_cd)




  # tmp <- lapply(rows, as.data.frame, USE.NAMES = TRUE) %>%
  #   dplyr::bind_rows()
  #   # tidyr::unnest_wider()
  # df <- as.data.frame(do.call(bind_rows, lapply(rows, as.data.frame)))
  #
  #
  # flattenlist(rows)
  # inner <- lapply(1:length(rows), function(k) {
  #   message("inner vals: ", k, "/", length(rows))
  #
  #
  #
  # }) %>%
  #   dplyr::bind_rows() %>%
  #   dplyr::mutate(id = i)
  #
  # inner %>%
  #   tidyr::pivot_wider(
  #     id_cols = c(-id),
  #     names_from  = "name",
  #     values_from = "vals"
  #   )
  #
  # inner$name %>% class()
  #
  # df <- separate(df, StringColumn, into = c("Column1", "Column2", "Column3"), sep = ":")


  # })
# con$Envelope$Body$getForecastsResponse[[1]] %>% length()



# # rxml %>% xml2::() %>% jsonlite::fromJSON()
# xml2::xml_find_all(rxml, ".//p")
# RCurl::curlPerform(url = "https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL",
#             # httpheader = headerFields,
#             postfields = body
# )
# # Print the SOAP request body
# print(body)
#
# # Print the SOAP request body
# print(body)
# nrcs$stationTriplet[1]
# nrcs$forecastPeriod
# content(rr)

# --------------------
# --------------------


#
# process_forecasts <- function(pts_path, nrcs_path) {
#
#   # read in water rights points
#   wr_pts <- readRDS(pts_path)
#
#   # read in NRCS forecasts data
#   nrcs   <- readr::read_csv(nrcs_path)
#
#   # remove missing dates, select relevant columns, and 50/90% Exceedance probability.
#   nrcs <-
#     nrcs %>%
#     dplyr::filter(!is.na(calculationDate)) %>%
#     dplyr::select(
#       station_code,
#       pub_date   = publicationDate,
#       date       = calculationDate,
#       ep         = exceedenceProbabilities,
#       exceed_val = exceedenceValues,
#       longitude,
#       latitude
#       ) %>%
#     dplyr::group_by(station_code, date) %>%
#     dplyr::filter(ep %in% c(50, 90)) %>%
#     tidyr::pivot_wider(
#       id_cols     = c(station_code, date, longitude,latitude ),
#       names_from  = ep,
#       values_from = exceed_val
#     ) %>%
#     stats::setNames(c("station_code", "date", "longitude", "latitude", "ep_50", "ep_90")) %>%
#     dplyr::mutate(
#       ep_90 = dplyr::case_when(
#         is.na(ep_90) ~ ep_50,
#         TRUE         ~ ep_90
#       )
#     ) %>%
#     dplyr::ungroup()
#
#   # extract unique NRCS forecast points
#   nrcs_pts <-
#     nrcs %>%
#     dplyr::group_by(station_code) %>%
#     dplyr::slice(1) %>%
#     sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(
#       near_id = 1:n()
#     )
#
#   # nearest forecast points to each water right point
#   near_pts <- sf::st_nearest_feature(wr_pts, nrcs_pts)
#
#   # add forecast point indexes as column
#   wr_pts$near_id <- near_pts
#
#   # join Water rights points with NRCS points
#   fx_join <-
#     wr_pts %>%
#     dplyr::left_join(
#       dplyr::select(sf::st_drop_geometry(nrcs_pts), station_code, near_id),
#       by = c("near_id")
#       ) %>%
#     sf::st_drop_geometry() %>%
#     dplyr::select(district, wdid, gnis_id, approp_date = appropriation_date, station_code)
#
#   # join forecasts data with WDID/GNIS ID/ DISTRICT data
#   wdid_forecasts <-
#     nrcs %>%
#     dplyr::left_join(
#       fx_join,
#       relationship = "many-to-many",
#       by           = "station_code"
#     ) %>%
#     dplyr::mutate(
#       date = as.Date(date)
#     ) %>%
#     dplyr::select(district, wdid, gnis_id, approp_date, station_code, date, ep_50, ep_90)
#
#   return(wdid_forecasts)
#
# }
#
# tmpjoin
# nrcs %>%
#   na.omit()
# tmp_fx <-
#   nrcs %>%
#   dplyr::filter(station_code %in% tmpjoin$station_code) %>%
#   dplyr::left_join(
#   # dplyr::inner_join(
#     tmpjoin,
#     by = "station_code"
#   )
#   # dplyr::select(district, wdid, gnis_id, approp_date, station_code, pub_date = publicationDate,
#   #               date = calculationDate,
#   #               ep = exceedenceProbabilities,
#   #               exceed_val = exceedenceValues) %>%
#   # dplyr::filter(ep %in% c(50, 90))
#
#
# nrcs %>%
#   dplyr::select(station_code,
#                 pub_date = publicationDate,
#                 date = calculationDate,
#                 ep = exceedenceProbabilities,
#                 exceed_val = exceedenceValues) %>%
#   dplyr::group_by(station_code, date) %>%
#   dplyr::filter(ep %in% c(50, 90)) %>%
#   tidyr::pivot_wider(
#     id_cols     = c(station_code, date),
#     names_from  = ep,
#     values_from = exceed_val
#   ) %>%
#   stats::setNames(c("station_code", "date", "ep_50", "ep_90")) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(station_code)
# # join forecasts data with WDID/GNIS ID/ DISTRICT data
# wdid_forecasts <-
#   nrcs %>%
#   dplyr::left_join(
#     dplyr::select(sf::st_drop_geometry(
#       dplyr::left_join(wr_pts,
#                        dplyr::select(sf::st_drop_geometry(nrcs_pts), station_code, near_id),
#                        by = c("near_id")
#                        )
#       ),
#       district, wdid, gnis_id, seniority, station_code
#       ),
#     by = "station_code"
#   )

# # Define the corner points
# corner_points <- matrix(c(-119.89014, 34.52399,
#                           -119.51042, 34.50155,
#                           -119.51042, 34.35095,
#                           -119.89014, 34.35095,
#                           -119.89014, 34.52399), ncol = 2, byrow = TRUE)
#
# # Create an sf polygon object
# polygon <- st_polygon(list(corner_points))
#
# aoi_final <- sf::st_sfc(polygon) %>% sf::st_as_sf(crs = 4326)
# aoi_final <-
#   aoi_final %>%
#   sf::st_bbox() %>%
#   sf::st_as_sfc() %>%
#   sf::st_as_sf()
#
# mapview::mapview(aoi_final)
#
#
# # I want the polygon to be a rectangle and include these 4 corner points (-119.89014, 34.51399), (-119.50836, 34.50155), (-119.51042, 34.35095), and (-119.89014, 34.35095). Can you please make an sf polygon in R from these coordinates?
#
# aoi2 <- AOI::aoi_get("Santa Barbara")
# aoi <-
#   data.frame(lon = c(-119.748), lat = c(34.44266)) %>%
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   sf::st_buffer(23000) %>%
#   sf::st_bbox() %>%
#   sf::st_as_sfc() %>%
#   sf::st_as_sf()
# mapview::mapview(aoi) + aoi2
#
#
#
# elev_high <- elevatr::get_elev_raster(aoi, z = 14)
# raster::writeRaster(elev_high, "santa_baraba_5m_dem.tif")
#
#
# elev_crop <- terra::rast(elev_high)
# elev_crop <- elev_crop %>%
#   terra::crop(terra::vect(aoi_final))
#
# terra:::writeRaster(elev_crop, "santa_baraba_5m_dem_crop.tif")
#
# plot(elev_crop)
# mapview::mapview(aoi) + aoi2  +elev_high

# # add
# nrcs_pts <-
#   nrcs_pts %>%
#   dplyr::mutate(
#     near_pts = 1:dplyr::row_number()
#   )
# mapview::mapview(tmp_wr) + tmp_for
# wr_pts[near_pts, ]
#
#
#
# nrcs_pts <-
#   nrcs %>%
#   dplyr::group_by(stationTriplet) %>%
#   dplyr::slice(1) %>%
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   dplyr::ungroup()
#
# nrcs_pts %>%
#   mapview::mapview()
# elevatr::get_elev_raster()
