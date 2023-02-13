admin_dates <- function() {

  date_df <- data.frame(
    # admin_number = 3561:61361,
    admin        = 3561:61361,
    admin_date   = seq.Date(from = as.Date("1859/10/01"), by = "1 day", length.out = 57801)
  )

  return(date_df)

}

admins_to_date <- function(
    admin_no
    ) {

  # admin_no <- dplyr::enquo(admin_no)
  # tmp <- unname(sapply(admin_no, admins_to_date)
  # unname(tmp)
  # length(admin_no)
  # admin_no <- "26302.20226"


  # if NA is given return NA
  if(is.na(admin_no) | admin_no == "NA") {
    return(NA)
  }

  # if NULL is given, return NULL
  if(is.null(admin_no) | admin_no == "NULL") {
    return(NULL)
  }

  # if date is lowest possible admin number
  if(admin_no == "0.00000") {

    admin_date <- "0001-01-01"

    return(admin_date)

  }

  # most senior water right date
  most_senior_date <- as.Date("1849-12-31")

  # if admin number has "00000" digits after period
  if(unlist(strsplit(admin_no, "[.]"))[2] == "00000") {

    # admin values left of period
    aleft  <- as.numeric(unlist(strsplit(admin_no, "[.]"))[1])

    # admin date == appropriation date
    admin_date <- as.character(most_senior_date + aleft)

    # if left side of admin number is NOT "00000"
  } else {

    # split admin number to the left and right of period
    aleft  <- as.numeric(unlist(strsplit(admin_no, "[.]"))[1])
    aright <- as.numeric(unlist(strsplit(admin_no, "[.]"))[2])

    # prior adjudication date
    prior_adjx  <- as.character(most_senior_date + aleft)

    # appropriation date
    appropx <- as.character(most_senior_date + aright)

    # admin_date <- prior_adjx

    # if prior adjudication date is AFTER appropriation date, than admin date is appropriation date
    if(appropx > prior_adjx) {

      admin_date <- appropx

    # if prior adjudication date is BEFORE appropriation date, than admin date is prior adjudication date
    } else {

      admin_date <- prior_adjx

    }
    # # if prior adjudication date is AFTER appropriation date, than admin date is appropriation date
    # if(prior_adjx > appropx) {
    #
    #   admin_date <- appropx
    #
    # # if prior adjudication date is BEFORE appropriation date, than admin date is prior adjudication date
    # } else {
    #
    #   admin_date <- prior_adjx
    #
    # }

  }

  return(admin_date)

}


# library(terra)
# library(dplyr)
# shp_path <- "D:/louisville_wildfire/counties/aoi_counties.gpkg"
# shp <- terra::vect(shp_path)
#
#
#
# fl_paths = c("D:/louisville_wildfire/Flame_Length_Rasters/flp0/w001001.adf",
#             "D:/louisville_wildfire/Flame_Length_Rasters/flp1/w001001.adf",
#             "D:/louisville_wildfire/Flame_Length_Rasters/flp2/w001001.adf",
#             "D:/louisville_wildfire/Flame_Length_Rasters/flp3/w001001.adf",
#             "D:/louisville_wildfire/Flame_Length_Rasters/flp4/w001001.adf",
#             "D:/louisville_wildfire/Flame_Length_Rasters/flp5/w001001.adf")
#
# save_dir <- "D:/louisville_wildfire/tifs/"
#
# for (i in seq_along(fl_paths)){
#   message(fl_paths[i])
#
#   terra::writeRaster(
#     terra::rast(fl_paths[i]),
#     filename = paste0(save_dir, "flame_length_", i-1, ".tif")
#     # filename = gsub(
#     #                 basename(fl_paths[i]),
#     #                 gsub("w001001.adf", paste0("flame_length", "_", i-1, ".tif"), basename(fl_paths[i])),
#     #                 fl_paths[i]
#     #               )
#     )
# }
# tif_paths <- list.files(save_dir, full.names = T)
#
#
# r <- terra::rast(tif_paths)
#
# r  <- terra::mask(
#       terra::crop(
#                   r,
#                   terra::project(
#                     shp,
#                     terra::crs(r)
#                     )
#                   ),
#       terra::project(
#         shp,
#         terra::crs(r)
#       )
#       )
# rsum <- sum(r)
# rsum
# plot(rsum)





