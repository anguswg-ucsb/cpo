#' Aggregate daily gridMET data to weekly means for all avaliable climate parameters
#'
#' @param aoi SF
#' @param varname Charcter vector, climateR::params for a list of gridMET climate parameters to choose from
#' @param start_date starting date string (YYYY-MM-DD ). Defaults to "1980-01-01"
#' @param end_date date string (YYYY-MM-DD ). Defaults to yesterday.
#' @param name_col character, name of column in SF object that that uniquely identifies each polygon. Default is "district".
#' @param wide logical, whether data should be return wide (column for each climate variable) or long (a column naming the variable and a column represnting the value of the variable). Default is TRUE, returns a wide dataframe
#' @param verbose logical, should messages print or not. Default is FALSE, no messages print
#' @return dataframe with weekly average climate variable values for each polygon in the provided aoi SF object
#' @export
get_gridmet <- function(
    aoi        = NULL,
    varname    = NULL,
    start_date = NULL,
    end_date   = NULL,
    name_col   = "district",
    wide       = TRUE,
    verbose    = FALSE
) {

  # if start date is NULL
  if(is.null(start_date)) {

    start_date = "1980-01-01"

  }

  # if end date is NULL
  if(is.null(end_date)) {

    end_date = Sys.Date() - 1

  }

  # make name column name lowercase
  name_col <- tolower(name_col)

  # make sure AOI is in correct CRS and is a MULTIPOLYGON
  aoi <-
    aoi %>%
    sf::st_transform(4326) %>%
    sf::st_cast("MULTIPOLYGON")

  # make lower case column names
  names(aoi) <- tolower(names(aoi))

  message(paste0(
    "Getting gridMET data...\n",
    "-------------------------",
    "\nStart date: ", start_date,
    "\nEnd date: ", end_date
  ))

  # get daily gridMET data
  gridmet <- climateR::getGridMET(
    AOI       = aoi,
    varname   = varname,
    startDate = start_date,
    endDate   = end_date
  )

  # district names
  district_names <- paste0(aoi[[name_col]])

  # remove gridMET "category" data that is accidently returned
  gridmet <- gridmet[names(gridmet) != "category"]

  message(paste0("Aggregating by week..."))

  # mask and crop variables for each polygon in aoi
  gridmet <- lapply(1:length(gridmet), function(i) {

    # loop over all polygons and crop/mask SpatRasters
    lapply(seq_len(nrow(aoi)), function(x) {

      crop_mask_raster(
        raster  = gridmet[[i]],
        polygon = aoi[x, ]
      )
    }
    ) %>%
      stats::setNames(district_names)

  }) %>%
    stats::setNames(varname)



  message(paste0("Calculating means..."))

  tidy_gridmet <- lapply(seq_along(gridmet), function(i) {

    # lapply counter message
    if(verbose) {
      message(paste0(i, "/", length(gridmet)))
    }

    lapply(seq_along(gridmet[[i]]), function(x) {

        gridmet[[i]][[x]] %>%
        as.data.frame(xy = F) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(), mean)) %>%
        tidyr::pivot_longer(cols = dplyr::everything()) %>%
        tidyr::separate(name, c("variable", "date"), sep = "_", extra = "merge") %>%
        dplyr::mutate(
          date      = as.Date(gsub("_", "-", date)),
          district  = names(gridmet[[i]])[x],
          units     = unique(terra::units(gridmet[[i]][[x]]))
        ) %>%
        dplyr::relocate(district, date, variable, units, value)

    }) %>%
      dplyr::bind_rows()

  }) %>%
    dplyr::bind_rows()

  # if wide is TRUE, then pivot the table wider and return that
  if(wide) {
    tidy_gridmet <-
      tidy_gridmet %>%
      dplyr::select(-units) %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "value")
  }

  return(tidy_gridmet)
}

# define a function to crop and mask a single SpatRaster for a single polygon
#' Internal function used in get_climate
#'
#' @param raster
#' @param polygon
#'
#' @return
#' @export
#'
#' @examples
crop_mask_raster <- function(raster, polygon) {

  # CROP AND MASK RASTERS
  msk <- terra::mask(
    terra::crop(raster, polygon),
    polygon
  )

  # dates
  dates     <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})", "\\1", names(raster)))

  # variable name
  var       <- gsub("_\\d{4}-\\d{2}-\\d{2}", "", names(raster))[1]

  # intervals
  intervals <- cut(dates, breaks = "week")

  # variable units
  var_units <- terra::units(raster)

  # calculate means across time
  msk <-
    msk %>%
    terra::tapp(intervals, fun = "mean")

  # assign time values
  terra::time(msk)  <- as.Date(unique(intervals))

  # assign unit values
  terra::units(msk) <- unique(var_units)

  # set names
  names(msk) <- gsub("\\.", "_", gsub("X", paste0(var, "_"), names(msk)))

  return(msk)
}

# Define function to get call data
get_call_data <- function(
    wdid_df,
    start_date,
    end_date,
    api_key = NULL,
    save_path
    ) {

  if(file.exists(save_path)) {

    message(paste0("Reading data from ---> ", save_path))

    call_df <- readRDS(save_path)

  } else {

    call_df <- lapply(1:nrow(wdid_df), function(i) {

      message(paste0(i, "/", nrow(wdid_df)))

      # GET request to CDSS API
      tryCatch({
        calls <- cdssr::get_call_analysis_wdid(
          wdid       = wdid_df$wdid[i],
          admin_no   = "99999.00000",
          start_date = start_date,
          end_date   = end_date,
          api_key    = api_key
        ) %>%
          dplyr::mutate(
            district            = wdid_df$district[i],
            wdid_gnis_id        = wdid_df$gnis_id[i],
            wdid_approp_date    = wdid_df$appropriation_date[i],
            wdid_structure_name = wdid_df$structure_name[i],
            wdid_structure_type = wdid_df$structure_type[i],
            seniority           = wdid_df$seniority[i]
          )
        calls
      }, error = function(e) {

        NULL
      })

    }) %>%
      dplyr::bind_rows()

    message(paste0("Saving data to path ---> ", save_path))

    saveRDS(call_df, save_path)

  }

  return(call_df)
}

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





