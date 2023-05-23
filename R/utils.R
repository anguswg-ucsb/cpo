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
      tidyr::pivot_wider(names_from = "variable", values_from = "value") %>%
      dplyr::mutate(
        district = dplyr::case_when(
          as.numeric(district) < 10 ~ paste0("0", district),
          TRUE                      ~ paste0(district)
        )
      )
  }

  return(tidy_gridmet)
}


go_get_snotel_data <- function(site_ids) {

  site_lst <- lapply(1:length(site_ids), function(i) {

    message(paste0("Site: ", i, "/", length(site_ids)))
    snotelr::snotel_download(site_id = site_ids[i], internal = TRUE)

  }) %>%
    dplyr::bind_rows()

  return(site_lst)

}

get_snotel <- function(
    site_path,
    district_path,
    start_lag = 3,
    end_lag   = 12
    ) {

  # path to snotel data
  # snotel_path <- "data/all_snotel_co.rds"

  # district shape path
  # district_path <- "data/water_districts_simple.geojson"

  # site_path <- "data/snotel_sites.csv"

  # read in snotel data
  site_df <- readr::read_csv(site_path)

  site_ids <- unique(site_df$site_id)

  snotel_df <- go_get_snotel_data(site_ids = site_ids)

  # water districts shape
  dists   <- sf::read_sf(district_path)

  # Convert the sequence to a data frame
  date_df <-
    data.frame(
      date = seq(as.Date("1979-12-31"), Sys.Date(), by = "7 days")
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

  snotel_df <-
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

  # add lagged SWE value by basin, default lag is 3-12 month lags
  snotel_df <-
    snotel_df %>%
    dplyr::group_by(basin) %>%
    timetk::tk_augment_lags(swe, .lags = seq(start_lag*4, end_lag*4, 4)) %>%
    stats::setNames(
      c("basin", "date", "swe",
        paste0("swe_lag_", seq(start_lag, end_lag), "_month"))
                    ) %>%
    dplyr::ungroup()

  # # add monthly by year and basin
  # snotel_df <-
  #   snotel_df %>%
  #   dplyr::mutate(
  #     year  = lubridate::year(date)
  #   ) %>%
  #   dplyr::left_join(
  #     na.omit(
  #       tidyr::pivot_wider(
  #         na.omit(
  #           dplyr::mutate(
  #             dplyr::ungroup(
  #               dplyr::summarise(
  #                 dplyr::group_by(
  #                   dplyr::mutate(snotel_df,
  #                                 month = lubridate::month(date, label = T),
  #                                 year  = lubridate::year(date)
  #                   ),
  #                   basin, month, year
  #                 ),
  #                 swe = max(swe, na.rm = T)
  #               )
  #             ),
  #             month = tolower(month)
  #           )
  #         ),
  #         names_from  = "month",
  #         names_glue  = "{month}_{.value}",
  #         values_from = "swe"
  #       )
  #     ),
  #     by = c("basin", "year")
  #   ) %>%
  #   dplyr::select(-year) %>%
  #   dplyr::ungroup()

  # tmp
  peaks <-
    snotel_df %>%
    dplyr::mutate(
      month = lubridate::month(date, label = T),
      year  = lubridate::year(date)
    ) %>%
    dplyr::group_by(basin, month, year) %>%
    dplyr::summarise(
      peak_swe = round(max(swe, na.rm = T), 4)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(month %in% c("Mar", "Apr", "May")) %>%
    na.omit() %>%
    dplyr::group_by(basin, year) %>%
    tidyr::pivot_wider(
      id_cols     = c(basin, year),
      names_from  = month,
      values_from = peak_swe
    ) %>%
    dplyr::ungroup()

  names(peaks) <- c("basin", "year",
                    c(paste0(tolower(names(peaks))[!grepl("basin|year", tolower(names(peaks)))], "_swe"))
                    )

  snotel_df <-
    snotel_df %>%
    dplyr::mutate(
      year  = lubridate::year(date)
    ) %>%
    dplyr::left_join(
      peaks,
      by = c("basin", "year")
    ) %>%
    dplyr::relocate(basin, date, swe, mar_swe, apr_swe, may_swe) %>%
    dplyr::select(-year)


  return(snotel_df)

}
aggreg_snotel <- function(
    site_path,
    district_path,
    start_lag = 3,
    end_lag   = 12
) {

  # path to snotel data
  # snotel_path <- "data/all_snotel_co.rds"

  # district shape path
  # district_path <- "data/water_districts_simple.geojson"

  # site_path <- "data/snotel_sites.csv"

  # water districts shape
  dists   <- sf::read_sf(district_path)

  # read in snotel data
  site_df <- readr::read_csv(site_path)

  # # read in snotel data
  # site_df <- readr::read_csv(site_path)
  # missing_sites <- readr::read_csv(missing_sites_path)
  # dists2 <- sf::st_join(
  #       dists,
  #       sf::st_as_sf(site_df, coords = c("lon", "lat"), crs = 4326)
  #     ) %>%
  #     dplyr::filter(!is.na(site_id)) %>%
  #     sf::st_drop_geometry() %>%
  #     dplyr::rename(district = DISTRICT) %>%
  #     dplyr::group_by(district) %>%
  #     dplyr::summarise(
  #       site_id = paste(site_id, collapse = ", ")
  #     ) %>%
  #     dplyr::bind_rows(missing_sites) %>%
  #     dplyr::left_join(
  #       dplyr::select(
  #         sf::st_drop_geometry(dists),
  #         basin = BASIN,
  #         district = DISTRICT
  #       ),
  #       by = "district"
  #     ) %>%
  #     dplyr::relocate(basin, district, site_id)
  #   readr::write_csv(dists2, "data/district_snotel_table.csv")
  #   sf::st_write(site_pts2, "snotel_sites_pts.gpkg")
  #   sf::st_write(dists2, "districts_with_snotel_ids.gpkg")
  #   dists2 %>%
  #     dplyr::mutate(
  #       needs_snotel = dplyr::case_when(
  #         is.na(site_id) ~ "YES",
  #         TRUE ~ "NO")) %>%
  #     ggplot2::ggplot() +
  #     ggplot2::geom_sf(ggplot2::aes(fill = needs_snotel)) +
  #     ggplot2::geom_sf(data = site_pts2)
  # site_df %>%
  #   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  #   mapview::mapview() + dists

  # Convert the sequence to a data frame
  date_df <-
    data.frame(
      date = seq(as.Date("1979-12-31"), Sys.Date(), by = "7 days")
    ) %>%
    dplyr::mutate(
      year      = lubridate::year(date),
      week_num  = strftime(date, format = "%V"),
      year_week = paste0(year, "_", week_num)
    )

  snotel_df <- lapply(1:nrow(site_df), function(i) {

    logger::log_info("Getting district {unique(site_df[i, ]$district)} snotel data...")

    # unique(site_df[i, ]$basin)
    ids <- tidyr::separate_rows(site_df[i, ], site_id, sep = ", ")$site_id

    snotel <- go_get_snotel_data(site_ids = ids) %>%
      dplyr::mutate(
        basin    = unique(site_df[i, ]$basin),
        district = unique(site_df[i, ]$district)
      )

    snotel

  })

  # average SWE across each districts snotel sites and dates
  snotel_df <-
    snotel_df %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(date, district, basin) %>%
    dplyr::summarise(
      swe = mean(snow_water_equivalent, na.rm =T)
    ) %>%
    dplyr::ungroup()

  # impute missing (NA/NaN) values with mean across the basin for each date
  # a handful of NAs persisted through this and so for that small amount,
  # so we impute the mean across the whole state, the idea being
  # that its such a small number of days so,
  # we take a snapshot of general SWE conditions across the whole state use that to replace missing values
  snotel_df <-
    snotel_df %>%
    dplyr::group_by(date, basin) %>%
    dplyr::mutate(
      swe  = ifelse(is.na(swe) |is.nan(swe), mean(swe, na.rm = TRUE), swe)
       ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
      swe  = ifelse(is.na(swe) |is.nan(swe), mean(swe, na.rm = TRUE), swe)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(datetime = date)

  # join daily district SWE values w/ weekly date dataframe and calc avg swe per week per district
  snotel_df <-
    snotel_df %>%
    dplyr::mutate(
      # year     = lubridate::year(datetime),
      # week_num = strftime(datetime, format = "%V"),
      year_week = paste0(lubridate::year(datetime), "_",  strftime(datetime, format = "%V"))
    ) %>%
    dplyr::filter(year_week %in% unique(date_df$year_week)) %>%
    dplyr::left_join(
      date_df,
      relationship = "many-to-many",
      by           = c("year_week")
      # by           = c("year", "week_num")
    ) %>%
    dplyr::group_by(basin, district, date) %>%
    dplyr::summarise(
      swe = mean(swe, na.rm =T)
    ) %>%
    dplyr::ungroup()
    # dplyr::filter(!is.na(date))

  # remove NAs in date column
  snotel_df <- dplyr::filter(snotel_df, !is.na(date))

  # get complete date range to fill out missing dates for some snotel sites
  date_range <-
    snotel_df %>%
    # dplyr::filter(basin == "South Platte") %>%
    dplyr::group_by(basin) %>%
    dplyr::add_count() %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(n) %>%
    dplyr::select(date) %>%
    dplyr::distinct()

  expanded_df  <-
    snotel_df %>%
    tidyr::complete(district, date = date_range$date) %>%
    dplyr::left_join(
      dplyr::distinct(dplyr::select(snotel_df, basin2 = basin, district)),
      by = "district"
    ) %>%
    dplyr::mutate(basin = ifelse(is.na(basin), basin2, basin)) %>%
    dplyr::select(-basin2) %>%
    dplyr::group_by(basin, date) %>%
    dplyr::mutate(swe = ifelse(is.na(swe), mean(swe, na.rm = TRUE), swe)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(swe))

  # # add lagged SWE value by basin, default lag is 3-12 month lags
  # snotel_df2 <-
  #   snotel_df2 %>%
  #   dplyr::group_by(basin, district) %>%
  #   timetk::tk_augment_lags(swe, .lags = seq(start_lag*4, end_lag*4, 4)) %>%
  #   stats::setNames(
  #     c("basin", "district", "date", "swe",
  #       paste0("swe_lag_", seq(start_lag, end_lag), "_month"))
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   na.omit()

  # # add monthly by year and basin
  # snotel_df <-
  #   snotel_df %>%
  #   dplyr::mutate(
  #     year  = lubridate::year(date)
  #   ) %>%
  #   dplyr::left_join(
  #     na.omit(
  #       tidyr::pivot_wider(
  #         na.omit(
  #           dplyr::mutate(
  #             dplyr::ungroup(
  #               dplyr::summarise(
  #                 dplyr::group_by(
  #                   dplyr::mutate(snotel_df,
  #                                 month = lubridate::month(date, label = T),
  #                                 year  = lubridate::year(date)
  #                   ),
  #                   basin, month, year
  #                 ),
  #                 swe = max(swe, na.rm = T)
  #               )
  #             ),
  #             month = tolower(month)
  #           )
  #         ),
  #         names_from  = "month",
  #         names_glue  = "{month}_{.value}",
  #         values_from = "swe"
  #       )
  #     ),
  #     by = c("basin", "year")
  #   ) %>%
  #   dplyr::select(-year) %>%
  #   dplyr::ungroup()

  # calculate March, April, May peak SWE values
  peaks <-
    expanded_df %>%
    dplyr::mutate(
      month = lubridate::month(date, label = T),
      year  = lubridate::year(date)
    ) %>%
    dplyr::group_by(basin, district, month, year) %>%
    dplyr::summarise(
      peak_swe = round(max(swe, na.rm = T), 4)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(month %in% c("Mar", "Apr", "May")) %>%
    na.omit() %>%
    dplyr::group_by(basin, district, year) %>%
    tidyr::pivot_wider(
      id_cols     = c(basin, district, year),
      names_from  = month,
      values_from = peak_swe
    ) %>%
    dplyr::ungroup() %>%
    na.omit()


  # cleanup names
  names(peaks) <- c("basin", "district", "year",
                    c(paste0(tolower(names(peaks))[!grepl("basin|district|year", tolower(names(peaks)))], "_swe")))

  # add Peak March, April, May SWE
  expanded_df <-
    expanded_df %>%
    dplyr::mutate(
      year  = lubridate::year(date)
    ) %>%
    dplyr::left_join(
      dplyr::select(peaks, -basin),
      by = c("district", "year")
    ) %>%
    dplyr::relocate(basin, district, date, swe, mar_swe, apr_swe, may_swe) %>%
    dplyr::select(-year) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3))) %>%
    dplyr::mutate(
      district = ifelse(district < 10, paste0("0", district), district)
    )

  # expanded_df %>%
  #   dplyr::select(-swe) %>%
  #   tidyr::pivot_longer(cols =contains("swe")) %>%
  #   # tidyr::pivot_longer(cols =("swe")) %>%
  #   dplyr::filter(basin == "South Platte") %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_line(ggplot2::aes(x = date, y = value, color = name), size = 1) +
  #   ggplot2::facet_wrap(~district)

  return(expanded_df)

}

make_soap_body <- function(forecast_period, station_code, element_cd, network, state) {
  # Create SOAP request body
  body <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>
                 <SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:q0="http://www.wcc.nrcs.usda.gov/ns/awdbWebService"
                       xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                        <SOAP-ENV:Body>
                          <q0:getForecasts>',
    '<stationTriplet>', station_code, ':', state, ':', network, '</stationTriplet>',
    '<elementCd>', element_cd, '</elementCd>',
    '<forecastPeriod>', forecast_period, '</forecastPeriod>',
    '</q0:getForecasts>
                        </SOAP-ENV:Body>
                      </SOAP-ENV:Envelope>'
  )


  return(xml2::read_xml(body))

}

get_forecast_data <- function(
    station_code,
    forecast_period,
    element_cd,
    network,
    state
) {

  res <- lapply(1:length(station_code), function(i) {

    # tryCatch({
    #   i= 1
    #   station_code = "06694920"

      # Create SOAP request body
      body <- make_soap_body(forecast_period, station_code[i], element_cd, network, state)

      # Save body as temporary XML file
      temp_file <- tempfile(fileext = ".xml")

      # write body to temporary file
      xml2::write_xml(body, temp_file)

      # NRCS SOAP API Client
      NRCS_URL = 'https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL'

      r <- httr::POST(NRCS_URL, body = httr::upload_file(temp_file))

      # Check the status code
      if (r$status_code == 200) {

        # Process the response
        resp_data <-
          r %>%
          httr::content() %>%
          xml2::as_list()

        # Delete the temporary XML file
        file.remove(temp_file)

        message("Getting forecast data - Station code: ", station_code[i])

        fx <- lapply(1:length(resp_data$Envelope$Body$getForecastsResponse), function(z) {

          rows <- resp_data$Envelope$Body$getForecastsResponse[[z]]

          row_lst <- lapply(rows, unlist)

          final <- data.frame(
            exceedance_prob = unlist(row_lst[names(row_lst) == "exceedenceProbabilities"]),
            exceedance_vals = as.numeric(unlist(purrr::map(row_lst[names(row_lst) == "exceedenceValues"], ~if (is.null(.x)) NA else .x)))
          )

          if (all(is.na(final$exceedance_vals))) {

            NULL

          } else {

            final <-
              final %>%
              dplyr::tibble() %>%
              dplyr::mutate(
                element_cd      = row_lst$elementCd,
                station_triplet = row_lst$stationTriplet,
                date            = row_lst$calculationDate,
                pub_date        = row_lst$publicationDate
              ) %>%
              tidyr::separate(station_triplet, into = c("usgs_id", "state", "network"), sep = ":",
                              remove = FALSE) %>%
              dplyr::select(date, pub_date, usgs_id, station_triplet,
                            element_cd, exceedance_prob, exceedance_vals)

            final
          }

        }) %>%
          dplyr::bind_rows()

        fx

      } else {

        message("Error occurred for station code: ", station_code[i], ". Status code: ", r$status_code)

        NULL

      }
      # }, error = function(e) {
      #   message("Error occurred for station code: ", station_code[i])
      #
      #   NULL
      #
      # })

    }) %>%
      dplyr::bind_rows()

  # result_df <- do.call(rbind, res)

  return(res)

}
#
# get_forecast_data <- function(station_codes, forecast_period, element_cd, network, state) {
#   result_list <- lapply(station_codes, function(station_code) {
#     # Create SOAP request body
#     body <- make_soap_body(forecast_period, station_code, element_cd, network, state)
#
#     # Save body as temporary XML file
#     temp_file <- tempfile(fileext = ".xml")
#     xml2::write_xml(body, temp_file)
#
#     # NRCS SOAP API Client
#     NRCS_URL <- 'https://wcc.sc.egov.usda.gov/awdbWebService/services?WSDL'
#
#     # Make POST request with XML body
#     r <- httr::POST(NRCS_URL, body = httr::upload_file(temp_file))
#
#     # Process the response
#     resp_data <- r %>% httr::content() %>% xml2::as_list()
#
#     # Delete the temporary XML file
#     file.remove(temp_file)
#
#     message("Getting forecast data - Station code: ", station_code)
#
#     fx <- lapply(1:length(resp_data$Envelope$Body$getForecastsResponse), function(i) {
#       rows <- resp_data$Envelope$Body$getForecastsResponse[[i]]
#       row_lst <- lapply(rows, unlist)
#
#       final <- data.frame(
#         exceedance_prob = unlist(row_lst[names(row_lst) == "exceedenceProbabilities"]),
#         exceedance_vals = unlist(purrr::map(row_lst[names(row_lst) == "exceedenceValues"], ~ if (is.null(.x)) NA else .x))
#       )
#
#       if (all(is.na(final$exceedance_vals))) {
#         NULL
#       } else {
#         final <- final %>%
#           dplyr::tibble() %>%
#           dplyr::mutate(
#             element_cd = row_lst$elementCd,
#             station_triplet = row_lst$stationTriplet,
#             date = row_lst$calculationDate,
#             pub_date = row_lst$publicationDate
#           ) %>%
#           tidyr::separate(station_triplet, into = c("usgs_id", "state", "network"), sep = ":", remove = FALSE) %>%
#           dplyr::select(date, pub_date, usgs_id, station_triplet, element_cd, exceedance_prob, exceedance_vals)
#         final
#       }
#     }) %>%
#       dplyr::bind_rows()
#
#     return(fx)
#   })
#
#   result_df <- do.call(rbind, result_list)
#   return(result_df)
# }

batch_get_forecasts <- function(station_df,
                                id_col = "usgs_id",
                                forecast_period,
                                element_cd,
                                network,
                                state) {
  # station_df <- sites_df
  # station_df <- ref_tbl[2, ]

    forecast_df <- lapply(1:nrow(station_df), function(k) {
      # k = 1

      # parse/clean USGS IDs
      ids <- stringr::str_replace_all(
        tidyr::separate_rows(station_df[k, ], any_of(id_col), sep = ", ")[[id_col]],
        "[^[:alnum:]]", ""
      )


      # state = "CO"
      # network = "USGS"
      # element_cd = "SRVO"
      # forecast_period = "APR-JUL"
      # ids2 = ids[1:3]
      # ids2 <- c(ids2, "sdfhfjs")
      # ids2 = "06700000"

      # get forecast data
      fx <- get_forecast_data(
                  station_code    = ids,
                  forecast_period = forecast_period,
                  element_cd      = element_cd,
                  network         = network,
                  state           = state
                  )

      # clean up output datasets
      fx <-
        fx %>%
        dplyr::mutate(
          basin    = unique(station_df[k, ]$basin),
          district = unique(station_df[k, ]$district)
          ) %>%
        dplyr::relocate(basin, district)

      fx

    }) %>%
      dplyr::bind_rows()

    return(forecast_df)
}

get_snotel_peaks <- function(
    site_df,
    id_col = "site_id"
    # district_path,
    # start_lag = 3,
    # end_lag   = 12
) {
  # site_df <-
  #   snotel_sites
  # %>%
  #   dplyr::filter(district == "02")
  # path to snotel data
  # snotel_path <- "data/all_snotel_co.rds"

  # district shape path
  # district_path <- "data/water_districts_simple.geojson"

  # site_path <- "data/snotel_sites.csv"

  # water districts shape
  # dists   <- sf::read_sf(district_path)

  # read in snotel data
  # site_df <- readr::read_csv(site_path)

  # # read in snotel data
  # site_df <- readr::read_csv(site_path)
  # missing_sites <- readr::read_csv(missing_sites_path)
  # dists2 <- sf::st_join(
  #       dists,
  #       sf::st_as_sf(site_df, coords = c("lon", "lat"), crs = 4326)
  #     ) %>%
  #     dplyr::filter(!is.na(site_id)) %>%
  #     sf::st_drop_geometry() %>%
  #     dplyr::rename(district = DISTRICT) %>%
  #     dplyr::group_by(district) %>%
  #     dplyr::summarise(
  #       site_id = paste(site_id, collapse = ", ")
  #     ) %>%
  #     dplyr::bind_rows(missing_sites) %>%
  #     dplyr::left_join(
  #       dplyr::select(
  #         sf::st_drop_geometry(dists),
  #         basin = BASIN,
  #         district = DISTRICT
  #       ),
  #       by = "district"
  #     ) %>%
  #     dplyr::relocate(basin, district, site_id)
  #   readr::write_csv(dists2, "data/district_snotel_table.csv")
  #   sf::st_write(site_pts2, "snotel_sites_pts.gpkg")
  #   sf::st_write(dists2, "districts_with_snotel_ids.gpkg")
  #   dists2 %>%
  #     dplyr::mutate(
  #       needs_snotel = dplyr::case_when(
  #         is.na(site_id) ~ "YES",
  #         TRUE ~ "NO")) %>%
  #     ggplot2::ggplot() +
  #     ggplot2::geom_sf(ggplot2::aes(fill = needs_snotel)) +
  #     ggplot2::geom_sf(data = site_pts2)
  # site_df %>%
  #   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  #   mapview::mapview() + dists

  # # Convert the sequence to a data frame
  # date_df <-
  #   data.frame(
  #     date = seq(as.Date("1979-12-31"), Sys.Date(), by = "7 days")
  #   ) %>%
  #   dplyr::mutate(
  #     year      = lubridate::year(date),
  #     week_num  = strftime(date, format = "%V"),
  #     year_week = paste0(year, "_", week_num)
  #   )

  snotel_df <- lapply(1:nrow(site_df), function(i) {

    logger::log_info("Getting district {unique(site_df[i, ]$district)} snotel data...")

    # unique(site_df[i, ]$basin)
    # ids <- tidyr::separate_rows(site_df[i, ], site_id, sep = ", ")$site_id
    # unique(site_df[i, ]$basin)
    # ids <- tidyr::separate_rows(site_df[i, ], any_of(id_col), sep = ", ")[[id_col]]

    ids <- stringr::str_replace_all(
      tidyr::separate_rows(site_df[i, ], any_of(id_col), sep = ", ")[[id_col]],
      "[^[:alnum:]]", ""
    )

    snotel <- go_get_snotel_data(site_ids = ids) %>%
                  dplyr::mutate(
                    basin    = unique(site_df[i, ]$basin),
                    district = unique(site_df[i, ]$district)
                  )

    snotel

  })

  # average SWE across each districts snotel sites and dates
  snotel_df <-
    snotel_df %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(date, district, basin) %>%
    dplyr::summarise(
      swe = mean(snow_water_equivalent, na.rm =T)
    ) %>%
    dplyr::ungroup()

  # snotel_df2 %>%

  # impute missing (NA/NaN) values with mean across the basin for each date
  # a handful of NAs persisted through this and so for that small amount,
  # so we impute the mean across the whole state, the idea being
  # that its such a small number of days so,
  # we take a snapshot of general SWE conditions across the whole state use that to replace missing values
  snotel_df <-
    snotel_df %>%
    dplyr::group_by(date, basin) %>%
    dplyr::mutate(
      swe  = ifelse(is.na(swe) |is.nan(swe), mean(swe, na.rm = TRUE), swe)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
      swe  = ifelse(is.na(swe) |is.nan(swe), mean(swe, na.rm = TRUE), swe)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(datetime = date)

  # yearly peak swe
  yearly_peak <-
    snotel_df %>%
    dplyr::mutate(
      year = as.character(lubridate::year(datetime))
    ) %>%
    dplyr::group_by(basin, district, year) %>%
    dplyr::summarize(
      peak_swe = max(swe, na.rm = T)
    ) %>%
    dplyr::ungroup()

  may_peak <-
    snotel_df %>%
    dplyr::mutate(
      year      = as.character(lubridate::year(datetime)),
      month_day = paste0(lubridate::month(datetime, label = T), "_", lubridate::day(datetime))
    ) %>%
    dplyr::group_by(district, year) %>%
    dplyr::filter(month_day == "May_1") %>%
    dplyr::ungroup() %>%
    dplyr::select(basin, district, year, may_swe = swe)

  final_swe <-
    dplyr::left_join(
      yearly_peak,
      may_peak,
      by = c("basin", "district", "year")
      )

  return(final_swe)


  # # join daily district SWE values w/ weekly date dataframe and calc avg swe per week per district
  # snotel_df <-
  #   snotel_df %>%
  #   dplyr::mutate(
  #     # year     = lubridate::year(datetime),
  #     # week_num = strftime(datetime, format = "%V"),
  #     year_week = paste0(lubridate::year(datetime), "_",  strftime(datetime, format = "%V"))
  #   ) %>%
  #   dplyr::filter(year_week %in% unique(date_df$year_week)) %>%
  #   dplyr::left_join(
  #     date_df,
  #     relationship = "many-to-many",
  #     by           = c("year_week")
  #     # by           = c("year", "week_num")
  #   ) %>%
  #   dplyr::group_by(basin, district, date) %>%
  #   dplyr::summarise(
  #     swe = mean(swe, na.rm =T)
  #   ) %>%
  #   dplyr::ungroup()
  # # dplyr::filter(!is.na(date))
  #
  # # remove NAs in date column
  # snotel_df <- dplyr::filter(snotel_df, !is.na(date))
  #
  # # get complete date range to fill out missing dates for some snotel sites
  # date_range <-
  #   snotel_df %>%
  #   # dplyr::filter(basin == "South Platte") %>%
  #   dplyr::group_by(basin) %>%
  #   dplyr::add_count() %>%
  #   dplyr::ungroup() %>%
  #   dplyr::slice_max(n) %>%
  #   dplyr::select(date) %>%
  #   dplyr::distinct()
  #
  # expanded_df  <-
  #   snotel_df %>%
  #   tidyr::complete(district, date = date_range$date) %>%
  #   dplyr::left_join(
  #     dplyr::distinct(dplyr::select(snotel_df, basin2 = basin, district)),
  #     by = "district"
  #   ) %>%
  #   dplyr::mutate(basin = ifelse(is.na(basin), basin2, basin)) %>%
  #   dplyr::select(-basin2) %>%
  #   dplyr::group_by(basin, date) %>%
  #   dplyr::mutate(swe = ifelse(is.na(swe), mean(swe, na.rm = TRUE), swe)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(!is.na(swe))

  # # add lagged SWE value by basin, default lag is 3-12 month lags
  # snotel_df2 <-
  #   snotel_df2 %>%
  #   dplyr::group_by(basin, district) %>%
  #   timetk::tk_augment_lags(swe, .lags = seq(start_lag*4, end_lag*4, 4)) %>%
  #   stats::setNames(
  #     c("basin", "district", "date", "swe",
  #       paste0("swe_lag_", seq(start_lag, end_lag), "_month"))
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   na.omit()

  # # add monthly by year and basin
  # snotel_df <-
  #   snotel_df %>%
  #   dplyr::mutate(
  #     year  = lubridate::year(date)
  #   ) %>%
  #   dplyr::left_join(
  #     na.omit(
  #       tidyr::pivot_wider(
  #         na.omit(
  #           dplyr::mutate(
  #             dplyr::ungroup(
  #               dplyr::summarise(
  #                 dplyr::group_by(
  #                   dplyr::mutate(snotel_df,
  #                                 month = lubridate::month(date, label = T),
  #                                 year  = lubridate::year(date)
  #                   ),
  #                   basin, month, year
  #                 ),
  #                 swe = max(swe, na.rm = T)
  #               )
  #             ),
  #             month = tolower(month)
  #           )
  #         ),
  #         names_from  = "month",
  #         names_glue  = "{month}_{.value}",
  #         values_from = "swe"
  #       )
  #     ),
  #     by = c("basin", "year")
  #   ) %>%
  #   dplyr::select(-year) %>%
  #   dplyr::ungroup()

  # # calculate March, April, May peak SWE values
  # peaks <-
  #   expanded_df %>%
  #   dplyr::mutate(
  #     month = lubridate::month(date, label = T),
  #     year  = lubridate::year(date)
  #   ) %>%
  #   dplyr::group_by(basin, district, month, year) %>%
  #   dplyr::summarise(
  #     peak_swe = round(max(swe, na.rm = T), 4)
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(month %in% c("Mar", "Apr", "May")) %>%
  #   na.omit() %>%
  #   dplyr::group_by(basin, district, year) %>%
  #   tidyr::pivot_wider(
  #     id_cols     = c(basin, district, year),
  #     names_from  = month,
  #     values_from = peak_swe
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   na.omit()
  #
  #
  # # cleanup names
  # names(peaks) <- c("basin", "district", "year",
  #                   c(paste0(tolower(names(peaks))[!grepl("basin|district|year", tolower(names(peaks)))], "_swe")))
  #
  # # add Peak March, April, May SWE
  # expanded_df <-
  #   expanded_df %>%
  #   dplyr::mutate(
  #     year  = lubridate::year(date)
  #   ) %>%
  #   dplyr::left_join(
  #     dplyr::select(peaks, -basin),
  #     by = c("district", "year")
  #   ) %>%
  #   dplyr::relocate(basin, district, date, swe, mar_swe, apr_swe, may_swe) %>%
  #   dplyr::select(-year) %>%
  #   dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 3))) %>%
  #   dplyr::mutate(
  #     district = ifelse(district < 10, paste0("0", district), district)
  #   )

  # expanded_df %>%
  #   dplyr::select(-swe) %>%
  #   tidyr::pivot_longer(cols =contains("swe")) %>%
  #   # tidyr::pivot_longer(cols =("swe")) %>%
  #   dplyr::filter(basin == "South Platte") %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_line(ggplot2::aes(x = date, y = value, color = name), size = 1) +
  #   ggplot2::facet_wrap(~district)

  # return(expanded_df)

}
# impute missing values w/ mean
impute_mean <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}

process_forecasts <- function(pts_path, nrcs_path) {

  # read in water rights points
  wr_pts <- readRDS(pts_path)

  # read in NRCS forecasts data
  nrcs   <- readr::read_csv(nrcs_path)

  # remove missing dates, select relevant columns, and 50/90% Exceedance probability.
  nrcs <-
    nrcs %>%
    dplyr::filter(!is.na(calculationDate)) %>%
    dplyr::select(
      station_code,
      pub_date   = publicationDate,
      date       = calculationDate,
      ep         = exceedenceProbabilities,
      exceed_val = exceedenceValues,
      longitude,
      latitude
    ) %>%
    dplyr::group_by(station_code, date) %>%
    dplyr::filter(ep %in% c(50, 90)) %>%
    tidyr::pivot_wider(
      id_cols     = c(station_code, date, longitude,latitude ),
      names_from  = ep,
      values_from = exceed_val
    ) %>%
    stats::setNames(c("station_code", "date", "longitude", "latitude", "ep_50", "ep_90")) %>%
    dplyr::mutate(
      ep_90 = dplyr::case_when(
        is.na(ep_90) ~ ep_50,
        TRUE         ~ ep_90
      )
    ) %>%
    dplyr::ungroup()

  # extract unique NRCS forecast points
  nrcs_pts <-
    nrcs %>%
    dplyr::group_by(station_code) %>%
    dplyr::slice(1) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      near_id = 1:n()
    )

  # nearest forecast points to each water right point
  near_pts <- sf::st_nearest_feature(wr_pts, nrcs_pts)

  # add forecast point indexes as column
  wr_pts$near_id <- near_pts

  # join Water rights points with NRCS points
  fx_join <-
    wr_pts %>%
    dplyr::left_join(
      dplyr::select(sf::st_drop_geometry(nrcs_pts), station_code, near_id),
      by = c("near_id")
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(district, wdid, gnis_id, approp_date = appropriation_date, station_code)

  # join forecasts data with WDID/GNIS ID/ DISTRICT data
  wdid_forecasts <-
    nrcs %>%
    dplyr::left_join(
      fx_join,
      relationship = "many-to-many",
      by           = "station_code"
    ) %>%
    dplyr::mutate(
      date = as.Date(date)
    ) %>%
    dplyr::select(district, wdid, gnis_id, approp_date, station_code, date, ep_50, ep_90)

  return(wdid_forecasts)

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
    api_key = NULL
) {

    call_df <- lapply(1:nrow(wdid_df), function(i) {

      message(paste0(i, "/", nrow(wdid_df)))

      # GET request to CDSS API
      tryCatch({
        calls <- cdssr::get_call_analysis_wdid(
          wdid       = wdid_df$wdid[i],
          # admin_no   = wdid_df$admin_number[i],
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
            wdid_admin_no       = wdid_df$admin_number[i],
            seniority           = wdid_df$seniority[i]
          )
        calls
      }, error = function(e) {

        NULL
      })

    }) %>%
      dplyr::bind_rows()

  return(call_df)

}
# Define function to get call data
get_call_data2 <- function(
    wdid_df,
    start_date,
    end_date,
    api_key = NULL
) {

  call_df <- lapply(1:nrow(wdid_df), function(i) {

    message(paste0(i, "/", nrow(wdid_df)))

    # GET request to CDSS API
    tryCatch({
      calls <- cdssr::get_call_analysis_wdid(
        wdid       = wdid_df$wdid[i],
        # admin_no   = wdid_df$admin_number[i],
        admin_no   = "99999.00000",
        start_date = start_date,
        end_date   = end_date,
        api_key    = api_key
      ) %>%
        dplyr::mutate(
          district            = wdid_df$district[i],
          wdid_gnis_id        = wdid_df$gnis_id[i],
          water_source        = wdid_df$water_source[i],
          wdid_approp_date    = wdid_df$appropriation_date[i],
          # wdid_structure_name = wdid_df$structure_name[i],
          # wdid_structure_type = wdid_df$structure_type[i],
          wdid_admin_no       = wdid_df$admin_number[i]
        )
      calls
    }, error = function(e) {

      NULL
    })

  }) %>%
    dplyr::bind_rows()

  return(call_df)

}

# # Define function to get call data
# get_call_data <- function(
#     wdid_df,
#     start_date,
#     end_date,
#     api_key = NULL,
#     save_path
#     ) {
#
#   # wdid_df    = wr_gnis
#   # start_date = start_date
#   # end_date   = end_date
#   # api_key    = api_key
#   # save_path  = save_path
#
#   if(file.exists(save_path)) {
#
#     message(paste0("Reading data from ---> ", save_path))
#
#     call_df <- readRDS(save_path)
#
#   } else {
#
#     # wdid_df <-
#     #   wdid_df %>%
#     #   dplyr::filter(district == "05")
#     # call_df2 <- lapply(1:nrow(wdid_df), function(i) {
#     #
#     #   message(paste0(i, "/", nrow(wdid_df)))
#     #
#     #   # GET request to CDSS API
#     #   tryCatch({
#     #     calls <- cdssr::get_call_analysis_wdid(
#     #       wdid       = wdid_df$wdid[i],
#     #       # admin_no   = wdid_df$admin_number[i],
#     #       admin_no   = "99999.00000",
#     #       start_date = "2001-01-01",
#     #       end_date   = "2004-01-01",
#     #       # start_date = start_date,
#     #       # end_date   = end_date,
#     #       api_key    = api_key
#     #     ) %>%
#     #       dplyr::mutate(
#     #         district            = wdid_df$district[i],
#     #         wdid_gnis_id        = wdid_df$gnis_id[i],
#     #         wdid_approp_date    = wdid_df$appropriation_date[i],
#     #         wdid_structure_name = wdid_df$structure_name[i],
#     #         wdid_structure_type = wdid_df$structure_type[i],
#     #         wdid_admin_no       = wdid_df$admin_number[i],
#     #         seniority           = wdid_df$seniority[i]
#     #       )
#     #     calls
#     #   }, error = function(e) {
#     #
#     #     NULL
#     #   })
#     #
#     # }) %>%
#     #   dplyr::bind_rows()
#
#     # call_df2 %>%
#     # ggplot2::ggplot() +
#     #   ggplot2::geom_line(ggplot2::aes(x = datetime, y = analysis_out_of_priority_percent_of_day)) +
#     #   ggplot2::facet_wrap(seniority~analysis_wdid, nrow = 6)
#     #
#     # call_df2 %>%
#     #   aggreg_calls() %>%
#     #   ggplot2::ggplot() +
#     #   ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct)) +
#     #   ggplot2::facet_wrap(seniority~wdid, nrow = 6)
#
#     call_df <- lapply(1:nrow(wdid_df), function(i) {
#
#       message(paste0(i, "/", nrow(wdid_df)))
#
#       # GET request to CDSS API
#       tryCatch({
#         calls <- cdssr::get_call_analysis_wdid(
#           wdid       = wdid_df$wdid[i],
#           # admin_no   = "99999.00000",
#           admin_no   = wdid_df$admin_number[i],
#           start_date = start_date,
#           end_date   = end_date,
#           api_key    = api_key
#         ) %>%
#           dplyr::mutate(
#             district            = wdid_df$district[i],
#             wdid_gnis_id        = wdid_df$gnis_id[i],
#             wdid_approp_date    = wdid_df$appropriation_date[i],
#             wdid_structure_name = wdid_df$structure_name[i],
#             wdid_structure_type = wdid_df$structure_type[i],
#             wdid_admin_no       = wdid_df$admin_number[i],
#             seniority           = wdid_df$seniority[i]
#           )
#         calls
#       }, error = function(e) {
#
#         NULL
#       })
#
#     }) %>%
#       dplyr::bind_rows()
#
#     message(paste0("Saving data to path ---> ", save_path))
#
#     saveRDS(call_df, save_path)
#
#   }
#
#   return(call_df)
# }

rectify_out_pct <- function(df) {

  # df <-
  #   df %>%
  #   dplyr::mutate(
  #     priority_date = dplyr::case_when(
  #       is.na(priority_date) ~ "2099-01-01",
  #       TRUE                 ~ priority_date
  #     ),
  #     analysis_out_of_priority_percent_of_day = dplyr::case_when(
  #       wdid_approp_date <= priority_date   ~ 0,
  #       TRUE                                ~ analysis_out_of_priority_percent_of_day
  #     )
  #   )
  # df2 <- dplyr::filter(daily_calls, analysis_wdid == "0100722")
  df <-
    df %>%
    dplyr::mutate(
      priority_date = dplyr::case_when(
        is.na(priority_date) ~ "2099-01-01",
        TRUE                 ~ priority_date
      ),
      analysis_out_of_priority_percent_of_day = dplyr::case_when(
        wdid_approp_date <= priority_date   ~ 0,
        TRUE                                ~ analysis_out_of_priority_percent_of_day
      )
    )
    # dplyr::group_by(datetime, analysis_wdid) %>%
    # dplyr::slice_min(analysis_out_of_priority_percent_of_day) %>%
    # # dplyr::mutate(
    # #   analysis_out_of_priority_percent_of_day = sum(analysis_out_of_priority_percent_of_day)
    # # ) %>%
    # # dplyr::slice(1) %>%
    # dplyr::ungroup()

  return(df)
}

# aggregate the output dataframe from get_call_data from daily timesteps to a weekly timestep to align with average weekly climate data
aggreg_calls2 <- function(df, rectify = TRUE) {

  if(rectify) {

    message(paste0("Rectifying out of priority percent..."))

    df <- rectify_out_pct(df)
    # daily_calls <- rectify_out_pct(dplyr::filter(daily_calls, district == "01"))
  }

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

  message(paste0("Calculating weekly out of priority data..."))

  # GNIS ID: 180817
  # WDIDs: "0100773", "0100722" ,"0100827"

  # length(unique(sub_calls$wdid))
  # sub_calls %>%
  #   dplyr::filter(datetime >= "2001-01-01", datetime <= "2004-01-01") %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_line(ggplot2::aes(x = datetime, y = out_pct)) +
  #   ggplot2::facet_wrap(seniority~wdid)
    # ggplot2::facet_grid(wdid~seniority)

  # # extract list of GNIS ID to mask out to only WDIDs of interest
  # wdid_mask <-
  #   sub_calls %>%
  #   dplyr::mutate(
  #     year      = lubridate::year(datetime),
  #     out_count = dplyr::case_when(
  #       out_pct > 0 ~ 1,
  #       TRUE ~ 0
  #     )
  #   ) %>%
  #   dplyr::group_by(year, district, wdid_gnis_id) %>%
  #   dplyr::mutate(
  #     out_pct   = mean(out_pct, na.rm = T),
  #     out_count = sum(out_count, na.rm = T)
  #   ) %>%
  #   dplyr::slice(1) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(district, wdid_gnis_id) %>%
  #   dplyr::summarise(
  #     out_count = mean(out_count, na.rm = T)
  #   ) %>%
  #   dplyr::group_by(district) %>%
  #   dplyr::slice(
  #     which.max(out_count)
  #   ) %>%
  #   dplyr::ungroup()

  # filter down to GNIS IDs of interest
  # sub_calls <- dplyr::filter(sub_calls, wdid_gnis_id %in% unique(wdid_mask$wdid_gnis_id))

  # get the max and min priorities by GNIS ID
  priorities <-
    sub_calls %>%
    dplyr::mutate(
      approp_date = as.Date(approp_date)
    ) %>%
    dplyr::group_by(district, wdid_gnis_id) %>%
    dplyr::slice(
      which.max(approp_date),
      which.min(approp_date)
    ) %>%
    dplyr::ungroup()

  # filter calls down to most senior and most junior on most out of priority stretch of river (GNIS ID)
  sub_calls <- dplyr::filter(sub_calls, wdid %in% unique(priorities$wdid))

  # Final weekly weekly data aggregation
  # Output variables:
    # out of percent priority weekly average
    # Binary indicator "out" in or out of priority week
    # count of days out of priority that week
    # count of days out of priority that month
  sub_calls <-
    sub_calls %>%
    dplyr::mutate(
      year     = lubridate::year(datetime),
      week_num = strftime(datetime, format = "%V")
      ) %>%
    dplyr::left_join(
      date_df,
      by = c("year", "week_num")
      ) %>%
    dplyr::group_by(district, date, wdid, wdid_gnis_id, seniority, approp_date  ) %>%
    dplyr::mutate(
      out_count = dplyr::case_when(
        out_pct > 0 ~ 1,
        TRUE ~ 0
      ),
      mon_year  = paste0(lubridate::month(datetime), "_", lubridate::year(datetime))
    ) %>%
    dplyr::group_by(district, mon_year, wdid) %>%
    dplyr::mutate(
      out_count_month = sum(out_count, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-mon_year) %>%
    dplyr::group_by(district, date, wdid, wdid_gnis_id, seniority, approp_date  ) %>%
    dplyr::mutate(
      out_count = sum(out_count, na.rm = T),
      out_pct   = mean(out_pct, na.rm = T)
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      out = ifelse(out_pct > 0, 1, 0)
    ) %>%
    dplyr::select(district,
                  date, wdid,
                  gnis_id     = wdid_gnis_id,
                  approp_date,
                  seniority,
                  out_pct,
                  out,
                  out_count_week = out_count,
                  out_count_month
                  )


  return(sub_calls)

}

# Takes in weekly call data calculations as inputs and calculates monthly data values
aggreg_calls_month <- function(df) {

  message(paste0("Calculating monthly out of priority data..."))

  out_mon <-
    df %>%
    # week_calls %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::mutate(
      year      = lubridate::year(date),
      month     = lubridate::month(date),
      mon_year  = paste0(lubridate::month(date), "_", lubridate::year(date))
    ) %>%
    dplyr::group_by(district,mon_year, wdid) %>%
    dplyr::mutate(
      out_pct = mean(out_pct, na.rm = T)
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      date = as.Date(paste0(year, "-", month, "-01")),
      out_count_month = dplyr::case_when(
        out_count_month > 31 ~ 30,
        TRUE                 ~ out_count_month
        ),
      out = dplyr::case_when(
        out_pct > 0 ~ 1,
        TRUE        ~ 0
        )
      ) %>%
    dplyr::select(-out_count_week, -mon_year,  -month, -year)

  return(out_mon)

}

# aggregate the output dataframe from get_call_data from daily timesteps to a weekly timestep to align with average weekly climate data
aggreg_calls <- function(df, rectify = TRUE) {

  if(rectify) {

    message(paste0("Rectifying out of priority percent"))

    df <- rectify_out_pct(df)
  }

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

  weekly_calls <-
    df %>%
    dplyr::select(datetime, district,
                  wdid = analysis_wdid,
                  wdid_gnis_id,
                  out_pct = analysis_out_of_priority_percent_of_day,
                  wdid_approp_date, wdid_structure_name, wdid_structure_type, seniority
    )  %>%
    # dplyr::filter(wdid_gnis_id %in% c("204805", "1385432")) %>%
    # dplyr::filter(wdid == "1803001") %>%
    dplyr::group_by(district, datetime, wdid, wdid_gnis_id, seniority, wdid_approp_date) %>%
    dplyr::summarise(
      out_pct = mean(out_pct, na.rm = T)
      ) %>%
    dplyr::mutate(
      year     = lubridate::year(datetime),
      week_num = strftime(datetime, format = "%V")
    ) %>%
    dplyr::left_join(
      date_df,
      by = c("year", "week_num")
    ) %>%
    dplyr::group_by(district, date, wdid, wdid_gnis_id, seniority, wdid_approp_date) %>%
    dplyr::mutate(
      out_count = dplyr::case_when(
        out_pct > 0 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    dplyr::summarise(
      out_count = sum(out_count, na.rm = T),
      out_pct = mean(out_pct, na.rm = T)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(district,
                  date, wdid,
                  gnis_id     = wdid_gnis_id,
                  approp_date = wdid_approp_date,
                  seniority,
                  out_pct,
                  out_count
                  )

  return(weekly_calls)

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

make_models <- function(
    df         = NULL,
    target_var = NULL,
    basin_name = NULL,
    model_type = "classification",
    strata     = NULL,
    nfolds     = 10,
    ncores     = 6,
    save_path  = NULL
) {

  # model_type = "regression"
  # df <- df %>% rename(!!new_colname := !!target_var)

  # # i = 2
  # basin_name <- unique(out_lst[[i]]$basin)[1]
  # df         = out_lst[[i]]
  # # # rm(df2)
  # # # df2 <- df %>%
  # # #   dplyr::filter(!duplicated(.))
  # #
  # target_var = "out"
  # model_type = "classification"
  # # # target_var = "out_pct"
  # # # model_type = "regression"
  # basin_name = basin_name
  # strata     = "seniority"
  # nfolds     = 5
  # ncores     = 6
  # save_path  = paste0("D:/cpo/models/")

  # ---- Preprocessing ----

  if(model_type == "classification") {

    df <-
      df %>%
      dplyr::select(-out_pct) %>%
      dplyr::rename( out := !!target_var) %>%
      dplyr::filter(!duplicated(.)) %>%
      dplyr::mutate(
        dplyr::across(where(is.character), as.factor)
      )

  } else {

    df <-
      df %>%
      dplyr::select(-out) %>%
      dplyr::rename( out := !!target_var) %>%
      dplyr::filter(!duplicated(.)) %>%
      dplyr::mutate(
        dplyr::across(where(is.character), as.factor)
      )

  }

  # rename target variable column to "out"
  # df <- dplyr::rename(df, out := !!target_var)

  # ---- Train/Test split ----

  set.seed(234)

  logger::log_info("Splitting data")
  out_split <- rsample::initial_split(df, strata = !!strata)

  logger::log_info("Creating training data")
  # training data split
  out_train <- rsample::training(out_split)

  logger::log_info("Creating testing data")
  # testinng data split
  out_test  <- rsample::testing(out_split)

  # ---- Recipes ----

  logger::log_info("Data preprocessing...")

  # GLMNET Recipe
  glmnet_recipe <-
    recipes::recipe(
      # formula = {{target_var}} ~ .,
      # data    = out_train
      # formula = rlang::new_formula(quote(!!target_var),quote(.)),
      formula = out ~ .,
      data    = out_train
    ) %>%
    recipes::update_role(
      basin, district,
      new_role = "ID"
    ) %>%
    # recipes::step_string2factor(one_of( "seniority")) %>%
    # recipes::step_str
    # themis::step_smote(seniority) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    recipes::step_zv(recipes::all_predictors()) %>%
    recipes::step_normalize(recipes::all_numeric_predictors())


  # bk <- recipes::prep(glmnet_recipe) %>% recipes::bake(new_data = NULL)

  # # # # XGBoost trees
  xgboost_recipe <-
    recipes::recipe(
      formula = out ~ .,
      data    = out_train
    ) %>%
    recipes::update_role(
      basin, district,
      new_role = "ID"
    ) %>%
  # recipes::step_string2factor(one_of("seniority")) %>%
    recipes::step_novel(recipes::all_nominal_predictors()) %>%
    recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE)

  kknn_recipe <-
    recipes::recipe(
      formula = out ~ .,
      data    = out_train
    ) %>%
    recipes::update_role(
      basin, district,
      new_role = "ID"
    )

  if (model_type == "classification") {


    # GLMNET Recipe  w/ smote algo
    glmnet_recipe <-
      glmnet_recipe %>%
      # themis::step_smote(out) %>%
      # themis::step_smote(seniority) %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_normalize(recipes::all_numeric_predictors())

    # XGBoost Recipe  w/ smote algo
    xgboost_recipe <-
      xgboost_recipe %>%
      # themis::step_smote(out) %>%
      # themis::step_smote(seniority) %>%
      recipes::step_zv(recipes::all_predictors())

    # K nearest neighbors recipe w/ smote algo
    kknn_recipe <-
      kknn_recipe %>%
      # themis::step_smote(out) %>%
      recipes::step_zv(all_predictors()) %>%
      recipes::step_normalize(all_numeric_predictors())

    #
  } else {


    # GLMNET recipe w/o smote algo
    glmnet_recipe <-
      glmnet_recipe %>%
      recipes::step_zv(recipes::all_predictors()) %>%
      recipes::step_normalize(recipes::all_numeric_predictors())

    # XGBoost Recipe  w/o smote algo
    xgboost_recipe <-
      xgboost_recipe %>%
      recipes::step_zv(recipes::all_predictors())

    # K nearest neighbors recipe w/o smote algo
    kknn_recipe <-
      kknn_recipe %>%
      recipes::step_zv(all_predictors()) %>%
      recipes::step_normalize(all_numeric_predictors())

  }
  # # XGBoost trees
  # xgboost_recipe <-
  #   recipes::recipe(
  #     formula = out ~ .,
  #     data    = out_train
  #   ) %>%
  #   recipes::update_role(
  #     basin,
  #     new_role = "ID"
  #   ) %>%
  #   recipes::step_string2factor(one_of("seniority")) %>%
  #   recipes::step_novel(recipes::all_nominal_predictors()) %>%
  #   recipes::step_dummy(recipes::all_nominal_predictors(), one_hot = TRUE) %>%
  #   themis::step_smote(out) %>%
  #   recipes::step_zv(recipes::all_predictors())

  # ---- Model specifications ----

  logger::log_info("Creating model specifications...")
  logger::log_info("Model type: ", model_type)

  if (model_type == "classification") {

    # GLMNET model specifications classification
    glmnet_spec <-
      parsnip::logistic_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_engine("glmnet")

    # # xgboost model - classification
    xgboost_spec <-
      parsnip::boost_tree(
        trees          = tune::tune(),
        min_n          = tune::tune(),
        tree_depth     = tune::tune(),
        learn_rate     = tune::tune(),
        loss_reduction = tune::tune(),
        sample_size    = tune::tune()
      ) %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_engine("xgboost", importance = "permutation")

    # KKNN Model - classification
    kknn_spec <-
      parsnip::nearest_neighbor(
        neighbors   = tune::tune(),
        weight_func = tune::tune()
      ) %>%
      parsnip::set_mode("classification") %>%
      parsnip::set_engine("kknn")

  } else if (model_type == "regression") {

    # # # GLMNET model specifications - regression
    glmnet_spec <-
      parsnip::linear_reg(
        penalty = tune::tune(),
        mixture = tune::tune()
      ) %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("glmnet")

    # # # xgboost model - regression
    xgboost_spec <-
      parsnip::boost_tree(
        trees          = tune::tune(),
        min_n          = tune::tune(),
        tree_depth     = tune::tune(),
        learn_rate     = tune::tune(),
        loss_reduction = tune::tune(),
        sample_size    = tune::tune()
      ) %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("xgboost", importance = "permutation")

    # KKNN Model - regression
    kknn_spec <-
      parsnip::nearest_neighbor(
        neighbors   = tune::tune(),
        weight_func = tune::tune()
      ) %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("kknn")
  }

  # ---- Cross Validation folds ----

  logger::log_info("Generating cross validation folds...")

  # Set seed for resampling
  set.seed(432)

  # CV folds
  out_folds <- rsample::vfold_cv(out_train, v = nfolds, strata = !!strata)

  logger::log_info("Making workflowset...")

  # ---- Workflow set of models ----
  out_wfs <-
    workflowsets::workflow_set(
      preproc = list(
        glmnet_rec      = glmnet_recipe,
        xgboost_rec     = xgboost_recipe,
        kknn_rec        = kknn_recipe
      ),
      models  = list(
        glmnet   = glmnet_spec,
        xgboost  = xgboost_spec,
        kknn     = kknn_spec
      ),
      cross = F
    )

  # Choose eval metrics
  if (model_type == "classification") {

    my_metrics <- yardstick::metric_set(roc_auc, accuracy, mn_log_loss)

  } else if (model_type == "regression") {

    my_metrics <- yardstick::metric_set(rsq, rmse)

  }

  logger::log_info("Starting {ncores} parallel workers...")

  # Set up parallelization, using computer's other cores
  parallel::detectCores(logical = FALSE)
  modeltime::parallel_start(ncores, .method = "parallel")


  logger::log_info("Tuning hyperparameters...")

  # Set Random seed
  set.seed(589)

  # Efficient Tuning of models in workflowset
  out_wfs <-
    out_wfs %>%
    workflowsets::workflow_map(
      "tune_race_anova",
      resamples = out_folds,
      # resamples = flow_roll_splits,
      grid      = 20,
      metrics   = my_metrics,
      control = finetune::control_race(
        verbose       = TRUE,
        save_pred     = TRUE,
        verbose_elim  = TRUE,
        save_workflow = TRUE
      ),
      verbose   = TRUE
    )

  # Stop parrallelization
  modeltime::parallel_stop()

  logger::log_info("Tuning complete!")

  # ---- Generate Metrics ----

  results <- extract_results(
    wfs        = out_wfs,
    basin_name = basin_name,
    model_type = model_type,
    train_data = out_train,
    test_data  = out_test,
    split_data = out_split

  )

  # final output list of data
  out <- list(
    "workflowset" = out_wfs,
    "results"     = results
  )

  # Model outputs path
  out_dir  <- paste0(save_path, "outputs/")
  out_path <- paste0(
    out_dir,
    tolower(gsub("[[:punct:][:blank:]]+", "_",  basin_name)),
    ifelse(model_type == "classification", "_class_", "_reg_"),
    "models.rds"
  )

  # checking if workflowset/ directory exists, and if not, creates one
  if(!dir.exists(out_dir)) {
    logger::log_info("Creating outputs directory:\n--> {out_dir}")

    dir.create(out_dir)

    logger::log_info("Saving {basin_name} {model_type} models:\n--> {out_path}")

    # Save Workflows/Resample results/Final fitted model
    saveRDS(
      out,
      out_path
    )

  } else {

    logger::log_info("Saving {basin_name} {model_type} models:\n--> {out_path}")

    # Save Workflows/Resample results/Final fitted model
    saveRDS(
      out,
      out_path
    )

  }

  return(out)

}

# # Make model comparison plots from workflowsets object
# make_comp_plot <- function(
#     wfs,
#     model_name = "ID",
#     model_type = "classification",
#     save_path = NULL
# ) {
#
#   # model_type = "classification"
#   # model_name = unique(out_lst[[i]]$basin)[1]
#   # wfs <- class_wfs
#   # ifelse(model_type == "classification", "class", "reg")
#   # # Plot path
#   # save_path <-   "D:/cpo/models/plots/"
#   # paste0(
#   #   save_path,
#   #   tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
#   #   ifelse(model_type == "classification", "_class_", "_reg_"),
#   #   "model_rank.png"
#   # )
#
#   # model_name = basin_name
#   # wfs        = reg_mods$workflowset
#   # model_name = basin_name
#   # model_type = "regression"
#   # save_path  = paste0("D:/cpo/models/")
#
#   # Comparing rmse rsq AND mae OF ALL MODELS
#   mod_comp_plot <-
#     wfs %>%
#     autoplot() +
#     ggplot2::geom_point(ggplot2::aes(color = wflow_id)) +
#     ggplot2::labs(
#       color = "Data Preprocessor",
#       title    = paste0(stringr::str_to_title(model_name), " Model Comparisons"),
#       subtitle = paste0(stringr::str_to_title(model_type), " models")
#     ) +
#     ggplot2::theme_bw()
#   # theme(legend.position = "none")
#
#   # reg_mod_comp_plot
#   logger::log_info('Saving {model_name} {model_type} model ranking plot\n{paste0(
#                       save_path,
#                       tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
#                       ifelse(model_type == "classification", "_class_", "_reg_"),
#                       "model_rank.png"
#                     )}')
#
#   # Model outputs path
#   out_dir  <- paste0(save_path, "plots/")
#   out_path <- paste0(
#     out_dir,
#     tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
#     ifelse(model_type == "classification", "_class_", "_reg_"),
#     "model_rank.png"
#   )
#
#   # checking if workflowset/ directory exists, and if not, creates one
#   if(!dir.exists(out_dir)) {
#     logger::log_info("Creating plot directory:\n--> {out_dir}")
#
#     dir.create(out_dir)
#
#     logger::log_info("Saving {basin_name} {model_type} models comparison plot:\n--> {out_path}")
#
#     # Save plot
#     ggplot2::ggsave(
#       out_path,
#       plot   = mod_comp_plot,
#       width  = 52,
#       height = 28,
#       units  = "cm"
#     )
#
#   } else {
#
#     logger::log_info("Saving {basin_name} {model_type} models comparison plot:\n--> {out_path}")
#
#     # Save plot
#     ggplot2::ggsave(
#       out_path,
#       plot   = mod_comp_plot,
#       width  = 52,
#       height = 28,
#       units  = "cm"
#     )
#
#   }
#
#   return(mod_comp_plot)
# }


extract_results <- function(
    wfs,
    basin_name,
    model_type,
    train_data,
    test_data,
    split_data,
    save_path
) {

  # train_data = out_train
  # test_data = out_test
  # split_data = out_split
  # wfs        = out_wfs
  # basin_name = basin_name
  # model_type = model_type
  # wfs        = out_wfs
  # basin_name = basin_name
  # model_type = model_type
  # train_data = out_train
  # test_data  = out_test
  # split_data = out_split

  # Table of model ranks
  mod_rank <- workflowsets::rank_results(wfs)

  # iterate through models in workflowsets and extract metrics and make predictions
  fit_mods <- lapply(1:length(wfs$wflow_id), function(z) {

    message("Summarizing model ", z, "/", length(wfs$wflow_id))

    # recipe name
    mod_recipe     <- wfs$wflow_id[z]

    # relative model rankings
    min_rank <- min(dplyr::filter(mod_rank, wflow_id == mod_recipe)$rank)
    max_rank <- max(dplyr::filter(mod_rank, wflow_id == mod_recipe)$rank)

    # make clean names
    clean_rec_name <- paste0(mod_recipe, "_", ifelse(model_type == "classification", "class", "reg"))
    simple_name    <- sub(".+_", "", mod_recipe)

    # extract workflow set results for recipe
    mod_results <- workflowsets::extract_workflow_set_result(wfs, mod_recipe)

    # Extract workflows
    mod_workflow <- extract_workflow(wfs, mod_recipe)

    logger::log_info("Fitting final model...\nPreprocessor: {mod_recipe}")

    met <- ifelse(model_type == "classification", "roc_auc", "rsq")

    # Finalize workflow fit
    mod_workflow_fit <-
      mod_workflow %>%
      tune::finalize_workflow(tune::select_best(mod_results, metric = met)) %>%
      fit(data = train_data)

    # Fit model to split train/test data
    mod_last_fit <- tune::last_fit(mod_workflow_fit, split_data)

    # Extract & save final fit to use for predictions
    mod_final_fit <- mod_last_fit$.workflow[[1]]

    if(model_type == "classification") {

      logger::log_info("Collecting training data predictions...")

      # training set predictions
      mod_train <-
        predict(mod_final_fit, train_data) %>%
        bind_cols(predict(mod_final_fit, train_data, type = "prob")) %>%
        dplyr::bind_cols(dplyr::select(train_data, out)) # Add the true outcome data back in

      logger::log_info("Collecting testing data predictions...")

      # testing set predictions
      mod_test <-
        predict(mod_final_fit, test_data) %>%
        bind_cols(predict(mod_final_fit, test_data, type = "prob")) %>%
        dplyr::bind_cols(dplyr::select(test_data, out))

    } else {

      logger::log_info("Collecting training data predictions...")

      # training set predictions
      mod_train <-
        predict(mod_final_fit, train_data) %>%
        dplyr::bind_cols(dplyr::select(train_data, out)) # Add the true outcome data back in

      logger::log_info("Collecting testing data predictions...")

      # testing set predictions
      mod_test <-
        predict(mod_final_fit, test_data) %>%
        dplyr::bind_cols(dplyr::select(test_data, out))
    }

    # logger::log_info("Collecting testing data predictions...")
    #
    # if(model_type == "classification") {
    #
    #   # testing set predictions
    #   mod_test <-
    #     predict(mod_final_fit, test_data) %>%
    #     bind_cols(predict(mod_final_fit, test_data, type = "prob")) %>%
    #     dplyr::bind_cols(dplyr::select(test_data, out))
    #
    # } else {
    #
    #   # testing set predictions
    #   mod_test <-
    #     predict(mod_final_fit, test_data) %>%
    #     dplyr::bind_cols(dplyr::select(test_data, out))
    # }

    if(model_type == "classification") {

      multi_metric <- yardstick::metric_set(roc_auc, pr_auc, accuracy)

    } else if(model_type == "regression") {

      multi_metric <- yardstick::metric_set(rmse, rsq, mae)
    }

    # est = ifelse(model_type == "classification", ".pred_class", ".pred")

    # Plot variable importance if avaliable for model
    vip_table <- tryCatch(
      {
        logger::log_info("Collecting variable importance...")

        # Variable importance dataframe
        # vip_table <-
        mod_last_fit %>%
          purrr::pluck(".workflow", 1) %>%
          extract_fit_parsnip() %>%
          vip::vi() %>%
          dplyr::mutate(
            model_type     = model_type,
            recipe         = clean_rec_name,
            min_model_rank = min_rank,
            max_model_rank = max_rank,
            basin          = basin_name
          )
      },
      error = function(e) {
        logger::log_error('Variable Importance is not avalaible for {simple_name} - {model_type}')
        logger::log_error('Setting vip_table to NULL')
        logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
        logger::log_error(message(e))

        NULL
        # vip_table <- NULL

      }
    )

    # # Train metrics
    # train_metrics <-
    #   mod_train %>%
    #   multi_metric(truth = out, estimate = .pred) %>%
    #   mutate(
    #     data           = "train",
    #     model          = mod_recipe,
    #     recipe         = clean_rec_name,
    #     min_model_rank = min_rank,
    #     max_model_rank = max_rank,
    #     basin          =  tolower(gsub("[[:punct:][:blank:]]+", "_",  basin_name))
    #   )

    # # Test metrics
    # test_metrics <-
    #   mod_test %>%
    #   multi_metric(truth = out, estimate = .pred) %>%
    #   mutate(
    #     data           = "test",
    #     model          = mod_recipe,
    #     recipe         = clean_rec_name,
    #     min_model_rank = min_rank,
    #     max_model_rank = max_rank,
    #     basin          =  tolower(gsub("[[:punct:][:blank:]]+", "_",  basin_name))
    #   )

    if(model_type == "classification") {

      logger::log_info("Creating confusion matrix...")

      cm <- yardstick::conf_mat(collect_predictions(mod_results), out, .pred_class)

    } else {

      cm <- NULL

    }

    logger::log_info("Extracting {model_type} metrics...")

    if(model_type == "classification") {

      metrics_df <- calc_class_metrics(
        final_fit    = mod_final_fit,
        fitted_train = mod_train,
        fitted_test  = mod_test,
        model_name   = clean_rec_name,
        basin_name   = basin_name
      )

    } else {

      metrics_df <- calc_reg_metrics(
        final_fit    = mod_final_fit,
        fitted_train = mod_train,
        fitted_test  = mod_test,
        model_name   = clean_rec_name,
        basin_name   = basin_name
      )

    }

    logger::log_info("Tidying model metrics... ")

    # final data outputs
    final_lst = list(
      "training"            = mod_train,
      "testing"             = mod_test,
      "metrics"             = metrics_df,
      "variable_importance" = vip_table,
      "confusion_matrix"    = cm,
      "model_results"       = mod_results,
      "last_fit"            = mod_last_fit,
      "final_fit"           = mod_final_fit
    )

    final_lst

  }) %>%
    stats::setNames(c(
      paste0(sub(".+_", "", wfs$wflow_id), "_", ifelse(model_type == "classification", "class", "reg"))
    )
    )

  return(fit_mods)

}

calc_class_metrics <- function(final_fit, fitted_train, fitted_test, model_name, basin_name) {

  # # training set predictions
  # mod_train <-
  #   predict(final_fit, train_data) %>%
  #   bind_cols(predict(final_fit, train_data, type = "prob")) %>%
  #   bind_cols(dplyr::select(train_data, out)) # Add the true outcome data back in
  #
  # # testing set predictions
  # mod_test <-
  #   predict(final_fit, test_data) %>%
  #   bind_cols(predict(final_fit, test_data, type = "prob")) %>%
  #   bind_cols(dplyr::select(test_data, out))

  # Train metrics
  train_metrics <-
    fitted_train %>%
    yardstick::metrics(truth = out, estimate = .pred_class, .pred_1) %>%
    dplyr::mutate(
      data   = "train",
      model  = model_name,
      basin  = basin_name
    )

  # Test metrics
  test_metrics <-
    fitted_test %>%
    yardstick::metrics(truth = out, estimate = .pred_class, .pred_1) %>%
    dplyr::mutate(
      data   = "test",
      model  = model_name,
      basin  = basin_name
    )

  sens_train <-
    fitted_train %>%
    yardstick::sens(truth = out, estimate = .pred_class) %>%
    dplyr::mutate(
      data   = "train",
      model  = model_name,
      basin  = basin_name
    )

  sens_test <-
    fitted_test %>%
    yardstick::sens(truth = out, estimate = .pred_class) %>%
    dplyr::mutate(
      data   = "test",
      model  = model_name,
      basin  = basin_name
    )

  spec_train <-
    fitted_train %>%
    yardstick::spec(truth = out, estimate = .pred_class)  %>%
    dplyr::mutate(
      data   = "train",
      model  = model_name,
      basin  = basin_name
    )

  spec_test <-
    fitted_test %>%
    yardstick::spec(truth = out, estimate = .pred_class) %>%
    dplyr::mutate(
      data   = "test",
      model  = model_name,
      basin  = basin_name
    )

  # Model train/test metrics
  mod_metrics <- dplyr::bind_rows(
    train_metrics, sens_train, spec_train,
    test_metrics, sens_test, spec_test
  ) %>%
    dplyr::relocate(basin)

  return(mod_metrics)
}

calc_reg_metrics <- function(final_fit, fitted_train, fitted_test, model_name, basin_name) {
  # fitted_train = mod_train
  # fitted_test = mod_test
  # final_fit = mod_final_fit

  # Train metrics
  train_metrics <-
    fitted_train %>%
    yardstick::metrics(truth = out, estimate = .pred) %>%
    dplyr::mutate(
      data   = "train",
      model  = model_name,
      basin  = basin_name
    )

  # Test metrics
  test_metrics <-
    fitted_test %>%
    yardstick::metrics(truth = out, estimate = .pred) %>%
    dplyr::mutate(
      data   = "test",
      model  = model_name,
      basin  = basin_name
    )

  # Model train/test metrics
  mod_metrics <-
    dplyr::bind_rows(
      train_metrics,
      test_metrics
    ) %>%
    dplyr::relocate(basin)

  return(mod_metrics)
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





