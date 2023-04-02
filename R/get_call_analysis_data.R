# Angus Watters
# CDSS call analysis data

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)
library(aws.s3)
# library(purrr)

source("R/utils.R")

# TODO go through and see if all of this stuff is superfluous
# ***********************
# ---- Paths to data ----
# ***********************

# Paths
wr_net_path    <- "data/water_right_netamounts.rds"
wr_trans_path  <- "data/water_right_transactions.rds"
districts_path <- "data/water_districts.rds"

# ***********************************
# ---- get water districts table ----
# ***********************************

if(file.exists(districts_path)) {

  message(paste0("Reading data from:\n---> ", districts_path))

  water_dists <- readRDS(districts_path)

} else {

  message(paste0("Data not found at path:\n---> ", districts_path))

  # water districts reference table
  water_dists <- cdssr::get_reference_tbl("waterdistricts")

  message(paste0("Saving data to path:\n---> ", districts_path))

  # save water districts reference table data
  saveRDS(water_dists, districts_path)

}

# ***************************************************
# ---- Water rights netamounts by water district ----
# ***************************************************

if(file.exists(wr_net_path)) {

  message(paste0("Reading data from:\n---> ", wr_net_path))

  wr_net <- readRDS(wr_net_path)

} else {

  message(paste0("Data not found at path:\n---> ", wr_net_path))

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

  message(paste0("Saving data to path:\n---> ", wr_net_path))

  # save water rights transactions data
  saveRDS(wr_net, wr_net_path)

}

# *****************************************************
# ---- Water rights transactions by water district ----
# *****************************************************

if(file.exists(wr_trans_path)) {

  message(paste0("Reading data from:\n---> ", wr_trans_path))

  wr_trans <- readRDS(wr_trans_path)

} else {

  message(paste0("Data not found at path:\n---> ", wr_trans_path))

  wr_trans <- lapply(1:nrow(water_dists), function(i) {

    message(paste0("District: ", water_dists$water_district[i], " - (", i, "/", nrow(water_dists), ")"))

    # GET request to CDSS API
    tryCatch({

      wr_trans <- cdssr::get_water_rights_trans(
        water_district    = water_dists$water_district[i]
      )
      wr_trans
    },
    error = function(e) {

      NULL

    })

  }) %>%
    dplyr::bind_rows()

  message(paste0("Saving data to path:\n---> ", wr_trans_path))

  # save water rights transactions data
  saveRDS(wr_trans, wr_trans_path)

}

# **********************************
# ---- unique WDIDs by district ----
# **********************************

# remove rights without a stream mile tag
wdid_lst <-
  wr_net %>%
  dplyr::tibble() %>%
  dplyr::filter(!is.na(stream_mile))

# unique WDID/Admin number combinations
wdid_lst <- unique(wdid_lst[,c('division', 'water_district', 'wdid')])
# wdid_lst <- unique(wr_trans[,c('division', 'water_district', 'wdid')])

# counts of WDIDs by district
district_counts <-
  wdid_lst %>%
  dplyr::group_by(division, water_district) %>%
  dplyr::tally() %>%
  dplyr::rename(wdids = n) %>%
  dplyr::arrange(wdids)

# save counts of WDIDs per district
readr::write_csv(district_counts, "data/district_wdid_counts.csv")

# split WDIDs into seperate dataframes for each district
district_split <-
  wdid_lst %>%
  dplyr::arrange(water_district) %>%
  dplyr::group_by(water_district) %>%
  dplyr::group_split()

# loop through list of each district dataframe and save out district WDIDs
for (i in 1:length(district_split)) {

  dist <- unique(district_split[[i]]$water_district)

  message(paste0("Saving District ", dist, " WDID - (", i, "/", length(district_split), ")"))

  readr::write_csv(
    district_split[[i]],
    paste0("D:/cpo/data/district_wdid2/wdid_district_", dist, ".csv")
    )


}

# ***********************************************
# ---- get call analysis data from S3 bucket ----
# ***********************************************

# output path to save S3 CSVs to
out_path <- "D:/cpo/data/call_analysis/aggregate/call_analysis3.csv"
# Name of S3 bucket
bucket_name <- "cpo-call-analysis-output"

# list buckets
aws.s3::bucketlist()

# check if local file already exists, otherwise hit up S3 bucket for CSVs and bind into large dataset
if(file.exists(out_path)) {

  message(paste0("Reading data from:\n---> ", out_path))

  call_df <- readr::read_csv(out_path)

} else {

      message(paste0("Data not found at path:\n---> ", out_path))

      # objects in S3 bucket
      bucket_objs <- aws.s3::get_bucket_df(
        bucket = bucket_name,
        region = "us-west-1",
        max    = Inf
      )

    # pull in all CSVs and save to local machine
    call_df <- lapply(1:nrow(bucket_objs), function(i) {

      message(paste0(i, "/", nrow(bucket_objs)))

      tryCatch({

      # read CSVs from S3 bucket
        aws.s3::get_object(
          bucket_objs$Key[i],
          region = "us-west-1",
          bucket = bucket_name,
          as     = "text"
          ) %>%
        readr::read_csv(show_col_types = FALSE) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

      },
        error = function(e) {
          NULL
          })
      }) %>%
    dplyr::bind_rows()

  message(paste0("Saving data to path:\n---> ", out_path))

  # save
  readr::write_csv(call_df, out_path)

}


# output path to save S3 CSVs to
out_dir <- "D:/cpo/data/call_analysis"
# Name of S3 bucket
bucket_name <- "cpo-call-analysis-output"

# list buckets
aws.s3::bucketlist()

# objects in S3 bucket
bucket_objs <- aws.s3::get_bucket_df(
  bucket = bucket_name,
  region = "us-west-1",
  max    = Inf
)
# check if directories for call analysis districts exists, otherwise create
if(!dir.exists(out_dir)) {

  message(paste0("Creating dir: ",  paste0(out_dir)))

  dir.create(
    paste0(out_dir)
    # paste0(out_dir, dirname(bucket_objs$Key[i]))
  )
  message(paste0("Creating dir: ",  paste0(out_dir, "/district")))

  dir.create(
    paste0(out_dir, "/district")
    )

  message(paste0("Creating dir: ", out_dir, "/district/<district_number> dirs"))

  for (i in 1:80) {
    dir.create(
      paste0(out_dir, "/district/", i)
    )
  }

}

# loop through S3 bucket objects and read in and save out to CSVs
for (i in 1:nrow(bucket_objs)) {

  tryCatch({

    # read CSVs from S3 bucket
    wdid_ca <- aws.s3::get_object(
      bucket_objs$Key[i],
      region = "us-west-1",
      bucket = bucket_name,
      as     = "text"
    ) %>%
      readr::read_csv(show_col_types = FALSE) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    # save
    readr::write_csv(
      wdid_ca,
      paste0(
        gsub("call_analysis", "", out_dir),
        bucket_objs$Key[i])
      )

  },
  error = function(e) {
    NULL
  })

}


csv_paths <- list.files(
  list.files(
    list.files(out_dir, full.names = T),
    full.names = T
  ),
  full.names = T
  )

# pull in all CSVs and save to local machine
call_df <- lapply(1:length(csv_paths), function(i) {

  message(paste0(i, "/", length(csv_paths)))

  ca_df <-
    csv_paths[i] %>%
    readr::read_csv(show_col_types = FALSE)

  ca_df <- ca_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # set clean names
  names(ca_df) <- gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2",  names(ca_df))))

  ca_df

}) %>%
  dplyr::bind_rows()

paste0(out_dir, "/aggregate/call_analysis2.csv")

# save
readr::write_csv(call_df, paste0(out_dir, "/aggregate/call_analysis2.csv"))

message(paste0("Saving data to path:\n---> ", out_path))

# save
readr::write_csv(call_df, out_path)
# **********************************
# ---- unique WDIDs by district ----
# **********************************

dist_wdids <-
  call_df %>%
  janitor::clean_names() %>%
  dplyr::select(analysis_date, analysis_wdid, analysis_wr_admin_no,
                priority_wdid, priority_admin_no, priority_date,
                out_pct = analysis_out_of_priority_percent_of_day,
                call_type
  ) %>%
  dplyr::mutate(
    year           = lubridate::year(analysis_date),
    admin_no       = sprintf("%.5f", analysis_wr_admin_no),
    prior_no       = sprintf("%.5f", priority_admin_no),
    water_district = substr(analysis_wdid, 0, 2),
    priority_date  = as.Date(priority_date)
  ) %>%
  dplyr::filter(water_district == "02", analysis_wdid %in% unique(dist_wdids$analysis_wdid)[1:20])

dist_wdids %>%
  dplyr::filter(year == 2015) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = analysis_date, y = out_pct)) +
  ggplot2::facet_wrap(~analysis_wdid)

dist_wdids %>%
  dplyr::filter(out_pct >0 ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = priority_date, y = out_pct))

# **********************************
# ---- unique WDIDs by district ----
# **********************************

# **********************************************
# ---- ID structures within water districts ----
# **********************************************

# ****************************
# ---- WDID call analysis ----
# ****************************


out_path <- "D:/cpo/data/call_analysis/call_analysis.csv"
call_df <- readr::read_csv("D:/cpo/data/call_analysis/call_analysis.csv")
#
out_calls <-
  call_df %>%
  janitor::clean_names() %>%
  dplyr::select(analysis_date, analysis_wdid, analysis_wr_admin_no,
                priority_wdid, priority_admin_no, priority_date,
                out_pct = analysis_out_of_priority_percent_of_day,
                call_type
                ) %>%
  dplyr::mutate(
    year       = lubridate::year(analysis_date),
    water_district = substr(analysis_wdid, 0, 2),
    priority_date = as.Date(priority_date)
  ) %>%
  dplyr::group_by(water_district, analysis_date) %>%
  dplyr::slice(which.min(priority_date))

  out_calls2 <-
    out_calls %>%
    dplyr::mutate(
      year       = lubridate::year(analysis_date),
      admin_date = unname(sapply(admin_no, admins_to_date)),
      prior_date = as.Date(unname(sapply(prior_no, admins_to_date)))
    ) %>%
    dplyr::group_by(analysis_date) %>%
    dplyr::slice(which.min(prior_date))
    dplyr::filter()
    length(unique(out_calls$water_district))
  out_path <- "D:/cpo/data/call_analysis/call_analysis.csv"
  call_df2 <- readr::read_csv("D:/cpo/data/call_analysis/call_analysis.csv")
  out_calls2 <- out_calls %>%
    dplyr::mutate(analysis_date = as.Date(analysis_date))
out_calls2 %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    year       = lubridate::year(analysis_date)
    ) %>%
  dplyr::filter(year %in% c(2020), water_district == "02") %>%
  # dplyr::filter(year %in% c(2019)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = analysis_date, y = priority_date)) +
  ggplot2::facet_wrap(~water_district)

# **********************************************
# ---- ID structures within water districts ----
# **********************************************

# get all county structures via spatial search from county centroids
struct_df <- lapply(1:nrow(water_dists), function(i) {

  message("Water district: ", water_dists$water_district[i], " - (", i, "/", nrow(water_dists), ")")

  structs <- cdssr::get_structures(
    water_district    = water_dists$water_district[1]
  )

  structs
  }
) %>%
  dplyr::bind_rows()


