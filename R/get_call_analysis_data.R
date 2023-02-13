# Angus Watters
# CDSS call analysis data

# devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(dplyr)

source("R/utils.R")

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
# ****************************************************************
# ---- run call analysis workflow in AWS using S3, Lambda, S3 ----
# ****************************************************************

library(aws.s3)
library(purrr)
# list buckets
aws.s3::bucketlist()

# model_data <- readRDS("model_data_all.rds")
# model_data      <- readRDS("statemod_climate_year.rds")

# Name of S3 bucket
bucket_name <- "cpo-call-analysis-output"

bucket_objs <- aws.s3::get_bucket_df(
  bucket = bucket_name,
  region = "us-west-1",
  max    = Inf
  )
# tmp <- readr::read_csv(aws.s3::get_object(bucket_objs$Key[1], region = "us-west-1", bucket = bucket_name, as = "text"))
# bucket_objs$Key[1]
# tmp <- readr::read_csv(aws.s3::get_object(bucket_objs$Key[1],
#                                           region = "us-west-1",
#                                           bucket = bucket_name,
#                                           as = "text"
#                                           ),
                       # col_names = TRUE,
                       # col_types = readr::cols(),
                       # show_col_types = FALSE
                       # ) %>%
#   dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
# lst <- bucket_objs$Key[1:3]
#
# # The bucket you are attempting to access must be addressed using the specified endpoint.
# call_analysis <- purrr::map_df(bucket_objs$Key, function(key){
#   readr::read_csv(
#     aws.s3::get_object(
#       key,
#       region = "us-west-1",
#       bucket = bucket_name,
#       as     = "text")
#     ) %>%
#     dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
# })

call_analysis <- lapply(1:nrow(bucket_objs), function(i) {

  message(paste0(i, "/", nrow(bucket_objs)))

  readr::read_csv(
    aws.s3::get_object(
      bucket_objs$Key[i],
      region = "us-west-1",
      bucket = bucket_name,
      as     = "text"),
    show_col_types = FALSE
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

}) %>%
  dplyr::bind_rows()

readr::write_csv(call_analysis, "D:/cpo/data/call_analysis/call_analysis.csv")
# ***********************************************
# ---- get call analysis data from S3 bucket ----
# ***********************************************

# **********************************
# ---- unique WDIDs by district ----
# **********************************


# nrow(wdid_lst)/10
#
#
#
# n = 18000 #number of groups
#
# wdid_split <-
#   wdid_lst %>%
#   group_by(row_number() %/% n) %>%
#   group_map(~ .x)
#
# length(wdid_split)
# wdid_split[4]
#
# "D:/cpo/data/district_wdid"
# cdssr::get_reference_tbl()
#
# district_split <-
#   wdid_lst %>%
#   dplyr::group_by(water_district) %>%
#   dplyr::tally() %>%
#   dplyr::rename(wdids = n) %>%
#   dplyr::arrange(wdids)
#   dplyr::arrange(-n)
#
# tmp2 <- tmp %>%
#   dplyr::rename(wdids = n) %>%
#   dplyr::arrange(wdids)
#   # dplyr::filter(water_district == 2)
#
# readr::write_csv(tmp2, "data/district_wdid_counts.csv")
# nodes <- readRDS("D:/cpo/data/node_id_table.rds")
#
# ddr_path <- "C:/Users/angus/OneDrive/Desktop/github/cpo_data/data/ddr/outputs/ddr.rds"
# ddr <- readRDS(ddr_path)
# **********************************************
# ---- ID structures within water districts ----
# **********************************************
# ****************************
# ---- WDID call analysis ----
# ****************************

wdid_lst %>%
  dplyr::group_by(water_district, wdid) %>%
  dplyr::tally() %>%
  dplyr::arrange(-n)

system.time(
calls <- cdssr::get_call_analysis_wdid(
          wdid       = "0803514",
          admin_no   = "99999.00000",
          start_date = "2010-01-01",
          end_date   = "2023-01-01"
        )
)


#
out_calls <-
  calls %>%
  dplyr::select(datetime, analysis_date, analysis_wdid, analysis_wr_admin_no,
                priority_wdid, priority_admin_no, priority_date,
                out_pct = analysis_out_of_priority_percent_of_day,
                call_type
                ) %>%
  dplyr::mutate(
    year       = lubridate::year(datetime),
    admin_date = unname(sapply(analysis_wr_admin_no, admins_to_date)),
    prior_date = as.Date(unname(sapply(priority_admin_no, admins_to_date)))
  )

out_calls %>%
  dplyr::filter(year == 2020) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = datetime, y = prior_date))
  # ggplot2::geom_line(ggplot2::aes(x = datetime, y = out_pct))
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
# structs$wdid %>%
#   unique() %>%
#   length()
#
# wr_trans %>%
#   dplyr::tibble() %>%
#   dplyr::filter(water_district == 1) %>%
#   .$wdid %>%
#   unique() %>%
#   length()
# *************************************
# ---- Historic/Active admin calls ----
# *************************************
# hcalls_df <- cdssr::get_admin_calls(
#   division    = 1,
#   active      = FALSE
# )
#
# hcalls_df <- cdssr::get_admin_calls(
#   division    = 1,
#   active      = TRUE
# )
# # get all county structures via spatial search from county centroids
# hcalls_df <- lapply(1:nrow(water_dists), function(i) {
#
#   message("Water district: ", water_dists$water_district[i], " - (", i, "/", nrow(water_dists), ")")
#
#   structs <- cdssr::get_admin_calls(
#     division    = 1,
#     active      = FALSE
#   )
#
#   structs
# }
# ) %>%
#   dplyr::bind_rows()

# *******************************************
# ---- Water rights transactions by WDID ----
# *******************************************

# # get all county structures via spatial search from county centroids
# wrt_struct <- lapply(1:nrow(struct_df), function(i) {
#
#   message("WR transactions: ", struct_df$wdid[i])
#
#   # GET request to CDSS API
#   tryCatch({
#
#     wr_trans <- cdssr::get_water_rights_trans(
#       wdid = struct_df$wdid[i]
#     )
#     wr_trans
#   },
#   error = function(e) {
#
#     NULL
#
#   })
#
# }) %>%
#   dplyr::bind_rows()

# # ****************************************************
# # ---- Water rights net amounts by water district ----
# # ****************************************************
#
# wr_net <- lapply(1:nrow(water_dists), function(i) {
#
#   message(paste0("Water rights net amounts: ", water_dists$water_district[i], " - (", i, "/", nrow(water_dists), ")"))
#
#   # GET request to CDSS API
#   tryCatch({
#
#     wr_nt <- cdssr::get_water_rights_netamount(
#       water_district    = water_dists$water_district[i]
#     )
#     wr_nt
#   },
#   error = function(e) {
#
#     NULL
#
#   })
#
# }) %>%
#   dplyr::bind_rows()

# ******************************
# ---- subset call analysis ----
# ******************************

# # slice most senior and junior rights from each wdid
# wr_tails <-
#   wr_trans %>%
#   dplyr::group_by(wdid, admin_number) %>%
#   dplyr::mutate(
#     admin_number_round = floor(as.numeric(admin_number))
#   ) %>%
#   dplyr::left_join(
#     admin_dates(),
#     by = c("admin_number_round" = "admin_number")
#   ) %>%
#   dplyr::ungroup()
#
# wr_tails2 <-
#   wr_tails %>%
#   dplyr::group_by(wdid) %>%
#   dplyr::arrange(date) %>%
#   dplyr::slice(1)
#   dplyr::select(date) %>%
#   na.omit()
# # total water rights per district
# wr_trans %>%
#   dplyr::group_by(water_district) %>%
#   dplyr::tally() %>%
#   dplyr::arrange(-n)
#
# # subset to single district
# sub_wdid <-
#   wr_trans %>%
#   dplyr::filter(water_district == 2)

# # remove wdids with only 1 admin number
# rm_wdids <-
#   wdid_dates %>%
#   dplyr::group_by(wdid) %>%
#   dplyr::tally() %>%
#   dplyr::filter(n == 1)
#
# # slice min and max dates for each wdid
# minmax_wdid <-
#   wdid_dates %>%
#   dplyr::tibble() %>%
#   na.omit() %>%
#   dplyr::filter(wdid %in% dplyr::filter(
#                                   dplyr::tally(
#                                     dplyr::group_by(wdid_dates, wdid)
#                                   ),
#                                   n > 3
#                                 )$wdid
#                 ) %>%
#   dplyr::group_by(wdid) %>%
#   dplyr::slice(
#     which.max(admin_date),
#     which.min(admin_date)
#     ) %>%
#   dplyr::ungroup()
#
# # save call analysis data
# saveRDS(minmax_wdid, "data/wdid_admin_minmax.rds")

# # get top 50 most seniors rights in each water district
# senior_rights <-
#   minmax_wdid %>%
#   dplyr::group_by(wdid) %>%
#   dplyr::slice(
#     which.min(admin_date)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(water_district) %>%
#   dplyr::arrange(admin_date, .by_group = T) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(water_district) %>%
#   dplyr::slice(1:30) %>%
#   dplyr::ungroup()

# get WDIDs of most senior 50 water rights in specified district
# district_wdids <-
#   senior_rights %>%
#   unique(dplyr::filter(senior_rights, water_district == 2)$wdid)

# # get WDIDs of most senior 50 water rights in specified district
# # get the most senior and most junior rights from each WDID that was found to
# # contain 50 of the most senior rights in specified district
# sub_wdids <-
#   minmax_wdid %>%
#   dplyr::filter(
#     wdid %in% unique(senior_rights$wdid)
#     )
#
# sub_wdids <-
#   minmax_wdid %>%
#   dplyr::filter(!wdid %in% unique(call_analysis_df$analysis_wdid)) %>%
#   dplyr::filter(!admin_number %in% unique(call_analysis_df$analysis_wr_admin_no))
#
# sub_wdids <-
#   wdid_dates %>%
#   dplyr::tibble() %>%
#   na.omit() %>%
#   dplyr::filter(!wdid %in% dplyr::filter(
#     dplyr::tally(
#       dplyr::group_by(wdid_dates, wdid)
#     ),
#     n == 1
#   )$wdid
#   )
#
#
# years <- c("2002", "2018", "2019", "2022")
# year_df <- data.frame(year1 = c("2002", "2018"), year2 = c("2005", "2022"))
#
# call_analysis_df <- lapply(1:nrow(sub_wdids), function(i) {
#
#   message(paste0("WDID Call analysis: ", sub_wdids$wdid[i], " - (", i, "/", nrow(sub_wdids), ")"))
#
#   year_lst <- lapply(1:nrow(year_df), function(x) {
#
#     # GET request to CDSS API
#     tryCatch({
#
#       # message("year: ", years[x])
#     message(paste0("year1: ", year_df$year1[x], " - year2: ",  year_df$year2[x]))
#
#       calls <- cdssr::get_call_analysis_wdid(
#         wdid       = sub_wdids$wdid[i],
#         admin_no   = sub_wdids$admin_number[i],
#         start_date = paste0(year_df$year1[x], "-01-01"),
#         end_date   = paste0(year_df$year2[x], "-01-01")
#       )
#
#       calls
#     },
#     error = function(e) {
#
#       NULL
#
#     })
#
#   }) %>%
#     dplyr::bind_rows()
#
#   year_lst
# #
# }) %>%
#   dplyr::bind_rows()
#
# # save call analysis data
# saveRDS(call_analysis_df, "data/call_analysis.rds")
#
#
# length(unique(call_analysis_df$analysis_wdid))
# unique(call_analysis_df$analysis_wr_admin_no)
# ca_df <-
#   call_analysis_df %>%
#   dplyr::group_by(analysis_wr_admin_no, analysis_wdid) %>%
#   dplyr::mutate(
#     out_hours  = 24*analysis_out_of_priority_percent_of_day,
#     day_count  = n(),
#     total_hours = (day_count * 24),
#     out_hour_pct = out_hours/total_hours
#     ) %>%
#   dplyr::relocate(out_hours, day_count) %>%
#   dplyr::mutate(
#     analysis_admin_round = floor(as.numeric(analysis_wr_admin_no)),
#     prior_admin_round    = floor(as.numeric(priority_admin_no))
#   ) %>%
#   dplyr::left_join(
#     admin_dates(),
#     by = c("analysis_admin_round" = "admin_number")
#   ) %>%
#   dplyr::left_join(
#     admin_dates(),
#     by = c("prior_admin_round"    = "admin_number")
#   ) %>%
#   dplyr::rename(date_x = date.x, date_y = date.y)
# ca_df$analysis_wdid %>% unique()
#
# ca_df %>%
#   dplyr::filter(analysis_wdid == "0700502")
#
# ca_df %>%
#   dplyr::mutate(
#     date_x = as.character(date_x),
#     date = as.Date(analysis_date),
#     year = lubridate::year(date)
#     ) %>%
#   dplyr::filter(analysis_wdid == "0700502", year == 2002) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(x = date, y = out_hour_pct, color = date_x)) +
#   ggplot2::facet_grid(date_x~analysis_wdid)
#
# ca_df %>%
#   dplyr::mutate(
#     date_x = as.character(date_x),
#     date   = as.Date(analysis_date),
#     year   = lubridate::year(date)
#   ) %>%
#   dplyr::filter(analysis_wdid == "0700502") %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_point(ggplot2::aes(x = date_x, y = out_hour_pct))


# unique(sub_wdid[,c('wdid', 'admin_number')]) %>%
#   dplyr::group_by(wdid) %>%
#   dplyr::mutate(admin_count = n()) %>%
#   .$admin_count %>%
#   hist(breaks = 50)


# nodes <- readRDS("D:/cpo/data/node_id_table.rds")
#
# ddr_path <- "C:/Users/angus/OneDrive/Desktop/github/cpo_data/data/ddr/outputs/ddr.rds"
# ddr <- readRDS(ddr_path)

# Direct Diversion Record
# ddr  <- readr::read_fwf(
  # file           = paste0(ddr_path, ddr_files[i]),
  # col_types      = cols(),
  # col_positions  = fwf_widths(c(12, 24, 17, 12, 11, 8))
# )
#
# uwdids <-
#   ddr %>%
#   dplyr::group_by(basin) %>%
#   dplyr::arrange(desc(priority), .by_group = T) %>%
#   dplyr::slice(1:10)
# length(unique(uwdids$admin))
# unique(ddr$admin)
