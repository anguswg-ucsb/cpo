library(dplyr)
library(ggplot2)
library(tidyr)

# read in call analysis data
call_analysis_df2 <- readRDS("data/call_analysis2.rds")
call_analysis_df3 <- readRDS("data/call_analysis3.rds")
call_analysis_df5 <- readRDS("data/call_analysis5.rds")
call_analysis_df6 <- readRDS("data/call_analysis6.rds")
# call_analysis_df <- readRDS("data/call_analysis.rds")
call_analysis_df <- dplyr::bind_rows(call_analysis_df2, call_analysis_df3, call_analysis_df5, call_analysis_df6)

length(unique(call_analysis_df$analysis_wr_admin_no))

# unique WDID/ADMIN pairs
uids <- dplyr::tibble(unique(call_analysis_df[,c( 'analysis_wdid', 'analysis_wr_admin_no')])) %>%
  dplyr::rename(wdid = analysis_wdid, admin_no = analysis_wr_admin_no)

# min and max admin numbers for each WDID (most senior and most junior rights for each WDID)
minmax_wdid       <- readRDS("data/wdid_admin_minmax.rds")

# call analysis column names
call_analysis_df %>%
  dplyr::tibble() %>%
  names()

# **********************
# ---- process data ----
# **********************
wdid_dates %>%
  dplyr::filter(water_district == 6) %>%
  dplyr::group_by(wdid) %>%
  dplyr::tally() %>%
  dplyr::arrange(-n)
sub_wdids <-
  wdid_dates %>%
  dplyr::tibble() %>%
  dplyr::filter(wdid == "0600564")
year_df <- data.frame(year1 = c("2002", "2018"),
                      year2 = c("2005", "2022"))
years <- c(2002, 2021)
call_analysis_df <- lapply(1:nrow(sub_wdids), function(i) {
# call_analysis_df <- lapply(1:130, function(i) {
  # call_analysis_df <- lapply(1:130, function(i) {
  message(paste0("WDID Call analysis: ", sub_wdids$wdid[i], " - (", i, "/", nrow(sub_wdids), ")"))

  year_lst <- lapply(1:length(years), function(x) {
  # year_lst <- lapply(1:nrow(year_df), function(x) {
    # GET request to CDSS API
    tryCatch({

      message("year: ", years[x])
      # message(paste0("year1: ", years[x], " - year2: ",  year_df$year2[x]))

      calls <- cdssr::get_call_analysis_wdid(
        wdid       = sub_wdids$wdid[i],
        admin_no   = sub_wdids$admin_number[i],
        # start_date = paste0(year_df$year1[x], "-04-01"),
        # end_date   = paste0(year_df$year2[x], "-09-01"),
        start_date = paste0(years[x], "-04-01"),
        end_date   = paste0(years[x], "-09-01"),
        api_key    = "O4AbCNRWLVsDkAPK3oDmnztmNPlUJWby"
      )

      calls
    },
    error = function(e) {

      NULL

    })

  }) %>%
    dplyr::bind_rows()

  year_lst
  #
}) %>%
  dplyr::bind_rows()

# ***********************
# ---- Cleaning data ----
# ***********************

sub_calls <-
  call_analysis_df %>%
  dplyr::tibble() %>%
  dplyr::rename(
    admin_no = analysis_wr_admin_no,
    prior_admin_no = priority_admin_no,
    out_pct  = analysis_out_of_priority_percent_of_day,
    wdid     = analysis_wdid,
    date     = analysis_date
  ) %>%
  dplyr::mutate(
    year     = lubridate::year(date),
    month    = lubridate::month(date),
    out_hr   = out_pct*24
  ) %>%
  dplyr::group_by(admin_no) %>%
  dplyr::mutate(
    admin_round        = floor(as.numeric(admin_no)),
    prior_admin_round  = floor(as.numeric(prior_admin_no))
  ) %>%
  dplyr::left_join(
    admin_dates(),
    by = c("admin_round" = "admin_number")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::relocate(datetime, wdid, admin_no, out_pct, out_hr, year, date, admin_date, priority_wdid, priority_date)

sub_calls %>%
  dplyr::filter(year == 2002) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = datetime, y = out_pct, color = admin_date))

sub_calls %>%
  dplyr::filter(year == 2002) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = datetime, y = out_pct, color = admin_date)) +
  ggplot2::facet_wrap(~admin_date)

call_outs <-
  sub_calls %>%
  dplyr::filter(year == 2021) %>%
  dplyr::group_by(datetime) %>%
  dplyr::filter(out_pct >0) %>%
  dplyr::slice(which.min(admin_date))

call_outs %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = datetime, y = admin_date, color = out_pct), size = 2)
  # ggplot2::geom_line(ggplot2::aes(x = datetime, y = admin_date))
  # ggplot2::facet_wrap(~admin_date)


# ***********************
# ---- Cleaning data ----
# ***********************
# STEPS:
  # rename columns
  # calculate # hours spent out of priority from day %
  # join admin date column
  # add senior/junior column
call_summary <-
  call_analysis_df %>%
  dplyr::tibble() %>%
  dplyr::rename(
    admin_no = analysis_wr_admin_no,
    prior_admin_no = priority_admin_no,
    out_pct  = analysis_out_of_priority_percent_of_day,
    wdid     = analysis_wdid,
    date     = analysis_date
  ) %>%
  dplyr::mutate(
    year     = lubridate::year(date),
    month    = lubridate::month(date),
    out_hr   = out_pct*24
  ) %>%
  dplyr::group_by(admin_no) %>%
  dplyr::mutate(
    admin_round        = floor(as.numeric(admin_no)),
    prior_admin_round  = floor(as.numeric(prior_admin_no))
  ) %>%
  dplyr::left_join(
    admin_dates(),
    by = c("admin_round" = "admin_number")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(wdid) %>%
  dplyr::mutate(
    relative_priority = dplyr::case_when(
      admin_date == min(admin_date) ~ "senior",
      admin_date == max(admin_date) ~ "junior"
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::relocate(wdid, admin_no, relative_priority, out_pct, out_hr, year, date, admin_date)

# **********************************
# ---- Summarize to yearly data ----
# **********************************
# STEPS:
  # sum total hours out of priority and avg pct of days out of priority by admin number and year
  # join columns back admin_date, wdid, relative_priority, water_district
call_years <-
  call_summary %>%
  dplyr::group_by(admin_no, year) %>%
  dplyr::summarise(
    out_hr  = sum(out_hr, na.rm = T),
    out_pct = mean(out_pct,  na.rm = T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    dplyr::ungroup(
      dplyr::slice(
        dplyr::group_by(
          dplyr::select(call_summary, division, wdid, admin_no, admin_date, relative_priority),
          admin_no
        ),
        1)
    ),
    by = "admin_no"
    ) %>%
    dplyr::left_join(
      dplyr::ungroup(
        dplyr::slice(
          dplyr::group_by(
            dplyr::select(minmax_wdid, water_district, wdid),
            wdid
          ),
          1)
        ),
      by = "wdid"
      )

# **********************************
# ---- Summarize to monthly data ----
# **********************************
# STEPS:
# sum total hours out of priority and avg pct of days out of priority by admin number and year
# join columns back admin_date, wdid, relative_priority, water_district
call_mon <-
  call_summary %>%
  dplyr::group_by(admin_no, year) %>%
  dplyr::summarise(
    out_hr  = sum(out_hr, na.rm = T),
    out_pct = mean(out_pct,  na.rm = T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    dplyr::ungroup(
      dplyr::slice(
        dplyr::group_by(
          dplyr::select(call_summary, division, wdid, admin_no, admin_date, relative_priority),
          admin_no
        ),
        1)
    ),
    by = "admin_no"
  ) %>%
  dplyr::left_join(
    dplyr::ungroup(
      dplyr::slice(
        dplyr::group_by(
          dplyr::select(minmax_wdid, water_district, wdid),
          wdid
        ),
        1)
    ),
    by = "wdid"
  )

# ***************
# ---- plots ----
# ***************

# remove districts with limited observations
rm_districts <-
  call_years %>%
  dplyr::group_by(water_district) %>%
  dplyr::tally() %>%
  dplyr::arrange(n) %>%
  dplyr::filter(n >= 100) %>%
  .$water_district

# remove years with limited observations
rm_years <-
  call_years %>%
  dplyr::group_by(year) %>%
  dplyr::tally() %>%
  dplyr::arrange(n) %>%
  dplyr::filter(n > 219) %>%
  .$year

# HISTOGRAM of out of priority percentage values
call_years %>%
  dplyr::filter(out_pct > 0) %>%
  .$out_pct %>%
  hist()

# POINT out_pct vs. admin_date (year)
# percent of time out of priority, by year
call_years %>%
  dplyr::filter(out_pct > 0) %>%
  # dplyr::filter(out_pct > 0, year %in% c(rm_years)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = admin_date, y = out_pct, color = relative_priority)) +
  # ggplot2::facet_grid(year~relative_priority)
  ggplot2::facet_wrap(.~year)

# POINT out_pct vs. admin_date (water_district)
# percent of time out of priority, by water_district
call_years %>%
  dplyr::filter(out_pct > 0) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = admin_date, y = out_pct, color = relative_priority), alpha = 0.5) +
  # ggplot2::facet_grid(year~water_district)
  ggplot2::facet_wrap(.~water_district)

# box plot of average out_pct
call_years %>%
  # dplyr::filter(out_pct > 0, water_district %in% c(rm_districts), year %in% c(rm_years)) %>%
  # dplyr::filter(out_pct > 0, water_district %in% c(rm_districts)) %>%
  # dplyr::filter(out_pct > 0, year %in% c(rm_years)) %>%
  dplyr::filter(out_pct > 0) %>%
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = admin_date, y = out_pct, fill = relative_priority)) +
  ggplot2::facet_wrap(.~water_district)
  # ggplot2::facet_wrap(.~year)

sort(unique(call_years$water_district))

# Reverse hydrograph



























