# Angus Watters
# Exploritory data analysis on climate and call analysis data

# load libraries
library(climateR)
library(terra)
library(dplyr)
library(ggplot2)
library(sf)
library(corrplot)
library(patchwork)
library(GGally)
library(ggpubr)

# Pull in climate/call analysis data for modelling.
# CAUTION, if climate and call analysis data is not already pulled and saved to data/ folder, then this operation can take a while as all climate data and call analysis data must be pulled and downloaded from the internet
source("R/get_model_data.R")
source("R/plot_utils.R")

# ----------------------------
# ---- Group by seniority ----
# ----------------------------

# local path to save plots to
save_path <- "D:/cpo/plots"

# loop through all climate variables and make a faceted district scatter plots for each climate variable vs out priority %
make_out_scatter_plots(df = mod_df, save_path = save_path)

# ---------------------------
# ---- Correlation plot -----
# ---------------------------

# make a list of correlation plots per district, save to specified path
corr_plots <- make_corr_plots(df = mod_df, save_path = save_path)

# ------------------------------------------
# ---- Monthly linear regression models ----
# ------------------------------------------

# take average values for each unique site across each month
sub_month <-
  mod_df %>%
  dplyr::mutate(
    month = lubridate::month(date),
    year  = lubridate::year(date)
  ) %>%
  # dplyr::filter(out_pct > 0) %>%
  dplyr::mutate(
    uid = paste0(district, "_", wdid, "_", gnis_id, "_", seniority, "_", as.character(as.Date(approp_date, format = "%Y-%m-%d")))
  ) %>%
  dplyr::select(-district, -wdid, -gnis_id, -seniority, -approp_date, -date) %>%
  dplyr::relocate(uid, month, year) %>%
  dplyr::group_by(uid, month, year) %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), \(x) round(x, 3))
    ) %>%
  dplyr::summarise(
    dplyr::across(
      c(out_pct:swe), \(x) mean(x,  na.rm = TRUE)
      # c("out_pct", "pr", "tmmx", "spi30d", "eddi30d", "eddi90d", "pdsi"),
    )
  )  %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    out_pct_log = log(out_pct)
    # out_pct_sqrt = sqrt(out_pct)
  )

# monthly averages of data
month_lm <-
  sub_month %>%
  # dplyr::select(-wdid, -gnis_id, -seniority) %>%
  dplyr::select(-month, -year, -out_pct_log) %>%
  # dplyr::rename(out_pct = out_pct_sqrt) %>%
  # dplyr::rename(out_pct = out_pct_log) %>%
  tidyr::pivot_longer(
    cols = c(-out_pct, -uid),
    # cols = c(-out_pct, -uid, -month, -year),
    names_to  = "pred_name",
    values_to = "pred_value"
  ) %>%
  dplyr::group_by(uid, pred_name) %>%
  dplyr::group_split() %>%
  as.list() %>%
  stats::setNames(
    lapply(1:length(.), function(i) {paste0(.[[i]]$uid[1], "_", .[[i]]$pred_name[1])})
  ) %>%
  purrr::map(~lm(out_pct ~ pred_value, data = .)) %>%
  dplyr::tibble(
    dvsub = names(.),
    untidied = .
  ) %>%
  dplyr::mutate(
    tidy = purrr::map(untidied, broom::tidy),
    glan = purrr::map(untidied, broom::glance)
  ) %>%
  tidyr::unnest(tidy, glan) %>%
  tidyr::separate(
    dvsub,
    into = c("district", "wdid", "gnis_id", "seniority", "approp_date", "pred_name"),
    sep  = "_"
  )

top_preds <-
  month_lm %>%
  dplyr::select(district, wdid, gnis_id, seniority, approp_date, pred_name, rsq = r.squared) %>%
  dplyr::group_by(wdid, gnis_id, seniority) %>%
  dplyr::arrange(-rsq) %>%
  dplyr::slice(1)

max(month_lm$r.squared, na.rm = T)
mean(month_lm$r.squared, na.rm = T)
hist(month_lm$r.squared)

# ----------------------------------
# ---- District rights averages ----
# ----------------------------------

#  average rights junior, median, senior rights across each district
sub_right <-
  mod_df %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), \(x) round(x, 3))
  ) %>%
  dplyr::mutate(
    uid = paste0(district, "_", seniority, "_")
  ) %>%
  dplyr::group_by(date, district, seniority) %>%
  dplyr::summarise(
    dplyr::across(
      c(out_pct:swe), \(x) mean(x,  na.rm = TRUE)
      # c("out_pct", "pr", "tmmx", "spi30d", "eddi30d", "eddi90d", "pdsi"),
    )
  ) %>%
  dplyr::ungroup()


right_lm <-
  sub_right %>%
  dplyr::mutate(
    uid = paste0(district, "_", seniority)
  ) %>%
  dplyr::select(-date, -district, -seniority) %>%
  tidyr::pivot_longer(
    cols = c(-out_pct, -uid),
    # cols = c(-out_pct, -uid, -month, -year),
    names_to  = "pred_name",
    values_to = "pred_value"
  ) %>%
  dplyr::group_by(uid, pred_name) %>%
  dplyr::group_split() %>%
  as.list() %>%
  stats::setNames(
    lapply(1:length(.), function(i) {paste0(.[[i]]$uid[1], "_", .[[i]]$pred_name[1])})
  ) %>%
  purrr::map(~lm(out_pct ~ pred_value, data = .)) %>%
  dplyr::tibble(
    dvsub = names(.),
    untidied = .
  ) %>%
  dplyr::mutate(
    tidy = purrr::map(untidied, broom::tidy),
    glan = purrr::map(untidied, broom::glance)
  ) %>%
  tidyr::unnest(tidy, glan) %>%
  tidyr::separate(
    dvsub,
    into = c("district", "seniority", "pred_name"),
    sep  = "_"
  )
hist(right_lm$r.squared)
max(right_lm$r.squared, na.rm = T)
mean(right_lm$r.squared, na.rm = T)
min(right_lm$r.squared, na.rm = T)

# ------------------------
# ---- Right o graphs ----
# ------------------------

calls_df <-  readRDS("data/wdid_call_analysis.rds")
tmp <-
  calls_df %>%
  dplyr::filter(analysis_wdid == "0100805") %>%
  dplyr::mutate(
    day   = lubridate::yday(datetime),
    year  = as.character(lubridate::year(datetime)),
    week  = lubridate::week(datetime),
    week_date = lubridate::weeks(datetime )
  ) %>%
  dplyr::mutate(
    priority_date = dplyr::case_when(
      is.na(priority_date) ~ Sys.Date(),
      TRUE                 ~ as.Date(priority_date)
      # is.na(priority_date) ~ as.Date("2019-12-31"),
      # TRUE                 ~ as.Date(priority_date)
    )
  )

tmp %>%
  dplyr::filter(year == "2002") %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = datetime, y = priority_date), size = 1.5) +
  ggplot2::labs(
    title = paste0("Right-o-graph"),
    subtitle = "WDID: 0100805",
    # caption = "Black horizontal line represents average % out of priority over the period of record",
    x     = "",
    y     = "Priority Date",
    color = "Year"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title        = ggplot2::element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle     = ggplot2::element_text(size = 14, hjust = 0.5),
    legend.title      = ggplot2::element_text(size = 18, hjust = 0.5, face = "bold"),
    legend.text       = ggplot2::element_text(size = 16),
    axis.title        = ggplot2::element_text(size = 16, face = "bold"),
    axis.text         = ggplot2::element_text(size = 16),
    legend.key.width  = unit(1.5, "cm"),
    legend.text.align = 0,
    legend.key.height = unit(1, "cm")
  )

tmp %>% .$wdid_gnis_id %>% unique()
wr_pts <-
  wr_net %>%
  dplyr::filter(gnis_id == "00171161") %>%
  sf::st_as_sf(
    coords = c("longitude", "latitude"),
    crs    = 4326
  )


flines <- gnis_flines %>%
  dplyr::filter(gnis_id == "171161")

mapview::mapview(flines) + wr_pts
ggplot2::ggplot() +
  ggplot2::
tmp %>%
  dplyr::filter(year == "2010") %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = datetime, y = priority_date),
                     alpha = 0.9, size = 2.5)
# calls_df %>%
#   dplyr::filter(analysis_wdid == "0100805")  %>%
#   dplyr::mutate(
#     day   = lubridate::yday(datetime),
#     year  = as.character(lubridate::year(datetime)),
#     week  = lubridate::week(datetime),
#     week_date = lubridate::weeks(datetime )
#   ) %>%
#   dplyr::group_by(year, week, analysis_wdid) %>%
#   dplyr::summarise(
#     out_pct       = mean(analysis_out_of_priority_percent_of_day, na.rm = T)/100,
#     priority_date = mean(priority_date, na.rm = T)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(wdid = analysis_wdid)


# --------------------
# ---- timeseries ----
# --------------------
# gnisid 171161
# wdid junior: 0100805
# wdid senior: 0100643
jun_out <-
  mod_df %>%
  dplyr::filter(district == "01", wdid == "0100805") %>%
  dplyr::mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
    )  %>%
  dplyr::select(date, wdid, swe, out_pct)

ts(out_ts$out_pct)

swe_ts <- ts(jun_out$swe)
out_ts <- ts(jun_out$out_pct)

model <- arima(out_ts, xreg = swe_ts)
summary(model)
plot(residuals(model))
acf(residuals(model))
pacf(residuals(model))

new_swe <- ts(c(10, 20, 30), frequency = 52)
forecast <- forecast(model, xreg = new_swe)
new_swe <- ts(c(10, 20, 30), frequency = 52)
pred <- predict(model, newdata = data.frame(swe = new_swe), interval = "prediction")
# swe_ts <- ts(jun_out$swe, frequency = 52)
# out_ts <- ts(jun_out$out_pct, frequency = 52)

# winter_swe
jun_out %>%
  dplyr::filter(
    month %in% c(11, 12, 1, 2, 3)
  ) %>%
  dplyr::select(date, wdid, swe)

jun_out %>%
  dplyr::filter(
    !month %in% c(11, 12, 1, 2, 3)
  ) %>%
  dplyr::select(date, wdid, out_pct)

jun_out %>%
  # dplyr::filter(year %in% c(2010, 2011)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = out_pct, y = swe))

sen_out <-
  mod_df %>%
  dplyr::filter(district == "01", wdid == "0100643") %>%
  dplyr::mutate(year = lubridate::year(date))

sen_out %>%
  dplyr::filter(year %in% c(2010, 2011)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = swe))

mod_df %>%
  dplyr::filter(district == "01", seniority == "senior") %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct)) +
  ggplot2::facet_wrap(gnis_id~wdid)
  # ggplot2::facet_grid(gnis_id~wdid)

mod_df %>%
  dplyr::filter(district == "01", seniority == "junior") %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct)) +
  ggplot2::facet_wrap(gnis_id~wdid)
  ggplot2::facet_wrap(~wdid)


mod_df %>%
  dplyr::filter(district == "01", seniority == "median") %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct)) +
  ggplot2::facet_wrap(~wdid)

# -------------------------------------
# -------------------------------------

# -------------------------------------
# ---- Dive into a single district ----
# -------------------------------------

# district of interest
doi <- c("02")

# make a subset of the data
sub_df <-
  mod_df %>%
  dplyr::filter(district %in% c(doi), date > start_date, date < end_date) %>%
  # dplyr::filter(out_pct > 0) %>%
  dplyr::mutate(
    out_pct_sqrt = sqrt(out_pct)
  )

# average data to monthly

sub_month =
  sub_df %>%
  dplyr::mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) %>%
  dplyr::group_by(month, year, wdid, gnis_id, seniority) %>%
    dplyr::summarise(
      dplyr::across(
        c("out_pct", "pr", "tmmx", "spi30d", "eddi30d", "eddi90d", "pdsi"),
        mean,
        na.rm = TRUE
      )
    )  %>%
    dplyr::ungroup()

month_lm <-
  sub_month %>%
  dplyr::mutate(
    uid = paste0(wdid, "_", gnis_id, "_", seniority)
  ) %>%
  # dplyr::select(-wdid, -gnis_id, -seniority) %>%
  dplyr::select(-month, -year, -wdid, -gnis_id, -seniority) %>%
  tidyr::pivot_longer(
    cols = c(-out_pct, -uid),
    # cols = c(-out_pct, -uid, -month, -year),
    names_to  = "pred_name",
    values_to = "pred_value"
  ) %>%
  dplyr::group_by(uid, pred_name) %>%
  dplyr::group_split() %>%
  as.list() %>%
  stats::setNames(
    lapply(1:length(.), function(i) {paste0(.[[i]]$uid[1], "_", .[[i]]$pred_name[1])})
  ) %>%
  purrr::map(~lm(out_pct ~ pred_value, data = .)) %>%
  dplyr::tibble(
    dvsub = names(.),
    untidied = .
  ) %>%
  dplyr::mutate(
    tidy = map(untidied, broom::tidy),
    glan = map(untidied, broom::glance)
  ) %>%
  tidyr::unnest(tidy, glan) %>%
  tidyr::separate(
    dvsub,
    into = c("wdid", "gnis_id", "seniority", "pred_name"),
    sep  = "_"
    )


  # dplyr::group_by(uid)
sub_month %>%
pivot_longer(
  cols = c(-x, -uid),
  names_to = "y_name",
  values_to = "y_value"
) %>%
  dplyr::group_by(uid, y_name) %>%
  dplyr::group_split() %>%
  as.list() %>%
  stats::setNames(
    lapply(1:length(.), function(i) {paste0(.[[i]]$uid[1], "_", .[[i]]$y_name[1])})
  ) %>%
  map(~lm(y_value ~ x, data = .)) %>%
  tibble(
    dvsub = names(.),
    untidied = .
  ) %>%
  mutate(
    tidy = map(untidied, broom::tidy),
    glan = map(untidied, broom::glance)
  ) %>%
  unnest(tidy, glan)
# month_grp =
# sub_month %>%
#   dplyr::group_by(month, wdid, gnis_id, seniority)
library(purrr)
library(tidyverse)
df <- data.frame(
  x=rnorm(100),
  eddi30d=rnorm(100),
  eddi90d=rnorm(100),
  pr=rnorm(100)
  ) %>%
  dplyr::mutate(
    uid = dplyr::case_when(

      x < -0.5 ~ "A",
      x >= -0.5 & x < 0.5 ~ "B",
      TRUE ~ "C"
    )
  )
df %>%
  pivot_longer(
    cols = c(-x, -uid),
    names_to = "y_name",
    values_to = "y_value"
  ) %>%
  # dplyr::group_by(uid, y_name) %>%
  # dplyr::group_split()
  split(.$y_name) %>%
  length()

sub_mods <-
  df %>%
  pivot_longer(
    cols = c(-x, -uid),
    names_to = "y_name",
    values_to = "y_value"
  ) %>%
  dplyr::group_by(uid, y_name) %>%
  dplyr::group_split() %>%
  as.list() %>%
  stats::setNames(
    lapply(1:length(.), function(i) {paste0(.[[i]]$uid[1], "_", .[[i]]$y_name[1])})
  ) %>%
  map(~lm(y_value ~ x, data = .)) %>%
  tibble(
    dvsub = names(.),
    untidied = .
  ) %>%
  mutate(
    tidy = map(untidied, broom::tidy),
    glan = map(untidied, broom::glance)
  ) %>%
  unnest(tidy, glan)
  # mutate(tidy = map(untidied, broom::tidy)) %>%
  # unnest(tidy)

nms = lapply(1:length(sub_mods), function(i) {paste0(sub_mods[[i]]$uid[1], "_", sub_mods[[i]]$y_name[1])})

models <-
  df %>%
  pivot_longer(
    cols = c(-x, -uid),
    names_to = "y_name",
    values_to = "y_value"
  ) %>%
  dplyr::group_by(uid, y_name) %>%
  dplyr::group_split() %>%
  as.list()  %>%
  # split(.$y_name) %>%
  map(~lm(y_value ~ x, data = .)) %>%
  tibble(
    dvsub = names(.),
    untidied = .
    ) %>%
  mutate(
    tidy = map(untidied, broom::tidy),
    glan = map(untidied, broom::glance)
    ) %>%
  unnest(tidy)

lm_results <-
  sub_month %>%
  dplyr::group_by(month, wdid, gnis_id, seniority) %>%
  tidyr::nest() %>%
  mutate(
    lm_result = map(data, ~ map(names(.)[!names(.) %in% c("month", "wdid", "gnis_id", "seniority", "out_pct")], ~ lm(formula(paste("out_pct ~", .)), data = .x)))
    ) %>%
  unnest(lm_result, .drop = TRUE)
  mutate(lm_result = map(data, ~lm(spi30d ~ eddi30d + eddi90d + pdsi, data = .))) %>%
  unnest(lm_result)
sub_month[, -1]
# View the results
lm_results
  # dplyr::mutate(
  #   out_pct = mean(out_pct, na.rm = TRUE),
  #   pr = mean(pr, na.rm = TRUE),
  #   tmmx = mean(tmmx, na.rm = TRUE),
  #   spi30d = mean(spi30d, na.rm = TRUE),
  #   eddi30d = mean(eddi30d, na.rm = TRUE),
  #   eddi90d = mean(eddi90d, na.rm = TRUE),
  #   pdsi = mean(pdsi, na.rm = TRUE)
  # )



# --------------------------
# ---- Correlation plot ----
# --------------------------
# Correlation between climate variables
corr_matrix <- cor(dplyr::select(sub_df, -district, -date, -wdid , -gnis_id, -approp_date, -seniority), use = "pairwise.complete.obs")

# Create a correlation plot
corrplot(corr_matrix, method = "circle")

# -------------------------
# ---- Regression plot ----
# -------------------------
# create scatter regression plots
ggscatter(sub_df,
          x = "out_pct_sqrt",
          y = "eddi1y",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "X Label", ylab = "Y Label",
          title = "Correlation Plot")

# --------------------------
# ---- Timeseries plots ----
# --------------------------

# plot a district out of priority percent facet plot, plot columns are GNIS IDs and rows are the
# relative seniority on that GNIS ID (junior, median, senior)
sub_df %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct)) +
  ggplot2::facet_grid(seniority~gnis_id)


sub_clim <-
  mod_df %>%
  dplyr::filter(district %in% c(doi)) %>%
  dplyr::select(date, wdid, gnis_id, seniority, out_pct, pr, tmmx, spi30d, eddi30d, eddi90d, pdsi)

# climate variable vs out_pct
sub_df %>%
  ggplot2::ggplot() +
  # ggplot2::geom_point(ggplot2::aes(x = pr, y = out_pct, color = seniority)) +
  ggplot2::geom_point(ggplot2::aes(x = out_pct, y = pr, color = seniority)) +
  ggplot2::facet_wrap(~seniority)
  # ggplot2::facet_grid(seniority~gnis_id)

# climate variable vs out_pct
sub_df %>%
  ggplot2::ggplot() +
  # ggplot2::geom_point(ggplot2::aes(x = pr, y = out_pct, color = seniority)) +
  ggplot2::geom_bar(ggplot2::aes(y = cut(out_pct, breaks = 10))) +
  ggplot2::facet_wrap(~seniority)

sub_df %>%
  # dplyr::group_by(seniority, gnis_id) %>%
  dplyr::filter(seniority == "junior") %>%
  dplyr::group_by(gnis_id) %>%
  # dplyr::slice(1:1000) %>%
  dplyr::ungroup() %>%
  dplyr::select(out_pct, pr, tmmx, spi30d, eddi30d, eddi90d, pdsi) %>%
  GGally::ggpairs()


# ------------------
# ---- Boxplots ----
# ------------------


# convert week to a date format so we can group by month
sub_df$date <- as.Date(paste0(df$date, "-1"), format="%Y-%U-%u")
sub_df$month <- format(sub_df$date, "%Y-%m")

# create the box plot
sub_df %>%
  dplyr::filter(district %in% c("06", "07", "05", "02")) %>%
  ggplot2::ggplot(ggplot2::aes(x=seniority, y=out_pct)) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_grid(.~district+month) +
  ggplot2::labs(title = "Out Percentage by Seniority Group", y = "Out Percentage")

# ************************************************************************

