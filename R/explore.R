# Angus Watters
# Exploritory data analysis on climate and call analysis data

# load libraries
library(climateR)
library(terra)
library(dplyr)
library(sf)
library(corrplot)

# Pull in climate/call analysis data for modelling.
# CAUTION, if climate and call analysis data is not already pulled and saved to data/ folder, then this operation can take a while as all climate data and call analysis data must be pulled and downloaded from the internet
source("R/get_model_data.R")

# *************************************
# ---- Dive into a single district ----
# *************************************
library(GGally)
doi <- c("02")
start_date = "2000-01-01"
end_date   = "2002-01-01"

# dataframe names
names(mod_df)

# make a subset of the data
sub_df <-
  mod_df %>%
  dplyr::filter(district %in% c(doi), date > start_date, date < end_date)

# plot a district out of priority percent facet plot, plot columns are GNIS IDs and rows are the
# relative seniority on that GNIS ID (junior, median, senior)
sub_df %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct)) +
  ggplot2::facet_grid(seniority~gnis_id)
gnis_id = "180770"

sub_clim <-
  mod_df %>%
  dplyr::filter(district %in% c(doi)) %>%
  # dplyr::filter(gnis_id == "180770") %>%
  dplyr::select(date, wdid, gnis_id, seniority, out_pct, pr, tmmx, spi30d, eddi30d, eddi90d, pdsi)

# climate variable vs out_pct
sub_clim %>%
  ggplot2::ggplot() +
  # ggplot2::geom_point(ggplot2::aes(x = pr, y = out_pct, color = seniority)) +
  ggplot2::geom_point(ggplot2::aes(x = out_pct, y = pr, color = seniority)) +
  ggplot2::facet_wrap(~seniority)
  # ggplot2::facet_grid(seniority~gnis_id)

# climate variable vs out_pct
sub_clim %>%
  ggplot2::ggplot() +
  # ggplot2::geom_point(ggplot2::aes(x = pr, y = out_pct, color = seniority)) +
  ggplot2::geom_bar(ggplot2::aes(y = cut(out_pct, breaks = 10))) +
  ggplot2::facet_wrap(~seniority)

sub_clim %>%
  # dplyr::group_by(seniority, gnis_id) %>%
  dplyr::filter(seniority == "junior") %>%
  dplyr::group_by(gnis_id) %>%
  # dplyr::slice(1:1000) %>%
  dplyr::ungroup() %>%
  dplyr::select(out_pct, pr, tmmx, spi30d, eddi30d, eddi90d, pdsi) %>%
  GGally::ggpairs()

# ************************************************************************

library(ggplot2)
library(tidyr)
library(GGally)

# calculate mean and standard deviation across districts and water rights
aggreg_df <-
    mod_df %>%
    # dplyr::filter(district %in% c("02", "07")) %>%
    # dplyr::filter(district %in% c("02")) %>%
    dplyr::group_by(date, district, seniority) %>%
    dplyr::summarise(
        dplyr::across(c(out_pct:z), list(mean = ~ mean(., na.rm = TRUE), sd = ~ sd(., na.rm = TRUE)))
        )

aggreg_df2 <-
  aggreg_df %>%
  dplyr::select(date, district, seniority, contains("mean")) %>%
  dplyr::filter(district %in%  c("06", "01", "07", "05", "64", "08", "09")) %>%
  dplyr::mutate(
    year = lubridate::year(date)
  )

aggreg_df2 %>%
  dplyr::filter(year %in% c(2003)) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = date, y = out_pct_mean)) +
  ggplot2::facet_grid(seniority~district)

# ---- Correlation plot ----
# Correlation between climate variables
corr_matrix <- cor(dplyr::select(clim_ts, -district, -date), use = "pairwise.complete.obs")

# Create a correlation plot
corrplot(corr_matrix, method = "circle")

# mod_df <-
#     call_df %>%
#     dplyr::left_join(
#         clim_ts,
#         by = c("district", "date")
#     )
#
# aggreg_df <-
#     mod_df %>%
#     # dplyr::filter(district %in% c("02", "07")) %>%
#     dplyr::filter(district %in% c("02")) %>%
#     dplyr::group_by(date, district, seniority) %>%
#     dplyr::summarise(
#         dplyr::across(c(out_pct:z), list(mean = ~ mean(., na.rm = TRUE), sd = ~ sd(., na.rm = TRUE)))
#         )
