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

