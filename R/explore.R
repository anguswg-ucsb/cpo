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
  # dplyr::group_by(date, seniority)

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
#
#
# # ---- Correlation plot ----
# library(corrplot)
# library(datasets)
# data(iris)
# myplot <- function(x, y, ...) {
#     points(x, y, ...)
#     abline(lm(y ~ x), col = "red")
# }
#
# pairs(iris)
# # Correlation between climate variables
# # corr_matrix <- cor(dplyr::select(mod_df, -district, -date), use = "pairwise.complete.obs")
#
# pairs(dplyr::select(mod_df,-c(1:6)))
# aggreg_lst <-
#     aggreg_df %>%
#     dplyr::group_by(seniority) %>%
#     dplyr::group_split()
#
# plt = list()
#
# nms <- aggreg_df$seniority %>% unique()
# plt_lst <- lapply(1:length(aggreg_lst), function(i) {
#     message(paste0(nms[i]))
#     # aggreg_lst[[i]] %>%
#     #     dplyr::select(aggreg_lst[[i]], contains("mean"))
#     corr_matrix <- cor(dplyr::select(aggreg_lst[[i]], contains("mean")), use = "pairwise.complete.obs")
#
#     # corr_matrix <- cor(dplyr::select(aggreg_lst[[i]],-c(1:6)), use = "pairwise.complete.obs")
#
#     # Create a correlation plot
#     cor_plot <- corrplot(corr_matrix, method = "circle")
#     cor_plot
# }) %>%
#     stats::setNames(nms)
# plt_lst$median
# plt_lst$junior
# for (i in 1:length(aggreg_lst)) {
#     # i = 1
#     aggreg_lst[[i]]
#     corr_matrix <- cor(dplyr::select(aggreg_lst[[i]],-c(1:6)), use = "pairwise.complete.obs")
#
#     # Create a correlation plot
#     cor_plot <- corrplot(corr_matrix, method = "circle")
#
#     plt[[i]] <- cor_plot
#
# }


