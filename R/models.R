
library(tidymodels)
library(dplyr)
library(tidyr)
library(ggplot2)

library(glmnet)
library(doParallel)
library(workflowsets)
library(finetune)
library(themis)

# load data
source("R/get_model_data.R")
source("R/plot_utils.R")
mod_df2 <-
  mod_df %>%
  dplyr::slice(1:100000)
out_cor <-
  mod_df %>%
  dplyr::select(where(is.numeric)) %>%
  # dplyr::select(pts_for, pts_against, point_diff) %>%
  # dplyr::select(-season, -contains("opp")) %>%
  cor(use =  "pairwise.complete.obs") %>%
  round(2) %>%
  reshape2::melt()

# spread_cor_plot <-
ggplot(
  data = out_cor, aes(
    x    = Var1,
    y    = Var2,
    fill = value
  )
) +
  geom_tile() +
  geom_text(
    aes(Var2, Var1, label = value),
    color = "black",
    size  = 4
  ) +
  scale_fill_gradient2(low="darkred", high="midnightblue", guide="colorbar") +
  theme(
    axis.text.x = element_text(angle = -45)
  )
