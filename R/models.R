
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

# dists <-
#   mod_df %>%
#   dplyr::select(basin, district) %>%
#   dplyr::group_by(district) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup()

# 1 - 9
# 19, 17
# 21, 22
# 31
# 38
#  64, 68
# 70, 72

dists <- c(
  "01", "02", "03", "04", "05",
  "06", "07", "08", "09", "17",
  "19", "21", "22", "31", "38",
  "64", "68", "70", "72"
           )


out_df <-
  mod_df %>%
  # dplyr::filter(district %in% dists) %>%
  # dplyr::filter(seniority != "median") %>%
  # dplyr::group_by(wdid, gnis_id) %>%
  dplyr::mutate(
    # median_date = mean(as.Date(approp_date))
    month = lubridate::month(date),
    seniority = dplyr::case_when(
      seniority %in% c("median") & as.Date(approp_date) >= "1930-01-01" ~ "junior",
      seniority %in% c("median") & as.Date(approp_date) < "1930-01-01"  ~ "senior",
      TRUE                                                              ~ seniority
    )
  ) %>%
  dplyr::filter(month %in% c(5, 6, 7, 8, 9)) %>%
  na.omit() %>%
  dplyr::select(basin, seniority, date, out_pct, pr, tmmn, tmmx, eddi1y, contains("swe"))  %>%
  # dplyr::select(basin, seniority, out_pct, pr, tmmn, tmmx, spi1y, contains("eddi"),  contains("swe"))  %>%
  # dplyr::mutate(dplyr::across(where(is.numeric), round, 4))
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 4))) %>%
  # dplyr::select(basin, district, date, wdid, gnis_id, approp_date, seniority, out_pct, pr, tmmn, tmmx, contains("eddi"), contains("swe"))
  # dplyr::select(-month)
  # dplyr::select(-month, -wdid, -gnis_id, -district) %>%
  dplyr::mutate(
    out_pct = dplyr::case_when(
      out_pct > 0 ~ "1",
      TRUE        ~ "0"
    ),
    out_pct = factor(out_pct, levels = c("1", "0"))
  ) %>%
  dplyr::select(-date)

out_lst <-
  out_df %>%
  dplyr::group_by(basin) %>%
  dplyr::group_split()

out_lst[[3]]

# meds <-out_df %>%
#   dplyr::filter(gnis_id == "182928") %>%
#   dplyr::select(1:8) %>%
#   dplyr::filter(seniority == "median") %>%
#   tidyr::pivot_wider( names_from = "seniority",values_from = "approp_date")
# non_meds <-out_df %>%
#   # dplyr::filter(gnis_id == "182928") %>%
#   dplyr::select(1:8) %>%
#   # dplyr::filter(seniority != "median") %>%
#   tidyr::pivot_wider(
#     # id_cols = c(-wdid),
#     names_from = "seniority",
#     values_from = "approp_date") %>%
#   dplyr::group_by(gnis_id) %>%
#   tidyr::fill(c(senior, junior, median), .direction = "updown") %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(date, gnis_id) %>%
#   dplyr::slice(1) %>%
#   dplyr::mutate(
#     j_diff = as.numeric(abs(as.Date(median) - as.Date(junior))),
#     s_diff = as.numeric(abs(as.Date(median) - as.Date(senior))),
#     new_right = dplyr::case_when(
#       s_diff >= j_diff ~ "junior",
#       TRUE            ~ "senior"))
# tmp2 <-out_df %>%
#   dplyr::left_join(
#     dplyr::select(non_meds, basin, district, wdid, gnis_id, date, new_right) ,
#     by = c("wdid", "gnis_id"))
#   dplyr::select(non_meds, basin, district, wdid, gnis_id, date, new_right)

# -------------------------
# ---- Test/Train data ----
# -------------------------

# Set random seed
set.seed(234)

# split data for train/test, stratify by quantiles of fantasy points
# nfl_split <- initial_split(nfl_df, strata = season)
# out_split <- rsample::initial_split(out_df, strata = seniority)
out_split <- rsample::initial_split(out_lst[[6]], strata = seniority)

# training data split
out_train <- rsample::training(out_split)

# testinng data split
out_test  <- rsample::testing(out_split)

out_train %>%
  count(seniority)

out_train %>%
  count(seniority) %>%
  dplyr::mutate(
    total_games = sum(n, na.rm = T),
    pct_total   = 100*(n/total_games)
  ) %>%
  ggplot() +
  geom_col(aes(x = seniority, y = pct_total)) +
  scale_y_continuous(limits = c(0, 100))

out_test %>%
  count(seniority)

out_test %>%
  count(seniority) %>%
  dplyr::mutate(
    total_games = sum(n, na.rm = T),
    pct_total   = 100*(n/total_games)
  ) %>%
  ggplot() +
  geom_col(aes(x = seniority, y = pct_total)) +
  scale_y_continuous(limits = c(0, 100))

# *****************
# ---- Recipes ----
# *****************

usemodels::use_ranger(out_pct~., data = out_train)
usemodels::use_glmnet(out_pct~., data = out_train)
usemodels::use_kknn(out_pct~., data = out_train)
usemodels::use_xgboost(out_pct~., data = out_train)

# Data preprocessing
logger::log_info("Data preprocessing...")

# GLMNET Recipe
glmnet_recipe <-
  recipes::recipe(
    formula = out_pct ~ .,
    data    = out_train
  ) %>%
  recipes::update_role(
    basin,
    new_role = "ID"
  ) %>%
  # recipes::step_naomit(recipes::all_predictors()) %>%
  recipes::step_string2factor(one_of( "seniority")) %>%
  recipes::step_novel(recipes::all_nominal_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors()) %>%
  # recipes::step_date(date) %>%
  recipes::step_zv(recipes::all_predictors()) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

# tmp <-
#   glmnet_recipe %>%
#   recipes::prep() %>%
#   recipes::bake(new_data = NULL)

# Ranger Random forest recipe
ranger_recipe <-
  recipes::recipe(
    formula = out_pct ~ .,
    data    = out_train
    ) %>%
  recipes::update_role(
    basin,
    new_role = "ID"
  ) %>%
  recipes::step_string2factor(one_of( "seniority")) %>%
  recipes::step_novel(recipes::all_nominal_predictors()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors()) %>%
  recipes::step_zv(recipes::all_predictors()) %>%
  # recipes::step_date(date) %>%
  recipes::step_normalize(recipes::all_numeric_predictors())

kknn_recipe <-
  recipes::recipe(
    formula = out_pct ~ .,
    data    = out_train
  ) %>%
  recipes::update_role(
    basin,
    new_role = "ID"
  ) %>%
  # recipes::step_date(date) %>%
  recipes::step_zv(all_predictors()) %>%
  recipes::step_normalize(all_numeric_predictors())

# XGBoost trees
xgboost_recipe <-
  recipes::recipe(
    formula = out_pct ~ .,
    data    = out_train
  ) %>%
  recipes::update_role(
    basin, new_role = "ID"
  ) %>%
  step_string2factor(one_of("seniority")) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # themis::step_smote(nfl_finish) %>%
  step_zv(all_predictors())

# # GLMNET model specifications
# glmnet_spec <-
#   parsnip::linear_reg(
#     penalty = tune::tune(),
#     mixture = tune::tune()
#     ) %>%
#   parsnip::set_mode("regression") %>%
#   parsnip::set_engine("glmnet")

# GLMNET model specifications
glmnet_spec <-
  parsnip::logistic_reg(
    penalty = tune::tune(),
    mixture = tune::tune()
  ) %>%
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("glmnet")


# ranger RF model specifications
ranger_spec <-
  parsnip::rand_forest(
    mtry  = tune::tune(),
    min_n = tune::tune(),
    trees = 1000
    ) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("ranger", importance = "permutation")

# K nearest neighbors
kknn_spec <-
  parsnip::nearest_neighbor(
    neighbors   = tune::tune(),
    weight_func = tune::tune()
    ) %>%
  parsnip::set_mode("regression") %>%
  parsnip::set_engine("kknn")

# xgboost_spec <-
#   boost_tree(
#     trees = tune(),
#     min_n = tune(),
#     tree_depth = tune(),
#     learn_rate = tune(),
#     loss_reduction = tune(),
#     sample_size = tune()) %>%
#   set_mode("regression") %>%
#   parsnip::set_engine("xgboost", importance = "permutation")

xgboost_spec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune()) %>%
  set_mode("classification") %>%
  parsnip::set_engine("xgboost", importance = "permutation")

# ********************************
# ---- Cross Validation folds ----
# ********************************

# Set seed for resampling
set.seed(432)

# CV folds
out_folds <- rsample::vfold_cv(out_train, v = 5, strata = seniority)
# nfl_folds <- rsample::bootstraps(nfl_train, strata = win)

# ---- Workflow set of models ----
out_wfs <-
  workflowsets::workflow_set(
    preproc = list(
      glmnet_rec      = glmnet_recipe,
      # kknn_rec        = kknn_recipe,
      xgboost_rec = xgboost_recipe
      # ranger_rec      = ranger_recipe
    ),
    models  = list(
      glmnet     = glmnet_spec,
      # kknn = kknn_spec,
      xgboost = xgboost_spec
      # ranger     = ranger_spec
    ),
    cross = F
  )

# Choose metrics
# my_metrics <- yardstick::metric_set(rsq, rmse, mae)
my_metrics <- yardstick::metric_set(roc_auc, accuracy, mn_log_loss)

# Set up parallelization, using computer's other cores
parallel::detectCores(logical = FALSE)
modeltime::parallel_start(6, .method = "parallel")

# Set Random seed
set.seed(589)

# # Tune models in workflowset
out_wfs <-
  out_wfs %>%
  workflowsets::workflow_map(
    "tune_grid",
    resamples = out_folds ,
    grid      = 20,
    metrics   = my_metrics,
    control   = control_grid(
      verbose   = TRUE,
      save_pred = TRUE),
    verbose   = TRUE
  )

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

rank_results(out_wfs)
# New facet label names for variable
# metric_labs <- c("MAE", "RMSE", "R2")
# names(metric_labs) <- c("MAE", "RMSE", "R2")

# Comparing Accuracy and ROC AUC of 7 models
reg_mod_comp_plot <-
  out_wfs %>%
  # out_wfs[c(1:6),] %>%
  autoplot() +
  labs(
    # col = "Models",
    col = "",
    title    = "Regression Model comparisons"
  )

reg_mod_comp_plot
for (i in 1:length(out_wfs$wflow_id)) {
# i = 1
model       <- out_wfs$wflow_id[i]
model_name  <- out_wfs$info[[i]]$model

model

mod_results <-
  out_wfs %>%
  workflowsets::extract_workflow_set_result(model)

# Extract workflows
mod_workflow <-
  out_wfs %>%
  extract_workflow(model)

# Model Engine text
model_engine <- mod_workflow$fit$actions$model$spec$engine

# Model Engine text
model_mode <- mod_workflow$fit$actions$model$spec$mode

logger::log_info("\n\nExtracting workflow & finalizing model fit:\n  --->  {model_name} - {model_mode}")
# mod_results$.metrics[[1]]

# print(select_best(mod_results, metric = "rmse"))
# print(select_best(mod_results, metric = "rsq"))
select_best(mod_results, metric = "roc_auc")
# rm(mod_workflow_fit)
# Finalize workflow fit
mod_workflow_fit <-
  mod_workflow %>%
  finalize_workflow(select_best(mod_results, metric = "roc_auc")) %>%
  # finalize_workflow(select_best(mod_results, metric = "rsq")) %>%
  fit(data = out_train)

# Fit model to split train/test data
mod_last_fit <- tune::last_fit(mod_workflow_fit, out_split)

# print(tune::collect_metrics(mod_results)$mean)
print(tune::collect_metrics(mod_last_fit))

# Extract & save final fit to use for predictions
mod_final_fit <- mod_last_fit$.workflow[[1]]

# Resampled CV Fold AUC ROC Curve
resample_roc_plot <-
  mod_results %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(out_pct, .pred_1) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal() +
  labs(
    title    = paste0("AUC-ROC Curve - ", model_name),
    subtitle = "Resample results from 10 Fold Cross Validation",
    x        = "1 - Specificity",
    y        = "Sensitivity"
  )
# save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
resample_plot_path  <-   paste0(ml_data_path, "plots/win_class_resample_aucroc_", model_name, ".png")
logger::log_info("\n\nSaving Resamples AUC-ROC curve: \n{resample_plot_path}")

# Export plot
ggsave(
  resample_plot_path,
  plot   = resample_roc_plot
)

# Plot variable importance if avaliable for model
tryCatch(
  {
    vip_plot <-
    mod_last_fit %>%
      pluck(".workflow", 1) %>%
      extract_fit_parsnip() %>%
      # vip::vi()
      # vip::vip() +
      vip::vip(num_features = 70) +
      # vip::vip(num_features = 30) +
      labs(
        title    = paste0("Variable Importance Scores - ", model_name),
        subtitle = "Regression",
        y        = "Importance",
        x        = "Variables"
      )

    # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
    vip_path  <-   paste0(ml_data_path, "plots/win_class_vip_", model_name, ".png")
    logger::log_info("\n\nSaving Variable Importance plot:\n{vip_path}")

    # Export plot
    ggsave(
      vip_path,
      plot   = vip_plot
    )
  },
  error = function(e) {
    logger::log_error('Variable Importance is not avalaible for {model_name} model')
    logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
    logger::log_error(message(e))
    # stop()
  }
)

}

