
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
source("R/utils.R")

# dists <- c(
  # "01", "02", "03", "04", "05",
  # "06", "07", "08", "09", "17",
  # "19", "21", "22", "31", "38",
  # "64", "68", "70", "72"
  #          )

mod_month %>%
  dplyr::filter(!is.na(station_code))
# start_lag = 1
# end_lag = 5

junior_week <-
  mod_df %>%
  dplyr::filter(seniority == "junior") %>%
  # dplyr::filter(!is.na(station_code)) %>%
  dplyr::mutate(
    year = lubridate::year(date)
  ) %>%
  dplyr::group_by(district, year) %>%
  timetk::tk_augment_lags(swe, .lags = seq(start_lag*4, end_lag*4, by = 4)) %>%
  dplyr::ungroup() %>%
  dplyr::filter_at(vars(c("swe_lag20")), any_vars(!is.na(.))) %>%
  dplyr::select(basin, district, date, wdid, approp_date, out_pct, out, out_count_week, eddi1y, contains("swe_lag"))

junior_lst <-
  junior_week %>%
  dplyr::group_by(basin) %>%
  dplyr::group_split()

senior_week <-
  mod_df %>%
  dplyr::filter(seniority == "senior") %>%
  dplyr::select(basin, district, date, wdid, gnis_id, station_code,
              approp_date,
              # seniority,
              out,
              out_count_week,
              # swe,
              apr_swe, may_swe, ep_50)

junior_month <-
  mod_month %>%
  dplyr::filter(seniority == "junior") %>%
  dplyr::mutate(
    month = lubridate::month(date)
  ) %>%
  # dplyr::filter(month %in% c(6, 7, 8, 9)) %>%
  dplyr::select(basin, district, date, wdid, gnis_id, station_code,
                approp_date,
                # seniority,
                out_pct,
                out,
                out_count_month,
                swe,
                apr_swe, may_swe, ep_50) %>%
  dplyr::filter(!is.na(ep_50))

out_df <-
  mod_df %>%
  # dplyr::filter(seniority != "median") %>%
  dplyr::filter(seniority == "junior") %>%
  dplyr::mutate(
    month = lubridate::month(date)
  ) %>%
  dplyr::filter(month %in% c(5, 6, 7, 8, 9)) %>%
  dplyr::filter(!is.na(ep_50)) %>%
  na.omit() %>%
  # dplyr::group_by(basin, district, seniority)
  # dplyr::group_by(basin, district, seniority, date) %>%
  # dplyr::summarise(
  #   dplyr::across(where(is.numeric), \(x) mean(x))
  # ) %>%
  dplyr::select(basin, district, seniority, date,
                out_pct, out,
                swe, eddi1y, ep_50
                # pr, tmmn, tmmx, spi90d, spi1y, eddi90d, eddi1y,
                # swe, mar_swe, apr_swe, may_swe
                )  %>%
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 4))) %>%
  dplyr::select(-date)
  # dplyr::rename(out_pct = out)

# out_df %>%
#   dplyr::select(-seniority) %>%
#   distinct()

# tmp %>%
#   # tidyr::pivot_longer(cols = c(approp_date)) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_col(ggplot2::aes(x = date, y = out_pct)) +
#   ggplot2::facet_wrap(~approp_date, nrow = 10)
#   tmp
#
  # out_df <-
  # out_df %>%
  # dplyr::mutate(
  #   out = dplyr::case_when(
  #     out_pct > 0 ~ "1",
  #     TRUE        ~ "0"
  #   ),
  #   out = factor(out, levels = c("1", "0"))
  # )

out_df$out_pct %>% hist()


out_lst <-
  out_df %>%
  dplyr::group_by(basin) %>%
  dplyr::group_split()

# ---- Basin Models ----

for (i in 1:length(out_lst)) {
  i = 6
  # basin_name <- unique(out_lst[[i]]$basin)[1]
  basin_name <- unique(junior_lst[[i]]$basin)[1]

  logger::log_info("{basin_name}")

  # # Modeling data subset
  # basin_df <-
  #   out_lst[[i]] %>%
  #   dplyr::select(-basin)
  # class_mods <- class_wfs


  class_mods <- make_models(
    # df         = out_lst[[i]],
    df         = junior_lst[[i]],
    # df         = dplyr::select(out_lst[[i]], -basin),
    target_var = "out",
    basin_name = basin_name,
    model_type = "classification",
    # strata     = "district",
    nfolds     = 5,
    ncores     = 6,
    save_path  = paste0("D:/cpo/models/")
  )
  dplyr::select(junior_lst[[i]], -out_pct, -out)
  dplyr::rename(
    dplyr::select(junior_lst[[i]], -out_pct),
    out_pct = out_count_week
  )
  reg_mods <- make_models(
    # df         = out_lst[[i]],
    df         =   dplyr::rename(
                        dplyr::select(junior_lst[[i]], -out_pct),
                        out_pct = out_count_week
                      ),
    # df         = dplyr::select(out_lst[[i]], -basin),
    target_var = "out_pct",
    basin_name = basin_name,
    model_type = "regression",
    # strata     = "district",
    nfolds     = 5,
    ncores     = 6,
    save_path  = paste0("D:/cpo/models/")
  )

  # classification model comparisons plot
  class_mod_comp <- make_comp_plot(
    wfs        = class_mods$workflowset,
    basin_name = basin_name,
    model_type = "classification",
    save_path  = "D:/cpo/models/"
  )

  # regression model comparisons plot
  reg_mod_comp <- make_comp_plot(
    wfs        = reg_mods$workflowset,
    basin_name = basin_name,
    model_type = "regression",
    save_path  = paste0("D:/cpo/models/")
    # save_path  = "D:/cpo/models/plots/"
  )

  # save_path = paste0(
  #   "D:/cpo/models/workflowsets/",
  #   tolower(gsub(" ", "_", df$basin[1])),
  #   ifelse(model_type == "classification", "_class_", "_reg_"),
  #   "workflowset.rds"
  #   )
}





# aoi <- AOI::aoi_get("Santa Barbara")
#
# elev10 <- elevatr::get_elev_raster(aoi, z = 10)
# elev8 <- elevatr::get_elev_raster(aoi, z = 8)
# elev6 <- elevatr::get_elev_raster(aoi, z = 6)
# raster::writeRaster(elev10, "santa_baraba_high_res_dem.tif")
# raster::writeRaster(elev8, "santa_baraba_med_res_dem.tif")
# raster::writeRaster(elev6, "santa_baraba_low_res_dem.tif")
#
# mapview::mapview(aoi) + elev
# result_summary <- function(
#     wfs,
#     model_name,
#     model_type,
#     train_data,
#     test_data,
#     split_data,
#     save_path
#     ) {
#
#   # Table of model ranks
#   mod_rank <- rank_results(wfs)
#   # train_data = out_train
#   # test_data = out_test
#   # split_data = out_split
#
#   # fit_mods <- lapply(1:length(wfs$wflow_id), function(z) {
#   fit_mods <- lapply(1:1, function(z) {
#
#     # z = 1
#     message("Summarizing model ", z, "/", length(wfs$wflow_id))
#
#     mod_recipe     <- wfs$wflow_id[z]
#     simple_name <- sub(".+_", "", mod_recipe)
#     # relative model rankings
#     min_rank <- min(filter(mod_rank, wflow_id == mod_recipe)$rank)
#     max_rank <- max(filter(mod_rank, wflow_id == mod_recipe)$rank)
#     #
#
#     clean_rec_name <- paste0(mod_recipe, "_", ifelse(model_type == "classification", "class", "reg"))
#
#     mod_results <-
#       wfs %>%
#       extract_workflow_set_result(mod_recipe)
#     # extract_workflow_set_result("log_rec_lasso")
#
#     # Extract workflows
#     mod_workflow <-
#       wfs %>%
#       extract_workflow(mod_recipe)
#
#     logger::log_info("Fitting final model...\nPreprocessor: {mod_recipe}")
#
#     met = ifelse(model_type == "classification", "roc_auc", "rsq")
#
#     # Finalize workflow fit
#     mod_workflow_fit <-
#       mod_workflow %>%
#       finalize_workflow(select_best(mod_results, metric = met)) %>%
#       fit(data = train_data)
#
#     # Fit model to split train/test data
#     mod_last_fit <- last_fit(mod_workflow_fit, split_data)
#     # print(collect_metrics(mod_last_fit))
#
#     # Extract & save final fit to use for predictions
#     mod_final_fit <- mod_last_fit$.workflow[[1]]
#
#     logger::log_info("Collecting final metrics...")
#
#     # # Resampled CV Fold AUC ROC Curve0-1 <-
#     # roc_plot <-
#     #   mod_results %>%
#     #   collect_predictions() %>%
#     #   group_by(id) %>%
#     #   roc_curve(out, .pred_1) %>%
#     #   ggplot(aes(1 - specificity, sensitivity, color = id)) +
#     #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
#     #   geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
#     #   coord_equal() +
#     #   labs(
#     #     title    = paste0("AUC-ROC Curve - ", model_name, "(", mod_recipe, ")"),
#     #     subtitle = "Resample results from 10 Fold Cross Validation",
#     #     x        = "1 - Specificity",
#     #     y        = "Sensitivity"
#     #   )
#
#     # training set predictions
#     mod_train <-
#       predict(mod_final_fit, train_data) %>%
#       bind_cols(dplyr::select(train_data, out)) # Add the true outcome data back in
#
#     # testing set predictions
#     mod_test <-
#       predict(mod_final_fit, test_data) %>%
#       bind_cols(dplyr::select(test_data, out))
#
#     # final_lst = c(
#     #   "training"            = mod_train,
#     #   "testing"             = mod_test,
#     #   "variable_importance" = vip_table
#     # )
#
#     if(model_type == "classification") {
#
#       multi_metric <- yardstick::metric_set(roc_auc, pr_auc, accuracy)
#
#     } else if(model_type == "regression") {
#
#       multi_metric <- yardstick::metric_set(rmse, rsq, mae)
#     }
#
#     # est = ifelse(model_type == "classification", ".pred_class", ".pred")
#
#     # Plot variable importance if avaliable for model
#     tryCatch(
#       {
#         # Variable importance dataframe
#         vip_table <-
#           mod_last_fit %>%
#           pluck(".workflow", 1) %>%
#           extract_fit_parsnip() %>%
#           vip::vi() %>%
#           mutate(
#             model_type     = model_type,
#             recipe         = clean_rec_name,
#             min_model_rank = min_rank,
#             max_model_rank = max_rank,
#             basin          = basin_name
#           )
#       },
#       error = function(e) {
#         logger::log_error('Variable Importance is not avalaible for {simple_name} - {model_type}')
#         logger::log_error('Setting vip_table to NULL')
#         logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
#         logger::log_error(message(e))
#
#         vip_table <- NULL
#
#       }
#     )
#     # # Variable importance dataframe
#     # vip_table <-
#     #   mod_last_fit %>%
#     #   pluck(".workflow", 1) %>%
#     #   extract_fit_parsnip() %>%
#     #   vip::vi() %>%
#     #   mutate(
#     #     model_type     = model_type,
#     #     recipe         = clean_rec_name,
#     #     min_model_rank = min_rank,
#     #     max_model_rank = max_rank,
#     #     basin          = basin_name
#     #   )
#
    # # Plot variable importance if avaliable for model
    # tryCatch(
    #   {
    #
    #   # Resampled CV Fold AUC ROC Curve0-1 <-
    #   roc_plot <-
    #     mod_results %>%
    #     collect_predictions() %>%
    #     group_by(id) %>%
    #     roc_curve(out, .pred_1) %>%
    #     ggplot(aes(1 - specificity, sensitivity, color = id)) +
    #     geom_abline(lty = 2, color = "gray80", size = 1.5) +
    #     geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
    #     coord_equal() +
    #     labs(
    #       title    = paste0("AUC-ROC Curve - ", model_name),
    #       subtitle = "Resample results from 10 Fold Cross Validation",
    #       x        = "1 - Specificity",
    #       y        = "Sensitivity"
    #     )
    #
    #     # # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
    #     # vip_path  <-   paste0(ml_data_path, "plots/win_class_vip_", model_name, ".png")
    #     logger::log_info('\n\nSaving ROC AUC Importance plot:\n{paste0(
    #                       save_path,
    #                       tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
    #                       # ifelse(model_type == "classification", "_class_", "_reg_"),
    #                            "_", simple_name,
    #                       "_roc_auc.plot"
    #                       )}')
    #
    #     # Export plot
    #     ggplot2::ggsave(
    #       paste0(
    #         save_path,
    #         tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
    #         "_", simple_name,
    #         # ifelse(model_type == "classification", "_class_", "_reg_"),
    #         "_roc_auc_plot.png"
    #       ),
    #       plot   = roc_plot
    #     )
    #   },
    #   error = function(e) {
    #     logger::log_error('ROC AUC not avalaible for {simple_name} - {model_type}')
    #     logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
    #     logger::log_error(message(e))
    #     # stop()
    #   }
    # )
    #
    # # Plot variable importance if avaliable for model
    # tryCatch(
    #   {
    #     vip_plot <-
    #       mod_last_fit %>%
    #       purrr::pluck(".workflow", 1) %>%
    #       extract_fit_parsnip() %>%
    #       vip::vip(num_features = 70) +
    #       labs(
    #         title    = paste0("Variable Importance Scores - ", model_name),
    #         subtitle = paste0(simple_name, " - ", model_type),
    #         y        = "Importance",
    #         x        = "Variables"
    #       )
    #
    #     # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
    #     # vip_path  <-   paste0(ml_data_path, "plots/win_class_vip_", model_name, ".png")
    #     logger::log_info('\n\nSaving Variable Importance plot:\n{paste0(
    #         save_path,
    #         tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
    #         "_", simple_name,
    #         ifelse(model_type == "classification", "_class_", "_reg_"),
    #         "vip_plot.png"
    #       )}')
    #
    #     # Export plot
    #     ggplot2::ggsave(
    #       paste0(
    #         save_path,
    #         tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
    #         "_", simple_name,
    #         ifelse(model_type == "classification", "_class_", "_reg_"),
    #         "vip_plot.png"
    #       ),
    #       plot   = vip_plot
    #     )
    #
    #   },
    #   error = function(e) {
    #     logger::log_error('Variable Importance is not avalaible for {simple_name} - {model_type}')
    #     logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
    #     logger::log_error(message(e))
    #     # stop()
    #   }
    # )
#
#     # # Train metrics
#     # train_metrics <-
#     #   mod_train %>%
#     #   multi_metric(truth = out, estimate = .pred) %>%
#     #   mutate(
#     #     data           = "train",
#     #     model          = mod_recipe,
#     #     recipe         = clean_rec_name,
#     #     min_model_rank = min_rank,
#     #     max_model_rank = max_rank,
#     #     basin          =  tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name))
#     #   )
#     #
#     # # Test metrics
#     # test_metrics <-
#     #   mod_test %>%
#     #   multi_metric(truth = out, estimate = .pred) %>%
#     #   mutate(
#     #     data           = "test",
#     #     model          = mod_recipe,
#     #     recipe         = clean_rec_name,
#     #     min_model_rank = min_rank,
#     #     max_model_rank = max_rank,
#     #     basin          =  tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name))
#     #   )
#
#     logger::log_info("Creating confusion matrix...")
#
#     cm_out <-
#       mod_results %>%
#       collect_predictions()
#
#     cm <- yardstick::conf_mat(cm_out, out, .pred_class)
#
#     # final data outputs
#     final_lst = list(
#       "training"            = mod_train,
#       "testing"             = mod_test,
#       "variable_importance" = vip_table,
#       "model_results"       = mod_results,
#       "last_fit"            = mod_last_fit,
#       "final_fit"           = mod_final_fit,
#       "confusion_matrix"    = cm
#     )
#
#     logger::log_info("Tidying model metrics... ")
#
#     final_lst
#
#     # Model metrics
#     # metrics_df          <- bind_rows(train_metrics, test_metrics)
#
#   }) %>%
#     stats::setNames(c(wfs$wflow_id))
#
#   return(fit_mods)
#
# }
#
#   reg_metrics_lst <- list()
#   vip_lst         <- list()
#   vip_rank_lst    <- list()
#
#   mod_recipe     <- wfs$wflow_id[z]
#
#   ranking <- mod_rank %>%
#     filter(wflow_id == mod_recipe)
#
#   # relative model rankings
#   min_rank <- min(ranking$rank)
#   max_rank <- max(ranking$rank)
#
#   clean_rec_name <- gsub("_rec_lasso", "" ,mod_recipe)
#
#   mod_results <-
#     short_models %>%
#     extract_workflow_set_result(mod_recipe)
#   # extract_workflow_set_result("log_rec_lasso")
#
#   # Extract workflows
#   mod_workflow <-
#     short_models %>%
#     extract_workflow(mod_recipe)
#
#   print(select_best(mod_results, metric = "rsq"))
#   print(select_best(mod_results, metric = "rmse"))
#
#   logger::log_info("Fitting final model...\nPreprocessor: {mod_recipe}")
#
#   # Finalize workflow fit
#   mod_workflow_fit <-
#     mod_workflow %>%
#     finalize_workflow(select_best(mod_results, metric = "rsq")) %>%
#     fit(data = short_train)
#
#   # Fit model to split train/test data
#   mod_last_fit <- last_fit(mod_workflow_fit, short_split)
#   print(collect_metrics(mod_last_fit))
#
#   # Extract & save final fit to use for predictions
#   mod_final_fit <- mod_last_fit$.workflow[[1]]
#
#   logger::log_info("Collecting final metrics...")
#
#   # Variable importance plot
#   vip_plot <-
#     mod_last_fit %>%
#     pluck(".workflow", 1) %>%
#     extract_fit_parsnip() %>%
#     vip::vip(num_features = 40) +
#     labs(
#       title    = paste0("Variable Importance Scores - ", stringr::str_to_title(
#         gsub("_", " ", basin_name)
#       )),
#       subtitle = "LASSO Regression",
#       caption  = paste0("Preprocessing recipe: ", clean_rec_name),
#       x        = "Importance",
#       y        = "Variables"
#     )
#
#   # training set predictions
#   mod_train <-
#     predict(mod_final_fit, short_train) %>%
#     bind_cols(dplyr::select(short_train, short)) # Add the true outcome data back in
#
#   # testing set predictions
#   mod_test <-
#     predict(mod_final_fit, short_test) %>%
#     bind_cols(dplyr::select(short_test, short))
#
#   multi_metric <- metric_set(rmse, rsq, mae)
#
#   # Train metrics
#   train_metrics <-
#     mod_train %>%
#     multi_metric(truth = short, estimate = .pred) %>%
#     mutate(
#       data           = "train",
#       model          = "lasso_regression",
#       recipe         = clean_rec_name,
#       min_model_rank = min_rank,
#       max_model_rank = max_rank,
#       basin          = basin_name
#     )
#
#   # Test metrics
#   test_metrics <-
#     mod_test %>%
#     multi_metric(truth = short, estimate = .pred) %>%
#     mutate(
#       data           = "test",
#       model          = "lasso_regression",
#       recipe         = clean_rec_name,
#       min_model_rank = min_rank,
#       max_model_rank = max_rank,
#       basin          = basin_name
#     )
#
#
#   logger::log_info("Tidying model metrics... ")
#
#   # Model metrics
#   reg_metrics          <- bind_rows(train_metrics, test_metrics)
#   print(reg_metrics)
#
#   reg_metrics_lst[[z]] <- reg_metrics
#
#   # Variable importance dataframe
#   vip_table <-
#     mod_last_fit %>%
#     pluck(".workflow", 1) %>%
#     extract_fit_parsnip() %>%
#     vip::vi() %>%
#     mutate(
#       model          = "lasso_regression",
#       recipe         = clean_rec_name,
#       min_model_rank = min_rank,
#       max_model_rank = max_rank,
#       basin          = basin_name
#     )
#
#   vip_lst[[z]] <- vip_table
#
#   # Variable importance dataframe
#   vip_rank_table <-
#     mod_last_fit %>%
#     pluck(".workflow", 1) %>%
#     extract_fit_parsnip() %>%
#     vip::vi(rank = T) %>%
#     mutate(
#       model          = "lasso_regression",
#       recipe         = clean_rec_name,
#       min_model_rank = min_rank,
#       max_model_rank = max_rank,
#       basin          = basin_name
#     )
#
#   vip_rank_lst[[z]] <- vip_rank_table
#

#
#   logger::log_info("Splitting into training and testing data")
#
#   set.seed(234)
#
#   # split data for train/test, stratify by quantiles of fantasy points
#   out_split <- rsample::initial_split(basin_df, strata = seniority)
#
#   # training data split
#   out_train <- rsample::training(out_split)
#
#   # testinng data split
#   out_test  <- rsample::testing(out_split)
#
#   # ---- Recipes ----
#
#   logger::log_info("Data preprocessing...")
#
#   # GLMNET Recipe
#   glmnet_recipe <-
#     recipes::recipe(
#       formula = out_pct ~ .,
#       data    = out_train
#     ) %>%
#     recipes::update_role(
#       basin,
#       new_role = "ID"
#     ) %>%
#     recipes::step_string2factor(one_of( "seniority")) %>%
#     recipes::step_novel(recipes::all_nominal_predictors()) %>%
#     recipes::step_dummy(recipes::all_nominal_predictors()) %>%
#     # recipes::step_date(date) %>%
#     themis::step_smote(out_pct) %>%
#     recipes::step_zv(recipes::all_predictors()) %>%
#     recipes::step_normalize(recipes::all_numeric_predictors())
#
#   kknn_recipe <-
#     recipes::recipe(
#       formula = out_pct ~ .,
#       data    = out_train
#     ) %>%
#     recipes::update_role(
#       basin,
#       new_role = "ID"
#     ) %>%
#     recipes::step_zv(all_predictors()) %>%
#     recipes::step_normalize(all_numeric_predictors())
#
#   # XGBoost trees
#   xgboost_recipe <-
#     recipes::recipe(
#       formula = out_pct ~ .,
#       data    = out_train
#     ) %>%
#     recipes::update_role(
#       basin, new_role = "ID"
#     ) %>%
#     step_string2factor(one_of("seniority")) %>%
#     step_novel(all_nominal_predictors()) %>%
#     step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
#     themis::step_smote(out_pct) %>%
#     step_zv(all_predictors())
#
#   # ---- Specs ----
#   logger::log_info("Specifiying LASSO model\nMixture = 1")
#
#   # xgboost model - classification
#   xgboost_spec <-
#     boost_tree(
#       trees = tune(),
#       min_n = tune(),
#       tree_depth = tune(),
#       learn_rate = tune(),
#       loss_reduction = tune(),
#       sample_size = tune()) %>%
#     set_mode("classification") %>%
#     parsnip::set_engine("xgboost", importance = "permutation")
#
#   # GLMNET model specifications classification
#   glmnet_spec <-
#     parsnip::logistic_reg(
#       penalty = tune::tune(),
#       mixture = tune::tune()
#     ) %>%
#     parsnip::set_mode("classification") %>%
#     parsnip::set_engine("glmnet")
#
#   # ********************************
#   # ---- Cross Validation folds ----
#   # ********************************
#
#   # Set seed for resampling
#   set.seed(432)
#
#   # CV folds
#   out_folds <- rsample::vfold_cv(out_train, v = 5, strata = seniority)
#   # nfl_folds <- rsample::bootstraps(nfl_train, strata = win)
#
#   # ---- Workflow set of models ----
#   out_wfs <-
#     workflowsets::workflow_set(
#       preproc = list(
#         glmnet_rec      = glmnet_recipe,
#         # kknn_rec        = kknn_recipe,
#         xgboost_rec = xgboost_recipe
#       ),
#       models  = list(
#         glmnet     = glmnet_spec,
#         # kknn = kknn_spec,
#         xgboost = xgboost_spec
#       ),
#       cross = F
#     )
#
#   # Choose metrics
#   my_metrics <- yardstick::metric_set(roc_auc, accuracy, mn_log_loss)
#   # my_metrics <- yardstick::metric_set(rsq, rmse, mae)
#
#   # Set up parallelization, using computer's other cores
#   parallel::detectCores(logical = FALSE)
#   modeltime::parallel_start(6, .method = "parallel")
#
#   # Set Random seed
#   set.seed(589)
#
#   # Efficient Tuning of models in workflowset
#   out_wfs <-
#     out_wfs %>%
#     workflowsets::workflow_map(
#       "tune_race_anova",
#       resamples = out_folds,
#       # resamples = flow_roll_splits,
#       grid      = 20,
#       metrics   = my_metrics,
#       control = finetune::control_race(
#         verbose       = TRUE,
#         save_pred     = TRUE,
#         verbose_elim  = TRUE,
#         save_workflow = TRUE
#       ),
#       verbose   = TRUE
#     )
#
#   # Stop parrallelization
#   modeltime::parallel_stop()
#
#   rank_results(out_wfs)
#
#   # Model fit path
#   wfs_path <- "D:/cpo/models/workflowsets/"
#
#   logger::log_info("Saving {basin_name} workflowset:\n{wfs_path}")
#
#   # Save Workflows/Resample results/Final fitted model
#   saveRDS(
#     out_wfs,
#     paste0(wfs_path, "/", basin_name,  "_workflowset.rds")
#   )
#
#
#   # Plot path
#   plot_path <-   "D:/cpo/models/plots/"
#
#   logger::log_info("Saving {basin_name} model ranking plot:\n{plot_path}")
#
#   # Comparing rmse rsq AND mae OF ALL MODELS
#   reg_mod_comp_plot <-
#     out_wfs %>%
#     autoplot() +
#     geom_point(aes(color = wflow_id)) +
#     labs(
#       color = "Data Preprocessor",
#       title    = paste0(stringr::str_to_title( gsub("_", " ", basin_name)), " Model Comparisons"),
#       subtitle = "Classification models predicting days out of priority"
#     ) +
#     th
#   # theme(legend.position = "none")
#
#   # reg_mod_comp_plot
#
#   # Save plot
#   ggsave(
#     paste0(
#       plot_path, "/model_rank/", basin_name,
#       "_model_rank",
#       ".png"
#     ),
#     plot   = reg_mod_comp_plot,
#     width  = 52,
#     height = 28,
#     units  = "cm"
#   )
#
#   # Table of model ranks
#   mod_rank <- rank_results(short_models)
#
#   reg_metrics_lst <- list()
#   vip_lst         <- list()
#   vip_rank_lst    <- list()
#   for (z in 1:length(short_models$wflow_id)) {
#
#     mod_recipe     <- short_models$wflow_id[z]
#
#     ranking <- mod_rank %>%
#       filter(wflow_id == mod_recipe)
#
#     # relative model rankings
#     min_rank <- min(ranking$rank)
#     max_rank <- max(ranking$rank)
#
#     clean_rec_name <- gsub("_rec_lasso", "" ,mod_recipe)
#
#     mod_results <-
#       short_models %>%
#       extract_workflow_set_result(mod_recipe)
#     # extract_workflow_set_result("log_rec_lasso")
#
#     # Extract workflows
#     mod_workflow <-
#       short_models %>%
#       extract_workflow(mod_recipe)
#
#     print(select_best(mod_results, metric = "rsq"))
#     print(select_best(mod_results, metric = "rmse"))
#
#     logger::log_info("Fitting final model...\nPreprocessor: {mod_recipe}")
#
#     # Finalize workflow fit
#     mod_workflow_fit <-
#       mod_workflow %>%
#       finalize_workflow(select_best(mod_results, metric = "rsq")) %>%
#       fit(data = short_train)
#
#     # Fit model to split train/test data
#     mod_last_fit <- last_fit(mod_workflow_fit, short_split)
#     print(collect_metrics(mod_last_fit))
#
#     # Extract & save final fit to use for predictions
#     mod_final_fit <- mod_last_fit$.workflow[[1]]
#
#     logger::log_info("Collecting final metrics...")
#
#     # training set predictions
#     mod_train <-
#       predict(mod_final_fit, short_train) %>%
#       bind_cols(dplyr::select(short_train, short)) # Add the true outcome data back in
#
#     # testing set predictions
#     mod_test <-
#       predict(mod_final_fit, short_test) %>%
#       bind_cols(dplyr::select(short_test, short))
#
#     multi_metric <- metric_set(rmse, rsq, mae)
#
#     # Train metrics
#     train_metrics <-
#       mod_train %>%
#       multi_metric(truth = short, estimate = .pred) %>%
#       mutate(
#         data           = "train",
#         model          = "lasso_regression",
#         recipe         = clean_rec_name,
#         min_model_rank = min_rank,
#         max_model_rank = max_rank,
#         basin          = basin_name
#       )
#
#     # Test metrics
#     test_metrics <-
#       mod_test %>%
#       multi_metric(truth = short, estimate = .pred) %>%
#       mutate(
#         data           = "test",
#         model          = "lasso_regression",
#         recipe         = clean_rec_name,
#         min_model_rank = min_rank,
#         max_model_rank = max_rank,
#         basin          = basin_name
#       )
#
#
#     logger::log_info("Tidying model metrics... ")
#
#     # Model metrics
#     reg_metrics          <- bind_rows(train_metrics, test_metrics)
#     print(reg_metrics)
#
#     reg_metrics_lst[[z]] <- reg_metrics
#
#     # Variable importance dataframe
#     vip_table <-
#       mod_last_fit %>%
#       pluck(".workflow", 1) %>%
#       extract_fit_parsnip() %>%
#       vip::vi() %>%
#       mutate(
#         model          = "lasso_regression",
#         recipe         = clean_rec_name,
#         min_model_rank = min_rank,
#         max_model_rank = max_rank,
#         basin          = basin_name
#       )
#
#     vip_lst[[z]] <- vip_table
#
#     # Variable importance dataframe
#     vip_rank_table <-
#       mod_last_fit %>%
#       pluck(".workflow", 1) %>%
#       extract_fit_parsnip() %>%
#       vip::vi(rank = T) %>%
#       mutate(
#         model          = "lasso_regression",
#         recipe         = clean_rec_name,
#         min_model_rank = min_rank,
#         max_model_rank = max_rank,
#         basin          = basin_name
#       )
#
#     vip_rank_lst[[z]] <- vip_rank_table
#
#     # # VIP path
#     #  vip_path  <-  here::here("data/models/lasso/variable_importance/")
#     #
#     #  # Save Variable Importance table
#     #  saveRDS( mod_workflow_fit,  paste0(spec_path, "/workflows/",  basin_name, "_", clean_rec_name, "_lasso_workflow.rds"))
#
#     # Variable importance plot
#     vip_plot <-
#       mod_last_fit %>%
#       pluck(".workflow", 1) %>%
#       extract_fit_parsnip() %>%
#       vip::vip(num_features = 40) +
#       labs(
#         title    = paste0("Variable Importance Scores - ", stringr::str_to_title(
#           gsub("_", " ", basin_name)
#         )),
#         subtitle = "LASSO Regression",
#         caption  = paste0("Preprocessing recipe: ", clean_rec_name),
#         x        = "Importance",
#         y        = "Variables"
#       )
#
#     # Plot path
#     # plot_path <-  here::here("data/models/lasso/plots/")
#     # save Regression plot
#     logger::log_info("Plotting variable importance plot \nSaving:\n{paste0(plot_path, '/variable_importance/')}")
#
#     # Save plot
#     ggsave(
#       paste0(
#         plot_path, "/variable_importance/", basin_name, "_", clean_rec_name,
#         "_variable_importance",
#         ".png"
#       ),
#       plot   = vip_plot,
#       width  = 52,
#       height = 28,
#       units  = "cm"
#     )
#
#     # Predicted vs. Observed plot
#     reg_plot <-
#       collect_predictions(mod_last_fit) %>%
#       # mutate(.pred2 = (.pred)^2) %>%
#       ggplot(aes(short, .pred)) +
#       geom_abline(lty = 2, color = "gray50") +
#       geom_point(size = 2, alpha = 0.5, color = "midnightblue") +
#       coord_fixed() +
#       labs(
#         title = paste0(stringr::str_to_title(gsub("_", " ", basin_name)),
#                        " - Observed vs. Predicted Monthly Water Shortages"),
#         subtitle = "LASSO regression",
#         caption  = paste0("Preprocessing recipe: ", clean_rec_name),
#         x = "Observed",
#         y = "Prediction"
#       ) +
#       th
#
#
#     # save Regression plot
#     logger::log_info("Plotting observed vs. predicted \nSaving:\n{paste0(plot_path, '/regression/')}")
#
#     # Export plot
#     ggsave(
#       paste0(
#         plot_path, "/regression/", basin_name, "_", clean_rec_name,
#         "_regression.png"
#       ),
#       plot   = reg_plot,
#       width  = 52,
#       height = 28,
#       units  = "cm"
#     )
#
#     # Model fit path
#     spec_path <- here::here("data/models/lasso/spec/")
#
#     # Save Workflows/Resample results/Final fitted model
#     saveRDS(
#       mod_workflow_fit,
#       paste0(spec_path, "/workflows/",  basin_name, "_", clean_rec_name, "_lasso_workflow.rds"))
#
#     saveRDS(
#       mod_last_fit,
#       paste0(spec_path, "/resamples/", basin_name, "_", clean_rec_name, "_lasso_resamples.rds"))
#
#     saveRDS(
#       mod_final_fit,
#       paste0(spec_path, "/fit/",  basin_name, "_", clean_rec_name, "_lasso_model.rds"))
#
#   }
#
#   logger::log_info("Binding Variable importance list of tables: \n --> final_vip_lst")
#
#   # Variable importance list
#   final_vip_lst[[i]]    <- bind_rows(vip_lst)
#
#   # Variable importance list
#   final_vip_rank_lst[[i]]    <- bind_rows(vip_rank_lst)
#
#   logger::log_info("Binding list of model metrics: \n --> final_metric_lst")
#
#   # Metrics list
#   final_metric_lst[[i]] <- bind_rows(reg_metrics_lst)
#
# }
#
# # Stop parrallelization
# modeltime::parallel_stop()
#
# # # Final Variable importance scores
# # lasso_vip     <- bind_rows(final_vip_lst)
# #
# # # Final Variable importance scores
# # lasso_rank_vip     <- bind_rows(final_vip_rank_lst)
# #
# # lasso_metrics <- bind_rows(final_metric_lst)
# #
# # # Path to model data folder
# # mod_data_path <- here::here("data/models/lasso/")
# #
# # # Save Variable importance scores table
# # saveRDS(
# #   lasso_vip,
# #   paste0(mod_data_path, "/metrics/lasso_reg_variable_importance.rds")
# # )
# #
# # # Save Variable importance scores table
# # saveRDS(
# #   lasso_rank_vip,
# #   paste0(mod_data_path, "/metrics/lasso_reg_variable_importance_rank.rds")
# # )
# #
# # # Save overall model metrics table
# # saveRDS(
# #   lasso_metrics,
# #   paste0(mod_data_path, "/metrics/lasso_reg_model_metrics.rds")
# # )
# #
# #
# #     # Set random seed
# #     set.seed(234)
# #
# #     # split data for train/test, stratify by quantiles of fantasy points
# #     out_split <- rsample::initial_split(out_lst[[6]], strata = seniority)
# #     # out_split <- rsample::initial_split(out_df, strata = seniority)
# #
# #     # training data split
# #     out_train <- rsample::training(out_split)
# #
# #     # testinng data split
# #     out_test  <- rsample::testing(out_split)
# #
# #     # *****************
# #     # ---- Recipes ----
# #     # *****************
# #
# #     # Data preprocessing
# #     logger::log_info("Data preprocessing...")
# #
# #     # GLMNET Recipe
# #     glmnet_recipe <-
# #       recipes::recipe(
# #         formula = out_pct ~ .,
# #         data    = out_train
# #       ) %>%
# #       recipes::update_role(
# #         basin,
# #         new_role = "ID"
# #       ) %>%
# #       # recipes::step_naomit(recipes::all_predictors()) %>%
# #       recipes::step_string2factor(one_of( "seniority")) %>%
# #       recipes::step_novel(recipes::all_nominal_predictors()) %>%
# #       recipes::step_dummy(recipes::all_nominal_predictors()) %>%
# #       # recipes::step_date(date) %>%
# #       themis::step_smote(out_pct) %>%
# #       recipes::step_zv(recipes::all_predictors()) %>%
# #       recipes::step_normalize(recipes::all_numeric_predictors())
# #
    # kknn_recipe <-
    #   recipes::recipe(
    #     formula = out_pct ~ .,
    #     data    = out_train
    #   ) %>%
    #   recipes::update_role(
    #     basin,
    #     new_role = "ID"
    #   ) %>%
    #   # recipes::step_date(date) %>%
    #   recipes::step_zv(all_predictors()) %>%
    #   recipes::step_normalize(all_numeric_predictors())
# #
# #     # XGBoost trees
# #     xgboost_recipe <-
# #       recipes::recipe(
# #         formula = out_pct ~ .,
# #         data    = out_train
# #       ) %>%
# #       recipes::update_role(
# #         basin, new_role = "ID"
# #       ) %>%
# #       step_string2factor(one_of("seniority")) %>%
# #       step_novel(all_nominal_predictors()) %>%
# #       step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
# #       themis::step_smote(out_pct) %>%
# #       step_zv(all_predictors())
# #
# #     # ------------------------------
# #     # ---- Model specifications ----
# #     # ------------------------------
# #
# #     # xgboost model - classification
# #     xgboost_spec <-
# #       boost_tree(
# #         trees = tune(),
# #         min_n = tune(),
# #         tree_depth = tune(),
# #         learn_rate = tune(),
# #         loss_reduction = tune(),
# #         sample_size = tune()) %>%
# #       set_mode("classification") %>%
# #       parsnip::set_engine("xgboost", importance = "permutation")
# #
# #     # GLMNET model specifications classification
# #     glmnet_spec <-
# #       parsnip::logistic_reg(
# #         penalty = tune::tune(),
# #         mixture = tune::tune()
# #       ) %>%
# #       parsnip::set_mode("classification") %>%
# #       parsnip::set_engine("glmnet")
# #
# #     # ********************************
# #     # ---- Cross Validation folds ----
# #     # ********************************
# #
# #     # Set seed for resampling
# #     set.seed(432)
# #
# #     # CV folds
# #     out_folds <- rsample::vfold_cv(out_train, v = 5, strata = seniority)
# #     # nfl_folds <- rsample::bootstraps(nfl_train, strata = win)
# #
# #     # ---- Workflow set of models ----
# #     out_wfs <-
# #       workflowsets::workflow_set(
# #         preproc = list(
# #           glmnet_rec      = glmnet_recipe,
# #           # kknn_rec        = kknn_recipe,
# #           xgboost_rec = xgboost_recipe
# #         ),
# #         models  = list(
# #           glmnet     = glmnet_spec,
# #           # kknn = kknn_spec,
# #           xgboost = xgboost_spec
# #         ),
# #         cross = F
# #       )
# #
# #     # Choose metrics
# #     my_metrics <- yardstick::metric_set(roc_auc, accuracy, mn_log_loss)
# #     # my_metrics <- yardstick::metric_set(rsq, rmse, mae)
# #
# #     # Set up parallelization, using computer's other cores
# #     parallel::detectCores(logical = FALSE)
# #     modeltime::parallel_start(6, .method = "parallel")
# #
# #     # Set Random seed
# #     set.seed(589)
# #
# #     # Efficient Tuning of models in workflowset
# #     out_wfs <-
# #       out_wfs %>%
# #       workflowsets::workflow_map(
# #         "tune_race_anova",
# #         resamples = out_folds,
# #         # resamples = flow_roll_splits,
# #         grid      = 20,
# #         metrics   = my_metrics,
# #         control = finetune::control_race(
# #           verbose       = TRUE,
# #           save_pred     = TRUE,
# #           verbose_elim  = TRUE,
# #           save_workflow = TRUE
# #         ),
# #         verbose   = TRUE
# #       )
# #
# #     # Stop parrallelization
# #     modeltime::parallel_stop()
# #
# #     rank_results(out_wfs)
# #
# #     # Comparing Accuracy and ROC AUC of 7 models
# #     reg_mod_comp_plot <-
# #       out_wfs %>%
# #       autoplot() +
# #       labs(
# #         col = "",
# #         title    = "Regression Model comparisons"
# #       )
# #
# #     reg_mod_comp_plot
# #     # i = 1
# #
# #     for (i in 1:length(out_wfs$wflow_id)) {
# #
# #         model       <- out_wfs$wflow_id[i]
# #         model_name  <- out_wfs$info[[i]]$model
# #
# #
        # mod_results <-
        #   out_wfs %>%
        #   workflowsets::extract_workflow_set_result(model)
# #
# #         # Extract workflows
# #         mod_workflow <-
# #           out_wfs %>%
# #           extract_workflow(model)
# #
# #         # Model Engine text
# #         model_engine <- mod_workflow$fit$actions$model$spec$engine
# #
# #         # Model Engine text
# #         model_mode <- mod_workflow$fit$actions$model$spec$mode
# #
# #         logger::log_info("\n\nExtracting workflow & finalizing model fit:\n  --->  {model_name} - {model_mode}")
# #
# #         select_best(mod_results, metric = "roc_auc")
# #
# #         # Finalize workflow fit
# #         mod_workflow_fit <-
# #           mod_workflow %>%
# #           finalize_workflow(select_best(mod_results, metric = "roc_auc")) %>%
# #           # finalize_workflow(select_best(mod_results, metric = "rsq")) %>%
# #           fit(data = out_train)
# #
# #         # Fit model to split train/test data
# #         mod_last_fit <- tune::last_fit(mod_workflow_fit, out_split)
# #
# #         # print(tune::collect_metrics(mod_results)$mean)
# #         print(tune::collect_metrics(mod_last_fit))
# #
# #         # Extract & save final fit to use for predictions
# #         mod_final_fit <- mod_last_fit$.workflow[[1]]
# #
# #         # Resampled CV Fold AUC ROC Curve0-1 <-
# #           mod_results %>%
# #           collect_predictions() %>%
# #           group_by(id) %>%
# #           roc_curve(out_pct, .pred_1) %>%
# #           ggplot(aes(1 - specificity, sensitivity, color = id)) +
# #           geom_abline(lty = 2, color = "gray80", size = 1.5) +
# #           geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
# #           coord_equal() +
# #           labs(
# #             title    = paste0("AUC-ROC Curve - ", model_name),
# #             subtitle = "Resample results from 10 Fold Cross Validation",
# #             x        = "1 - Specificity",
# #             y        = "Sensitivity"
# #           )
# #         # # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
# #         # resample_plot_path  <-   paste0(ml_data_path, "plots/win_class_resample_aucroc_", model_name, ".png")
# #         # logger::log_info("\n\nSaving Resamples AUC-ROC curve: \n{resample_plot_path}")
# #         #
# #         # # Export plot
# #         # ggsave(
# #         #   resample_plot_path,
# #         #   plot   = resample_roc_plot
# #         # )
# #
# #         # Plot variable importance if avaliable for model
# #         tryCatch(
# #           {
# #             vip_plot <-
# #             mod_last_fit %>%
# #               pluck(".workflow", 1) %>%
# #               extract_fit_parsnip() %>%
# #               # vip::vi()
# #               # vip::vip() +
# #               vip::vip(num_features = 70) +
# #               # vip::vip(num_features = 30) +
# #               labs(
# #                 title    = paste0("Variable Importance Scores - ", model_name),
# #                 subtitle = "Regression",
# #                 y        = "Importance",
# #                 x        = "Variables"
# #               )
# #
# #             # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
# #             vip_path  <-   paste0(ml_data_path, "plots/win_class_vip_", model_name, ".png")
# #             logger::log_info("\n\nSaving Variable Importance plot:\n{vip_path}")
# #
# #             # Export plot
# #             ggsave(
# #               vip_path,
# #               plot   = vip_plot
# #             )
# #           },
# #           error = function(e) {
# #             logger::log_error('Variable Importance is not avalaible for {model_name} model')
# #             logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
# #             logger::log_error(message(e))
# #             # stop()
# #           }
# #         )
# #
# #     }
# #
# # }
