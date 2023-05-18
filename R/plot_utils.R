
make_plots <- function(wfs, basin_name, model_type, save_path) {

  mod_comp_plot <- make_comp_plot(
    wfs        = wfs,
    basin_name = basin_name,
    model_type = model_type,
    save_path  = save_path
  )
# wfs <- reg_mods
# basin_name
# model_type   = "regression"

  vip_plot <- make_vip_plot(
      wfs        = wfs,
      basin_name = basin_name,
      model_type = model_type
      # save_path  = save_path
    )


  # Model outputs path
  out_dir  <- paste0(save_path, "plots/")
  comp_path <- paste0(
                    out_dir,
                    tolower(gsub("[[:punct:][:blank:]]+", "_",  basin_name)),
                    ifelse(model_type == "classification", "_class_", "_reg_"),
                    "model_rank.png"
                  )

  vip_path <- paste0(
                  out_dir,
                  tolower(gsub("[[:punct:][:blank:]]+", "_",  basin_name)),
                  ifelse(model_type == "classification", "_class_", "_reg_"),
                  "variable_importance.png"
                )

  # checking if workflowset/ directory exists, and if not, creates one
  if(!dir.exists(out_dir)) {
    logger::log_info("Creating plot directory:\n--> {out_dir}")

    dir.create(out_dir)

    logger::log_info("Saving {basin_name} {model_type} models comparison plot:\n--> {comp_path}")

    # Save plot
    ggplot2::ggsave(
      comp_path,
      plot   = mod_comp_plot,
      width  = 52,
      height = 28,
      units  = "cm"
    )

  } else {

    logger::log_info("Saving {basin_name} {model_type} models comparison plot:\n--> {comp_path}")

    # Save plot
    ggplot2::ggsave(
      comp_path,
      plot   = mod_comp_plot,
      width  = 52,
      height = 28,
      units  = "cm"
    )

  }
}

# Make model comparison plots from workflowsets object
make_comp_plot <- function(
    wfs,
    basin_name = "ID",
    model_type = "classification",
    save_path = NULL
) {

  # Comparing rmse rsq AND mae OF ALL MODELS
  mod_comp_plot <-
    wfs %>%
    autoplot() +
    ggplot2::geom_point(ggplot2::aes(color = wflow_id)) +
    ggplot2::labs(
      color = "Data Preprocessor",
      title    = paste0(stringr::str_to_title(basin_name), " Model Comparisons"),
      subtitle = paste0(stringr::str_to_title(model_type), " models")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 16, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 10),
      axis.title    = ggplot2::element_text(size = 12),
      strip.text.x  = ggplot2::element_text(size =12, face = "bold"),
      legend.title  = ggplot2::element_text(size = 12, face = "bold"),
      legend.text   = ggplot2::element_text(size =10)
    )
  # theme(legend.position = "none")

  # # Model outputs path
  # out_dir  <- paste0(save_path, "plots/")
  # out_path <- paste0(
  #                 out_dir,
  #                 tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
  #                 ifelse(model_type == "classification", "_class_", "_reg_"),
  #                 "model_rank.png"
  #               )
  #
  # # checking if workflowset/ directory exists, and if not, creates one
  # if(!dir.exists(out_dir)) {
  #   logger::log_info("Creating plot directory:\n--> {out_dir}")
  #
  #   dir.create(out_dir)
  #
  #   logger::log_info("Saving {basin_name} {model_type} models comparison plot:\n--> {out_path}")
  #
  #   # Save plot
  #   ggplot2::ggsave(
  #     out_path,
  #     plot   = mod_comp_plot,
  #     width  = 52,
  #     height = 28,
  #     units  = "cm"
  #   )
  #
  # } else {
  #
  #   logger::log_info("Saving {basin_name} {model_type} models comparison plot:\n--> {out_path}")
  #
  #   # Save plot
  #   ggplot2::ggsave(
  #     out_path,
  #     plot   = mod_comp_plot,
  #     width  = 52,
  #     height = 28,
  #     units  = "cm"
  #   )
  #
  # }

  return(mod_comp_plot)
}

make_vip_plot <- function(
    wfs,
    basin_name,
    model_type,
    save_path
) {
  # wfs        = wfs
  # basin_name = basin_name
  # model_type = model_type
  mod_names <- strsplit(names(wfs$results), "_")

  vip_tables <- lapply(1:length(wfs$results), function(i) {
    # i = 2
    fitted_model <- wfs$results[[i]]$last_fit

    vip_tbl <- tryCatch(
      {

          fitted_model %>%
          purrr::pluck(".workflow", 1) %>%
          extract_fit_parsnip() %>%
          vip::vi() %>%
          # vip::vi(num_features = 70) +
          dplyr::mutate(
            basin      = basin_name,
            model      = mod_names[[i]][1],
            model_type = model_type
          )
        # ggplot2::labs(
        #   title    = paste0("Variable Importance Scores - ", basin_name),
        #   subtitle = paste0(simple_name, " - ", model_type),
        #   y        = "Importance",
        #   x        = "Variables"
        # )

      },
      error = function(e) {
        logger::log_error('Variable Importance is not avalaible for {basin_name} - {model_type}')
        logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
        logger::log_error(message(e))
        NULL
      }
    )
    vip_tbl

  })

  # Remove NULL elements from the list using indexing and is.null
  vip_tables <- dplyr::bind_rows(vip_tables[!sapply(vip_tables, is.null)])

  vip_plots <-
    vip_tables %>%
    # dplyr::filter(model != "glmnet") %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = Importance, y = Variable
                                   # fill = Sign
                                   )
                      ) +
    ggplot2::labs(
      title    = paste0("Variable Importance Scores - ", basin_name),
      subtitle = paste0("(", model_type, ")"),
      y        = "Importance",
      x        = "Variables"
    ) +
    ggplot2::facet_wrap(~model, scale = "free") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 16, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5),
      axis.text     = ggplot2::element_text(size = 10),
      axis.title    = ggplot2::element_text(size = 12),
      strip.text.x  = ggplot2::element_text(size =12, face = "bold"),
      legend.title  = ggplot2::element_text(size = 12, face = "bold"),
      legend.text   = ggplot2::element_text(size =10)
    )
  vip_plots
  return(vip_plots)

}

make_roc_plot <- function(
    wfs,
    basin_name,
    model_type,
    save_path
    ) {
  mod_results <-
    wfs$workflowset %>%
    workflowsets::extract_workflow_set_result("kknn_rec_kknn")

  mod_names <- strsplit(names(wfs$results), "_")

  roc_plots <- lapply(1:length(wfs$workflowset$wflow_id), function(i) {

    model_results <- wfs$results[[i]]$model_results

    tryCatch(
      {
        model_results %>%
          collect_predictions() %>%
          group_by(id) %>%
          roc_curve(out, .pred) %>%
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

        fitted_model %>%
          purrr::pluck(".workflow", 1) %>%
          extract_fit_parsnip() %>%
          vip::vi(num_features = 70) %>%
          # vip::vi(num_features = 70) +
          dplyr::mutate(
            basin      = basin_name,
            model      = mod_names[[i]][1],
            model_type = model_type
          )
        # ggplot2::labs(
        #   title    = paste0("Variable Importance Scores - ", basin_name),
        #   subtitle = paste0(simple_name, " - ", model_type),
        #   y        = "Importance",
        #   x        = "Variables"
        # )

      },
      error = function(e) {
        logger::log_error('Variable Importance is not avalaible for {basin_name} - {model_type}')
        logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
        logger::log_error(message(e))
        # stop()
      }
    )

  })
  wfs$workflowset$wflow_id
  # Plot variable importance if avaliable for model
  tryCatch(
    {

      # Resampled CV Fold AUC ROC Curve0-1 <-
      roc_plot <-
        mod_results %>%
        collect_predictions() %>%
        group_by(id) %>%
        roc_curve(out, .pred_1) %>%
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

      # # save ROC AUC plot to "aw-poudre-2020/dflow/boatable_day_plots/
      # vip_path  <-   paste0(ml_data_path, "plots/win_class_vip_", model_name, ".png")
      logger::log_info('\n\nSaving ROC AUC Importance plot:\n{paste0(
                          save_path,
                          tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
                          # ifelse(model_type == "classification", "_class_", "_reg_"),
                               "_", simple_name,
                          "_roc_auc.plot"
                          )}')

      # Export plot
      ggplot2::ggsave(
        paste0(
          save_path,
          tolower(gsub("[[:punct:][:blank:]]+", "_",  model_name)),
          "_", simple_name,
          # ifelse(model_type == "classification", "_class_", "_reg_"),
          "_roc_auc_plot.png"
        ),
        plot   = roc_plot
      )
    },
    error = function(e) {
      logger::log_error('ROC AUC not avalaible for {simple_name} - {model_type}')
      logger::log_error('vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv')
      logger::log_error(message(e))
      # stop()
    }
  )


}
make_corr_plots <- function(df, save_path) {

   # df <-
   #  mod_df %>%
   #  dplyr::mutate(month = lubridate::month(date)) %>%
   #  dplyr::filter(month %in% c(6, 7, 8, 9))

  df <-
    df %>%
    dplyr::group_by(district) %>%
    dplyr::group_split()

  cor_lst <- lapply(1:length(df), function(i) {

    d <- df[[i]]

    message(paste0("Calculating correlations - (District ", unique(d$district), ")"))

  out_cor <-
      d %>%
      dplyr::select(where(is.numeric)) %>%
      cor(use =  "pairwise.complete.obs") %>%
      round(2) %>%
      reshape2::melt()

    cor_plot <-
      ggplot2::ggplot(
        data = out_cor, aes(
          x    = Var1,
          y    = Var2,
          fill = value
        )
      ) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(
        aes(Var2, Var1, label = value),
        color = "black",
        size  = 4
      ) +
      ggplot2::labs(
        title = paste0("Correlation Matrix - District ", unique(d$district)),
        x = "",
        y = ""
      ) +
      ggplot2::scale_fill_gradient2(low="darkred", high="midnightblue", guide="colorbar") +
      # viridis::scale_fill_viridis() +
      ggplot2::theme(
        plot.title  = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = -45)
      )

    message(paste0("Saving correlation plot vs ",
                   "\n---> ",
                   save_path, "/correlation_mat_",  unique(d$district), ".png")
    )
    # Save the plots to files
    ggplot2::ggsave(
      paste0(save_path, "/correlation_mat_",  unique(d$district), ".png"),
      cor_plot,
      height = 8,
      width  = 18,
      scale  = 1
    )

  })

  return(cor_lst)

}

make_rights_corr_plots <- function(df, save_path) {

  # df <-
  #  mod_df %>%
  #  dplyr::mutate(month = lubridate::month(date)) %>%
  #  dplyr::filter(month %in% c(6, 7, 8, 9))

  df <-
    df %>%
    dplyr::group_by(district) %>%
    dplyr::group_split()

  cor_lst <- lapply(1:length(df), function(i) {

    d <- df[[i]]

    message(paste0("Calculating correlations by right - (District ", unique(d$district), ")"))

    rights <-
      d %>%
      dplyr::filter(seniority != "median") %>%
      dplyr::group_by(seniority) %>%
      dplyr::group_split()

    right_lst <- lapply(1:length(rights), function(z) {

      # water right category
      right_cat <- rights[[z]]$seniority[1]

      out_cor <-
        rights[[z]] %>%
        # d %>%
        dplyr::select(where(is.numeric)) %>%
        cor(use =  "pairwise.complete.obs") %>%
        round(2) %>%
        reshape2::melt() %>%
        dplyr::mutate(seniority = right_cat)
      out_cor

    }) %>%
      dplyr::bind_rows()

      cor_plot <-
        ggplot2::ggplot(
          data = right_lst,
          ggplot2::aes(
            x    = Var1,
            y    = Var2,
            fill = value
          )
        ) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(
          aes(Var2, Var1, label = value),
          color = "black",
          size  = 3
        ) +
        ggplot2::labs(
          title = paste0("Correlation Matrices - District ",
                         unique(d$district)),
          x = "",
          y = ""
        ) +
        ggplot2::facet_wrap(~seniority, nrow = length(unique(right_lst$seniority))) +
        # ggplot2::facet_wrap(~seniority, nrow = 1) +
        ggplot2::scale_fill_gradient2(low="darkred",
                                      high="midnightblue",
                                      guide="colorbar") +
        # viridis::scale_fill_viridis() +
        ggplot2::theme(
          plot.title  = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = -45),
          strip.text = ggplot2::element_text(size = 14, face = "bold")
        )

      message(paste0("Saving correlation plot vs ",
                     "\n---> ",
                     save_path, "/correlation_mat_",  unique(d$district), "_rights.png")
      )



    # Save the plots to files
    ggplot2::ggsave(
      paste0(save_path, "/correlation_mat_",  unique(d$district), "_rights.png"),
      cor_plot,
      height = 10,
      width  = 18,
      scale  = 1
    )

  })

  return(cor_lst)

}

make_out_scatter_plots <- function(df, save_path) {

  plot_df <-
    df %>%
    dplyr::filter(out_pct > 0) %>%
    dplyr::mutate(
      out_pct_log  = log(out_pct)
      # out_bin = dplyr::case_when(
      #   out_pct <= 25                ~ "0_25",
      #   out_pct > 25 & out_pct <= 50 ~ "25_50",
      #   out_pct > 50 & out_pct <= 75 ~ "50_75",
      #   TRUE                         ~ "75_100"
      # )
      # out_pct_sqrt = sqrt(out_pct)
    )

  cols_to_plot <- names(plot_df)[!grepl("district|date|wdid|gnis_id|approp_date|seniority|out_bin|out_pct|out_pct_log|out_pct_wins|out_pct_sqrt",
                                        names(plot_df))]

  # Loop through each column and create the plots
  for (col in cols_to_plot) {

    message(paste0("plotting var: ", col))

    # Plot the out_pct variable against the current column with facet_wrap by district
    junior_plot <-
      plot_df %>%
      dplyr::filter(seniority == "junior") %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(aes(x = out_pct, y = .data[[col]]), size = 0.5, alpha = 0.7, color = "coral") +
      ggplot2::facet_wrap(~district) +
      ggplot2::labs(
        # title = paste0("Out priority % vs. ", col, " - (junior)")
        title = paste0("Junior rights"),
        subtitle = paste0("Out priority % vs. ", col)
      ) +
      # ggplot2::scale_color_manual(values = c("coral")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle  = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8)
      )


    # Plot the out_pct variable against the current column with facet_wrap by district
    med_plot <-
      plot_df %>%
      dplyr::filter(seniority == "median") %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(aes(x = out_pct, y = .data[[col]]),  size = 0.5, alpha = 0.7, color = "dodgerblue3") +
      ggplot2::facet_wrap(~district) +
      ggplot2::labs(
        # title = paste0("Out priority % vs. ", col, " - (median)")
        title = paste0("Median rights"),
        subtitle = paste0("Out priority % vs. ", col)
      ) +
      # ggplot2::scale_color_manual(values = c("dodgerblue3")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle  = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8)
      )

    # Plot the out_pct variable against the current column with facet_wrap by district
    senior_plot <-
      plot_df %>%
      dplyr::filter(seniority == "senior") %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(aes(x = out_pct, y = .data[[col]]),  size = 0.5, alpha = 0.7, color = "forestgreen") +
      ggplot2::facet_wrap(~district) +
      ggplot2::labs(

        title = paste0("Senior rights"),
        subtitle = paste0("Out priority % vs. ", col)
        # title = paste0("Out priority % vs. ", col, " - (senior)")
      ) +
      # ggplot2::scale_color_manual(values = c("forestgreen")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle  = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8)
      )


    out_pct_plot <- junior_plot + med_plot + senior_plot


    # Plot the out_pct variable against the current column with facet_wrap by district
    junior_log_plot <-
      plot_df %>%
      dplyr::filter(seniority == "junior") %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(aes(x = out_pct_log, y = .data[[col]]),  size = 0.5, alpha = 0.7, color = "coral") +
      ggplot2::facet_wrap(~district) +
      ggplot2::labs(
        # title = paste0("Out priority % (log) vs. ", col, " - (junior)")
        title = paste0("Junior rights"),
        subtitle = paste0("Out priority % (log) vs. ", col)
      ) +
      # ggplot2::scale_color_manual(values = c("coral")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle  = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8)
      )


    # Plot the out_pct variable against the current column with facet_wrap by district
    med_log_plot <-
      plot_df %>%
      dplyr::filter(seniority == "median") %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(aes(x = out_pct_log, y = .data[[col]]),  size = 0.5, alpha = 0.7, color ="dodgerblue3") +
      ggplot2::facet_wrap(~district) +
      ggplot2::labs(
        # title = paste0("Out priority % (log) vs. ", col, " - (median)")
        title = paste0("Median rights"),
        subtitle = paste0("Out priority % (log) vs. ", col)
      ) +
      # ggplot2::scale_color_manual(values = c("dodgerblue3")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle  = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8)
      )

    # Plot the out_pct variable against the current column with facet_wrap by district
    senior_log_plot <-
      plot_df %>%
      dplyr::filter(seniority == "senior") %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(aes(x = out_pct_log, y = .data[[col]]), size = 0.5, alpha = 0.7, color = "forestgreen")  +
      ggplot2::facet_wrap(~district) +
      ggplot2::labs(
        title = paste0("Senior rights"),
        subtitle = paste0("Out priority % (log) vs. ", col)
      ) +
      # ggplot2::scale_color_manual(values = c("forestgreen")) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle  = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8)
      )


    out_pct_log_plot <- junior_log_plot + med_log_plot + senior_log_plot

    message(paste0("Saving out_pct vs ", col, " plot",
                   "\n---> ",save_path, "/", col, "_out_pct.png"))
    # Save the plots to files
    ggplot2::ggsave(paste0(save_path, "/", col, "_out_pct.png"),
                    out_pct_plot,
                    height = 8,
                    width = 18,
                    scale = 1
    )

    message(paste0("Saving out_pct_log vs ", col, " plot",
                   "\n---> ",save_path, "/", col, "_out_pct_log.png"))

    ggplot2::ggsave(paste0(save_path, "/", col, "_out_pct_log.png"),
                    out_pct_log_plot,
                    height = 8,
                    width = 18,
                    scale = 1
    )

  }

}
