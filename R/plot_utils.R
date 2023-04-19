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
