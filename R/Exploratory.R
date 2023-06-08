# Emma Golub
# Script that performs exploratory analysis on predictor and response variables

library(tidyverse)

output_dir <- "./R/exploratory/output/"

#load summarized data of response and predictor variables
df <- read.csv("./data/annual_model_data.csv")

# Scatterplot functions
make_out_scatter_plots <- function(df, save_path) {

  cols_to_plot_predictors <- names(df)[grepl("peak_swe|may_swe|apr_fx_apr_to_sep|may_fx_apr_to_sep|eddi180d|eddi270d|eddi1y|eddi2y|eddi5y",
                             names(df))]

  response_vars <- names(df)[grepl("avg_call_year|avg_call_may_sep|avg_call_june_sep|avg_call_july_sep|avg_call_aug_sep|min_call_may_sep|min_call_june_sep|min_call_july_sep|min_call_aug_sep",
                   names(df))]
  # grepl searches for matches of certain character pattern in a vector of character strings

  # Loop through each column and create the plots
  for (col in cols_to_plot_predictors) {
    for (var in response_vars) {

    message(paste0("plotting var: ", col))

    # Plot the avg_call variable against the current column with facet_wrap by district
    plot_all <-
      df %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(aes(x = .data[[var]], y = .data[[col]]), size = 0.9, alpha = 0.7, color = "midnightblue") +
      ggplot2::facet_wrap(~district) +
      ggplot2::labs(
        title = paste0(var, " vs. ", col),
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle  = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 8)
      )

    # Saving all plots
    message(paste0("Saving ", var, "vs ", col, " plot",
                   "\n---> ",save_path, "/", col, "_", var, ".png"))
    # Save the plots to files
    ggplot2::ggsave(paste0(save_path, "/", col, "_", var, ".png"),
                    plot_all,
                    height = 8,
                    width = 18,
                    scale = 1
    )
  }
  }
}

# Correlation plots across districts
make_corr_plots <- function(df, save_path) {

  df <-
    df %>%
    dplyr::group_by(district) %>%
    dplyr::group_split()

  #iterating over dataframe and applying this function to each district
  cor_lst <- lapply(1:length(df), function(i) {

    d <- df[[i]] #for each district in df

    message(paste0("Calculating correlations - (District ", unique(d$district), ")"))

    out_cor <-
      d %>%
      dplyr::select(where(is.numeric)) %>% # Selects only the numeric columns from a data frame
      cor(use =  "pairwise.complete.obs") %>% #Calculates the pairwise correlation between the selected numeric columns
      round(2) %>% #reounds values to two decimal places
      reshape2::melt() #Reshapes the correlation matrix into a long-format data frame

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

# Correlation plots across dataset
make_corr_plot_dataset <- function(df, save_path) {


  #iterating over dataframe and applying this function to each district
  #cor_lst <- lapply(1:length(df), function(i) {

    #d <- df[[i]] #for each district in df

    message(paste0("Calculating correlations"))

    out_cor <-
      df %>%
      dplyr::select(where(is.numeric)) %>% # Selects only the numeric columns from a data frame
      cor(use =  "pairwise.complete.obs") %>% #Calculates the pairwise correlation between the selected numeric columns
      round(2) %>% #rounds values to two decimal places
      reshape2::melt() #Reshapes the correlation matrix into a long-format data frame

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
        title = paste0("Correlation Matrix Across Entire Dataset"),
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
                   save_path, "/correlation_mat.png")
    )
    # Save the plots to files
    ggplot2::ggsave(
      paste0(save_path, "/correlation_mat.png"),
      cor_plot,
      height = 8,
      width  = 18,
      scale  = 1
    )

  #})

  #return(cor_lst)
}


#Run plotting functions
make_out_scatter_plots(df, output_dir)

df_for_corr <- subset(df, select= -c(basin,wdid,gnis_id,water_source, approp_date,year, apr_fx_apr_to_jul,may_fx_apr_to_jul,may_fx_may_to_jul,may_fx_may_to_sep, lon,lat))
make_corr_plots(df_for_corr, output_dir)

df_for_corr2 <- subset(df, select= -c(district,basin,wdid,gnis_id,water_source, approp_date,year, apr_fx_apr_to_jul,may_fx_apr_to_jul,may_fx_may_to_jul,may_fx_may_to_sep, lon,lat))
make_corr_plot_dataset(df_for_corr2, output_dir)

