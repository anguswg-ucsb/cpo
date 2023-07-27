##################################################################################################
# Water Call Analysis
# 7/26/23

# Part 1: Pulling data from CDSS
# Part 2: Cleaning data and de-trending
# Part 3: Plotting relationships and trends
# Part 4: Linear Regression Model

# Functions are defined in the beginning of the script. Scroll to the bottom to initialize
#  arguments, wrangle data and run the functions.
#################################################################################################

rm(list = ls())

library(tidyverse)
#library(corrplot)
library(gridExtra)
library(lubridate)

# Make sure to change your working directory
currwd <- "C:/Users/emmag/OneDrive/EMMA_user/Docs/Career+Internship/Lynker/CPO/CPO_Local_Repo/cpo/"
getwd()
setwd(currwd)

#################################################################################################
# Part 1: Pulling data from CDSS

# FUNCTION GET CDSS CALL DATA
#' @title Get CDSS call data:
#' @description The function collects CDSS daily call years.
#' @param start_date Start date in format: "YY-MM-DD" {string}
#' @param end_date End date in format: "YY-MM-DD" {string}
#' @param wdids A vector of WDID strings for the specified district {character}
#' @return returns a csv file called "cdss_call_data.csv" into a local "/data" folder.

get_call_data <- function(start_date, end_date, wdids){

  # Create a dataframe with a single column containing WDID values
  wdids_df <- data.frame(wdid = wdids)

    data <- lapply(1:nrow(wdids_df), function(i) {
      message("Pausing iteration for 1.5 minutes...")

      # add pause in loop as to not overwhelm CDSS resources, DO NOT CHANGE WHEN running large number of WDIDs/districts
      Sys.sleep(90)

      message("Iteration resuming...")

      tryCatch({

        calls <- cdssr::get_call_analysis_wdid(
          wdid = wdids_df$wdid[i],
          admin_no   = "99999.00000",
          start_date,
          end_date,
          api_key    = NULL
        )
        calls

      }, error = function(e) {
      NULL
    })

  }) %>%
    dplyr::bind_rows()

  write.csv(data, paste0("data/cdss_call_data.csv"), row.names = FALSE)

  return(data)
}


#################################################################################################
# Part 2: Cleaning data and de-trending

#' @title Water call analysis, data cleaning and regression visualizations
#' @description The function cleans the data by standardizing max priority call year to 1970 and changing free river NAs to it.
#' @param df_call CSV of cdss-collected call years by WDID and district (i.e., "cdss_call_data.csv" or "daily_wdid_calls_data.csv") {dataframe}
#' @param df_predictor CSV of predictor by WDID and district (i.e., "annual_model_upd_eddi.csv") {dataframe}
#' @param distr A vector of district numbers {numeric}. Make sure to change distr and filter out df_call and df_predictor accordingly in data wrangling section.
#' @return returns the final cleaned dataframe as "result" and saves all figures locally as .pngs.

water_call_analysis <- function(df_call, df_predictor, distr){

    # Final Dataset - Free River & Max Date Fix -------------------------------
    data_clean_final <- df_call %>%
      #fix max priority year
      mutate(priority_year = year(priority_date)) %>%
      mutate(priroity_month = month(priority_date)) %>%
      mutate(priroity_day = day(priority_date)) %>%
      # standardize the max priority date
      mutate(priority_date_clean_temp = if_else(priority_year >=1971,
                                           ymd("19701231"), ymd(priority_date))) %>%
      # change free river NAs to 1970-12-31
      mutate(priority_date_clean_final = if_else(analysis_out_of_priority_percent_of_day == 0,
                                             ymd("19701231"), ymd(priority_date_clean_temp))) %>%
      # build the averaging routine
      group_by(analysis_wdid) %>%
      mutate(year = year(analysis_date)) %>%
      mutate(month = month(analysis_date)) %>%
      mutate(avg_condit = if_else(month >= 4 & month <=9,
                                  "AMJJAS avg","ONDJFM skip")) %>%
      select(analysis_wdid, year, month, avg_condit, priority_date_clean_final) %>%
      #na.omit() %>%
      # group by year & average conditions to
      group_by(year, avg_condit) %>%
      summarise(summer_call = mean(priority_date_clean_final, na.rm = TRUE)) %>%
      mutate(day_call = day(summer_call)/31) %>%
      mutate(month_call = month(summer_call)-1) %>%
      mutate(month_call_decimal = month_call+day_call) %>%
      mutate(month_call_decimal2 = month_call_decimal/12) %>%
      mutate(year_call = year(summer_call)) %>%
      mutate(call_year_decimal = year_call+month_call_decimal2)
    #print(data_clean_final)

    data_clean_final_by_district <- df_call %>%
      #fix max priority year
      mutate(priority_year = year(priority_date)) %>%
      mutate(priroity_month = month(priority_date)) %>%
      mutate(priroity_day = day(priority_date)) %>%
      # standardize the max priority date
      mutate(priority_date_clean_temp = if_else(priority_year >=1971,
                                                ymd("19701231"), ymd(priority_date))) %>%
      # change free river NAs to 1970-12-31
      mutate(priority_date_clean_final = if_else(analysis_out_of_priority_percent_of_day == 0,
                                                 ymd("19701231"), ymd(priority_date_clean_temp))) %>%
      # build the averaging routine
      group_by(district) %>%
      mutate(year = year(analysis_date)) %>%
      mutate(month = month(analysis_date)) %>%
      mutate(avg_condit = if_else(month >= 4 & month <=9,
                                  "AMJJAS avg","ONDJFM skip")) %>%
      select(analysis_wdid, year, month, district, avg_condit, priority_date_clean_final) %>%
      group_by(year, district) %>%
      summarise(summer_call = mean(priority_date_clean_final, na.rm = TRUE)) %>%
      mutate(day_call = day(summer_call)/31) %>%
      mutate(month_call = month(summer_call)-1) %>%
      mutate(month_call_decimal = month_call+day_call) %>%
      mutate(month_call_decimal2 = month_call_decimal/12) %>%
      mutate(year_call = year(summer_call)) %>%
      mutate(call_year_decimal = year_call+month_call_decimal2)

    # Plot the Average Call Year by Year
    p4 <- ggplot(data_clean_final, aes_string(x="year", y = "summer_call", group = "avg_condit")) +
      #stat_summary(fun.data=mean_cl_normal) +
      geom_line(aes(col = avg_condit, linetype=avg_condit)) +
      #geom_smooth(method='lm', formula= y~x) +
      theme_bw() +
      ggtitle(paste0("District ", paste(distr, collapse = ","), ": 6-month Average Call by Season (final clean data)"))
    ggsave(
      paste0("District", paste(distr, collapse = ","), "-6-monthAverageCallbySeason-finalcleandata.png"),
      plot = p4,
      width = 10, height = 10
    )

    # # Plot the Average Call Year by district
    # p5 <- ggplot(data_clean_final_by_district, aes_string(x="year", y = "summer_call", group = "district")) +
    #   #stat_summary(fun.data=mean_cl_normal) +
    #   geom_line(aes(col = district, linetype=district)) +
    #   #geom_smooth(method='lm', formula= y~x) +
    #   theme_bw() +
    #   ggtitle(paste0("6-month Average Summer Call by District"))
    # ggsave(
    #   paste0("6-monthAverageSummerCallbydistrict-finalcleandata.png"),
    #   plot = p5,
    #   width = 10, height = 10
    # )


    # Combine "Clean data" with Predictor Dataset -------------------------------------------------

    predictor_data2 <- df_predictor %>%
      #filter(district == distr) %>%
      # filter 1982 > for SWE
      select('district', 'year', 'wdid', 'may_swe', 'peak_swe',
             'apr_fx_apr_to_sep', 'may_fx_apr_to_sep',
             'avg_call_year', 'avg_call_may_sep', 'min_call_may_sep',
             'avg_call_june_sep', 'min_call_june_sep', 'avg_call_july_sep', 'min_call_july_sep',
             'avg_call_aug_sep', 'min_call_aug_sep',
             'apr_eddi30d', 'may_eddi30d', 'apr_eddi90d', 'may_eddi90d',
             'apr_eddi180d', 'may_eddi180d', 'apr_eddi1y', 'may_eddi1y')

    # merge the two datasets together
    data_merge <- predictor_data2 %>%
      left_join(., data_clean_final, by = "year", relationship = "many-to-many")
    print(data_merge)

    # select the data needed
    data3 <- data_merge %>%
      #select(-district) %>% #wdid, year
      select(-day_call, -month_call, -month_call_decimal,-month_call_decimal2, -year_call, -summer_call) %>%
      # use the summer average - this is important!
      filter(avg_condit == "AMJJAS avg") %>%
      # remove the summer average column since it's character
      select(-avg_condit) %>%
      na.omit()


    # data3_matrix <- as.matrix(data3)
    # print(data3_matrix)
    # corr_matrix <- cor(data3_matrix)
    # # full grid (has repeating values)
    # corrplot::corrplot(corr_matrix, method=c("circle"), type=c("full"), na.or.zero = TRUE)


    # convert WDID to character for plotting
    data3$wdid <- as.character(data3$wdid)


    #################################################################################################
    # Part 3: Plotting relationships and trends
    p1 <- ggplot(data3, aes_string(x="year", y = "may_swe", group = "wdid")) +
      #stat_summary(fun.data=mean_cl_normal) +
      geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
      #geom_smooth(method='lm', formula= y~x) +
      theme_bw() +
      ggtitle("May SWE by WDID")

    p2 <- ggplot(data3, aes_string(x="year", y = "may_fx_apr_to_sep", group = "wdid")) +
      #stat_summary(fun.data=mean_cl_normal) +
      geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
      #geom_smooth(method='lm', formula= y~x) +
      theme_bw() +
      ggtitle("May NRCS Forecast by WDID")

    p3 <- ggplot(data3, aes_string(x="year", y = "call_year_decimal", group = "wdid")) +
      #stat_summary(fun.data=mean_cl_normal) +
      geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
      #geom_smooth(method='lm', formula= y~x) +
      theme_bw() +
      ggtitle("Revised Summer Call Year by WDID")

    plot_title <- paste0("District ", paste(distr, collapse = ","), " - Regression Summary - Clean Data")
    ggsave(
      paste0(plot_title, ".png"),
      width = 14, height = 8,
      grid.arrange(p1, p2, p3, nrow = 3,
                   top = plot_title,
                   right = "")
    )

    #################################################################################################
    # Part 4: Linear Regression Model

    # test linear regression models
    lm1 <- lm(call_year_decimal~may_swe, data3)
    lm2 <- lm(call_year_decimal~may_fx_apr_to_sep, data3)
    print(paste("call year vs may swe R2:", summary(lm1)$r.squared))
    print(paste("call year vs may forecast R2:", summary(lm2)$r.squared))


    # Plot the May SWE Vs Summer Average Call
    p6 <- ggplot(data3, aes(x = may_swe, y = call_year_decimal)) +
      stat_summary(fun.data=mean_cl_normal) +
      geom_point(aes(color = as.factor(wdid))) +
      geom_smooth(method='lm', formula= y~x) +
      theme_bw() +
      ggtitle("Summer Average Call Year vs May SWE by WDID")

    p7 <- ggplot(data3, aes(x = may_fx_apr_to_sep, y = call_year_decimal)) +
      stat_summary(fun.data=mean_cl_normal) +
      geom_point(aes(color = as.factor(wdid))) +
      geom_smooth(method='lm', formula= y~x) +
      theme_bw() +
      ggtitle("Summer Average Call vs May NRCS Forecast by WDID")

    plot_title <- paste0("District ", paste(distr, collapse = ","), " - Regression Analysis Summary")
    ggsave(
      paste0(plot_title, ".png"),
      width = 10, height = 10,
      grid.arrange(p6, p7, nrow = 2,
                   top = plot_title,
                   right = "")
    )

    # Return the final cleaned and aggregated call data
    return(data_clean_final)

}



add_leading_zero <- function(x) {
  ifelse(nchar(x) < 7, paste0("0", x), x)
}

################################################################################
# Initialize variables:

start_date <- "1970-01-01"
end_date <- "2023-07-01"

# COMMENT/UNCOMMENT FOR YOUR VECTOR OF WDIDS:
# Rachel:
#wdids <- c('100839','100843','102003','200740','201015','203375','301143','303347','303676','400766','400861','400868','404167','500991','502131','504081','602105','604256','700812','702108','801000','801180','902107','904288','2300679') %>%
#  sapply(add_leading_zero)

# Angus:
#wdids <- c('2302103','2302119','4800572','4803987','4804002','4904484','4904485','4904487','6400503','6402411','6499999','6500523','6504485','7600505','7600600','8002107','8002126','8003550','0108327','0110900','0203936','0208828','0301305') %>%
# sapply(add_leading_zero)

# Emma
wdids <- c('0301307','0302215','0303777','0400559','0400639','0405057','0500796','0601423','0601428','0601559','0604255','0700859','0703391','0801020','0808159','0901591','0905632','2300615','2302120','2302429','4800581','4801523','4803679','4804000','6405603','6406695','8005507') %>%
  sapply(add_leading_zero)

#df_call1 <- read_csv("data/daily_wdid_calls_data.csv")
df_call1 <- read_csv("data/cdss_call_data.csv") # THIS SHOULD BE YOUR OUTPUT FROM RUNNING GET_CALL_DATA()
df_predictor1 <- read_csv("data/annual_model_upd_eddi_v2.csv")

distr <- c(80)# 1, 2, 3, 4, 5, 6, 7, 8, 9, 23, 64, 80)

################################################################################
# Wrangle data
df_predictor <- df_predictor1[df_predictor1$district %in% c(1,2,3,4,5,6,7,8,9,23,64,80),]
df_predictor$wdid <- as.character(df_predictor$wdid)

df_predictor$wdid <- sapply(df_predictor$wdid, add_leading_zero)

df_call1$district <- trimws(df_call1$district)
df_call <- df_call1[df_call1$district %in% c('01', '02', '03', '04', '05', '06', '07', '08', '09', '23', '64', '80'), ]


# unique_values_df_call <- df_call %>%
#   mutate(CombinedColumns = paste(analysis_wdid, district, sep = "_")) %>%
#   distinct(CombinedColumns, .keep_all = TRUE) %>%
#   select(analysis_wdid, district)
#
# unique_values_df_predictor <- df_predictor %>%
#   mutate(CombinedColumns = paste(wdid, district, sep = "_")) %>%
#   distinct(CombinedColumns, .keep_all = TRUE) %>%
#   select(wdid, district)
# unique_values_df_predictor$district <- as.character(unique_values_df_predictor$district)

#################################################################################################
# Try out the functions:
get_call_data(start_date, end_date, wdids)

# UNCOMMENT TO RUN PLOTS AND REGRESSION ANALYSIS:
#result <- water_call_analysis(df_call, df_predictor, distr)
