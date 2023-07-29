##################################################################################################
# Water Call Analysis
# 7/28/23

# Part 1: Function 1: Pulling data from CDSS
# Part 2: Function 2: Cleaning data and de-trending
# Part 3: Function 3: Linear regression Model
# Part 4: Initializing arguments and running the functions
# Part 5: Plotting and visualizations

# Functions are defined in the beginning of the script. Scroll to the bottom to initialize
#  arguments, wrangle data and run the functions. Make sure to run functions section first.
#  Then run everything after Part 4 (line 235).
#################################################################################################
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

#' @title Water call analysis, data cleaning
#' @description The function cleans the data by standardizing max priority call year to 1970 and changing free river NAs to it.
#' @param df_call CSV of cdss-collected call years by WDID and district (i.e., "cdss_call_data.csv" or "daily_wdid_calls_data.csv") {dataframe}
#' @param df_predictor CSV of predictor by WDID and district (i.e., "annual_model_upd_eddi.csv") {dataframe}
#' @param distr A vector of district numbers {numeric}. Make sure to change distr and filter out df_call and df_predictor accordingly in data wrangling section.
#' @return returns the final cleaned dataframe as "result"
water_call_analysis <- function(df_call, df_predictor){

  #out <- list() # I was trying to save relevant dfs here so I could return more than one df... but wasn't working.

    # Detrended Call Dataset - Free River & Max Date Fix -------------------------------------------------
    call_detrended_data <- df_call %>%
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
      select(district, analysis_wdid, year, month, avg_condit, priority_date_clean_final) %>%
      # group by year & average conditions to
      group_by(year, district, avg_condit) %>% # don't group by wdid here
      summarise(summer_call = mean(priority_date_clean_final, na.rm = TRUE)) %>%
      mutate(day_call = day(summer_call)/31) %>%
      mutate(month_call = month(summer_call)-1) %>%
      mutate(month_call_decimal = month_call+day_call) %>%
      mutate(month_call_decimal2 = month_call_decimal/12) %>%
      mutate(year_call = year(summer_call)) %>%
      mutate(call_year_decimal = year_call+month_call_decimal2) #%>%
      #rename(wdid = analysis_wdid)
    #print(call_detrended_data)
    write_csv(call_detrended_data, "data/call_detrended_data.csv")


    call_detrended_data_by_district <- df_call %>%
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
    write_csv(call_detrended_data_by_district, "data/call_detrended_data_by_district.csv")


    # Merge Detrended Call Data with Predictor Dataset -------------------------------------------------

    predictor_data <- df_predictor %>%
      select('district', 'year', 'wdid', 'gnis_id', 'may_swe',
             'peak_swe', 'apr_fx_apr_to_sep', 'may_fx_apr_to_sep',
             'apr_eddi30d', 'may_eddi30d', 'apr_eddi90d', 'may_eddi90d',
             'apr_eddi180d', 'may_eddi180d', 'apr_eddi1y', 'may_eddi1y')
    #df_predictor$district <- as.numeric(df_predictor$district)
    predictor_data <- predictor_data %>% mutate(district = as.numeric(district))

    # replace district 5 SWE data with those from district 6, University Camp 838 for more representation
    univ_camp_swe <- read_csv("data/district5_SWE_replacement.csv")

    predictor_data$may_swe[predictor_data$district == 5 & predictor_data$year >= 1980 & predictor_data$year <= 2022] <- univ_camp_swe$may_swe
    predictor_data$peak_swe[predictor_data$district == 5 & predictor_data$year >= 1980 & predictor_data$year <= 2022] <- univ_camp_swe$peak_swe

    # Up until this point, predictor_data and call_detrended_data have the same
    #overlap <- intersect(unique(predictor_data$wdid), unique(call_detrended_data$wdid))


    # merge the two datasets together
    data_merge <- left_join(call_detrended_data, predictor_data, by = c("district", "year"), relationship = "many-to-many") %>% # maybe don't group by "wdid" too
      select(-day_call, -month_call, -month_call_decimal,-month_call_decimal2, -year_call, -summer_call) %>%
      # use the summer average - this is important!
      filter(avg_condit == "AMJJAS avg") %>%
      # remove the summer average column since it's character
      select(-avg_condit)

    # Double check wdids
    #overlap2 <- intersect(unique(predictor_data$wdid), unique(data_merge$wdid))

    # filter out NAs a better way
    detrended_all_data_final <- data_merge[rowSums(!is.na(data_merge[c("may_swe", "peak_swe", "apr_fx_apr_to_sep", "may_fx_apr_to_sep", "apr_eddi90d", "may_eddi90d")])) >= 1, ]
    #detrended_all_data_final2 <- data_merge[complete.cases(data_merge[c("may_swe", "peak_swe", "apr_fx_apr_to_sep", "may_fx_apr_to_sep", "apr_eddi90d", "may_eddi90d", "call_year_decimal")])]
    #detrended_all_data_final3 <- na.omit(data_merge)

    # Double check wdids
    #unique(detrended_all_data_final$wdid)
    #overlap3 <- intersect(unique(detrended_all_data_final$wdid), unique(data_merge$wdid))
    # df1_unique <- detrended_all_data_final$wdid[!(detrended_all_data_final$wdid %in% data_merge$wdid)]
    # df2_unique <- data_merge$wdid[!(data_merge$wdid %in% detrended_all_data_final$wdid)]

    write_csv(detrended_all_data_final, "data/detrended_all_data_final.csv")

    # Return the final cleaned, aggregated data
    return(detrended_all_data_final)

}

#################################################################################################
# Part 3: Linear Regression Model

model <- function(data, distr){
  # Initialize an empty dataframe to store the results
  regression_performances <- data.frame(district = character(),
                          r_squared_lm1 = numeric(),
                          r_squared_lm2 = numeric(),
                          r_squared_lm3 = numeric(),
                          r_squared_lm4 = numeric(),
                          r_squared_lm5 = numeric(),
                          stringsAsFactors = FALSE)

  for (district in distr) {
    # Filter the data for the current district
    district_data <- data[data$district == district, ]

    # Test linear regression models
    lm1 <- lm(call_year_decimal ~ may_swe, data = district_data)
    lm2 <- lm(call_year_decimal ~ peak_swe, data = district_data)
    lm3 <- lm(call_year_decimal ~ may_fx_apr_to_sep, data = district_data)
    lm4 <- lm(call_year_decimal ~ apr_fx_apr_to_sep, data = district_data)
    lm5 <- lm(call_year_decimal ~ apr_eddi90d, data = district_data)

    # Get R-squared values for each model
    r_squared_lm1 <- summary(lm1)$r.squared
    r_squared_lm2 <- summary(lm2)$r.squared
    r_squared_lm3 <- summary(lm3)$r.squared
    r_squared_lm4 <- summary(lm4)$r.squared
    r_squared_lm5 <- summary(lm5)$r.squared

    # Append results to the dataframe
    regression_performances <- rbind(regression_performances, data.frame(district = district,
                                             r_squared_lm1 = r_squared_lm1,
                                             r_squared_lm2 = r_squared_lm2,
                                             r_squared_lm3 = r_squared_lm3,
                                             r_squared_lm4 = r_squared_lm4,
                                             r_squared_lm5 = r_squared_lm5))
  }

  return(regression_performances)
}


add_leading_zero <- function(x) {
  ifelse(nchar(x) < 7, paste0("0", x), x)
}

################################################################################
# Part 4: Initializations

# ONLY RUN THIS SECTION AFTER HAVING RUN THE ABOVE FUNCTIONS SET UP FIRST
# ------------------------------------------------------------------------------

# Initializing variables for get_call_data(): --------uncomment to get call data

#start_date <- "1970-01-01"
#end_date <- "2023-07-01"

# COMMENT/UNCOMMENT FOR YOUR VECTOR OF WDIDS:
# Rachel:
#wdids <- c('100839','100843','102003','200740','201015','203375','301143','303347','303676','400766','400861','400868','404167','500991','502131','504081','602105','604256','700812','702108','801000','801180','902107','904288','2300679') %>%
#  sapply(add_leading_zero)

# Angus:
#wdids <- c('2302103','2302119','4800572','4803987','4804002','4904484','4904485','4904487','6400503','6402411','6499999','6500523','6504485','7600505','7600600','8002107','8002126','8003550','0108327','0110900','0203936','0208828','0301305') %>%
# sapply(add_leading_zero)

# Emma
#wdids <- c('0301307','0302215','0303777','0400559','0400639','0405057','0500796','0601423','0601428','0601559','0604255','0700859','0703391','0801020','0808159','0901591','0905632','2300615','2302120','2302429','4800581','4801523','4803679','4804000','6405603','6406695','8005507') %>%
#  sapply(add_leading_zero)

------------------------------------
# Initialize variables/preprocess datasets for water_call_analysis():

# Call datasets: ------------------------------------
df_call_AW <- read_csv("data/cdss_call_data_AW.csv")
df_call_EG <- read_csv("data/cdss_call_data_EG.csv")
df_call_RB <- read_csv("data/cdss_call_data_RB.csv")
df_call_d5_6_upd <- read_csv("data/daily_call_data_district5_6.csv")

# Merge
df_call <- dplyr::bind_rows(df_call_AW, df_call_EG, df_call_RB, df_call_d5_6_upd)

# Preprocess a bit (include districts, make sure leading zeros are included)
df_call$analysis_wdid <- add_leading_zero(df_call$analysis_wdid)

df_call <- df_call %>%
  mutate(district = case_when(
    str_detect(analysis_wdid, "^01") ~ 1,
    str_detect(analysis_wdid, "^02") ~ 2,
    str_detect(analysis_wdid, "^03") ~ 3,
    str_detect(analysis_wdid, "^04") ~ 4,
    str_detect(analysis_wdid, "^05") ~ 5,
    str_detect(analysis_wdid, "^06") ~ 6,
    str_detect(analysis_wdid, "^07") ~ 7,
    str_detect(analysis_wdid, "^08") ~ 8,
    str_detect(analysis_wdid, "^09") ~ 9,
    str_detect(analysis_wdid, "^23") ~ 23,
    str_detect(analysis_wdid, "^64") ~ 64,
    str_detect(analysis_wdid, "^80") ~ 80,
    TRUE ~ NA_integer_
    ))
df_call <- df_call[df_call$district %in% c(1,2,3,4,5,6,7,8,9,23,64,80),]
#df_call$district <- as.character(df_call$district)

# One last filtering of wdids:
df_call <- df_call %>% filter(analysis_wdid == "0100839" | analysis_wdid == "0100843" |
                                analysis_wdid == "0200740" | analysis_wdid == "0201015" |
                                analysis_wdid == "0301305" | analysis_wdid == "0301307" |
                                analysis_wdid == "0302215" | analysis_wdid == "0303347" |
                                analysis_wdid == "0400559" | analysis_wdid == "0400639" |

                                #analysis_wdid == "0500796" | analysis_wdid == "0502131" |
                                #analysis_wdid == "0504081" | analysis_wdid == "0602105" |
                                #analysis_wdid == "0604255" |

                                analysis_wdid == "0602107" | analysis_wdid == "0602108" | # these are updated Boulder Creek and St Vrain WDIDs that might provide better coverage than previous WDIDS
                                analysis_wdid == "0602115" | analysis_wdid == "0600603" |
                                analysis_wdid == "0502130" | analysis_wdid == "0502124" |
                                analysis_wdid == "0502110" | analysis_wdid == "0502113" |
                                analysis_wdid == "0500523" |

                                analysis_wdid == "0700812" |
                                analysis_wdid == "0702108" | analysis_wdid == "0703391" |
                                analysis_wdid == "0801000" | analysis_wdid == "0801020" |
                                analysis_wdid == "0801180" | analysis_wdid == "0901591" |
                                analysis_wdid == "0902107" | analysis_wdid == "0904288" |
                                analysis_wdid == "0905632" | analysis_wdid == "2300679" |
                                analysis_wdid == "2302103" | analysis_wdid == "2302119" |
                                analysis_wdid == "2302120" | analysis_wdid == "2302429" |
                                analysis_wdid == "6400503" | analysis_wdid == "8002107" |
                                analysis_wdid == "8002126" | analysis_wdid == "8003550")



# Save raw call data
write_csv(df_call, "data/cdss_raw_daily_call_data_combined.csv")


# Predictor datasets: ------------------------------------
df_predictor <- read_csv("data/annual_model_upd_eddi_v2.csv")
df_predictor <- df_predictor[df_predictor$district %in% c("01","02","03","04","05","06","07","08","09","23","64","80"),]
df_predictor$wdid <- as.character(df_predictor$wdid)
df_predictor$wdid <- add_leading_zero(df_predictor$wdid)


#------------------------------------------------------------------------
# Try out the functions:

#get_call_data(start_date, end_date, wdids)

result <- water_call_analysis(df_call, df_predictor)
data_final <- result

# Specify district(s) for linear regression and plotting: you can change these.
distr <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 23, 64, 80)

reg_performances <- model(data_final, distr)


#################################################################################################
# Part 5: Plotting/Visualizations
#
# Loop for each district
for (d in distr){

  data <- data_final[data_final$district == d,]

  # # detrended_all_data_final_matrix <- as.matrix(detrended_all_data_final)
  # # print(detrended_all_data_final_matrix)
  # # corr_matrix <- cor(detrended_all_data_final_matrix)
  # # # full grid (has repeating values)
  # # corrplot::corrplot(corr_matrix, method=c("circle"), type=c("full"), na.or.zero = TRUE)

  #-------------------------------------------------------------------------------
  # Visualizing predictor and call data trends: grouped by WDID:
  p1 <- ggplot(data, aes_string(x="year", y = "may_swe", group = "wdid")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("May SWE by WDID")

  p2 <- ggplot(data, aes_string(x="year", y = "may_fx_apr_to_sep", group = "wdid")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("May NRCS Forecast by WDID")

  p3 <- ggplot(data, aes_string(x="year", y = "peak_swe", group = "wdid")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Peak SWE by WDID")

  p4 <- ggplot(data, aes_string(x="year", y = "apr_fx_apr_to_sep", group = "wdid")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("April NRCS Forecast by WDID")

  p5 <- ggplot(data, aes_string(x="year", y = "apr_eddi90d", group = "wdid")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("April EDDI 90d by WDID")

  p6 <- ggplot(data, aes_string(x="year", y = "call_year_decimal", group = "wdid")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line(aes(col = as.factor(wdid), linetype = as.factor(wdid))) +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Revised Summer Call Year by WDID")

  plot_title <- paste0("District ", paste(d), " - Regression Summary - Clean Data by WDID")
  ggsave(
    paste0("R/regression/", plot_title, ".png"),
    width = 14, height = 8,
    grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 6,
                 top = plot_title,
                 right = "")
  )

  #-------------------------------------------------------------------------------
  # Visualizing call vs predictor performances: grouped by WDID
  p7 <- ggplot(data, aes(x = may_swe, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point(aes(color = as.factor(wdid))) +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call Year vs May SWE by WDID")

  p8 <- ggplot(data, aes(x = may_fx_apr_to_sep, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point(aes(color = as.factor(wdid))) +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs May NRCS Forecast by WDID")

  p9 <- ggplot(data, aes(x = peak_swe, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point(aes(color = as.factor(wdid))) +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs Peak SWE by WDID")

  p10 <- ggplot(data, aes(x = apr_fx_apr_to_sep, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point(aes(color = as.factor(wdid))) +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs April NRCS Forecast by WDID")

  p11 <- ggplot(data, aes(x = apr_eddi90d, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point(aes(color = as.factor(wdid))) +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs April EDDI 90d by WDID")

  plot_title <- paste0("District ", paste(d), " - Regression Analysis Summary by WDID")
  ggsave(
    paste0("R/regression/", plot_title, ".png"),
    width = 10, height = 10,
    grid.arrange(p7, p8, p9, p10, p11, nrow = 5,
                 top = plot_title,
                 right = "")
  )

  #-------------------------------------------------------------------------------
  # Visualizing predictor and call trends: not grouped by WDID
  p12 <- ggplot(data, aes_string(x="year", y = "may_swe")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line() +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("May SWE")

  p13 <- ggplot(data, aes_string(x="year", y = "may_fx_apr_to_sep")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line() +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("May NRCS Forecast")

  p14 <- ggplot(data, aes_string(x="year", y = "peak_swe")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line() +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Peak SWE")

  p15 <- ggplot(data, aes_string(x="year", y = "apr_fx_apr_to_sep")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line() +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("April NRCS Forecast")

  p16 <- ggplot(data, aes_string(x="year", y = "apr_eddi90d")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line() +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("April EDDI 90d")

  p17 <- ggplot(data, aes_string(x="year", y = "call_year_decimal")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line() +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Revised Summer Call Year")

  plot_title <- paste0("District ", paste(d), " - Regression Summary - Clean Data")
  ggsave(
    paste0("R/regression/", plot_title, ".png"),
    width = 14, height = 8,
    grid.arrange(p12, p13, p14, p15, p16, p17, nrow = 6,
                 top = plot_title,
                 right = "")
  )

  #-------------------------------------------------------------------------------
  # Visualizing predictor vs call performances: not grouped by WDID
    p18 <- ggplot(data, aes(x = may_swe, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call Year vs May SWE")

  p19 <- ggplot(data, aes(x = may_fx_apr_to_sep, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs May NRCS Forecast")

  p20 <- ggplot(data, aes(x = peak_swe, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs Peak SWE")

  p21 <- ggplot(data, aes(x = apr_fx_apr_to_sep, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs April NRCS Forecast")

  p22 <- ggplot(data, aes(x = apr_eddi90d, y = call_year_decimal)) +
    stat_summary(fun.data=mean_cl_normal) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle("Summer Average Call vs April EDDI 90d")

  plot_title <- paste0("District ", paste(d), " - Regression Analysis Summary")
  ggsave(
    paste0("R/regression/", plot_title, ".png"),
    width = 10, height = 10,
    grid.arrange(p18, p19, p20, p21, p22, nrow = 5,
                 top = plot_title,
                 right = "")
  )

  # Plot the Average Call Year by Year
  call_detrended_data <- read_csv("data/call_detrended_data.csv")
  call_detrended_data <- call_detrended_data[call_detrended_data$district == d,]
  p23 <- ggplot(call_detrended_data, aes_string(x="year", y = "summer_call", group = "avg_condit")) +
    #stat_summary(fun.data=mean_cl_normal) +
    geom_line(aes(col = as.factor(avg_condit), linetype=as.factor(avg_condit))) +
    #geom_smooth(method='lm', formula= y~x) +
    theme_bw() +
    ggtitle(paste0("District ", paste(d), ": 6-month Average Call by Season (final clean data)"))
  ggsave(
    paste0("R/regression/District", paste(d), "-6-monthAverageCallbySeason-finalcleandata.png"),
    plot = p23,
    width = 10, height = 10
  )
}

# # Plot the Average Call Year by district
# call_detrended_data_by_district <- read_csv("data/call_detrended_data_by_district.csv")
# p24 <- ggplot(call_detrended_data_by_district, aes_string(x="year", y = "summer_call", group = "district")) +
#   #stat_summary(fun.data=mean_cl_normal) +
#   geom_line(aes(col = as.factor(dist), linetype=as.factor(dist))) +
#   #geom_smooth(method='lm', formula= y~x) +
#   theme_bw() +
#   ggtitle(paste0("6-month Average Summer Call by District"))
# ggsave(
#   paste0("6-monthAverageSummerCallbydistrict-finalcleandata.png"),
#   plot = p24,
#   width = 10, height = 10
# )
