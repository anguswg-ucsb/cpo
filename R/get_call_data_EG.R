##################################################################################################
# Simplified Water Call Analysis - Getting Call Data
# 7/26/23

# Function is defined in the beginning of the script. Scroll to the bottom to initialize
#  arguments, wrangle data and run the function.
#################################################################################################
library(tidyverse)
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

  tryCatch({

  data <- lapply(1:nrow(wdids_df), function(i) {
    message("Pausing iteration for 1.5 minutes...")

    # add pause in loop as to not overwhelm CDSS resources, DO NOT CHANGE WHEN running large number of WDIDs/districts
    Sys.sleep(90)

    message("Iteration resuming...")

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


add_leading_zero <- function(x) {
  ifelse(nchar(x) < 7, paste0("0", x), x)
}

################################################################################
# Initialize variables:

start_date <- "1980-01-01"
end_date <- "1980-12-31"

# COMMENT/UNCOMMENT FOR YOUR VECTOR OF WDIDS:
# Rachel:
#wdids <- c('100643','100805','100616','100897','100820','100776','100773','100722','100862','103828','100555','103574','100844','103554','103557','100532','103824','100718','100824','100759','100568','103002','103585','100684','100572','103657','100839','100843','102003','200838','202209','200809','200517') %>%
#  sapply(add_leading_zero)

# Angus:
#wdids <- c('200740','201015','203375','303570','303725','300853','303480','302106','300994','303712','301109','302132','300906','300530','301143','303347','303676','400577','400782','400766','400861','400868','404167','500511','501050','500621','500749','500991','502131','504081','600538','600536','602105') %>%
# sapply(add_leading_zero)

# Emma
wdids <- c('604256','700502','700553') %>% #'700812','702108','803532','800802','800902','802227','801000','801180','900816','902262','902107','904288','2300914','2301097','2300559','2301191','2300652','2306853','2300679','2302103','2302119','6400589','6400536','6400503','6402411','6499999','8000667','8005800','8002107','8002126') %>%
  sapply(add_leading_zero)

#################################################################################################
# Run the function:
get_call_data(start_date, end_date, wdids)


