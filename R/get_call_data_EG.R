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

#################################################################################################
# Run the function:
get_call_data(start_date, end_date, wdids)


