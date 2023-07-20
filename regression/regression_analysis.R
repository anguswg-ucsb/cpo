################################################################################
# Script performing simple linear regression among call years and predictors.
# Emma Golub
# Updated at: 7/12/23
################################################################################

library(tidyverse)

# Read data with call years and all predictors
df <- read.csv("./data/annual_model_upd_eddi.csv") %>%
  select(-c("X","basin", "water_source", "gnis_id", "approp_date"))
values_count <- sapply(lapply(df, unique), length)  # Identify variables with 1 value

# Wrangle data, exclude NAs
df <- df[complete.cases(df), ]
dim(df)

# Define dataframe for just predictor and response variables
# RIGHT NOW THERE IS AN ISSUE WITH FACTOR LEVELS IN THE DATAFRAME. WILL NEED TO
#  SIMPLIFY DATAFRAME TO ONE THAT CAN BE USED PROPERLY IN LM().

# Define predictor-response pairings for each district
districts <- list(
  d1 = c("apr_eddi90d", "avg_call_year"),
  d2 = c("apr_eddi90d", "min_call_aug_sep"),
  d3 = c("apr_eddi90d", "min_call_aug_sep"),
  d4 = c("apr_eddi90d", "min_call_june_sep"),
  d5 = c("may_swe", "min_call_may_sep"),
  d6 = c("apr_eddi90d", "avg_call_year"),
  d7 = c("may_swe", "avg_call_july_sep"),
  d8 = c("may_swe", "avg_call_may_sep"),
  d9 = c("may_swe", "avg_call_year"),
  d23 = c("may_fx_apr_to_sep", "avg_call_may_sep"),
  d48 = c("apr_eddi90d", "min_call_june_sep"),
  d64 = c("apr_fx_apr_to_sep", "avg_call_may_sep"),
  d80 = c("may_swe", "avg_call_june_sep")
)

# Consider including additional pairings with multiple responses later for a multiple linear regression...

# Define function to perform linear regression for each district
perform_linear_regression <- function(district, response_var, predictor_var) {
  call_model <- df %>%
    filter(district == district)
  lm_model <- lm(response_var ~ predictor_var, data = call_model)
  return(lm_model)
}

# Perform linear regression for each district
results <- list()
for (district in names(districts)) {
  response_var <- districts[[district]][1]
  predictor_var <- districts[[district]][2]
  model <- perform_linear_regression(district, response_var, predictor_var)
  results[[district]] <- model
}

# Accessing the results
# For example, to access the linear regression model for district "d1":
district_model_d1 <- results$d1
summary(district_model_d1)  # Display summary of the model
