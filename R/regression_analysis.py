# Linear Regression Model and Output for CPO Water Right Call Analysis
# Emma Golub
# 7/13/23

#######################################################################
# Univariate Linear Regression
#######################################################################

import os
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error
import numpy as np

# Create an output directory
output_dir = '../output/univariate_reg'
os.makedirs(output_dir, exist_ok=True)

# Read data with call years and all predictors
df = pd.read_csv("../data/annual_model_upd_eddi.csv")
df = df.drop(columns=["basin", "water_source", "gnis_id", "approp_date"])

# Wrangle data, exclude NAs
# Exclude rows with missing values or insignificant district
df = df.dropna()
districts_to_exclude = [49, 65, 76]
df = df[~df['district'].isin(districts_to_exclude)]

# Define key-value predictor-response pairings for each district
district_variables = {
    "1": ["apr_eddi90d", "avg_call_year"],
    "2": ["apr_eddi90d", "min_call_aug_sep"],
    "3": ["apr_eddi90d", "min_call_aug_sep"],
    "4": ["apr_eddi90d", "min_call_june_sep"],
    "5": ["may_swe", "min_call_may_sep"],
    "6": ["apr_eddi90d", "avg_call_year"],
    "7": ["may_swe", "avg_call_july_sep"],
    "8": ["may_swe", "avg_call_may_sep"],
    "9": ["may_swe", "avg_call_year"],
    "23": ["may_fx_apr_to_sep", "avg_call_may_sep"],
    "48": ["apr_eddi90d", "min_call_june_sep"],
    "64": ["apr_fx_apr_to_sep", "avg_call_may_sep"],
    "80": ["may_swe", "avg_call_june_sep"]
}

# Create a dictionary to store the regression results for each district
regression_results = {}

# Create a dictionary to store the summary statistics for each district
summary_statistics = {}

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]

    # Extract the predictor and response variables
    predictor = district_data[predictor_col]
    response = district_data[response_col]

    # Perform linear regression
    model = LinearRegression()
    model.fit(predictor.values.reshape(-1, 1), response)
    predicted_response = model.predict(predictor.values.reshape(-1, 1))

    # Calculate R-squared
    r2 = r2_score(response, predicted_response)

    # Calculate RMSE
    rmse = np.sqrt(mean_squared_error(response, predicted_response))

    # Store regression results and summary statistics
    regression_results[district] = (predictor, response, predicted_response)
    summary_statistics[district] = {'R-squared': r2, 'RMSE': rmse}

    # Plot regression line and scatter plot
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=predictor, y=response, ci=None, color='darkblue')
    plt.title(f'Linear Regression - District {district}', color='black')
    plt.xlabel(predictor_col, color='gray')
    plt.ylabel(response_col, color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {r2:.4f}', color='black', transform=plt.gca().transAxes)
    plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)

    # Show the plot
    plt.show()

    # Save the plot
    filename = os.path.join(output_dir, f'district_{district}.png')
    plt.savefig(filename, dpi=250)

# Display summary statistics for each district
for district, stats in summary_statistics.items():
    print(f"District {district}:")
    print(f"R-squared: {stats['R-squared']:.4f}")
    print(f"RMSE: {stats['RMSE']:.4f}")
    print()
    


#######################################################################
# Multiple Linear Regression (run this separately from the first)
# This compares performance of multiple predictors to best response variable
#######################################################################

import os
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error
import numpy as np

# Create an output directory
output_dir = '../output/multiple_reg'
os.makedirs(output_dir, exist_ok=True)

# Read data with call years and all predictors
df = pd.read_csv("../data/annual_model_upd_eddi.csv")
df = df.drop(columns=["basin", "water_source", "gnis_id", "approp_date"])

# Wrangle data, exclude NAs
# Exclude rows with missing values or insignificant district
df = df.dropna()
districts_to_exclude = [49, 65, 76]
df = df[~df['district'].isin(districts_to_exclude)]

# Define the key-value pairs for response variable and predictors by district
district_variables = {
    '1': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_year'),
    '2': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'min_call_aug_sep'),
    '3': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'min_call_aug_sep'),
    '4': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'min_call_june_sep'),
    '5': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_year'),
    '6': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_year'),
    '7': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_july_sep'),
    '8': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_may_sep'),
    '9': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_year'),
    '23': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_may_sep'),
    '48': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'min_call_june_sep'),
    '64': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_may_sep'),
    '80': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'apr_eddi90d'], 'avg_call_june_sep')
}

# Create a dictionary to store the regression results for each district
regression_results = {}

# Create a dictionary to store the summary statistics for each district
summary_statistics = {}

# Iterate over each unique district
for district, (predictor_cols, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]

    # Extract the predictors and response variables
    predictors = district_data[predictor_cols]
    response = district_data[response_col]

    # Perform multiple linear regression
    model = LinearRegression()
    model.fit(predictors, response)
    predicted_response = model.predict(predictors)

    # Calculate R-squared
    r2 = r2_score(response, predicted_response)

    # Calculate RMSE
    rmse = np.sqrt(mean_squared_error(response, predicted_response))

    # Store regression results and summary statistics
    regression_results[district] = (predictors, response, predicted_response)
    summary_statistics[district] = {'R-squared': r2, 'RMSE': rmse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=predicted_response, y=response, ci=None, color='darkblue')
    plt.title(f'Multiple Linear Regression - District {district}', color='black')
    plt.xlabel('Predicted Response', color='gray')
    plt.ylabel('Actual Response', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {r2:.4f}', color='black', transform=plt.gca().transAxes)
    plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot as a PNG file
    filename = os.path.join(output_dir, f'district_{district}.png')
    plt.savefig(filename, dpi=250)

# Display summary statistics for each district
for district, stats in summary_statistics.items():
    print(f"District {district}:")
    print(f"R-squared: {stats['R-squared']:.4f}")
    print(f"RMSE: {stats['RMSE']:.4f}")
    print()
