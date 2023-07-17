# Linear Regression Model and Output for CPO Water Right Call Analysis
# Emma Golub
# Last updated: 7/17/23

#######################################################################
# Univariate Linear Regression
#######################################################################

import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.model_selection import train_test_split
import numpy as np
import scipy.stats as stats

# Create an output directory
output_dir = '../output/univariate_reg'
os.makedirs(output_dir, exist_ok=True)

# Read data with call years and all predictors
df = pd.read_csv("../data/annual_model_upd_eddi.csv")
df = df.drop(columns=["basin", "water_source", "gnis_id", "approp_date"])

# Wrangle data, exclude NAs
districts_to_exclude = [49, 65, 76]
df = df[~df['district'].isin(districts_to_exclude)]
# Check value types in the dataframe

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

    # Exclude rows with missing values in the predictor and response columns
    district_data = district_data.dropna(subset=[predictor_col, response_col])
    print(district_data.shape)

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col]
    response = district_data[response_col]

# # Plot regression line and scatter plot to check relationships
    # plt.figure(figsize=(6.5, 4.5), dpi=250)
    # sns.regplot(x=predictor, y=response, ci=None, color='darkblue')
    # plt.title(f'Linear Regression - District {district}', color='black')
    # plt.xlabel(predictor_col, color='gray')
    # plt.ylabel(response_col, color='gray')
    # plt.xticks(color='gray')
    # plt.yticks(color='gray')
    # # Show summary statistics in the plot
    # plt.text(0.05, 0.9, f'R-squared: {r2:.4f}', color='black', transform=plt.gca().transAxes)
    # plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)
    # plt.show()
    # # Save the plot
    # filename = os.path.join(output_dir, f'regression_district_{district}.png')
    # plt.savefig(filename, dpi=250)

    # Perform linear regression. Split data into training and testing.
    X_train, X_test, y_train, y_test = train_test_split(predictor, response, test_size=0.25)
    model = LinearRegression()
    model.fit(X_train.values.reshape(-1, 1), y_train)
    y_pred = model.predict(X_test.values.reshape(-1, 1))

    # Calculate metrics
    r2 = r2_score(y_test, y_pred)
    mae = mean_absolute_error(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    print("R2:", r2)
    print("MAE:", mae)
    print("MSE:", mse)
    print("RMSE:", rmse)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'R-squared': r2, 'RMSE': rmse, 'MAE': mae, 'MSE': mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Univariate Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictor {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')
    
    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {r2:.4f}', color='black', transform=plt.gca().transAxes)
    plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250) # why is it printing out blank


# Display summary statistics for each district
for district, stats in summary_statistics.items():
    print(f"District {district}:")
    print(f"R-squared: {stats['R-squared']:.4f}")
    print(f"RMSE: {stats['RMSE']:.4f}")
    print(f"MSE: {stats['MSE']:.4f}")
    print(f"MAE: {stats['MAE']:.4f}")
    print()
    

#######################################################################
# Multiple Linear Regression (run this separately from the first)
# This compares performance of multiple predictors to best response variable
#######################################################################

import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.model_selection import train_test_split
import numpy as np
import scipy.stats as stats
from statsmodels.stats.outliers_influence import OLSInfluence
from statsmodels.regression.linear_model import OLS

# Create an output directory
output_dir = '../output/multiple_reg'
os.makedirs(output_dir, exist_ok=True)

# Read data with call years and all predictors
df = pd.read_csv("../data/annual_model_upd_eddi.csv")
df = df.drop(columns=["basin", "water_source", "gnis_id", "approp_date"])

# Wrangle data, exclude NAs
districts_to_exclude = [49, 65, 76]
df = df[~df['district'].isin(districts_to_exclude)]

# Define the key-value pairs for response variable and predictors by district
district_variables = {
    '1': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '2': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'min_call_aug_sep'),
    '3': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'min_call_aug_sep'),
    '4': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'min_call_june_sep'),
    '5': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '6': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '7': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_july_sep'),
    '8': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_may_sep'),
    '9': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '23': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_may_sep'),
    '48': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'min_call_june_sep'),
    '64': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_may_sep'),
    '80': (['apr_fx_apr_to_sep', 'may_fx_apr_to_sep', 'apr_eddi90d', 'may_swe'], 'avg_call_june_sep')
}

# Create a dictionary to store the regression results for each district
regression_results = {}

# Create a dictionary to store the summary statistics for each district
summary_statistics = {}

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]
    
    # Exclude rows with NAs in the predictor and response columns - simultaneously
    district_data = district_data.dropna(subset=[*predictor_col, response_col]) 
    #print(district_data)

    ############################################################################
    # Z-SCORE OUTLIER APPROACH
    # # Identify and remove outliers using Z-Score for the combined columns
    # # Combine predictor_col and response_col for outlier detection
    # combined_cols = predictor_col + [response_col]
    # z_scores = np.abs(stats.zscore(district_data[combined_cols], axis=0))
    # threshold = 5  # Try different values
    # outlier_indices = np.any(z_scores > threshold, axis=1)

    # # Filter out rows with outliers from both predictor_col and response_col
    # predictor = district_data[predictor_col][~outlier_indices]
    # response = district_data[response_col][~outlier_indices]
    
    ############################################################################
    # COOK'S DISTANCE OUTLIER APPROACH (better for linear regression)
    #* BUG STILL NEEDS ADJUSTMENTS... TOO MANY OUTLIERS REMOVED, SOMETHING FUNKY WITH DISTRICT 3
    # Extract the predictors and response variables
    predictor = district_data[predictor_col]
    response = district_data[response_col]

    # Fit linear regression model
    model = OLS(response, predictor)
    results = model.fit()

    # Calculate Cook's distance for each data point
    infl = OLSInfluence(results)
    cook_dist = infl.cooks_distance[0]

    # Set a threshold for Cook's distance to identify influential points
    threshold = 4 / predictor.shape[0]  # Try different values (4,5,6)

    # Identify influential points (outliers) based on Cook's distance and the current threshold
    outlier_indices = cook_dist > threshold

    # Filter out rows with influential points from both predictor_col and response_col
    predictor = predictor[~outlier_indices]
    response = response[~outlier_indices]

    # Check outliers removed
    print(f"Original number of samples: {df[df['district'] == int(district)].shape[0]}")
    print(f"Number of samples after removing outliers from predictor_col: {predictor.shape[0]}")
    print(f"Number of samples after removing outliers from response_col: {response.shape[0]}")

    if predictor.shape[0] < district_data.shape[0]:
        print("Outliers were removed from predictor_col.")
    else:
        print("No outliers were removed from predictor_col.")
        
    if response.shape[0] < district_data.shape[0]:
        print("Outliers were removed from response_col.")
    else:
        print("No outliers were removed from response_col.")

    # Perform multiple linear regression
    X_train, X_test, y_train, y_test = train_test_split(predictor, response, test_size = 0.25, random_state=42) # Set random_state for reproducibility
    # Reshape response variable if necessary
    y_train = np.ravel(y_train)
    y_test = np.ravel(y_test)   
    model = LinearRegression()
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)

    # Calculate metrics
    r2 = r2_score(y_test, y_pred)
    mae = mean_absolute_error(y_test,y_pred)
    #squared True returns MSE value, False returns RMSE value.
    mse = mean_squared_error(y_test,y_pred) #default=True
    rmse = mean_squared_error(y_test,y_pred,squared=False)
    print("R2:", r2)
    print("MAE:",mae)
    print("MSE:",mse)
    print("RMSE:",rmse)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'R-squared': r2, 'RMSE': rmse, 'MAE': mae, 'MSE': mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Multiple Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictors {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {r2:.4f}', color='black', transform=plt.gca().transAxes)
    plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot as a PNG file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)

