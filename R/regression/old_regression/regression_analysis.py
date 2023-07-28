# Linear Regression Model and Output for CPO Water Right Call Analysis
# Emma Golub
# Last updated: 7/19/23

import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import scipy.stats as stats
from sklearn.linear_model import SGDRegressor, LinearRegression
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from statsmodels.stats.outliers_influence import OLSInfluence
from statsmodels.regression.linear_model import OLS


#######################################################################
# Univariate Linear Regression
#######################################################################

# Create an output directory
output_dir = '../regression/output/univariate_reg'
os.makedirs(output_dir, exist_ok=True)

# Read data with call years and all predictors
df = pd.read_csv("../data/annual_model_v07202023.csv")
df = df.drop(columns=["basin", "water_source", "gnis_id", "approp_date"])

# Wrangle data, exclude NAs
districts_to_exclude = [49, 65, 76]
df = df[~df['district'].isin(districts_to_exclude)]
# Check value types in the dataframe

# Define key-value predictor-response pairings for each district
district_variables = {
    "1": ["apr_eddi90d", "avg_call_year"],
    "2": ["apr_eddi90d", "avg_call_year"], #replaced with avg instead of min
    "3": ["apr_eddi90d", "avg_call_year"], #replaced with avg instead of min
    "4": ["apr_eddi90d", "avg_call_year"], #replaced with avg instead of min
    "5": ["may_swe", "avg_call_year"], #replaced with avg instead of min
    "6": ["apr_eddi90d", "avg_call_year"],
    "7": ["may_swe", "avg_call_july_sep"],
    "8": ["may_swe", "avg_call_may_sep"],
    "9": ["may_swe", "avg_call_year"],
    "23": ["may_fx_apr_to_sep", "avg_call_may_sep"],
    "48": ["apr_eddi90d", "avg_call_year"], #replaced with avg instead of min
    "64": ["apr_fx_apr_to_sep", "avg_call_may_sep"],
    "80": ["may_swe", "avg_call_june_sep"]
}

# Create a dictionary to store the regression results for each district
regression_results = {}

# Create a dictionary to store the summary statistics for each district
summary_statistics = {}

######################################################################################################
# APPROACH A: REGULAR REGRESSION
"""
# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]
    
    # Exclude rows with NAs in the predictor and response columns - simultaneously
    district_data = district_data.dropna(subset=[predictor_col, response_col]) 
    #print(district_data)

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col]
    response = district_data[response_col]

    # Perform linear regression. Split data into training and testing.
    X_train, X_test, y_train, y_test = train_test_split(predictor, response, test_size=0.25)
    model = LinearRegression()
    model.fit(X_train.values.reshape(-1, 1), y_train)
    y_pred = model.predict(X_test.values.reshape(-1, 1))

    # Calculate metrics
    r2_model = r2_score(y_test, y_pred)
    mae = mean_absolute_error(y_test,y_pred)
    mse = mean_squared_error(y_test,y_pred)
    rmse = mean_squared_error(y_test,y_pred,squared=False)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'R-squared': r2_model, 'RMSE': rmse, 'MAE': mae, 'MSE': mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Univariate Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictors {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {r2_model:.4f}', color='black', transform=plt.gca().transAxes)
    plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot as a PNG file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)

# Display summary statistics for each district
for district, stats in summary_statistics.items():
    #print(f"District {district}:")
    print(f"R-squared: {stats['R-squared']:.4f}") 

"""
######################################################################################################
# APPROACH A.1: REGULAR REGRESSION W OUTLIER REMOVAL

def detect_outliers(data):
    z_scores = stats.zscore(data)
    abs_z_scores = np.abs(z_scores)
    return np.where(abs_z_scores > 3)

def remove_outliers(data, outliers):
    return data[~np.isin(data.index, outliers)]

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]

    # Exclude rows with NAs in the predictor and response columns - simultaneously
    district_data = district_data.dropna(subset=[predictor_col, response_col]) 

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col]
    response = district_data[response_col]

    # Identify and remove outliers from the predictor and response
    predictor_outliers = detect_outliers(predictor)
    response_outliers = detect_outliers(response)
    outliers = np.union1d(predictor_outliers, response_outliers) #return values in either predictor or response outliers
    predictor = remove_outliers(predictor, outliers)
    response = remove_outliers(response, outliers)

    # Perform linear regression. Split data into training and testing.
    X_train, X_test, y_train, y_test = train_test_split(predictor, response, test_size=0.25)
    model = LinearRegression()
    model.fit(X_train.values.reshape(-1, 1), y_train)
    y_pred = model.predict(X_test.values.reshape(-1, 1))

    # Calculate metrics
    r2_model = r2_score(y_test, y_pred)
    mae = mean_absolute_error(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    rmse = mean_squared_error(y_test, y_pred, squared=False)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'R-squared': r2_model, 'RMSE': rmse, 'MAE': mae, 'MSE': mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Univariate Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictors {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {r2_model:.4f}', color='black', transform=plt.gca().transAxes)
    plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot as a PNG file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)

# Display summary statistics for each district
for district, stats in summary_statistics.items():
    print(f"R-squared: {stats['R-squared']:.4f}")

############################################################################################
"""# APPROACH B: K-FOLDS AND SDG
# Define the number of folds for cross-validation
num_folds = 7
kfold = KFold(n_splits=num_folds) #Kfold object

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]

    # Exclude rows with missing values in the predictor and response columns
    district_data = district_data.dropna(subset=[predictor_col, response_col])
    print(district_data.shape)

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col].values
    response = district_data[response_col].values

    # Normalize the predictor for better convergence in gradient descent
    predictor = (predictor - np.mean(predictor)) / np.std(predictor)
    # Try normalizing the response variable next?

    # Initialize lists to store evaluation metrics for each fold
    r2_scores = []
    mse_scores = []
    mae_scores = []

    # Perform k-fold cross-validation
    for train_indices, test_indices in kfold.split(predictor):
        # Split the data into training and testing sets for the current fold
        X_train, X_test = predictor[train_indices], predictor[test_indices]
        y_train, y_test = response[train_indices], response[test_indices]

        # Reshape the input arrays for sklearn
        X_train = X_train.reshape(-1, 1)
        X_test = X_test.reshape(-1, 1)

        # Create an instance of the SGDRegressor
        model = SGDRegressor(penalty="l2", alpha=0.0001) #incorporate ridge regression, uses squared error loss fcn

        # Fit the model on the training data
        model.fit(X_train, y_train)

        # Make predictions on the test data
        y_pred = model.predict(X_test)

        # Calculate evaluation metrics
        r2 = r2_score(y_test, y_pred)
        mse = mean_squared_error(y_test, y_pred)
        mae = mean_absolute_error(y_test, y_pred)

        # Append the evaluation metrics to the respective lists
        r2_scores.append(r2)
        mse_scores.append(mse)
        mae_scores.append(mae)

    # Calculate the average evaluation metrics across all folds
    avg_r2 = np.mean(r2_scores)
    avg_mse = np.mean(mse_scores)
    avg_mae = np.mean(mae_scores)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'Avg R-squared': avg_r2, 'Avg MSE': avg_mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Univariate Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictor {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {avg_r2:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot to a file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)
    plt.show()

    # Print the regression results and summary statistics for the current district
    print(f"District {district}:")
    print("R2 Scores:", r2_scores)
    print("MSE Scores:", mse_scores)
    print("MAE Scores:", mae_scores)
    print("Average R2:", avg_r2)
    print("Average MSE:", avg_mse)
    print("Average MAE:", avg_mae)

    # Display summary statistics for each district
for district, stats in summary_statistics.items():
    #print(f"District {district}:")
    print(f"Avg R-squared: {stats['Avg R-squared']:.4f}") 
"""

############################################################################################
""" 
# APPROACH B.1: K-FOLDS AND SDG WITH OUTLIER REMOVAL

def detect_outliers(data):
    z_scores = stats.zscore(data)
    abs_z_scores = np.abs(z_scores)
    return np.where(abs_z_scores > 4)

def remove_outliers(data, outliers):
    return data[~np.isin(np.arange(len(data)), outliers)]

# Define the number of folds for cross-validation
num_folds = 4
kfold = KFold(n_splits=num_folds) #Kfold object

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]

    # Exclude rows with missing values in the predictor and response columns
    district_data = district_data.dropna(subset=[predictor_col, response_col])
    print(district_data.shape)

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col].values
    response = district_data[response_col].values

    # Identify and remove outliers from the predictor and response
    predictor_outliers = detect_outliers(predictor)
    response_outliers = detect_outliers(response)
    outliers = np.union1d(predictor_outliers, response_outliers)
    predictor = remove_outliers(predictor, outliers)
    response = remove_outliers(response, outliers)

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

    # Normalize the predictor for better convergence in gradient descent
    predictor = (predictor - np.mean(predictor)) / np.std(predictor)
    # Try normalizing the response variable next?

    # Initialize lists to store evaluation metrics for each fold
    r2_scores = []
    mse_scores = []
    mae_scores = []

    # Perform k-fold cross-validation
    for train_indices, test_indices in kfold.split(predictor):
        # Split the data into training and testing sets for the current fold
        X_train, X_test = predictor[train_indices], predictor[test_indices]
        y_train, y_test = response[train_indices], response[test_indices]

        # Reshape the input arrays for sklearn
        X_train = X_train.reshape(-1, 1)
        X_test = X_test.reshape(-1, 1)

        # Create an instance of the SGDRegressor
        model = SGDRegressor(penalty="l2", alpha=0.0001) #incorporate ridge regression, uses squared error loss fcn

        # Fit the model on the training data
        model.fit(X_train, y_train)

        # Make predictions on the test data
        y_pred = model.predict(X_test)

        # Calculate evaluation metrics
        r2 = r2_score(y_test, y_pred)
        mse = mean_squared_error(y_test, y_pred)
        mae = mean_absolute_error(y_test, y_pred)

        # Append the evaluation metrics to the respective lists
        r2_scores.append(r2)
        mse_scores.append(mse)
        mae_scores.append(mae)

    # Calculate the average evaluation metrics across all folds
    avg_r2 = np.mean(r2_scores)
    avg_mse = np.mean(mse_scores)
    avg_mae = np.mean(mae_scores)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'Avg R-squared': avg_r2, 'Avg MSE': avg_mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Univariate Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictor {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'Avg R2: {avg_r2:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot to a file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)
    plt.show()

    # Print the regression results and summary statistics for the current district
    print(f"District {district}:")
    print("R2 Scores:", r2_scores)
    print("MSE Scores:", mse_scores)
    print("MAE Scores:", mae_scores)
    print("Average R2:", avg_r2)
    print("Average MSE:", avg_mse)
    print("Average MAE:", avg_mae)

    # Display summary statistics for each district
for district, stats in summary_statistics.items():
    #print(f"District {district}:")
    print(f"Avg R-squared: {stats['Avg R-squared']:.4f}") 
 
"""
# # Plot regression line and scatter plot to check relationships
#     plt.figure(figsize=(6.5, 4.5), dpi=250)
#     sns.regplot(x=predictor, y=response, ci=None, color='darkblue')
#     plt.title(f'Linear Regression - District {district}', color='black')
#     plt.xlabel(predictor_col, color='gray')
#     plt.ylabel(response_col, color='gray')
#     plt.xticks(color='gray')
#     plt.yticks(color='gray')
#     # Show summary statistics in the plot
#     plt.text(0.05, 0.9, f'R-squared: {r2_lin:.4f}', color='black', transform=plt.gca().transAxes)
#     plt.text(0.05, 0.8, f'Correlation coefficient: {corr:.4f}', color='black', transform=plt.gca().transAxes)
#     plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)
#     # Save the plot
#     filename = os.path.join(output_dir, f'regression_district_{district}.png')
#     plt.savefig(filename, dpi=250)


#######################################################################
# Multiple Linear Regression 
# This compares performance of multiple predictors to best response variable
#######################################################################

# Create an output directory
output_dir = '../regression/output/multiple_reg'
os.makedirs(output_dir, exist_ok=True)

# Read data with call years and all predictors
df = pd.read_csv("../data/annual_model_v07202023.csv")
df = df.drop(columns=["basin", "water_source", "gnis_id", "approp_date"])

# Wrangle data, exclude NAs
districts_to_exclude = [49, 65, 76]
df = df[~df['district'].isin(districts_to_exclude)]

# Define the key-value pairs for response variable and predictors by district
district_variables = {
    '1': (['apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '2': (['apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '3': (['apr_eddi90d', 'may_fx_apr_to_sep'], 'avg_call_year'),
    '4': (['apr_eddi90d', 'may_fx_apr_to_sep'], 'avg_call_year'),
    '5': (['apr_eddi90d', 'may_fx_apr_to_sep'], 'avg_call_year'),
    '6': (['apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '7': (['apr_eddi90d', 'may_swe'], 'avg_call_july_sep'),
    '8': (['apr_eddi90d', 'may_swe'], 'avg_call_may_sep'),
    '9': (['apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '23': (['apr_eddi90d', 'may_fx_apr_to_sep'], 'avg_call_may_sep'),
    '48': (['apr_eddi90d', 'may_swe'], 'avg_call_year'),
    '64': (['apr_eddi90d', 'apr_fx_apr_to_sep'], 'avg_call_may_sep'),
    '80': (['apr_eddi90d', 'apr_fx_apr_to_sep'], 'avg_call_june_sep')
}

# Create a dictionary to store the regression results for each district
regression_results = {}

# Create a dictionary to store the summary statistics for each district
summary_statistics = {}

#############################################################################################################
# APPROACH A.2 NORMAL REGRESSION WITH OUTLIER REMOVAL
""" def detect_outliers(data):
    z_scores = stats.zscore(data)
    abs_z_scores = np.abs(z_scores)
    return np.where(abs_z_scores > 3)

def remove_outliers(data, outliers):
    return data[~np.isin(np.arange(len(data)), outliers)] """

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]
    
    # Exclude rows with NAs in the predictor and response columns - simultaneously
    district_data = district_data.dropna(subset=[*predictor_col, response_col]) 
    #print(district_data)

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col]
    response = district_data[response_col]

"""     # Identify and remove outliers from the predictor and response
    predictor_outliers = detect_outliers(predictor)
    response_outliers = detect_outliers(response)
    outliers = np.union1d(predictor_outliers, response_outliers)
    predictor = remove_outliers(predictor, outliers)
    response = remove_outliers(response, outliers)

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

    # Normalize the predictor for better convergence in gradient descent
    predictor = (predictor - np.mean(predictor)) / np.std(predictor) """


    # Perform multiple linear regression
    X_train, X_test, y_train, y_test = train_test_split(predictor, response, test_size = 0.25, random_state=42) # Set random_state for reproducibility
    # Reshape response variable if necessary
    y_train = np.ravel(y_train)
    y_test = np.ravel(y_test)   
    model = LinearRegression()
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)

    # Calculate metrics
    r2_model = r2_score(y_test, y_pred)
    mae = mean_absolute_error(y_test,y_pred)
    mse = mean_squared_error(y_test,y_pred)
    rmse = mean_squared_error(y_test,y_pred,squared=False)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'R-squared': r2_model, 'RMSE': rmse, 'MAE': mae, 'MSE': mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Multiple Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictors {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'R-squared: {r2_model:.4f}', color='black', transform=plt.gca().transAxes)
    plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot as a PNG file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)

# Display summary statistics for each district
for district, stats in summary_statistics.items():
    #print(f"District {district}:")
    print(f"R-squared: {stats['R-squared']:.4f}") 



#############################################################################################################
""" # APPROACH B.1: K-FOLDS AND SDG 
# BUG WITH SPLITTING DATA INTO FOLDS

# Define the number of folds for cross-validation
num_folds = 7
kfold = KFold(n_splits=num_folds) #Kfold object

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]

    # Exclude rows with missing values in the predictor and response columns
    district_data = district_data.dropna(subset=[*predictor_col, response_col])
    print(district_data.shape)

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col]
    response = district_data[response_col]

    # Normalize the predictor for better convergence in gradient descent
    predictor = (predictor - np.mean(predictor)) / np.std(predictor)
    # Try normalizing the response variable next?

    # Initialize lists to store evaluation metrics for each fold
    r2_scores = []
    mse_scores = []
    mae_scores = []

    # Perform k-fold cross-validation
    for train_indices, test_indices in kfold.split(predictor):
        # Split the data into training and testing sets for the current fold
        X_train, X_test = predictor[train_indices], predictor[test_indices]
        y_train, y_test = response[train_indices], response[test_indices]

        # Reshape the input arrays for sklearn
        X_train = X_train.reshape(-1, 1)
        X_test = X_test.reshape(-1, 1)

        # Create an instance of the SGDRegressor
        model = SGDRegressor(penalty="l2", alpha=0.0001) #incorporate ridge regression, uses squared error loss fcn

        # Fit the model on the training data
        model.fit(X_train, y_train)

        # Make predictions on the test data
        y_pred = model.predict(X_test)

        # Calculate evaluation metrics
        r2 = r2_score(y_test, y_pred)
        mse = mean_squared_error(y_test, y_pred)
        mae = mean_absolute_error(y_test, y_pred)

        # Append the evaluation metrics to the respective lists
        r2_scores.append(r2)
        mse_scores.append(mse)
        mae_scores.append(mae)

    # Calculate the average evaluation metrics across all folds
    avg_r2 = np.mean(r2_scores)
    avg_mse = np.mean(mse_scores)
    avg_mae = np.mean(mae_scores)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'Avg R-squared': avg_r2, 'Avg MSE': avg_mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Univariate Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictor {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'Avg R2: {avg_r2:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot to a file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)
    plt.show()

    # Print the regression results and summary statistics for the current district
    print(f"District {district}:")
    print("R2 Scores:", r2_scores)
    print("MSE Scores:", mse_scores)
    print("MAE Scores:", mae_scores)
    print("Average R2:", avg_r2)
    print("Average MSE:", avg_mse)
    print("Average MAE:", avg_mae)

    # Display summary statistics for each district
for district, stats in summary_statistics.items():
    #print(f"District {district}:")
    print(f"Avg R-squared: {stats['Avg R-squared']:.4f}")  """

    #############################################################################################################
# APPROACH B.1: K-FOLDS AND SDG AND OUTLIER REMOVAL
""" 
def detect_outliers(data):
    z_scores = stats.zscore(data)
    abs_z_scores = np.abs(z_scores)
    return np.where(abs_z_scores > 3)

def remove_outliers(data, outliers):
    return data[~np.isin(np.arange(len(data)), outliers)]

# Define the number of folds for cross-validation
num_folds = 7
kfold = KFold(n_splits=num_folds) #Kfold object

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]

    # Exclude rows with missing values in the predictor and response columns
    district_data = district_data.dropna(subset=[predictor_col, response_col])
    print(district_data.shape)

    # Extract the predictor and response vectors
    predictor = district_data[predictor_col].values
    response = district_data[response_col].values

    # Identify and remove outliers from the predictor and response
    predictor_outliers = detect_outliers(predictor)
    response_outliers = detect_outliers(response)
    outliers = np.union1d(predictor_outliers, response_outliers)
    predictor = remove_outliers(predictor, outliers)
    response = remove_outliers(response, outliers)

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

    # Normalize the predictor for better convergence in gradient descent
    predictor = (predictor - np.mean(predictor)) / np.std(predictor)
    # Try normalizing the response variable next?

    # Initialize lists to store evaluation metrics for each fold
    r2_scores = []
    mse_scores = []
    mae_scores = []

    # Perform k-fold cross-validation
    for train_indices, test_indices in kfold.split(predictor):
        # Split the data into training and testing sets for the current fold
        X_train, X_test = predictor[train_indices], predictor[test_indices]
        y_train, y_test = response[train_indices], response[test_indices]

        # Reshape the input arrays for sklearn
        X_train = X_train.reshape(-1, 1)
        X_test = X_test.reshape(-1, 1)

        # Create an instance of the SGDRegressor
        model = SGDRegressor(penalty="l2", alpha=0.0001) #incorporate ridge regression, uses squared error loss fcn

        # Fit the model on the training data
        model.fit(X_train, y_train)

        # Make predictions on the test data
        y_pred = model.predict(X_test)

        # Calculate evaluation metrics
        r2 = r2_score(y_test, y_pred)
        mse = mean_squared_error(y_test, y_pred)
        mae = mean_absolute_error(y_test, y_pred)

        # Append the evaluation metrics to the respective lists
        r2_scores.append(r2)
        mse_scores.append(mse)
        mae_scores.append(mae)

    # Calculate the average evaluation metrics across all folds
    avg_r2 = np.mean(r2_scores)
    avg_mse = np.mean(mse_scores)
    avg_mae = np.mean(mae_scores)

    # Store regression results and summary statistics
    regression_results[district] = (X_test, y_test, y_pred)
    summary_statistics[district] = {'Avg R-squared': avg_r2, 'Avg MSE': avg_mse}

    # Plot predicted vs. actual response
    plt.figure(figsize=(6.5, 4.5), dpi=250)
    sns.regplot(x=y_pred, y=y_test, ci=None, color='darkblue')
    plt.title(f'Univariate Linear Regression Performance - District {district}', color='black')
    plt.xlabel(f'Predicted Response {response_col} by Predictor {predictor_col}', color='gray')
    plt.ylabel(f'Actual Response {response_col}', color='gray')
    plt.xticks(color='gray')
    plt.yticks(color='gray')

    # Show summary statistics in the plot
    plt.text(0.05, 0.9, f'Avg R2: {avg_r2:.4f}', color='black', transform=plt.gca().transAxes)

    # Save the plot to a file
    filename = os.path.join(output_dir, f'performance_district_{district}.png')
    plt.savefig(filename, dpi=250)
    plt.show()

    # Print the regression results and summary statistics for the current district
    print(f"District {district}:")
    print("R2 Scores:", r2_scores)
    print("MSE Scores:", mse_scores)
    print("MAE Scores:", mae_scores)
    print("Average R2:", avg_r2)
    print("Average MSE:", avg_mse)
    print("Average MAE:", avg_mae)

    # Display summary statistics for each district
for district, stats in summary_statistics.items():
    #print(f"District {district}:")
    print(f"Avg R-squared: {stats['Avg R-squared']:.4f}")  """