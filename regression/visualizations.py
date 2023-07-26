# Data Exploration - Visualizations

import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

# Create an output directory
output_dir = '../regression/output/visualizations'
os.makedirs(output_dir, exist_ok=True)

# Read data with call years and all predictors
df = pd.read_csv("../data/annual_model_v07202023.csv") # change the file for different datasets
#df = pd.read_csv("../data/call_model_drytail_lower_half.csv") # change the file for different datasets
#df = pd.read_csv("../data/call_model_drytail_lower_tercile.csv") # change the file for different datasets
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

# Create a list to store the correlation values for each district and predictor-response pairing
correlation_values = []

# Iterate over each unique district
for district, (predictor_col, response_col) in district_variables.items():
    # Filter the DataFrame for the current district
    district_data = df[df['district'] == int(district)]
    
    # Exclude rows with NAs in the predictor and response columns - simultaneously
    district_data = district_data.dropna(subset=[predictor_col, response_col]) 
    #print(district_data)

    # Calculate the correlation between the predictor and response variables
    correlation = district_data[predictor_col].corr(district_data[response_col])

    # Store the correlation value for the current district and predictor-response pairing
    correlation_values.append((district, correlation))

    # Plot regression line and scatter plot for each district to check relationships
    # plt.figure(figsize=(6.5, 4.5), dpi=250)
    # sns.regplot(x=predictor, y=response, ci=None, color='darkblue')
    # plt.title(f'Linear Regression - District {district}', color='black')
    # plt.xlabel(predictor_col, color='gray')
    # plt.ylabel(response_col, color='gray')
    # plt.xticks(color='gray')
    # plt.yticks(color='gray')
    # # Show summary statistics in the plot
    # plt.text(0.05, 0.9, f'R-squared: {r2_lin:.4f}', color='black', transform=plt.gca().transAxes)
    # plt.text(0.05, 0.8, f'Correlation coefficient: {corr:.4f}', color='black', transform=plt.gca().transAxes)
    # plt.text(0.05, 0.85, f'RMSE: {rmse:.4f}', color='black', transform=plt.gca().transAxes)
    # # Save the plot
    # filename = os.path.join(output_dir, f'regression_district_{district}.png')
    # plt.savefig(filename, dpi=250)

    
# Convert the correlation values to a DataFrame
correlation_df = pd.DataFrame(correlation_values, columns=['District', 'Correlation'])

""" # Create the box and whisker plot
plt.figure(figsize=(10, 6))
sns.boxplot(x='District', y='Correlation', data=correlation_df, color='skyblue')
plt.title('Variation of Correlation between Response and Predictor across Districts', color='black')
plt.xlabel('District', color='gray')
plt.ylabel('Correlation', color='gray')
plt.xticks(color='gray')
plt.yticks(color='gray')
filename = os.path.join(output_dir, 'correlation_boxplot.png')
plt.savefig(filename, dpi=250) """

# Create a bar plot showing the average correlation for each district
plt.figure(figsize=(10, 6))
sns.barplot(x='District', y='Correlation', data=correlation_df, color='skyblue', ci='sd')
plt.title('Correlation between Response and Predictor Variables across Districts', color='black')
plt.xlabel('District', color='gray')
plt.ylabel('Correlation', color='gray')
plt.xticks(color='gray')
plt.yticks(color='gray')
plt.axhline(y=0.5, color='red', linestyle='--') #sufficient correlation threshold
filename = os.path.join(output_dir, 'correlation_barplot.png')
plt.savefig(filename, dpi=250)

# Create a swarm plot showing the individual correlation values for each district
plt.figure(figsize=(10, 6))
sns.swarmplot(x='District', y='Correlation', data=correlation_df, color='#1f77b4', size = 8)
plt.title('Correlation between Response and Predictor across Districts', color='black')
plt.xlabel('District', color='gray')
plt.ylabel('Correlation', color='gray')
plt.xticks(color='gray')
plt.yticks(color='gray')
filename = os.path.join(output_dir, 'correlation_swarmplot.png')
plt.savefig(filename, dpi=250)