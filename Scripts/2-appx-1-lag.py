import pandas as pd
from uuid import uuid4

admin1_d_dt = pd.read_csv('Data/model_dataset_admin1_D_date.csv', index_col=0)

def calculate_lagged_correlations(df, inflow_column, lags=range(1, 31)):
    """
    Calculate correlations between the specified inflow column and new_cases
    with the inflow column lagged by 1 to 30 days, grouped by admin1Pcod_2.
    
    Parameters:
    - df: DataFrame with columns admin1Pcod_2, date_2, inflow_column, new_cases
    - inflow_column: Name of the column to lag (e.g., 'admin1_inflow_trip_count')
    - lags: Range of lags (default: 1 to 30 days)
    
    Returns:
    - DataFrame with lag and corresponding correlation
    """
    # Ensure date_2 is in datetime format
    df = df.copy()
    df['date_2'] = pd.to_datetime(df['date_2'])
    
    # Sort by admin1Pcod_2 and date_2 to ensure proper lagging
    df = df.sort_values(['admin1Pcod_2', 'date_2'])
    
    # Initialize results list
    correlations = []
    
    # Calculate correlations for each lag
    for lag in lags:
        # Create lagged column within each admin1Pcod_2 group
        df[f'lag_{lag}'] = df.groupby('admin1Pcod_2')[inflow_column].shift(lag)
        
        # Calculate correlation between lagged inflow and new_cases
        # Drop NaN values resulting from lagging
        corr = df[[f'lag_{lag}', 'new_cases']].dropna().corr().iloc[0, 1]
        
        # Store result
        correlations.append({'lag': lag, 'correlation': corr})
        
        # Clean up lagged column to save memory
        df = df.drop(f'lag_{lag}', axis=1)
    
    # Convert results to DataFrame
    result_df = pd.DataFrame(correlations)
    
    return result_df

# List of columns to analyze
inflow_columns = [
    'admin1_inflow_trip_count',
    'admin1_inbound_admin2_crossbound_trip_count',
    'admin2_inbound_trip_count'
]

# Analyze each inflow column and find the lag with the largest correlation
for column in inflow_columns:
    # Calculate lagged correlations
    result = calculate_lagged_correlations(admin1_d_dt, inflow_column=column, lags=range(1, 31))
    
    # Find the lag with the largest correlation
    max_corr_row = result.loc[result['correlation'].abs().idxmax()]
    max_lag = int(max_corr_row['lag'])
    max_corr = max_corr_row['correlation']
    
    # Print the result
    print(f"For {column}:")
    print(f"The lag with the largest correlation is {max_lag} days with a correlation of {max_corr:.4f}")
    print()