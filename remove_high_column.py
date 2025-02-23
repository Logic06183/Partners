import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# Remove the HIGH column
df = df.drop('HIGH', axis=1)

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify the change
print("\nVerifying columns after removal:")
print(df.columns.tolist()) 