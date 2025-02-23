import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# First, set HEAT = 0 for University of Cape Town/The Health Foundation
df.loc[df['Institution'] == 'University of Cape Town/The Health Foundation', 'HEAT'] = 0

# Then, set HEAT = 1 for Climate Systems Analysis Group
df.loc[df['Institution'] == 'Climate Systems Analysis Group', 'HEAT'] = 1

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify the changes
print("\nVerifying changes:")
print("\nClimate Systems Analysis Group entry:")
print(df[df['Institution'] == 'Climate Systems Analysis Group'])
print("\nUCT/Health Foundation entry:")
print(df[df['Institution'] == 'University of Cape Town/The Health Foundation']) 