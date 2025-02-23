import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# Update BioHEAT participation for Division of Human Genetics
df.loc[df['Institution'] == 'Division of Human Genetics, University of the Witwatersrand', 'BioHEAT'] = 1

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify the change
print("\nVerifying BioHEAT participation for Division of Human Genetics:")
print(df[df['Institution'] == 'Division of Human Genetics, University of the Witwatersrand']) 