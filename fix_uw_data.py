import pandas as pd

# Read the CSV file
df = pd.read_csv('partners_data_with_coords.csv')

# Update University of Washington's project participation
# Should be in CHAMNHA and HEAT, but not in GHAP
df.loc[df['Institution'] == 'University of Washington', ['CHAMNHA', 'HEAT', 'GHAP']] = [1, 1, 0]

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify the change
uw_data = df[df['Institution'] == 'University of Washington']
print("Updated University of Washington data:")
print(uw_data) 