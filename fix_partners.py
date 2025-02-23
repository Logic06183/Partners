import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# Fix University of Michigan - change from GHAP to HEAT
df.loc[df['Institution'] == 'University of Michigan', 'GHAP'] = 0
df.loc[df['Institution'] == 'University of Michigan', 'HEAT'] = 1

# Remove all HIGH participation since it's not a valid category
df['HIGH'] = 0

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify the changes
print("\nVerifying University of Michigan's updated participation:")
print(df[df['Institution'] == 'University of Michigan'])

print("\nVerifying no institutions have HIGH participation:")
print(f"Number of institutions in HIGH: {len(df[df['HIGH'] == 1])}") 