import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# List of all HIGH_Horizons partners
high_horizons_partners = [
    'Aga Khan University',
    'World Health Organization',
    'London School of Hygiene and Tropical Medicine',
    'Lunds University',
    'Karolinska Institute',  # Note: Listed as "Karolinska Institute" in CSV
    'Azienda Sanitaria Locale Roma',
    'Denmark Technical University',
    'University of Graz'
]

# First, print current HIGH_Horizons partners
print("Current HIGH_Horizons partners:")
current_partners = df[df['HIGH_Horizons'] == 1]['Institution'].tolist()
print("\nFound in CSV:")
for partner in current_partners:
    print(f"- {partner}")

# Update HIGH_Horizons participation
for partner in high_horizons_partners:
    if partner not in current_partners:
        print(f"\nAdding {partner} to HIGH_Horizons")
        df.loc[df['Institution'] == partner, 'HIGH_Horizons'] = 1

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify final HIGH_Horizons partners
print("\nFinal HIGH_Horizons partners after update:")
final_partners = df[df['HIGH_Horizons'] == 1]['Institution'].tolist()
for partner in final_partners:
    print(f"- {partner}") 