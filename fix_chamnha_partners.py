import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# List of all CHAMNHA partners
chamnha_partners = [
    'Aga Khan University',
    'Institut de Recherche en Sciences de la Santé',
    'London School of Hygiene and Tropical Medicine',
    'University of Washington',
    'University of Oslo',
    'Karolinska Institute',  # Note: Listed as "Karolinska Institute" in CSV
    'South African Medical Research Council'
]

# First, print current CHAMNHA partners
print("Current CHAMNHA partners:")
current_partners = df[df['CHAMNHA'] == 1]['Institution'].tolist()
print("\nFound in CSV:")
for partner in current_partners:
    print(f"- {partner}")

# Update CHAMNHA participation
for partner in chamnha_partners:
    if partner not in current_partners:
        print(f"\nAdding {partner} to CHAMNHA")
        df.loc[df['Institution'] == partner, 'CHAMNHA'] = 1

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify final CHAMNHA partners
print("\nFinal CHAMNHA partners after update:")
final_partners = df[df['CHAMNHA'] == 1]['Institution'].tolist()
for partner in final_partners:
    print(f"- {partner}") 