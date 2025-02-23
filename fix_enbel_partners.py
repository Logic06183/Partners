import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# List of all ENBEL partners
enbel_partners = [
    'Aga Khan University',
    'University of Botswana',
    'London School of Hygiene and Tropical Medicine',
    'Umea University',
    'University of Graz',
    'Azienda Sanitaria Locale Roma',
    'Tartu Ulikool',
    'Folkehelseinstituttet',
    'Center for International Climate Research',
    'Royal College of Surgeons in Ireland',
    'Health and Environment Alliance',
    'International Red Cross Red Crescent Centre on Climate Change and Disaster Preparedness',
    'University Paul Sabatier Toulouse',
    'Goeteborgs University',
    'Ilmatieteen Laitos',
    'Lunds University'
]

# First, print current ENBEL partners
print("Current ENBEL partners:")
current_partners = df[df['ENBEL'] == 1]['Institution'].tolist()
print("\nFound in CSV:")
for partner in current_partners:
    print(f"- {partner}")

# Update ENBEL participation
for partner in enbel_partners:
    if partner not in current_partners:
        print(f"\nAdding {partner} to ENBEL")
        df.loc[df['Institution'] == partner, 'ENBEL'] = 1

# Save the updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Verify final ENBEL partners
print("\nFinal ENBEL partners after update:")
final_partners = df[df['ENBEL'] == 1]['Institution'].tolist()
for partner in final_partners:
    print(f"- {partner}")

# Check for any missing partners
print("\nChecking for missing partners:")
csv_institutions = set(df['Institution'].tolist())
missing = []
for partner in enbel_partners:
    if partner not in csv_institutions:
        missing.append(partner)
        print(f"Warning: {partner} not found in CSV") 