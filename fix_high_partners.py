import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# First, reset all HIGH and HIGH_Horizons participation to 0
df['HIGH'] = 0
df['HIGH_Horizons'] = 0

# Define HIGH_Horizons partners
high_horizons_partners = [
    'Lunds University',
    'Karolinska Institute',
    'Denmark Technical University',
    'University of Graz',
    'CeSHHAR',
    'Aga Khan University',  # This is likely the same as Aga Khan Health Service
    'Wits Health Consortium',
    'World Health Organization',
    'London School of Hygiene and Tropical Medicine'
]

# Add new partners that aren't in the CSV
new_partners = [
    {
        'Institution': 'Ghent University',
        'City': 'Ghent',
        'Country': 'Belgium',
        'CHAMNHA': 0,
        'HEAT': 0,
        'HIGH': 0,
        'ENBEL': 0,
        'GHAP': 0,
        'HAPI': 0,
        'BioHEAT': 0,
        'HIGH_Horizons': 1,
        'Funder': 0,
        'lon': 3.7174243,
        'lat': 51.0543422
    },
    {
        'Institution': 'University of Thessaly',
        'City': 'Volos',
        'Country': 'Greece',
        'CHAMNHA': 0,
        'HEAT': 0,
        'HIGH': 0,
        'ENBEL': 0,
        'GHAP': 0,
        'HAPI': 0,
        'BioHEAT': 0,
        'HIGH_Horizons': 1,
        'Funder': 0,
        'lon': 22.9444191,
        'lat': 39.3621095
    }
]

# Add new partners to dataframe
new_df = pd.DataFrame(new_partners)
df = pd.concat([df, new_df], ignore_index=True)

# Update HIGH_Horizons participation
for partner in high_horizons_partners:
    df.loc[df['Institution'] == partner, 'HIGH_Horizons'] = 1

# Save updated CSV
df.to_csv('partners_data_with_coords.csv', index=False)

# Print verification
print("HIGH_Horizons partners after update:")
print(df[df['HIGH_Horizons'] == 1]['Institution'].tolist()) 