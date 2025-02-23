import pandas as pd

# Read the current CSV
df = pd.read_csv('partners_data_with_coords.csv')

# Add missing partners
new_partners = [
    {
        'Institution': 'Clinical Research Network Norway',
        'City': 'Oslo',
        'Country': 'Norway',
        'CHAMNHA': 1,
        'HEAT': 0,
        'HIGH': 0,
        'ENBEL': 0,
        'GHAP': 0,
        'HAPI': 0,
        'BioHEAT': 0,
        'HIGH_Horizons': 0,
        'Funder': 1,
        'lon': 10.7389701,
        'lat': 59.9133301
    }
]

# Convert new partners to DataFrame
new_df = pd.DataFrame(new_partners)

# Concatenate with existing DataFrame
df = pd.concat([df, new_df], ignore_index=True)

# Fix name discrepancies
name_updates = {
    'Karolinska Institute': 'Karolinska University',
    'IBM Research Africa': 'IBM Research Africa - Johannesburg'
}

for old_name, new_name in name_updates.items():
    df.loc[df['Institution'] == old_name, 'Institution'] = new_name

# Save updated CSV
df.to_csv('partners_data_with_coords.csv', index=False) 