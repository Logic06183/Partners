import pandas as pd

# Read the CSV
df = pd.read_csv('partners_data_with_coords.csv')

# Print current Norwegian institutions in CSV:
norwegian_inst = df[df['Country'] == 'Norway']
print(norwegian_inst[['Institution', 'CHAMNHA', 'HEAT', 'HIGH', 'ENBEL', 'GHAP', 'HAPI', 'BioHEAT', 'HIGH_Horizons', 'Funder']])

# List of Norwegian ENBEL partners with their exact names
norwegian_enbel_partners = {
    'Center for International Climate Research': 'CICERO',
    'Folkehelseinstituttet': 'Norwegian Institute of Public Health'
}

# Verify ENBEL participation
for inst in norwegian_enbel_partners.keys():
    if inst in df['Institution'].values:
        print(f"\nFound {inst}:")
        print(df[df['Institution'] == inst][['Institution', 'ENBEL']])
    else:
        print(f"\nWarning: {inst} not found in CSV") 