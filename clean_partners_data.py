import pandas as pd
import unidecode

def clean_partners_data(input_file='partner_updated.csv', output_file='partners_cleaned.csv'):
    # Read the tab-separated file
    df = pd.read_csv(input_file, sep='\t')
    
    # Fix character encoding for known cases
    encoding_fixes = {
        'YaoundÃ©': 'Yaoundé',
        'CÃ´te d\'Ivoire': 'Côte d\'Ivoire',
        'HouphouÃ«t': 'Houphouët',
        'UniversitÃ¤t': 'Universität'
    }
    
    # Apply encoding fixes to all columns
    for col in df.columns:
        for wrong, correct in encoding_fixes.items():
            df[col] = df[col].astype(str).str.replace(wrong, correct)
    
    # Standardize column names
    df.columns = df.columns.str.strip().str.replace(' ', '_')
    
    # Standardize city names capitalization
    df['City'] = df['City'].str.title()
    
    # Standardize institution names
    df['Institution'] = df['Institution'].apply(lambda x: 
        x.replace(' & ', ' and ')
         .replace('University of the Witwatersand', 'University of the Witwatersrand')
    )
    
    # Convert coordinates to float and round to consistent precision (6 decimal places)
    df['lon'] = pd.to_numeric(df['lon'], errors='coerce').round(6)
    df['lat'] = pd.to_numeric(df['lat'], errors='coerce').round(6)
    
    # Sort by country and city
    df = df.sort_values(['Country', 'City'])
    
    # Save as CSV with UTF-8 encoding
    df.to_csv(output_file, index=False, encoding='utf-8')
    
    return df

if __name__ == "__main__":
    cleaned_df = clean_partners_data()
    
    # Print summary of changes
    print("Data cleaning completed:")
    print(f"Number of rows: {len(cleaned_df)}")
    print(f"Number of columns: {len(cleaned_df.columns)}")
    print("\nColumn names:")
    for col in cleaned_df.columns:
        print(f"- {col}") 