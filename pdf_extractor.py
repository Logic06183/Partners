import PyPDF2
import re
import csv
from typing import List, Dict

def extract_institutional_partners(pdf_path) -> List[Dict]:
    # Open the PDF file in binary read mode
    with open(pdf_path, 'rb') as file:
        # Create a PDF reader object
        pdf_reader = PyPDF2.PdfReader(file)
        
        # Initialize a list to store institutional partners
        partners = []
        
        # Extract text from each page and look for institutional partners
        for page in pdf_reader.pages:
            text = page.extract_text()
            
            # Look for patterns that might indicate institutional partners
            # Adjust these patterns based on your PDF format
            partner_patterns = [
                r"(?i)institutional partner[s]?\s*:\s*([^\n]+)",
                r"(?i)partner institution[s]?\s*:\s*([^\n]+)",
                r"(?i)partnering institution[s]?\s*:\s*([^\n]+)"
            ]
            
            for pattern in partner_patterns:
                matches = re.finditer(pattern, text)
                for match in matches:
                    partner_name = match.group(1).strip()
                    # Create a dictionary for each partner
                    partner_dict = {
                        "Institution": partner_name,
                        "City": "",  # These would need to be filled manually
                        "Country": "",
                        "CHAMNHA": 0,
                        "HEAT": 0,
                        "HIGH": 0,
                        "ENBEL": 0,
                        "GHAP": 0,
                        "HAPI": 0,
                        "BioHEAT": 0,
                        "HIGH_Horizons": 0
                    }
                    partners.append(partner_dict)
        
        return partners

def save_to_csv(partners: List[Dict], output_file: str = "new_partners.csv"):
    if not partners:
        print("No partners found to save")
        return
        
    fieldnames = ["Institution", "City", "Country", "CHAMNHA", "HEAT", "HIGH", 
                  "ENBEL", "GHAP", "HAPI", "BioHEAT", "HIGH_Horizons"]
    
    with open(output_file, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(partners)

if __name__ == "__main__":
    pdf_path = "example.pdf"
    try:
        partners = extract_institutional_partners(pdf_path)
        if partners:
            print("Found institutional partners:")
            for partner in partners:
                print(f"- {partner['Institution']}")
            
            # Save to CSV
            save_to_csv(partners)
            print(f"\nPartners saved to new_partners.csv")
        else:
            print("No institutional partners found in the document")
            
    except Exception as e:
        print(f"An error occurred: {str(e)}")