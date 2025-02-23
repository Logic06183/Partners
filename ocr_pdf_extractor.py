from pdf2image import convert_from_path
import pytesseract
import re
from PIL import Image

def extract_institutional_partners_from_scanned_pdf(pdf_path):
    # Convert PDF to images
    pages = convert_from_path(pdf_path)
    
    # Initialize a list to store institutional partners
    partners = []
    
    # Process each page
    for page in pages:
        # Extract text using OCR
        text = pytesseract.image_to_string(page)
        
        # Look for patterns that might indicate institutional partners
        partner_patterns = [
            r"(?i)institutional partner[s]?\s*:\s*([^\n]+)",
            r"(?i)partner institution[s]?\s*:\s*([^\n]+)",
            r"(?i)partnering institution[s]?\s*:\s*([^\n]+)"
        ]
        
        for pattern in partner_patterns:
            matches = re.finditer(pattern, text)
            for match in matches:
                partners.append(match.group(1).strip())
    
    return partners

if __name__ == "__main__":
    pdf_path = "example.pdf"
    try:
        partners = extract_institutional_partners_from_scanned_pdf(pdf_path)
        if partners:
            print("Found institutional partners:")
            for partner in partners:
                print(f"- {partner}")
        else:
            print("No institutional partners found in the document")
            
    except Exception as e:
        print(f"An error occurred: {str(e)}") 