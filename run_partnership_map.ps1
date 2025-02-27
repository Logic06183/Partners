# PowerShell script to run the partnership map R script

Write-Host "Running Wits Planetary Health Partnership Maps..." -ForegroundColor Cyan
Write-Host ""

# Set the R script path
$R_Script = "C:\Users\CraigParker\OneDrive - Wits Health Consortium\PHR PC\Desktop\Partners\partnership_map.R"

# Set the specific Rscript path based on the user's R installation
$Rscript_Path = "C:\Users\CraigParker\AppData\Local\Programs\R\R-4.4.1\bin\Rscript.exe"

if (Test-Path $Rscript_Path) {
    Write-Host "Found Rscript at: $Rscript_Path" -ForegroundColor Green
    Write-Host "Running partnership map script..." -ForegroundColor Yellow
    Write-Host ""
    
    # Run the R script
    & $Rscript_Path $R_Script
    
    Write-Host ""
    Write-Host "Script execution completed." -ForegroundColor Green
    Write-Host "The map with Europe inset will be created and saved as:" -ForegroundColor Cyan
    Write-Host "- Wits_Planetary_Health_partnership_map_for_Wellcome.pdf" -ForegroundColor White
    Write-Host "- Wits_Planetary_Health_partnership_map_for_Wellcome.png" -ForegroundColor White
} 
else {
    Write-Host "ERROR: Could not find Rscript.exe at the expected location." -ForegroundColor Red
    Write-Host "Trying to find RStudio to open the script..." -ForegroundColor Yellow
    
    # Try to find RStudio
    $RStudio_Paths = @(
        "C:\Program Files\RStudio\bin\rstudio.exe",
        "C:\Program Files\RStudio Desktop\bin\rstudio.exe",
        "C:\Program Files (x86)\RStudio\bin\rstudio.exe",
        "C:\Program Files (x86)\RStudio Desktop\bin\rstudio.exe"
    )
    
    $Found_RStudio = $false
    
    foreach ($Path in $RStudio_Paths) {
        if (Test-Path $Path) {
            Write-Host "Found RStudio at: $Path" -ForegroundColor Green
            Start-Process $Path -ArgumentList $R_Script
            $Found_RStudio = $true
            break
        }
    }
    
    if (-not $Found_RStudio) {
        Write-Host "Could not find RStudio installation." -ForegroundColor Red
        Write-Host "Please open RStudio manually and run the script located at:" -ForegroundColor Yellow
        Write-Host $R_Script -ForegroundColor White
    }
}

Write-Host ""
Write-Host "Press any key to exit..."
$null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
