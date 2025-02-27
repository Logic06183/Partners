# PowerShell script to run R script
$script_path = "C:\Users\CraigParker\OneDrive - Wits Health Consortium\PHR PC\Desktop\Partners\partnership_map.R"

# Attempt to execute the R script using different possible paths for R
$r_paths = @(
    "C:\Users\CraigParker\AppData\Local\Programs\R\R-4.4.1\bin\Rscript.exe",
    "R",
    "Rscript",
    "C:/Program Files/R/R-4.3.2/bin/Rscript.exe",
    "C:/Program Files/R/R-4.2.0/bin/Rscript.exe",
    "C:/Program Files/R/R-4.1.0/bin/Rscript.exe",
    "${env:ProgramFiles}/R/R-4.3.2/bin/Rscript.exe", 
    "${env:ProgramFiles}/R/R-4.2.0/bin/Rscript.exe",
    "${env:ProgramFiles}/R/R-4.1.0/bin/Rscript.exe"
)

$success = $false

foreach ($r_path in $r_paths) {
    Write-Host "Attempting to run R script with: $r_path"
    
    # Check if the path exists for executable paths
    if ($r_path -ne "R" -and $r_path -ne "Rscript" -and (-not (Test-Path $r_path))) {
        Write-Host "$r_path does not exist, skipping..."
        continue
    }
    
    # Try to run the script
    & $r_path $script_path 2>&1
    
    if ($LASTEXITCODE -eq 0) {
        $success = $true
        Write-Host "Successfully executed R script using $r_path"
        break
    } else {
        Write-Host "Failed with $r_path with exit code $LASTEXITCODE"
    }
}

if (-not $success) {
    Write-Host "Failed to execute R script with any of the R paths. Please check R installation."
}
