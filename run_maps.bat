@echo off
echo Running Wits Planetary Health Partnership Maps...
echo.

REM Set the R script path
set R_SCRIPT="C:\Users\CraigParker\OneDrive - Wits Health Consortium\PHR PC\Desktop\Partners\partnership_map.R"

REM Set the specific Rscript path based on the user's R installation
set RSCRIPT_PATH="C:\Users\CraigParker\AppData\Local\Programs\R\R-4.4.1\bin\Rscript.exe"

if exist %RSCRIPT_PATH% (
    echo Found Rscript at: %RSCRIPT_PATH%
    echo Running partnership map script...
    echo.
    %RSCRIPT_PATH% %R_SCRIPT%
    
    echo.
    echo Script execution completed.
    echo The map with Europe inset will be created and saved as:
    echo - Wits_Planetary_Health_partnership_map_for_Wellcome.pdf
    echo - Wits_Planetary_Health_partnership_map_for_Wellcome.png
) else (
    echo ERROR: Could not find Rscript.exe at the expected location.
    echo Please open RStudio manually and run the script.
    echo Script path: %R_SCRIPT%
    echo.
    echo Alternatively, you can try opening RStudio with the script...
    
    REM Try to open RStudio with the script
    FOR %%p IN (
      "C:\Program Files\RStudio\bin\rstudio.exe"
      "C:\Program Files\RStudio Desktop\bin\rstudio.exe"
      "C:\Program Files (x86)\RStudio\bin\rstudio.exe"
      "C:\Program Files (x86)\RStudio Desktop\bin\rstudio.exe"
    ) DO (
      IF EXIST %%p (
        echo Found RStudio at: %%p
        start "" %%p %R_SCRIPT%
        goto :end
      )
    )
    
    echo Could not find RStudio installation. Please open RStudio manually.
)

:end
echo.
echo Press any key to exit...
pause > nul
