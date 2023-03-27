If (Test-Path validator){
  Remove-Item validator -Recurse
}

if (Test-Path validator-configuration){
  Remove-Item validator-configuration -Recurse
}

If (Test-Path visualization){
  Remove-Item visualization -Recurse
}

If (Test-Path java){
  Remove-Item java -Recurse
}

Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator/releases/download/v1.5.0/validator-1.5.0-distribution.zip -OutFile validator.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2023-02-17/validator-configuration-xrechnung_2.3.1_2023-02-17.zip -OutFile validator-configuration.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2023-01-31/xrechnung-2.3.1-xrechnung-visualization-2023-01-31.zip -OutFile visualization.zip
Invoke-WebRequest -Uri https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.6%2B10/OpenJDK17U-jre_x64_windows_hotspot_17.0.6_10.zip -OutFile jre.zip

Expand-Archive validator.zip
Expand-Archive validator-configuration.zip
Expand-Archive visualization.zip
Expand-Archive jre.zip
Move-Item .\jre\jdk-17.0.6+10-jre .\java
Remove-Item jre -Recurse

If (Test-Path validator.zip){
  Remove-Item validator.zip
}
If (Test-Path validator-configuration.zip){
  Remove-Item validator-configuration.zip
}
If (Test-Path visualization.zip){
  Remove-Item visualization.zip
}
If (Test-Path jre.zip){
  Remove-Item jre.zip
}
