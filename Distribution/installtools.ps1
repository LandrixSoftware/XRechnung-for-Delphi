If (Test-Path validator){
  Remove-Item validator -Recurse
}

if (Test-Path validator-configuration){
  Remove-Item validator-configuration -Recurse
}
if (Test-Path validator-configuration23x){
  Remove-Item validator-configuration23x -Recurse
}
if (Test-Path validator-configuration30x){
  Remove-Item validator-configuration30x -Recurse
}

If (Test-Path visualization){
  Remove-Item visualization -Recurse
}
If (Test-Path visualization23x){
  Remove-Item visualization23x -Recurse
}
If (Test-Path visualization30x){
  Remove-Item visualization30x -Recurse
}

If (Test-Path java){
  Remove-Item java -Recurse
}

If (Test-Path apache-fop){
  Remove-Item apache-fop -Recurse
}

Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator/releases/download/v1.5.0/validator-1.5.0-distribution.zip -OutFile validator.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2023-05-12/validator-configuration-xrechnung_2.3.1_2023-05-12.zip -OutFile validator-configuration23x.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2023-11-15/validator-configuration-xrechnung_3.0.1_2023-11-15.zip -OutFile validator-configuration30x.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2023-05-12/xrechnung-2.3.1-xrechnung-visualization-2023-05-12.zip -OutFile visualization23x.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2023-11-15/xrechnung-3.0.1-xrechnung-visualization-2023-11-15.zip -OutFile visualization30x.zip
Invoke-WebRequest -Uri https://github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.6%2B10/OpenJDK17U-jre_x64_windows_hotspot_17.0.6_10.zip -OutFile jre.zip
#Invoke-WebRequest -Uri https://github.com/itplr-kosit/xrechnung-schematron/releases/tag/release-2.0.2 -OutFile schematron.zip
#Invoke-WebRequest -Uri "https://www.apache.org/dyn/closer.cgi?filename=/xmlgraphics/fop/binaries/fop-2.8-bin.zip&action=download" -OutFile fop.zip

Expand-Archive validator.zip
Expand-Archive validator-configuration23x.zip
Expand-Archive validator-configuration30x.zip
Expand-Archive visualization23x.zip
Expand-Archive visualization30x.zip
Expand-Archive jre.zip
#Expand-Archive fop.zip
Move-Item .\jre\jdk-17.0.6+10-jre .\java
Remove-Item jre -Recurse
#Move-Item .\fop\fop-2.8 .\apache-fop
#Remove-Item fop -Recurse

If (Test-Path validator.zip){
  Remove-Item validator.zip
}
If (Test-Path validator-configuration23x.zip){
  Remove-Item validator-configuration23x.zip
}
If (Test-Path validator-configuration30x.zip){
  Remove-Item validator-configuration30x.zip
}
If (Test-Path visualization23x.zip){
  Remove-Item visualization23x.zip
}
If (Test-Path visualization30x.zip){
  Remove-Item visualization30x.zip
}
If (Test-Path jre.zip){
  Remove-Item jre.zip
}
If (Test-Path fop.zip){
  Remove-Item fop.zip
}
