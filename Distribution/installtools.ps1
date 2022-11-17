If (Test-Path validator){
  Remove-Item validator -Recurse
}

If (Test-Path validator-configuration-220){
  Remove-Item validator-configuration-220 -Recurse
}

If (Test-Path visualization){
  Remove-Item visualization -Recurse
}

If (Test-Path java){
  Remove-Item java -Recurse
}

Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator/releases/download/v1.4.2/validator-1.4.2-distribution.zip -OutFile validator.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2022-11-15/validator-configuration-xrechnung_2.2.0_2022-11-15.zip -OutFile validator-configuration-220.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2022-11-15/xrechnung-2.2.0-xrechnung-visualization-2022-11-15.zip -OutFile visualization.zip
Invoke-WebRequest -Uri https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u292-b10_openj9-0.26.0/OpenJDK8U-jre_x64_windows_openj9_8u292b10_openj9-0.26.0.zip -OutFile jre.zip

Expand-Archive validator.zip
Expand-Archive validator-configuration-220.zip
Expand-Archive visualization.zip
Expand-Archive jre.zip
Move-Item .\jre\jdk8u292-b10-jre .\java
Remove-Item jre -Recurse

If (Test-Path validator.zip){
  Remove-Item validator.zip
}
If (Test-Path validator-configuration-220.zip){
  Remove-Item validator-configuration-220.zip
}
If (Test-Path visualization.zip){
  Remove-Item visualization.zip
}
If (Test-Path jre.zip){
  Remove-Item jre.zip
}
