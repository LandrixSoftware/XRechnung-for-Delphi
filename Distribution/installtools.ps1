If (Test-Path validator){
  Remove-Item validator -Recurse
}

If (Test-Path validator-configuration-122){
  Remove-Item validator-configuration-122 -Recurse
}

If (Test-Path validator-configuration-200){
  Remove-Item validator-configuration-200 -Recurse
}
If (Test-Path validator-configuration-211){
  Remove-Item validator-configuration-211 -Recurse
}

If (Test-Path visualization){
  Remove-Item visualization -Recurse
}

If (Test-Path java){
  Remove-Item java -Recurse
}

Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator/releases/download/v1.4.2/validator-1.4.2-distribution.zip -OutFile validator.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2019-12-30/validator-configuration-xrechnung_1.2.2_2019-12-30.zip -OutFile validator-configuration-122.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2021-11-15/validator-configuration-xrechnung_2.1.1_2021-11-15.zip -OutFile validator-configuration-211.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2021-11-15/xrechnung-2.1.1-xrechnung-visualization-2021-11-15.zip -OutFile visualization.zip
Invoke-WebRequest -Uri https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u265-b01/OpenJDK8U-jre_x64_windows_hotspot_8u265b01.zip -OutFile jre.zip

Expand-Archive validator.zip
Expand-Archive validator-configuration-122.zip
Expand-Archive validator-configuration-211.zip
Expand-Archive visualization.zip
Expand-Archive jre.zip
Move-Item .\jre\jdk8u265-b01-jre .\java
Remove-Item jre -Recurse

If (Test-Path validator.zip){
  Remove-Item validator.zip
}
If (Test-Path validator-configuration-122.zip){
  Remove-Item validator-configuration-122.zip
}
If (Test-Path validator-configuration-211.zip){
  Remove-Item validator-configuration-211.zip
}
If (Test-Path visualization.zip){
  Remove-Item visualization.zip
}
If (Test-Path jre.zip){
  Remove-Item jre.zip
}
