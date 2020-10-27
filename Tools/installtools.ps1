If (Test-Path validator){
  Remove-Item validator -Recurse
}

If (Test-Path openxrechnungtoolbox){
  Remove-Item openxrechnungtoolbox -Recurse
}

If (Test-Path validator-configuration-122){
  Remove-Item validator-configuration-122 -Recurse
}

If (Test-Path validator-configuration-200){
  Remove-Item validator-configuration-200 -Recurse
}

If (Test-Path visualization){
  Remove-Item visualization -Recurse
}

If (Test-Path saxon){
  Remove-Item saxon -Recurse
}

If (Test-Path jre){
  Remove-Item jre -Recurse
}

Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator/releases/download/v1.3.1/validationtool-1.3.1.zip -OutFile validator.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2019-12-30/validator-configuration-xrechnung_1.2.2_2019-12-30.zip -OutFile validator-configuration-122.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2020-07-31/validator-configuration-xrechnung_2.0.0_2020-07-31.zip -OutFile validator-configuration-200.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2020-07-31/xrechnung-2.0.0-xrechnung-visualization-2020-07-31.zip -OutFile visualization.zip
Invoke-WebRequest -Uri https://kumisystems.dl.sourceforge.net/project/saxon/Saxon-HE/10/Java/SaxonHE10-2J.zip -OutFile saxon.zip
Invoke-WebRequest -Uri https://github.com/jcthiele/OpenXRechnungToolbox/releases/download/2020-08-28/OpenXRechnungToolbox_v1.0_Windows_64bit.zip -OutFile openxrechnungtoolbox.zip
Invoke-WebRequest -Uri https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u265-b01/OpenJDK8U-jre_x64_windows_hotspot_8u265b01.zip -OutFile jre.zip

Expand-Archive validator.zip
Expand-Archive validator-configuration-122.zip
Expand-Archive validator-configuration-200.zip
Expand-Archive visualization.zip
Expand-Archive saxon.zip
Expand-Archive openxrechnungtoolbox.zip
Expand-Archive jre.zip

If (Test-Path validator.zip){
  Remove-Item validator.zip
}
If (Test-Path validator-configuration-122.zip){
  Remove-Item validator-configuration-122.zip
}
If (Test-Path validator-configuration-200.zip){
  Remove-Item validator-configuration-200.zip
}
If (Test-Path visualization.zip){
  Remove-Item visualization.zip
}
If (Test-Path saxon.zip){
  Remove-Item saxon.zip
}
If (Test-Path openxrechnungtoolbox.zip){
  Remove-Item openxrechnungtoolbox.zip
}
If (Test-Path jre.zip){
  Remove-Item jre.zip
}
