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

Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator/releases/download/v1.3.1/validationtool-1.3.1.zip -OutFile validator.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2019-12-30/validator-configuration-xrechnung_1.2.2_2019-12-30.zip -OutFile validator-configuration-122.zip
Invoke-WebRequest -Uri https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2020-07-31/validator-configuration-xrechnung_2.0.0_2020-07-31.zip -OutFile validator-configuration-200.zip
Invoke-WebRequest -Uri https://github.com/jcthiele/OpenXRechnungToolbox/releases/download/2020-08-28/OpenXRechnungToolbox_v1.0_Windows_64bit.zip -OutFile openxrechnungtoolbox.zip

Expand-Archive validator.zip
Expand-Archive validator-configuration-122.zip
Expand-Archive validator-configuration-200.zip
Expand-Archive openxrechnungtoolbox.zip

If (Test-Path validator.zip){
  Remove-Item validator.zip
}
If (Test-Path validator-configuration-122.zip){
  Remove-Item validator-configuration-122.zip
}
If (Test-Path validator-configuration-200.zip){
  Remove-Item validator-configuration-200.zip
}
If (Test-Path openxrechnungtoolbox.zip){
  Remove-Item openxrechnungtoolbox.zip
}
