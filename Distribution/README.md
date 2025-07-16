# XRechnung Validierung

Das Skript installtools.ps1 mit PowerShell ausfuehren, um die Tools zu installieren bzw. zu aktualisieren.

Dabei wird die JRE von AdoptOpenJDK genutzt. Fragen zur Lizenz von AdoptOpenJDK 
finden Sie unter https://adoptium.net/de/docs/faq/ .

Sollte bei der Ausführung des Skripts folgende Fehlermeldung erscheinen:

    .\installtools.ps1 : Die Datei "C:\...\Distribution\installtools.ps1" kann nicht geladen werden, da die Ausführung von Skripts auf diesem System deaktiviert ist. Weitere Informationen finden Sie unter
    "about_Execution_Policies" (https:/go.microsoft.com/fwlink/?LinkID=135170).
    In Zeile:1 Zeichen:1
    + .\installtools.ps1
    + ~~~~~~~~~~~~~~~~~~
        + CategoryInfo          : Sicherheitsfehler: (:) [], PSSecurityException
        + FullyQualifiedErrorId : UnauthorizedAccess

Dann bitte folgende Schritte durchführen:

 - Drücke die [Windows]-Taste und tippe dann PowerShell ein.
 - Führe es als Administrator aus.
 - Kopiere und füge den folgenden Befehl ein und drücke [Enter]:

```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope LocalMachine
```
oder auch

```powershell
Set-ExecutionPolicy Unrestricted
```

 - Schließe PowerShell und versuche es erneut.

## Historie
- 16.07.2025 Update\
  https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2025-07-10/validator-configuration-xrechnung_3.0.2_2025-07-10.zip\
  https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2025-07-10/xrechnung-3.0.2-xrechnung-visualization-2025-07-10.zip

- 03.04.2025 Update\
  https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2025-03-21/validator-configuration-xrechnung_3.0.2_2025-03-21.zip\
  https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2025-03-21/xrechnung-3.0.2-xrechnung-visualization-2025-03-21.zip

- 19.11.2024 Update\
  https://github.com/itplr-kosit/xrechnung-schematron/releases/download/release-2.2.0/xrechnung-3.0.2-schematron-2.2.0.zip\
  https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2024-10-31/validator-configuration-xrechnung_3.0.2_2024-10-31.zip

- 21.02.2024 Update\
  https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/download/release-2024-06-20/validator-configuration-xrechnung_3.0.2_2024-06-20.zip\
  https://github.com/itplr-kosit/xrechnung-visualization/releases/download/v2024-06-20/xrechnung-3.0.2-xrechnung-visualization-2024-06-20.zip

- 16.11.2023 Update\
  https://github.com/itplr-kosit/xrechnung-visualization/releases/tag/v2023-11-15\
  https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/tag/release-2023-11-15\

- 22.09.2023 Update\
  xrechnung-3.0.1-xrechnung-visualization-2023-09-22.zip\
  validator-configuration-xrechnung_3.0.1_2023-09-22.zip

- 12.05.2023 Update\
  validator-configuration-xrechnung_2.3.1_2023-05-12.zip\
  xrechnung-2.3.1-xrechnung-visualization-2023-05-12.zip

- 14.04.2023 Apache FOP added - experimental

- 24.03.2023 Update\
  validator-configuration-xrechnung_2.3.1_2023-02-17.zip\
  xrechnung-2.3.1-xrechnung-visualization-2023-01-31.zip\
  OpenJDK17U-jre_x64_windows_hotspot_17.0.6_10.zip
  
- 05.12.2022 Update\
  validator-1.5.0-distribution.zip

- 17.11.2022 Update\
  validator-configuration-xrechnung_2.2.0_2022-11-15.zip\
  xrechnung-2.2.0-xrechnung-visualization-2022-11-15.zip

- 12.09.2022 Update\
  validator-configuration-xrechnung_2.2.0_2022-07-15.zip\
  xrechnung-2.2.0-xrechnung-visualization-2022-07-31.zip\
  jdk8u292-b10_openj9-0.26.0/OpenJDK8U-jre_x64_windows_openj9_8u292b10_openj9-0.26.0.zip

- 05.04.2022 Update\
  xrechnung-2.1.1-xrechnung-visualization-2021-11-15\
  validator-configuration-xrechnung_2.1.1_2021-11-15

- 01.10.2021 Update\
  validationtool-1.4.2\
  validator-configuration-xrechnung_2.1.1_2021-07-31\
  xrechnung-2.1.1-xrechnung-visualization-2021-07-31

- 02.02.2021 Neues Release XRechnung Visualization 2020-12-31 compatible with XRechnung 2.0.1

- 18.12.2020 Neues Release validator-configuration-xrechnung release-2020-12-31

## Tool-Verzeichnis

https://github.com/itplr-kosit/validator

https://github.com/itplr-kosit/validator-configuration-xrechnung

https://github.com/itplr-kosit/xrechnung-visualization

https://adoptium.net/de/

https://xmlgraphics.apache.org/fop/2.8/running.html

## Versionen

https://github.com/itplr-kosit/validator/releases/tag/v1.4.2

https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/tag/release-2022-07-15

https://github.com/itplr-kosit/xrechnung-visualization/releases/tag/v2022-07-31

https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u292-b10_openj9-0.26.0/OpenJDK8U-jre_x64_windows_openj9_8u292b10_openj9-0.26.0.zip

https://www.apache.org/dyn/closer.cgi?filename=/xmlgraphics/fop/binaries/fop-2.8-bin.zip&action=download
