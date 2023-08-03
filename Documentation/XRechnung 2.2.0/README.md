# XRechnung Bundle

Eine integriertes Bundle mit dem Spezifikationsdokument für den Standard [XRechnung](http://www.xoev.de/de/xrechnung) und unterstützende Komponenten.

## Überblick Bestandteile

| Name                               | Version im Bundle | Kommentar |
|------------------------------------|-------------------|-----------|
| XRechnung Specification            | 2.2.0       | -         |
| XRechnung Syntax-Binding           | zu 2.2.0       | keine Änderungen, kompatibel zu XRechnung |
| Validator                          | 1.4.2           | | 
| XRechnung Validator Konfiguration  | 2022-11-15       | |
| XRechnung Schematron               | 1.7.3      | |
| XRechnung Visualization            | 2022-11-15       | |
| XRechnung Testsuite                 | 2022-11-15       | |

## Änderungen zum letzten Release

Es ist zu beachten, dass dieses Release alle vorangegangenen Änderungen der XRechnung Bundle Version 2.2.0 zur Version 2.1.1 beinhaltet.

### Validator

* Keine Änderungen

Details siehe: https://github.com/itplr-kosit/validator/releases/tag/v1.4.2

### Validator Konfiguration XRechnung

* Jetzt mit Testsuite v2022-11-15 und Schematron v1.7.3
* Neue CEN Unit Tests wurden hinzugefügt
* Struktur der Tests "Unexpected Behaviour of CEN Rules" wurde angepasst
* Möglichkeit zur Validierung von Dateien jetzt auch über das ant target `create-validator-reports-from-custom-tests` möglich

Details siehe: https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/tag/release-2022-11-15

### XRechnung Schematron Regeln

* BR-DE-21 lässt auch in CII den Extension Specification Identifier zu
* Minor Bugs wurden behoben

Details siehe: https://github.com/itplr-kosit/xrechnung-schematron/releases/tag/release-1.7.3

### XRechnung Visualisierung

* Fehlende Anzeige von Inhaltselementen und weitere Minor Bugs wurden behoben
* PDF und HTML Darstellung wurden vereinheitlicht
* Tests wurden erweitert

Details siehe: https://github.com/itplr-kosit/xrechnung-visualization/releases/tag/v2022-11-15

### XRechnung Testsuite

* Fehlerhafte Verwendung von Scheme identifiern in `04.05a-INVOICE_uncefact.xml` wurde korrigiert

Details siehe: https://github.com/itplr-kosit/xrechnung-testsuite/releases/tag/release-2022-11-15

## Bundle Bestandteile Details

### Validator (Prüftool)

Das Prüftool ist ein Programm, welches XML-Dateien (Dokumente) in Abhängigkeit von ihren Dokumenttypen gegen verschiedene Validierungsregeln (XML Schema und Schematron) prüft und das Ergebnis zu einem Konformitätsbericht (Konformitätsstatus *valid* oder *invalid*) mit einer Empfehlung zur Weiterverarbeitung (*accept*) oder Ablehnung (*reject*) aggregiert. Mittels Konfiguration kann bestimmt werden, welche der Konformitätsregeln durch ein Dokument, das zur Weiterverarbeitung empfohlen (*accept*) wird, verletzt sein dürfen.

Das Prüftool selbst ist fachunabhängig und kennt weder spezifische Dokumentinhalte noch Validierungsregeln. Diese werden im Rahmen einer Prüftool-Konfiguration definiert, welche zur Anwendung des Prüftools erforderlich ist.

Weitere Details auf der [Validator Projektseite](https://github.com/itplr-kosit/validator).

### Validator Konfiguration XRechnung

Eine eigenständige Konfiguration für den Standard [XRechnung](http://www.xoev.de/de/xrechnung) wird ebenfalls auf [GitHub bereitgestellt](https://github.com/itplr-kosit/validator-configuration-xrechnung) ([Releases](https://github.com/itplr-kosit/validator-configuration-xrechnung/releases)). Diese enthält alle notwendigen Ressourcen zu der Norm EN16931 (XML-Schema und [Schematron Regeln](https://github.com/CenPC434/validation) u.a.) und die [XRechnung Schematron Regeln](https://github.com/itplr-kosit/xrechnung-schematron) in ihren aktuellen Versionen.

Weitere Details auf der [Validator Konfiguration XRechnung Projektseite](https://github.com/itplr-kosit/validator-configuration-xrechnung).

### XRechnung Schematron Regeln

Technische Implementierung der Geschäftsregeln des Standards [XRechnung](http://www.xoev.de/de/xrechnung) in Schematron Rules für XML Validierung.

Weitere Details auf der [XRechnung Schematron Regeln Projektseite](https://github.com/itplr-kosit/xrechnung-schematron).

### XRechnung Visualisierung

XSL Transformatoren für die Generierung von HTML Web-Seiten und PDF Dateien.

Diese zeigen den Inhalt von elektronischen Rechnungen an, die dem Standard [XRechnung](http://www.xoev.de/de/xrechnung) entsprechen.

Weitere Details auf der [XRechnung Visualisierung Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).

### XRechnung Testsuite

Valide Testdokumente des Standards [XRechnung](http://www.xoev.de/de/xrechnung).

Diese dienen dazu, bei Organisationen, die IT-Fachverfahren herstellen und betreiben, das Verständnis der [XRechnung-Spezifikation](https://www.xoev.de/de/xrechnung) zu fördern, indem die umfangreichen und komplexen Vorgaben und Besonderheiten der Spezifikation durch valide Testdokumente veranschaulicht werden. Die Testdokumente stehen zur freien Verfügung für die Einbindung in eigene Testverfahren.

Weitere Details auf der [XRechnung Testsuite Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).
