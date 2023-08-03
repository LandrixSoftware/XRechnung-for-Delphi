# XRechnung Bundle

Ein integriertes Bundle mit dem Spezifikationsdokument für den Standard [XRechnung](https://xeinkauf.de/xrechnung/) und unterstützende Komponenten.

## Überblick Bestandteile

| Name                               | Version im Bundle | Kommentar |
|------------------------------------|-------------------|-----------|
| XRechnung Specification            | 2.3.1       | -         |
| XRechnung Syntax-Binding           | zu 2.3.1       | ergänzt, kompatibel zu XRechnung 2.3.x |
| Validator                          | 1.5.0           |           |
| XRechnung Validator Konfiguration  | 2023-01-31      |           |
| XRechnung Schematron               | 1.8.0           |           |
| XRechnung Visualization            | 2023-01-31           |           |
| XRechnung Testsuite                | 2023-01-31          |           |

## Änderungen zum letzten Release

### Spezifikation

* Gegenüber der am 27.01.2023 veröffentlichen Version XRechnung 2.3.0 lediglich zwei zusätzliche editorielle Änderungen.

Details siehe: https://projekte.kosit.org/xrechnung/xrechnung/-/issues/88

### Syntax-Binding

* Syntaxbinding für BG-DEX-09, BT-DEX-001, BT-DEX-002, BT-DEX-003 wurde hinzugefügt.

### Validator

* Keine kritischen Änderungen

Details siehe: https://github.com/itplr-kosit/validator/releases/tag/v1.5.0

### Validator Konfiguration XRechnung

* Jetzt mit Testsuite 2023-01-31, Schematron 1.8.0 und CEN 1.3.9

Details siehe: https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/tag/release-2023-01-31

### XRechnung Schematron Regeln

* Regeln BR-DE-29, BR-DE-30 und BR-DE-31 zur Prüfung der mandatorischen Elemente von BG-19 wurden hinzugefügt
* Extension Regeln BR-DEX-09, BR-DEX-10, BR-DEX-11 und BR-DEX-12 für Third Party Payment (Fremdleistungen) wurden hinzugefügt

Details siehe: https://github.com/itplr-kosit/xrechnung-schematron/releases/tag/release-1.8.0

### XRechnung Visualisierung

* Darstellung von Third Party Payment
* Kleinere Bugs behoben

Details siehe: https://github.com/itplr-kosit/xrechnung-visualization/releases/tag/v2023-01-31

### XRechnung Testsuite

* Neuer Testfall `05.01a-INVOICE_ubl.xml` zur Darstellung von Third Party Payment wurde hinzugefügt
* Fehlendes BT-90 in `04.02a-INVOICE_ubl.xml` wurde hinzugefügt

Details siehe: https://github.com/itplr-kosit/xrechnung-testsuite/releases/tag/release-2023-01-31

## Bundle Bestandteile Details

### Validator (Prüftool)

Das Prüftool ist ein Programm, welches XML-Dateien (Dokumente) in Abhängigkeit von ihren Dokumenttypen gegen verschiedene Validierungsregeln (XML Schema und Schematron) prüft und das Ergebnis zu einem Konformitätsbericht (Konformitätsstatus *valid* oder *invalid*) mit einer Empfehlung zur Weiterverarbeitung (*accept*) oder Ablehnung (*reject*) aggregiert. Mittels Konfiguration kann bestimmt werden, welche der Konformitätsregeln durch ein Dokument, das zur Weiterverarbeitung empfohlen (*accept*) wird, verletzt sein dürfen.

Das Prüftool selbst ist fachunabhängig und kennt weder spezifische Dokumentinhalte noch Validierungsregeln. Diese werden im Rahmen einer Prüftool-Konfiguration definiert, welche zur Anwendung des Prüftools erforderlich ist.

Weitere Details auf der [Validator Projektseite](https://github.com/itplr-kosit/validator).

### Validator Konfiguration XRechnung

Eine eigenständige Konfiguration für den Standard [XRechnung](https://xeinkauf.de/xrechnung/) wird ebenfalls auf [GitHub bereitgestellt](https://github.com/itplr-kosit/validator-configuration-xrechnung) ([Releases](https://github.com/itplr-kosit/validator-configuration-xrechnung/releases)). Diese enthält alle notwendigen Ressourcen zu der Norm EN16931 (XML-Schema und [Schematron Regeln](https://github.com/CenPC434/validation) u.a.) und die [XRechnung Schematron Regeln](https://github.com/itplr-kosit/xrechnung-schematron) in ihren aktuellen Versionen.

Weitere Details auf der [Validator Konfiguration XRechnung Projektseite](https://github.com/itplr-kosit/validator-configuration-xrechnung).

### XRechnung Schematron Regeln

Technische Implementierung der Geschäftsregeln des Standards [XRechnung](https://xeinkauf.de/xrechnung/) in Schematron Rules für XML Validierung.

Weitere Details auf der [XRechnung Schematron Regeln Projektseite](https://github.com/itplr-kosit/xrechnung-schematron).

### XRechnung Visualisierung

XSL Transformatoren für die Generierung von HTML Web-Seiten und PDF Dateien.

Diese zeigen den Inhalt von elektronischen Rechnungen an, die dem Standard [XRechnung](https://xeinkauf.de/xrechnung/) entsprechen.

Weitere Details auf der [XRechnung Visualisierung Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).

### XRechnung Testsuite

Valide Testdokumente des Standards [XRechnung](https://xeinkauf.de/xrechnung/).

Diese dienen dazu, bei Organisationen, die IT-Fachverfahren herstellen und betreiben, das Verständnis der [XRechnung-Spezifikation](https://xeinkauf.de/xrechnung/versionen-und-bundles/) zu fördern, indem die umfangreichen und komplexen Vorgaben und Besonderheiten der Spezifikation durch valide Testdokumente veranschaulicht werden. Die Testdokumente stehen zur freien Verfügung für die Einbindung in eigene Testverfahren.

Weitere Details auf der [XRechnung Testsuite Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).
