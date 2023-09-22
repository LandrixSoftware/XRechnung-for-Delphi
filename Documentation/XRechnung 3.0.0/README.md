# XRechnung Bundle

Ein integriertes Bundle mit dem Spezifikationsdokument für den Standard [XRechnung](https://xeinkauf.de/xrechnung/) und unterstützende Komponenten.

## Überblick Bestandteile

| Name                               | Version im Bundle | Kommentar |
|------------------------------------|-------------------|-----------|
| XRechnung Specification            | 3.0.0       |           |
| XRechnung Syntax-Binding           | zu 3.0.0       | keine Änderungen, kompatibel zu XRechnung 3.0.x |
| Validator                          | 1.5.0           |           |
| XRechnung Validator Konfiguration  | 2023-07-31      |           |
| XRechnung Schematron               | 2.0.0           |           |
| XRechnung Visualization            | 2023-07-31           |           |
| XRechnung Testsuite                | 2023-07-31          |           |

## Änderungen zum letzten Release

### Spezifikation

* Adaption diverser fachlicher Regeln aus Peppol BIS Billing 3.0 (siehe Kapitel 12.5 der Spezifikation XRechnung 3.0.0)
  * PEPPOL-EN16931-R001
  * PEPPOL-EN16931-R005
  * PEPPOL-EN16931-R008
  * PEPPOL-EN16931-R010
  * PEPPOL-EN16931-R020
  * PEPPOL-EN16931-R040
  * PEPPOL-EN16931-R041
  * PEPPOL-EN16931-R042
  * PEPPOL-EN16931-R046
  * PEPPOL-EN16931-R055
  * PEPPOL-EN16931-R061
  * PEPPOL-EN16931-R101
  * PEPPOL-EN16931-R111
  * PEPPOL-EN16931-R120
  * PEPPOL-EN16931-R121
  * PEPPOL-EN16931-R130
* BR-DE-29 wird durch PEPPOL-EN16931-R061 ersetzt
* Streichung der Möglichkeit, Verzugszinsen in BT-20 zu übermitteln

Details siehe Anhang C. Versionshistorie der Spezifikation XRechnung 3.0.0

### Syntax-Binding

* Keine Änderungen gegenüber der am 02.02.2023 veröffentlichten Version

### Validator

* keine Änderungen

Details siehe: https://github.com/itplr-kosit/validator/releases/tag/v1.5.0

### Validator Konfiguration XRechnung

* jetzt mit CEN Schematron 1.3.10, XRechnung Schematron v2.0.0 und XRechnung Testsuite 2023-07-31
* Anpassung der Tests an die neuen Schematron Regeln

Details siehe: https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/tag/release-2023-07-31

### XRechnung Schematron Regeln

* neue Schematron Regeln:

  * PEPPOL-EN16931-R001
  * PEPPOL-EN16931-R005
  * PEPPOL-EN16931-R008
  * PEPPOL-EN16931-R010
  * PEPPOL-EN16931-R020
  * PEPPOL-EN16931-R040
  * PEPPOL-EN16931-R041
  * PEPPOL-EN16931-R042
  * PEPPOL-EN16931-R043
  * PEPPOL-EN16931-R044
  * PEPPOL-EN16931-R046
  * PEPPOL-EN16931-R053
  * PEPPOL-EN16931-R054
  * PEPPOL-EN16931-R055
  * PEPPOL-EN16931-R061
  * PEPPOL-EN16931-R101
  * PEPPOL-EN16931-R110
  * PEPPOL-EN16931-R111
  * PEPPOL-EN16931-R120
  * PEPPOL-EN16931-R121
  * PEPPOL-EN16931-R130

* BR-DE-29 wurde entfernt
* BR-DE-18 lässt keine Angabe von Verzugszinsen mehr zu

Details siehe: https://github.com/itplr-kosit/xrechnung-schematron/releases/tag/release-2.0.0

### XRechnung Visualisierung

* Anpassung der Tests an die neuen Schematron Regeln

Details siehe: https://github.com/itplr-kosit/xrechnung-visualization/releases/tag/v2023-07-31

### XRechnung Testsuite

* Anpassung der Referenznachrichten an die neuen Schematron Regeln
* neue technische Testfälle zur umfassenden Darstellung aller Elemente des semantischen Datenmodells der CIUS XRechnung

Details siehe: https://github.com/itplr-kosit/xrechnung-testsuite/releases/tag/release-2023-07-31

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

Weitere Details auf der [XRechnung Testsuite Projektseite](https://github.com/itplr-kosit/xrechnung-testsuite).
