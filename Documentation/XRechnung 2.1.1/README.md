# XRechnung Bundle

Eine integriertes Bundle mit dem Spezifikationsdokument des für den Standard [XRechnung](http://www.xoev.de/de/xrechnung) und unterstützennden Komponenten.

## Ueberblick Bestandteile

| Name                               | Version im Bundle | Kommentar |
|------------------------------------|-------------------|-----------|
| XRechnung Specification            | 2.1.1       | -         |
| XRechnung Syntax-Binding           | zu 2.1.1       | keine Aenderungen, kompatibel zu XRechnung |
| Validator                          | 1.4.2           | | 
| XRechnung Validator Konfiguration  | 2021-07-31       | |
| XRechnung Schematron               | 1.6.0      | |
| XRechnung Visualization            | 2021-07-31       | |
| XRechnung Tessuite                 | 2021-07-31       | |
## Aenderungen zum letzten Release

### Validator

* Kleinere Bugs behoben, keine kritischen Aenderungen

Details siehe: https://github.com/itplr-kosit/validator/releases/tag/v1.4.2

### Validator Konfiguration XRechnung

* Basiert auf einem Fork der CEN Regeln, da ein kritischer Fehler bei der Regel BR-CO-15 nicht von CEN behoben wurde. Den Fork hat die KoSIT beauftragt. 
* Wichtigste Aenderungen betreffen die integrierten Regeln der CEN
* Diese koennen insbesondere sein:
  1. Bei CII Syntax:
    - BR-CO-10, BR-CO-08, BR-Z-08 wurde vollständig bzgl. decimal-Verwendung überarbeitet -> **ggf. kritisch**
    - bzgl. BR-AE-XX, BR-E-XX, BR-G-XX wurde der Einstiegsmatch korrigiert -> **ggf. kritisch**
    - BR-CO-17 Rundungsproblematik wurde beareitet -> **ggf. kritisch**
    - Match für CII-SR-169, CII-SR-171 (Warning) wurde komplett geändert -> **ggf. kritisch**
    - in Zusammenhang mit BR-CL-23 sind offenbar auch Werte entfallen (z.B. 64) -> **kritisch**
    - in Zusammenhang mit BR-CO-09 wurden auch Codes entfernt (z.B. AN) -> **kritisch**   * 
  2. Bei UBL Syntax
    - UBL-SR-43 von name auf local-name umgestellt -> **kritisch**
    - BR-CO-10 wurde vollständig bzgl. decimal-Verwendung überarbeitet -> **ggf. kritisch**
    - BR-CO-15 bzgl. Anzahl TaxTotal korrigiert -> **ggf. kritisch**
    - in Zusammenhang mit BR-CL-23 sind offenbar auch Werte entfallen (z.B. 64) -> **kritisch**

Details siehe: https://github.com/itplr-kosit/validator-configuration-xrechnung/releases/tag/release-2021-07-31

### XRechnung Schematron Regeln

* Regel BR-DE-13 wurde durch einzelne Regeln BR-DE-23, BR-DE-24 und BR-DE-25 ersetzt
* Regel BR-DE-26 zur Pruefung einer Preceeding Invoice

Details siehe: https://github.com/itplr-kosit/xrechnung-schematron/releases/tag/release-1.6.0


### XRechnung Visualisierung

* Darstellung der Zeilennummern ist frei konfigurierbar
* Moeglichkeit zur tabelarischen Darstellung von Rechnungszeilen in PDF 

Details siehe: https://github.com/itplr-kosit/xrechnung-visualization/releases/tag/v2021-07-31

### XRechnung Testsuite

* Eingie neue Geschaeftsfaelle abgebildet
* Auf XRechnung 2.1.x ge-up-dated

Details siehe: https://github.com/itplr-kosit/xrechnung-testsuite/releases/tag/release-2021-07-31

## Bundle Bestandteile Details

### Validator (Prüftool)

Das Prüftool ist ein Programm, welches XML-Dateien (Dokumente) in Abhängigkeit von ihren Dokumenttypen gegen verschiedene Validierungsregeln (XML Schema und Schematron) prüft und das Ergebnis zu einem Konformitätsbericht (Konformitätsstatus valid oder invalid) mit einer Empfehlung zur Weiterverarbeitung (accept) oder Ablehnung (reject) aggregiert. Mittels Konfiguration kann bestimmt werden, welche der Konformitätsregeln durch ein Dokument, das zur Weiterverarbeitung empfohlen (accept) wird, verletzt sein dürfen.

Das Prüftool selbst ist fachunabhängig und kennt keine spezifischen Dokumentinhalte noch Validierungsregeln. Diese werden im Rahmen einer Prüftool-Konfiguration definiert, welche zur Anwendung des Prüftools erforderlich ist.

Weitere Details auf der [Validator Projektseite](https://github.com/itplr-kosit/validator).

### Validator Konfiguration XRechnung

Eine eigenständige Konfiguration für den Standard [XRechnung](http://www.xoev.de/de/xrechnung) wird ebenfalls auf [GitHub bereitgestellt](https://github.com/itplr-kosit/validator-configuration-xrechnung) ([Releases](https://github.com/itplr-kosit/validator-configuration-xrechnung/releases)). Diese enthält alle notwendigen Ressourcen zu der Norm EN16931 (XML-Schema und [Schematron Regeln] (https://github.com/CenPC434/validation) u.a.) und die [XRechnung Schematron Regeln](https://github.com/itplr-kosit/xrechnung-schematron) in ihren aktuellen Versionen.

Weitere Details auf der [Validator Konfiguration XRechnung Projektseite](https://github.com/itplr-kosit/validator-configuration-xrechnung).

### XRechnung Schematron Regeln

Technische Implementierung der Geschäftsregeln des Standards [XRechnung](http://www.xoev.de/de/xrechnung) in Schematron Rules für XML Validierung.

Weitere Details auf der [XRechnung Schematron Regeln Projektseite](https://github.com/itplr-kosit/xrechnung-schematron).

### XRechnung Visualisierung

XSL Transformatoren für die Generierung von HTML Web-Seiten.

Diese zeigen den Inhalt von elektronischen Rechnungen an die dem Standard [XRechnung](http://www.xoev.de/de/xrechnung) entsprechen.

Weitere Details auf der [XRechnung Visualisierung Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).

### XRechnung Testsuite

Valide Testdokumente des es Standards [XRechnung](http://www.xoev.de/de/xrechnung).

Diese dienen dazu bei Herstellern und Betreibern von IT-Fachverfahren
das Verständnis der [XRechnung-Spezifikation](https://www.xoev.de/de/xrechnung) zu fördern, indem die umfangreichen und komplexen
Vorgaben und Besonderheiten der Spezifikation durch valide Testdokumente veranschaulicht werden. Die Testdokumente stehen zur freien Verfügung für die Einbindung in eigene Testverfahren.

Weitere Details auf der [XRechnung Testsuite Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).
