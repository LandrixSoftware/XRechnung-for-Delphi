# Xrechnung Bundle

Eine integriertes Bundle mit dem Spezifikationsdokument des für den Standard [XRechnung](http://www.xoev.de/de/xrechnung) und unterstützenden Komponenten.

## Validator (Prüftool)

Das Prüftool ist ein Programm, welches XML-Dateien (Dokumente) in Abhängigkeit von ihren Dokumenttypen gegen verschiedene Validierungsregeln (XML Schema und Schematron) prüft und das Ergebnis zu einem Konformitätsbericht (Konformitätsstatus valid oder invalid) mit einer Empfehlung zur Weiterverarbeitung (accept) oder Ablehnung (reject) aggregiert. Mittels Konfiguration kann bestimmt werden, welche der Konformitätsregeln durch ein Dokument, das zur Weiterverarbeitung empfohlen (accept) wird, verletzt sein dürfen.

Das Prüftool selbst ist fachunabhängig und kennt keine spezifischen Dokumentinhalte noch Validierungsregeln. Diese werden im Rahmen einer Prüftool-Konfiguration definiert, welche zur Anwendung des Prüftools erforderlich ist.

Weitere Details auf der [Validator Projektseite](https://github.com/itplr-kosit/validator).

## Validator Konfiguration XRechnung

Eine eigenständige Konfiguration für den Standard [XRechnung](http://www.xoev.de/de/xrechnung) wird ebenfalls auf [GitHub bereitgestellt](https://github.com/itplr-kosit/validator-configuration-xrechnung) ([Releases](https://github.com/itplr-kosit/validator-configuration-xrechnung/releases)). Diese enthält alle notwendigen Ressourcen zu der Norm EN16931 (XML-Schema und [Schematron Regeln] (https://github.com/CenPC434/validation) u.a.) und die [XRechnung Schematron Regeln](https://github.com/itplr-kosit/xrechnung-schematron) in ihren aktuellen Versionen.

Weitere Details auf der [Validator Konfiguration XRechnung Projektseite](https://github.com/itplr-kosit/validator-configuration-xrechnung).

## XRechnung Schematron Regeln

Technische Implementierung der Geschäftsregeln des Standards [XRechnung](http://www.xoev.de/de/xrechnung) in Schematron Rules für XML Validierung.

Weitere Details auf der [XRechnung Schematron Regeln Projektseite](https://github.com/itplr-kosit/xrechnung-schematron).

## XRechnung Visualisierung

XSL Transformatoren für die Generierung von HTML Web-Seiten.

Diese zeigen den Inhalt von elektronischen Rechnungen an die dem Standard [XRechnung](http://www.xoev.de/de/xrechnung) entsprechen.

Weitere Details auf der [XRechnung Visualisierung Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).

## XRechnung Testsuite

Valide Testdokumente des es Standards [XRechnung](http://www.xoev.de/de/xrechnung).

Diese dienen dazu bei Herstellern und Betreibern von IT-Fachverfahren
das Verständnis der [XRechnung-Spezifikation](https://www.xoev.de/de/xrechnung) zu fördern, indem die umfangreichen und komplexen
Vorgaben und Besonderheiten der Spezifikation durch valide Testdokumente veranschaulicht werden. Die Testdokumente stehen zur freien Verfügung für die Einbindung in eigene Testverfahren.

Weitere Details auf der [XRechnung Testsuite Projektseite](https://github.com/itplr-kosit/xrechnung-visualization).
