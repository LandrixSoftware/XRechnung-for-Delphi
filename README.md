[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5V8N3XFTU495G)

# XRechnung-for-Delphi

Erstellen von Rechnungen im 

- XRechnung-UBL-Format (Universal Business Language)
- XRechnung-CII-Format (Cross Industrie Invoice)

## Version

Aktuelle XRechnung-Versionen

- 3.0.2 gültig ab 1.2.2024 - Übersicht der Pflichtfelder unter https://blog.seeburger.com/de/xrechnung-version-3-0-1-gueltig-ab-01-februar-2024/
- 2.3.1 gültig ab 1.8.2023

## Beispiele

Zu finden unter Samples\XRechnungUnit2TestCases.pas

## ZUGFeRD Support

Um den Import von ZUGFeRD-Rechnungen zu ermöglichen, wurde die 
Bibliothek ZUGFeRD-for-Delphi integriert. Die Quellen hier zu finden:

https://github.com/LandrixSoftware/ZUGFeRD-for-Delphi

Aktivieren Sie dazu auch in der Unit intf.XRechnung.pas den Compiler-Schalter ZUGFeRD_Support.

```delphi
{$DEFINE ZUGFeRD_Support}
```
Ebenso steht zusätzlich eine Klasse TZUGFeRDAdditionalContent zur Verfügung, um weitere ZUGFeRD-Profil-Inhalte zu laden, die nicht vom XRechnungs-Profil unterstützt werden, z.B. die abweichende Rechnungsanschrift. Eine Instanz dieser Klasse kann man optional an die Methode TXRechnungInvoiceAdapter.LoadFrom...() übergeben. Sie enthält außerdem die geladene ZUGFeRD-Rechnung als 
komplettes Objekt. Hier kann man selbst weitere ZUGFeRD-Inhalte auslesen.

```delphi
  TZUGFeRDAdditionalContent = class
  public
    ZUGFeRDInvoice : TZUGFeRDInvoiceDescriptor;

    InvoiceeTradePartyFound : Boolean;
    InvoiceeTradeParty : TInvoiceAccountingParty;

    SpecifiedLogisticsServiceChargeFound : Boolean;
  end;

  var error : String;
  var inv : TInvoice := TInvoice.Create;
  var invAdditionalData : TZUGFeRDAdditionalContent := TZUGFeRDAdditionalContent.Create;
  try
    if TXRechnungInvoiceAdapter.LoadFromFile(inv, aFileName,
                                  error, invAdditionalData) then
    begin
      invAdditionalData.ZUGFeRDInvoice. .....
    end;  
  finally
    invAdditionalData.Free;
    inv.Free;
  end;
```

## Delphi 6 Support

Für Delphi 6 gibt es eine spezielle Version, die unter dem Verzeichnis Delphi6 zu finden ist. 
Diese Version enthält keine ZUGFeRD-for-Delphi-Unterstützung. Derzeit ist eine komplette ZUGFeRD-Unterstützung 
für Delphi 6 nicht geplant.

## Hilfsfunktion für den XRechnung-Export

Prüft, ob die zu exportierende Rechnung den Anforderungen der XRechnung entspricht. Gibt False zurück, wenn die Rechnung Werte enthält, die nicht im XRechnung-Profil erlaubt sind. Die Funktion ist nicht vollständig und wird ständig erweitert.

```delphi
unit intf.XRechnung;

TXRechnungInvoiceAdapter.ConsistencyCheck(_Invoice : TInvoice; _Version : TXRechnungVersion) : Boolean;
```
## Weitere Informationen zu XRechnung

- UBL-Format
  https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice

- CII-Format
  https://portal3.gefeg.com/invoice/tthome/index/617afdc4-623f-44e0-a05b-5b878840e508

- Validieren von XRechnung
  https://ecosio.com/de/peppol-und-xml-dokumente-online-validieren/

- https://portal3.gefeg.com/projectdata/invoice/deliverables/installed/publishingproject/factur-x%20(zugferd%202.0)/factur-x;%20draft;%20extended.scm/html/021.htm?https://portal3.gefeg.com/projectdata/invoice/deliverables/installed/publishingproject/factur-x%20(zugferd%202.0)/factur-x;%20draft;%20extended.scm/html/02233.htm

- https://www.verband-e-rechnung.org/xrechnung/

- https://xeinkauf.de/

- https://xeinkauf.de/xrechnung/versionen-und-bundles/

- https://github.com/itplr-kosit

- https://cranesoftwrights.github.io/resources/Crane-UBL-2.2-Skeleton/Crane-UBL-Invoice-2.2.html#result

- https://www.e-rechnung-bund.de/wp-content/uploads/2023/04/Uebersichtslisten-Eingabefelder-OZG-RE.pdf

- https://www.deutschebahn.com/en/invoicing-6930178#collapse6130808

# Lizenz / License

english version below

Die Bibliothek "XRechnung-for-Delphi" unterliegt eine Doppellizenz. Sie können sie kostenlos und
 ohne Einschränkungen unter der [GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) verwenden, oder Sie erwerben
eine Lizenz zur kommerziellen Nutzung unter der [Landrix Software Commercial License](commercial.license.md)

Eine kommerzielle Lizenz gewährt Ihnen das Recht, XRechnung-for-Delphi 
in Ihren eigenen Anwendungen zu verwenden. Lizenzfrei und ohne Verpflichtung zur 
Offenlegung Ihres Quellcodes oder Änderungen an die Landrix Software oder einer anderen Partei. 
Eine kommerzielle Lizenz gilt auf Dauer und berechtigt Sie kostenlos zu allen zukünftigen Updates.

Jede Firma, die Anwendungen mit der Bibliothek XRechnung-for-Delphi entwickelt, benötigt eine Lizenz.
Die Kosten dafür betragen 200,00 EUR zzgl. MwSt. pro Firma.

Bitte senden Sie eine E-Mail an info@landrix.de, um eine Rechnung mit den Zahlungsinformationen anzufordern.

Support- und Erweiterungsanfragen von lizensierten Benutzern werden bevorzugt behandelt. 
Neue Entwicklungen können abhängig von der für die Implementierung erforderlichen Zeit zusätzliche Kosten verursachen.

english version

The "XRechnung-for-Delphi" library is dual-licensed. You may choose to use it under the restrictions of 
the [GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) at no cost to you, or you may purchase 
for user under the [Landrix Software Commercial License](./commercial.license.md)

A commercial licence grants you the right to use XRechnung-for-Delphi in your own applications, 
royalty free, and without any requirement to disclose your source code nor any modifications to
Landrix Software to any other party. A commercial licence lasts into perpetuity, and 
entitles you to all future updates, free of charge.

A commercial licence is sold per company developing applications that use XRechnung-for-Delphi. 
The cost is 200,00 EUR plus VAT per company.

Please send an e-mail to info@landrix.de to request an invoice which will contain the bank details.

Support and enhancement requests submitted by users that pay for 
support will be prioritised. New developments may incur additional costs depending on time required for implementation.
