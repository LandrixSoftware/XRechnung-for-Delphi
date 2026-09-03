[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5V8N3XFTU495G)

# XRechnung-for-Delphi

Elektronische Rechnungen **schreiben und lesen** – in Delphi und FreePascal, ohne externe Werkzeuge.
Auch die in ein ZUGFeRD-/Factur-X-PDF eingebettete Rechnung holt die Bibliothek selbst heraus, ohne
Mustang, PDFtk oder Java.

## Formate und Versionen

| Format | schreiben | lesen |
|---|:---:|:---:|
| XRechnung 3.0.2 – UBL (Universal Business Language) | ja | ja |
| XRechnung 3.0.2 – CII (Cross Industry Invoice) | ja | ja |
| ZUGFeRD / Factur-X 2.5.2 – EN16931 | ja | ja |
| ZUGFeRD / Factur-X 2.5.2 – EXTENDED | ja | ja |
| Peppol BIS Billing 3.0 | ja | ja |
| ältere XRechnung (1.2 bis 2.3) und fremde EN16931-CIUS | – | ja |
| ZUGFeRD 1.0 (`rsm:CrossIndustryDocument`) | – | – |

Seit dem 1.2.2024 darf nur noch als XRechnung 3.0.x geschrieben werden; ältere Stände lassen sich
weiterhin einlesen, die Feldstruktur ist dieselbe. Die Zuordnung der BT-/BG-Nummern zu den Feldern
des Datenmodells steht als Kommentar in [intf.Invoice.pas](intf.Invoice.pas), eine Übersicht der
Pflichtfelder findet sich unter [xeinkauf.de](https://xeinkauf.de/xrechnung/versionen-und-bundles/).

## Plattformen

- **Delphi** XE7 oder neuer, Win32 und Win64 – XML über MSXML
- **FreePascal** 3.2.2 / Lazarus mit `{$MODE DELPHIUNICODE}`, Windows und Linux – XML über `fcl-xml`
- **Delphi 6**: eigene Fassung im Verzeichnis [Delphi6/](Delphi6/), ohne die optionale
  ZUGFeRD-for-Delphi-Anbindung

Der Lese- und Schreibcode ist für beide Compiler derselbe; unter FreePascal liegt mit
[intf.XRechnungXmlShim.pas](intf.XRechnungXmlShim.pas) eine Anpassungsschicht darunter.

## Einbindung

Die Units des Wurzelverzeichnisses in den Suchpfad legen – weitere Abhängigkeiten gibt es nicht.

| Unit | Rolle |
|---|---|
| [intf.Invoice.pas](intf.Invoice.pas) | Datenmodell: `TInvoice`, `TInvoiceLine`, Codelisten |
| [intf.XRechnung.pas](intf.XRechnung.pas) | Fassade: `TXRechnungInvoiceAdapter`, Versionserkennung |
| [intf.XRechnung_3_0.pas](intf.XRechnung_3_0.pas) | Lesen und Schreiben von UBL und CII |
| [intf.XRechnungHelper.pas](intf.XRechnungHelper.pas) | XPath-Lesehelfer |
| [intf.XRechnungPdfExtract.pas](intf.XRechnungPdfExtract.pas) | Anhangsextraktion aus PDF |
| [intf.XRechnungXmlShim.pas](intf.XRechnungXmlShim.pas) | nur FreePascal |
| [intf.XRechnungValidationHelperJava.pas](intf.XRechnungValidationHelperJava.pas) | optional: Aufruf von Validator und Visualisierung |

## Rechnung schreiben

```delphi
uses
  intf.Invoice, intf.XRechnung;

var
  inv : TInvoice;
begin
  inv := TInvoice.Create;
  try
    inv.InvoiceNumber := 'R2026-0815';
    inv.InvoiceIssueDate := EncodeDate(2026,9,3);
    inv.InvoiceDueDate := EncodeDate(2026,10,1);
    inv.InvoiceTypeCode := itc_CommercialInvoice;
    inv.InvoiceCurrencyCode := 'EUR';
    inv.TaxCurrencyCode := 'EUR';
    inv.BuyerReference := TInvoiceEmptyLeitwegID.NON_EXISTENT;   //B2B ohne Leitweg-ID

    with inv.AccountingSupplierParty do
    begin
      RegistrationName := 'Verkaeufer GmbH';
      Address.StreetName := 'Hauptstrasse 1';
      Address.City := 'Verkaeuferstadt';
      Address.PostalZone := '01234';
      Address.CountryCode := 'DE';
      VATCompanyID := 'DE123456788';
      ContactName := 'Meier';
      ContactTelephone := '030 0815';
      ContactElectronicMail := 'meier@verkaeufer.de';
      ElectronicAddressSellerBuyer := 'rechnung@verkaeufer.de';   //BT-34
      ElectronicAddressSellerBuyerSchemeID := 'EM';
    end;

    with inv.AccountingCustomerParty do
    begin
      RegistrationName := 'Kaeufer AG';
      Address.StreetName := 'Nebenweg 2';
      Address.City := 'Kaeuferstadt';
      Address.PostalZone := '05678';
      Address.CountryCode := 'DE';
      ElectronicAddressSellerBuyer := 'rechnung@kaeufer.de';      //BT-49
      ElectronicAddressSellerBuyerSchemeID := 'EM';
    end;

    inv.DeliveryInformation.ActualDeliveryDate := EncodeDate(2026,9,1);

    with inv.PaymentTypes.AddPaymentType do
    begin
      PaymentMeansCode := ipmc_SEPACreditTransfer;
      FinancialAccount := 'DE75512108001245126199';
      FinancialAccountName := 'Verkaeufer GmbH';
    end;
    inv.PaymentTermsType := iptt_Net;
    inv.PaymentTermNetNote := 'Zahlbar bis zum 01.10.2026 ohne Abzug.';

    with inv.InvoiceLines.AddInvoiceLine do
    begin
      ID := '1';
      Name := 'Beratungsleistung';
      Quantity := 2;
      UnitCode := iuc_hour;
      NetPriceAmount := 100;
      LineAmount := 200;
      TaxPercent := 19.0;
      TaxCategory := idtfcc_S_StandardRate;
    end;

    with inv.TaxAmountSubtotals.AddTaxAmount do
    begin
      TaxPercent := 19.0;
      TaxCategory := idtfcc_S_StandardRate;
      TaxableAmount := 200.00;
      TaxAmount := 38.00;
    end;
    inv.TaxAmountTotal := 38.00;

    inv.LineAmount := 200.00;
    inv.TaxExclusiveAmount := 200.00;
    inv.TaxInclusiveAmount := 238.00;
    inv.PayableAmount := 238.00;

    TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,'rechnung-ubl.xml');
  finally
    inv.Free;
  end;
end;
```

Dieses Beispiel erzeugt eine gegen den KoSIT-Validator gültige XRechnung 3.0.2. Für ein anderes
Zielformat genügt es, die Version zu wechseln: `XRechnungVersion_30x_UNCEFACT` (CII),
`ZUGFeRDEN16931Version_250`, `ZUGFeRDExtendedVersion_250` oder `PeppolBillingVersion_30`. Neben
`SaveToFile` gibt es `SaveToStream` und `SaveToXMLStr`.

Ob eine Rechnung ins Zielprofil passt, beantwortet vorab:

```delphi
if not TXRechnungInvoiceAdapter.ConsistencyCheck(inv,XRechnungVersion_30x_UBL,errorCode) then
  //enthält Werte, die dieses Profil nicht erlaubt - errorCode sagt welche
```

Die Prüfung ist nicht vollständig und wird laufend erweitert.

## Rechnung lesen

Derselbe Aufruf nimmt XML und PDF entgegen; erst der letzte Parameter erlaubt den PDF-Weg:

```delphi
var
  inv : TInvoice;
  error : String;
begin
  inv := TInvoice.Create;
  try
    if TXRechnungInvoiceAdapter.LoadFromFile(inv,'rechnung.pdf',error,true) then
      //gelesen - egal ob XML oder ZUGFeRD-/Factur-X-PDF
  finally
    inv.Free;
  end;
```

Dazu passend gibt es `LoadFromStream`, `LoadFromXMLStr` sowie
`TXRechnungValidationHelper.GetXRechnungVersion`, das ein Format erkennt, bevor man es einliest.

Ausdrücklich aus einem PDF liest `LoadFromPdfFile` / `LoadFromPdfStream` – sie liefern zusätzlich den
Namen des Anhangs (`factur-x.xml`, `xrechnung.xml`, `ZUGFeRD-invoice.xml`, …). Sämtliche Anhänge eines
PDFs, auch Beilagen nach BG-24, bekommt man über `TXRechnungPdfExtractor.ExtractAllFromFile`. Die
Extraktion ist vollständig in Pascal geschrieben: eigener PDF-Objektparser mit xref-, XRef- und
Object-Streams. Verschlüsselte PDFs mit leerem Benutzerpasswort (reines Berechtigungspasswort) werden
gelesen; AES-verschlüsselte und passwortgeschützte PDFs werden gemeldet, nicht geraten. Details unter
[Tests/PdfExtract/README.md](Tests/PdfExtract/README.md).

## Validierung und Visualisierung

Im Verzeichnis [Distribution/](Distribution/) richtet `installtools.ps1` die Werkzeuge ein: den
KoSIT-Validator mit den Konfigurationen für XRechnung, Peppol BIS und ZUGFeRD, dazu Saxon, eine JRE,
Apache FOP, Mustang und beide Visualisierungen – die der KoSIT (HTML und PDF, deutsch) und das
offizielle OpenPEPPOL-Stylesheet für Peppol BIS Billing 3.0. Angesprochen werden sie über
[intf.XRechnungValidationHelperJava.pas](intf.XRechnungValidationHelperJava.pas); Einzelheiten stehen
in [Distribution/README.md](Distribution/README.md).

## Beispiele und Tests

- [Samples/](Samples/) – VCL-Demo zum Erzeugen, Validieren und Visualisieren. Die Testfälle in
  `XRechnungUnit2TestCases.pas` decken vom Minimalbeispiel bis zu Skonto, Differenzbesteuerung und
  Reverse-Charge alles ab, was die Bibliothek kann.
- [ValidXMLExamples/](ValidXMLExamples/) – 102 aus diesen Testfällen erzeugte Rechnungen, alle gegen
  den KoSIT-Validator geprüft (Schema, Schematron, Acceptance).
- [Tests/FreePascal/](Tests/FreePascal/) – Schreib-Parität und Lese-Roundtrip unter FreePascal gegen
  dieselben 102 Dateien.
- [Tests/PdfExtract/](Tests/PdfExtract/) – Test der PDF-Anhangsextraktion unter Delphi und FreePascal.

## Weitere Themen

- [Kennungen der Parteien (BT-29 / BT-46)](Documentation/Parteikennungen-BT-29-BT-46.md) – wann eine
  Kreditor-Nummer und wann eine GLN geschrieben wird, und warum in UBL nur eine von beiden ankommt.
  **Enthält einen Migrationshinweis für Anwendungen bis Version 3.0.2.7.**
- [Optionaler ZUGFeRD-Lesepfad](Documentation/ZUGFeRD-Support.md) – Anbindung von
  [ZUGFeRD-for-Delphi](https://github.com/LandrixSoftware/ZUGFeRD-for-Delphi) über den Schalter
  `ZUGFeRD_Support`, um an Profil-Inhalte jenseits des XRechnungs-Modells zu kommen.

## Weiterführende Links

- [Peppol BIS Billing 3.0 – UBL-Syntax](https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice)
- [xeinkauf.de – XRechnung-Versionen und Bundles](https://xeinkauf.de/xrechnung/versionen-und-bundles/)
- [KoSIT auf GitHub – Validator, Konfiguration, Visualisierung](https://github.com/itplr-kosit)
- [GEFEG-Profilbrowser – CII / Factur-X](https://portal3.gefeg.com/invoice/tthome/index/617afdc4-623f-44e0-a05b-5b878840e508)
- Online validieren: [ecosio](https://ecosio.com/de/peppol-und-xml-dokumente-online-validieren/) · [portinvoice](https://www.portinvoice.com/) · [valitool](https://valitool.org/)

## Lizenz / License

english version below

Die Bibliothek "XRechnung-for-Delphi" unterliegt eine Doppellizenz. Sie können sie kostenlos und
 ohne Einschränkungen unter der [GPL v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) verwenden, oder Sie erwerben
eine Lizenz zur kommerziellen Nutzung unter der [Landrix Software Commercial License](commercial.license.md)

Eine kommerzielle Lizenz gewährt Ihnen das Recht, XRechnung-for-Delphi 
in Ihren eigenen Anwendungen zu verwenden. Lizenzfrei und ohne Verpflichtung zur 
Offenlegung Ihres Quellcodes oder Änderungen an die Landrix Software oder einer anderen Partei. 
Eine kommerzielle Lizenz gilt auf Dauer und berechtigt Sie kostenlos zu allen zukünftigen Updates.

Jede Firma, die Anwendungen mit der Bibliothek XRechnung-for-Delphi entwickelt, benötigt eine Lizenz.
Die Kosten dafür betragen 450,00 EUR zzgl. MwSt. pro Firma.

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
The cost is 450,00 EUR plus VAT per company.

Please send an e-mail to info@landrix.de to request an invoice which will contain the bank details.

Support and enhancement requests submitted by users that pay for 
support will be prioritised. New developments may incur additional costs depending on time required for implementation.
