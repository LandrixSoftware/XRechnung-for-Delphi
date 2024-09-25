{
Copyright (C) 2024 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

License
This file is not official part of the package XRechnung-for-Delphi.

This is provided as is, expressly without a warranty of any kind.
You use it at your own risc.
}

unit XRechnungUnit2TestCases;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,System.IOUtils,System.Win.COMObj,System.UITypes,
  System.StrUtils,Vcl.Forms,
  intf.Invoice;

type
  TInvoiceTestCases = class(TObject)
  public
    class procedure Gesamtbeispiel(inv : TInvoice; Zahlungsbedingung : Integer;
                       NachlaesseZuschlaegeVerwenden, AbschlagsrechnungAbziehen,
                       AnhaengeVerwenden, LieferanschriftAusgeben : Boolean);
    class procedure Kleinunternehmerregelung(inv : TInvoice);
    class procedure Paragr13b(inv : TInvoice);
    class procedure Austauschteilesteuer(inv : TInvoice);
    class procedure Differenzbesteuerung(inv : TInvoice);
    class procedure TitelPositionsgruppen(inv : TInvoice);
    class procedure MinimalbeispielB2BOhneLeitwegID(inv : TInvoice);
    class procedure Rechnungskorrektur(inv : TInvoice);
    class procedure Gutschrift(inv : TInvoice);
    class procedure PreiseinheitGroesser1(inv : TInvoice);
    class procedure Lastschrift(inv : TInvoice);
    class procedure InnergemeinschaftlicheLieferungEUohneMehrwertsteuer(inv : TInvoice);
  end;

implementation

{ TInvoiceTestCases }

class procedure TInvoiceTestCases.Austauschteilesteuer(inv: TInvoice);
var
  suc : Boolean;
begin
  //https://www.comarch.de/produkte/datenaustausch-und-dokumentenmanagement/altteilsteuer-in-der-xrechnung/
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Notes.AddNote.Content := 'Rechnung enthaelt 100 EUR (Umsatz)Steuer auf Altteile gem. Abschn. 10.5 Abs. 3 UStAE';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Austauschteil'; //Kurztext
    Description := 'Langtext Austauschteil'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    SellersItemIdentification := 'A0815'; //Artikelnummer
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 1000; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 1000; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 1000;
  end;
  //10% AT-Steuer auf das Austauschteil
  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '02'; //Positionsnummer
    Name := 'Bemessungsgrundlage und Umsatzsteuer auf Altteil'; //Kurztext
    Description := 'Bemessungsgrundlage und Umsatzsteuer auf Altteil'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 100; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 100; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 100;
  end;
  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '03'; //Positionsnummer
    Name := 'Korrektur/Stornierung Bemessungsgrundlage der Umsatzsteuer auf Altteil'; //Kurztext
    Description := 'Korrektur/Stornierung Bemessungsgrundlage der Umsatzsteuer auf Altteil'; //Laengere Beschreibung
    Quantity := -1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 0.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_Z_ZeroRatedGoods;
    GrossPriceAmount := 100; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 100; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := -100;
  end;
  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '04'; //Positionsnummer
    Name := 'Montage'; //Kurztext
    Description := 'Montage'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Std',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 500; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 500; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 500;
  end;

  inv.TaxAmountTotal := 304.00; //Summe der gesamten MwSt
  //2 MwSt-Saetze
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 19.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxExemptionReason := '';
    TaxableAmount := 1000.00+100.00+500.00;
    TaxAmount     := 190.00 +19.00 +95.00;
  end;
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 0.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_Z_ZeroRatedGoods;
    TaxExemptionReason := '';
    TaxableAmount := -100.0;
    TaxAmount := 0.0;
  end;

  inv.LineAmount := 1000.00+100.00-100.00+500.00;         //Summe
  inv.TaxExclusiveAmount := 1000.00+100.00-100.00+500.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := inv.TaxExclusiveAmount + 190.00 +19.00 +95.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 1804.00;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.Differenzbesteuerung(inv: TInvoice);
var
  suc : Boolean;
begin
  //Das Beispiel fuer eine Differenzbesteuerung ist ein Vorschlag und wurde offiziell nicht validiert.
  //https://www.comarch.de/produkte/datenaustausch-und-dokumentenmanagement/altteilsteuer-in-der-xrechnung/
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Notes.AddNote.Content := 'Das Fahrzeug/der Artikel ist differenzbesteuert nach Paragraph 25a UStG Sonderregelung fuer Gebrauchtgegenstaende';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Fahrzeug mit Differenzbesteuerung'; //Kurztext
    Description := 'Fahrzeug mit Differenzbesteuerung'+#13#10+'Einkauf 4000,00 EUR'+#13#10+
                   'Marge 840,34 EUR'+#13#10+
                   'Verkauf Brutto 5000,00 EUR'+#13#10+
                   'Enthaltene MwSt. 159,66'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 0.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_E_ExemptFromTax;
    GrossPriceAmount := 5000; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 5000; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 5000;
  end;

  inv.TaxAmountTotal := 0.00; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 0.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_E_ExemptFromTax;
    TaxExemptionReason := 'Differenzbesteuerung';
    TaxableAmount := 5000.00;
    TaxAmount     := 00.00;
  end;

  inv.LineAmount := 5000.00;         //Summe
  inv.TaxExclusiveAmount := 5000.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 5000.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 5000.00;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.Gesamtbeispiel(inv: TInvoice;
  Zahlungsbedingung: Integer; NachlaesseZuschlaegeVerwenden,
  AbschlagsrechnungAbziehen, AnhaengeVerwenden,
  LieferanschriftAusgeben: Boolean);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30; //Leistungs-/Lieferzeitpunkt Beginn
  inv.InvoicePeriodEndDate := Date-1;    //Leistungs-/Lieferzeitpunkt Ende
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Notes.AddNote.Content := 'keine';
  inv.PurchaseOrderReference := 'B0815'; //Bestell-Nr. optional
  inv.ProjectReference := 'PR456789';
  inv.ContractDocumentReference := 'V876543210';
  inv.DeliveryReceiptNumber := 'Lieferschein123';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.AdditionalStreetName := 'Hinterhaus'; //optional
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountrySubentity := 'Sachsen';     //optional
  inv.AccountingSupplierParty.Address.AddressLine := 'Gate 64';  //optional
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  //Eine Gruppe von Informationselementen, die Informationen ueber die Anschrift liefern, an die
  //die Waren geliefert oder an der die Dienstleistungen erbracht werden. Die Gruppe ist nur zu
  //verwenden, wenn die Lieferanschrift von der Erwerberanschrift abweicht. Wenn die Waren
  //abgeholt werden, ist die Abholadresse die Lieferadresse. Eine vollstaendige gueltige Anschrift
  //ist anzugeben.
  if LieferanschriftAusgeben then
  begin
    inv.DeliveryInformation.Name := 'Firma die es bekommt';
    inv.DeliveryInformation.Address.StreetName := 'Lieferstrasse 1';
    inv.DeliveryInformation.Address.City := 'Lieferstadt';
    inv.DeliveryInformation.Address.PostalZone := '05678';
    inv.DeliveryInformation.Address.CountryCode := 'DE';
  end;

  //Lieferdatum
  inv.DeliveryInformation.ActualDeliveryDate := Date-1;

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  //verschiedene Zahlungsbedingungen, Verzugszinsen fehlt noch
  case Zahlungsbedingung of
    1 :
    begin
      inv.PaymentTermsType := iptt_Net;
      inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);
    end;
    2 :
    begin
      inv.PaymentTermsType := iptt_CashDiscount1;
      inv.PaymentTermCashDiscount1Days := 7;
      inv.PaymentTermCashDiscount1Percent := 4.25;
      inv.PaymentTermCashDiscount1Base := 0;
    end;
    3 :
    begin
      inv.PaymentTermsType := iptt_CashDiscount2;
      inv.PaymentTermCashDiscount1Days := 7;
      inv.PaymentTermCashDiscount1Percent := 4.25;
      inv.PaymentTermCashDiscount1Base := 0;
      inv.PaymentTermCashDiscount2Days := 14;
      inv.PaymentTermCashDiscount2Percent := 3;
      inv.PaymentTermCashDiscount2Base := 0;
    end;
    else
      inv.PaymentTermsType := iptt_None;
  end;

  // Der Dateiname des angehaengten Dokuments muss innerhalb einer
  //Rechnung eindeutig sein (nicht case-sensitiv). Die Dateinamenserweiterung (extension), in der meist der Typ der
  //Datei angegeben wird, ist dabei Teil des Dateinamens und wird bei der Bestimmung der Eindeutigkeit einbezogen.

  //Sofern das Binaerobjekt vom Typ XML ist, darf das angehaengte XML keine Elemente beinhalten, welche
  //wiederum ein eigenstaendiges XML-Dokument beinhaltet. Rechnungssteller und Rechnungsempfaenger sollten sich
  //zur Sicherstellung der Verarbeitung bzgl. des zu nutzenden XML vorab abstimmen.
  if AnhaengeVerwenden then
  begin
    with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_application_pdf) do
    begin
      ID := 'attachment.pdf';
      DocumentDescription := 'Eine PDF';
      Filename := 'attachment.pdf';
      EmbedDataFromFile(ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName))) +'attachment.pdf');
    end;
    with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_image_png) do
    begin
      ID := 'attachment.png';
      Filename := 'attachment.png';
      EmbedDataFromFile(ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName))) +'attachment.png');
    end;
    with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_image_jpeg) do
    begin
      ID := 'attachment.jpg';
      Filename := 'attachment.jpg';
      EmbedDataFromFile(ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName))) +'attachment.jpg');
    end;
    with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_text_csv) do
    begin
      ID := 'attachment.csv';
      Filename := 'attachment.csv';
      EmbedDataFromFile(ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName))) +'attachment.csv');
    end;
    //ohne Daten nur bis XRechnung 2.3.1 erlaubt
    //with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_application_vnd_openxmlformats_officedocument_spreadsheetml_sheet) do
    //begin
    //  ID := 'attachment.xlsx';
    //  Filename := 'attachment.xlsx';
    //end;
    //ohne Daten nur bis XRechnung 2.3.1 erlaubt
    //with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_application_vnd_oasis_opendocument_spreadsheet) do
    //begin
    //  ID := 'attachment.ods';
    //  Filename := 'attachment.ods';
    //end;

    //wird bei Ausgabe Version 1.2.2 nicht beachtet, da nicht unterstuetzt
    //gibt nach aktuellem Validierungstool noch einen Fehler
    //with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_application_xml) do
    //begin
    //  ID := 'attachment.xml';
    //  Filename := 'attachment.xml';
    //  EmbedDataFromFile(ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName))) +'attachment.xml');
    //end;
    //Externer Anhang
    with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_application_pdf) do
    begin
      ID := 'attachment-external.pdf';
      Filename := 'attachment-external.pdf';
      ExternalReference := 'http://meinserver.de/attachment-external.pdf';
      EmbedDataFromFile(ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName))) +'attachment.pdf');
    end;
  end;


  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '001'; //Positionsnummer
    GlobalID_EAN_GTIN := '978381582086'; //EAN
    //Note : String; //Hinweis
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 2; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    //TODO Artikelnummer Kaeufer
    SellersItemIdentification := 'A0815'; //Artikelnummer
    TaxPercent := 7.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 50; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 50; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 100;

    with ItemAttributes.AddItemAttribute do
    begin
      Name := 'Key1';
      Value := 'xyz';
    end;
    with ItemAttributes.AddItemAttribute do
    begin
      Name := 'Key2';
      Value := '123';
    end;
  end;
  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '002'; //Positionsnummer
    //Note : String; //Hinweis
    Name := 'Kurzinfo Artikel 2'; //Kurztext
    Description := 'Langtext Artikel 2'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    //TODO Artikelnummer Kaeufer
    SellersItemIdentification := 'A0816'; //Artikelnummer
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;

    //Rabatt auf den Bruttopreis generieren
    if NachlaesseZuschlaegeVerwenden then
    begin
      GrossPriceAmount := 150; //Brutto-Einzelpreis
      DiscountOnTheGrossPrice := 50; //50 EUR Rabatt
    end else
    begin
      GrossPriceAmount := 100; //Brutto-Einzelpreis
      DiscountOnTheGrossPrice := 0;
    end;
    NetPriceAmount := 100; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 100;

    //Nachlass zur Position generieren
    if NachlaesseZuschlaegeVerwenden then
    with AllowanceCharges.AddAllowanceCharge do
    begin
      ChargeIndicator := false;
      ReasonCodeAllowance := TInvoiceAllowanceOrChargeIdentCode.iacic_Discount;
      BaseAmount := 100.00;
      MultiplierFactorNumeric := 5; //5 Prozent auf 100 EUR
      Amount := 5.00;
      //Nicht erforderlich TaxPercent := 19.0;
      //Nicht erforderlich TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
      LineAmount := LineAmount - Amount; //Positionssumme um Nachlass reduzieren
    end;
  end;

  //Nachlaesse oder Zuschlaege zur Rechnung generieren
  if NachlaesseZuschlaegeVerwenden then
  begin
    with inv.AllowanceCharges.AddAllowanceCharge do
    begin
      //Haeufig genutzte Gruende

      //41 Bonus for works ahead of schedule - Bonus fuer fruehzeitig erfuellte Aufgaben
      //42 Other bonus - sonstiger Bonus
      //60 Manufacturer s consumer discount - Verbrauchernachlass des Herstellers
      //62 Due to military status - Wegen militaerischem Status
      //63 Due to work accident - Wegen Arbeitsunfall
      //64 Special agreement - Sondervereinbarung
      //65 Production error discount - Nachlass wegen Produktionsmangel
      //66 New outlet discount - Nachlass fuer neue Vertriebsstelle
      //67 Sample discount - Musternachlass
      //68 End of range discount - Nachlass fuer Auslaufsortiment
      //70 Incoterm discount - Incoterm Nachlass
      //71 Point of sales threshold allowance - Freibetrag fuer Umsaetze zum Verkaufszeitpunkt
      //88 Material surcharge/deduction - Materialzuschlag/-Nachlass
      //95 Discount - Nachlass
      //100 Special rebate - Sonderarbatt
      //102 Fixed long term - Feste Laufzeit
      //103 Temporary - Vorlaeufig
      //104 Standard - Regulaer
      ChargeIndicator := false;
      ReasonCodeAllowance := TInvoiceAllowanceOrChargeIdentCode.iacic_Discount;
      Reason := 'Nachlass 1';
      BaseAmount := 50.00;
      MultiplierFactorNumeric := 10; //10 Prozent auf 50 EUR
      Amount := 5.00;
      TaxPercent := 19.0;
      TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    end;
    with inv.AllowanceCharges.AddAllowanceCharge do
    begin
      ChargeIndicator := false;
      ReasonCodeAllowance := TInvoiceAllowanceOrChargeIdentCode.iacic_Discount;
      Reason := 'Nachlass 2 ohne Angabe von Basisbetrag und Nachlassprozente.';
      BaseAmount := 0.00;
      MultiplierFactorNumeric := 0;
      Amount := 5.00;
      TaxPercent := 19.0;
      TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    end;
    with inv.AllowanceCharges.AddAllowanceCharge do
    begin
      ChargeIndicator := true;
      ReasonCodeCharge := TInvoiceSpecialServiceDescriptionCode.issdc_AAA_Telecommunication;
      Reason := 'Zuschlag fuer Kommunikation';
      BaseAmount := 10.00;
      MultiplierFactorNumeric := 10; //10 Prozent auf 10 EUR
      Amount := 1.00;
      TaxPercent := 19.0;
      TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    end;
  end;

  inv.TaxAmountTotal := 26.0; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 7.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxableAmount := 100.0;
    TaxAmount := 7.0;
  end;
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 19.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxableAmount := 100.0;
    TaxAmount := 19.0;
  end;

  inv.LineAmount := 200.0;         //Summe
  inv.TaxExclusiveAmount := 200.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 226.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 226.00;      //Summe Zahlbar MwSt

  //Wenn Nachlaesse oder Zuschlaege zur Rechnung vorhanden sind
  //Rechnungssummen anpassen
  if NachlaesseZuschlaegeVerwenden then
  begin
    inv.LineAmount := inv.LineAmount - 5.00;
    inv.AllowanceTotalAmount := 5.00 + 5.00;
    inv.ChargeTotalAmount := 1.00;
    inv.TaxAmountSubtotals[1].TaxableAmount := inv.TaxAmountSubtotals[1].TaxableAmount - 5.00 - 5.00 + 1.00 - 5.00;
    inv.TaxAmountSubtotals[1].TaxAmount := inv.TaxAmountSubtotals[1].TaxAmount - 0.95 - 0.95 + 0.19 - 0.95;
    inv.TaxAmountTotal := inv.TaxAmountTotal - 0.95 - 0.95 + 0.19 - 0.95;
    inv.TaxExclusiveAmount := inv.TaxExclusiveAmount - inv.AllowanceTotalAmount + inv.ChargeTotalAmount - 5.00;
    inv.TaxInclusiveAmount := inv.TaxInclusiveAmount - 5.00 - 0.95 - 5.00 - 0.95 + 1.00 + 0.19 - 5.00 -0.95;
    inv.PayableAmount := inv.TaxInclusiveAmount;
  end;

  //Abschlagszahlungen abziehen
  //Hinweise: Evtl koennte man noch die Abschlagsrechnungen als Anhang anhaengen
  if AbschlagsrechnungAbziehen then
  begin
    //Im CII-ZUGFeRD-Format ist hier nur maximal eine Referenz moeglich
    with inv.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
    begin
      ID := 'R2020-0001';
      IssueDate := Date-100; //Rechnungsdatum
    end;
    inv.PrepaidAmount := 100.00; //Euro angezahlt
    inv.PayableAmount := inv.PayableAmount - inv.PrepaidAmount; //Vom Zahlbetrag abziehen
  end;

  //TODO PayableRoundingAmount
end;

class procedure TInvoiceTestCases.Gutschrift(inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := 0;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CreditNote; //Gutschrift
  inv.InvoiceCurrencyCode := 'EUR';
  inv.Notes.AddNote.Content := 'Notiz zur Gutschrift';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  inv.AccountingSupplierParty.AdditionalLegalInformationSeller := 'Keine Ausweisung der Umsatzsteuer, da Kleinunternehmer gemaess Paragraph 19 UStG';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  //Pflichtangabe
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Provision'; //Kurztext
    Description := 'Provision'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 0.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_E_ExemptFromTax;
    GrossPriceAmount := 500; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 500; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 500;
  end;

  inv.TaxAmountTotal := 0.00; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 0.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_E_ExemptFromTax;
    TaxExemptionReason := 'Keine Ausweisung der Umsatzsteuer, da Kleinunternehmer gemaess Paragraph 19 UStG';
    TaxableAmount := 500.00;
    TaxAmount     := 00.00;
  end;

  inv.LineAmount := 500.00;         //Summe
  inv.TaxExclusiveAmount := 500.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 500.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 500.00;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.InnergemeinschaftlicheLieferungEUohneMehrwertsteuer(
  inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := TInvoiceEmptyLeitwegID.NON_EXISTENT; //B2B ohne Leitweg-ID

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  inv.AccountingSupplierParty.AdditionalLegalInformationSeller := 'Keine Ausweisung der Umsatzsteuer, da Kleinunternehmer gemaess Paragraph 19 UStG';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  //Pflichtangabe
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'AT';
  inv.AccountingCustomerParty.VATCompanyID := 'AT12345678';
//  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.at';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.at'; //BT-49

  inv.DeliveryInformation.Name := 'Firma die es bekommt';
  inv.DeliveryInformation.Address.StreetName := 'Lieferstrasse 1';
  inv.DeliveryInformation.Address.City := 'Lieferstadt';
  inv.DeliveryInformation.Address.PostalZone := '05678';
  inv.DeliveryInformation.Address.CountryCode := 'AT';
  inv.DeliveryInformation.ActualDeliveryDate := Date-1;

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Homepage erstellt'; //Kurztext
    Description := 'Homepage erstellt'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 0.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices;
    GrossPriceAmount := 5000; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 5000; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 5000;
  end;

  inv.TaxAmountTotal := 0.00; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 0.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices;
    TaxExemptionReason := 'Keine Ausweisung der Umsatzsteuer - innergemeinschaftliche Lieferung EU';
    TaxableAmount := 5000.00;
    TaxAmount     := 00.00;
  end;

  inv.LineAmount := 5000.00;         //Summe
  inv.TaxExclusiveAmount := 5000.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 5000.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 5000.00;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.Kleinunternehmerregelung(inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  inv.AccountingSupplierParty.AdditionalLegalInformationSeller := 'Keine Ausweisung der Umsatzsteuer, da Kleinunternehmer gemaess Paragraph 19 UStG';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  //Pflichtangabe
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Homepage erstellt'; //Kurztext
    Description := 'Homepage erstellt'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 0.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_E_ExemptFromTax;
    GrossPriceAmount := 5000; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 5000; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 5000;
  end;

  inv.TaxAmountTotal := 0.00; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 0.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_E_ExemptFromTax;
    TaxExemptionReason := 'Keine Ausweisung der Umsatzsteuer, da Kleinunternehmer gemaess Paragraph 19 UStG';
    TaxableAmount := 5000.00;
    TaxAmount     := 00.00;
  end;

  inv.LineAmount := 5000.00;         //Summe
  inv.TaxExclusiveAmount := 5000.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 5000.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 5000.00;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.Lastschrift(inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := TInvoiceEmptyLeitwegID.NON_EXISTENT; //B2B ohne Leitweg-ID

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPADirectDebit; //Lastschrift
  inv.PaymentID := 'Verwendungszweck der Lastschrift...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentMandateID := 'MANDATID'; //Mandats-ID
  inv.AccountingSupplierParty.BankAssignedCreditorIdentifier := '111111'; //Glaeubiger-ID

  inv.PaymentTermsType := iptt_None;

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 360; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 360; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 360;
  end;

  inv.TaxAmountTotal := 68.40; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 19.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxableAmount := 360.0;
    TaxAmount := 68.40;
  end;

  inv.LineAmount := 360.0;         //Summe
  inv.TaxExclusiveAmount := 360.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 428.40; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 428.40;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.MinimalbeispielB2BOhneLeitwegID(
  inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := TInvoiceEmptyLeitwegID.NON_EXISTENT; //B2B ohne Leitweg-ID

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_InstrumentNotDefined; //Nicht definiert

  inv.PaymentTermsType := iptt_None;

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 360; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 360; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 360;
  end;

  inv.TaxAmountTotal := 68.40; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 19.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxableAmount := 360.0;
    TaxAmount := 68.40;
  end;

  inv.LineAmount := 360.0;         //Summe
  inv.TaxExclusiveAmount := 360.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 428.40; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 428.40;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.Paragr13b(inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Notes.AddNote.Content := 'keine';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  //Pflichtangabe
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01.01'; //Positionsnummer
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 4.00; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    SellersItemIdentification := 'A0815'; //Artikelnummer
    TaxPercent := 0.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_AE_VATReverseCharge;
    GrossPriceAmount := 50; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 50; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 200.00;
  end;

  inv.TaxAmountTotal := 0.0; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_AE_VATReverseCharge;
    TaxExemptionReason := 'Hiermit erlaube ich mir folgende Rechnung fuer Bauleistungen zu stellen. Die Umsatzsteuer fuer diese Leistung schuldet nach Paragraph 13b UStG der Leistungsempfaenger.';
    TaxableAmount := 200.0;
    TaxAmount := 0.0;
  end;

  inv.LineAmount := 200.0;         //Summe
  inv.TaxExclusiveAmount := 200.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 200.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 200.00;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.PreiseinheitGroesser1(inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := TInvoiceEmptyLeitwegID.NON_EXISTENT; //B2B ohne Leitweg-ID

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_InstrumentNotDefined; //Nicht definiert

  inv.PaymentTermsType := iptt_None;

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 360; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 360; //Netto-Einzelpreis
    BaseQuantity := 10; //Preiseinheit
    BaseQuantityUnitCode := UnitCode; //Preiseinheit Mengeneinheit
    LineAmount := 36;
  end;

  inv.TaxAmountTotal := 6.84; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 19.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxableAmount := 36.0;
    TaxAmount := 6.84;
  end;

  inv.LineAmount := 36.0;         //Summe
  inv.TaxExclusiveAmount := 36.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 42.84; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 42.84;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.Rechnungskorrektur(inv: TInvoice);
var
  suc : Boolean;
begin
  inv.InvoiceNumber := 'R2020-0815';
  //Korrektur bezieht sich auf Rechnung xxx
  with inv.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
  begin
    ID := 'R2020-0814';
    IssueDate := Date-40;
  end;
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CorrectedInvoice; //Storno/Rechnungskorrektur
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := TInvoiceEmptyLeitwegID.NON_EXISTENT; //B2B ohne Leitweg-ID

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_InstrumentNotDefined; //Nicht definiert

  inv.PaymentTermsType := iptt_None;

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := -1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 360; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 360; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := -360;
  end;

  inv.TaxAmountTotal := -68.40; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 19.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxableAmount := -360.0;
    TaxAmount := -68.40;
  end;

  inv.LineAmount := -360.0;         //Summe
  inv.TaxExclusiveAmount := -360.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := -428.40; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := -428.40;      //Summe Zahlbar MwSt
end;

class procedure TInvoiceTestCases.TitelPositionsgruppen(inv: TInvoice);
var
  suc : Boolean;
begin
  //Die Validierungswarnung scheint derzeit noch normal zu sein.
  //[UBL-CR-646]-A UBL invoice should not include the InvoiceLine SubInvoiceLine

  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;          //Rechnungsdatum
  inv.InvoiceDueDate := Date+30;         //Faelligkeitsdatum
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Notes.AddNote.Content := 'keine';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstrasse 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678';
  inv.AccountingSupplierParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkaeufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefuellt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstrasse 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678';
  inv.AccountingCustomerParty.VATCompanyNumber := '222/111/4444';
  inv.AccountingCustomerParty.ContactName := 'Mueller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PaymentFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PaymentFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := Format('Zahlbar bis zum %s ohne Abzug.',[DateToStr(inv.InvoiceDueDate)]);

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    //Note : String; //Hinweis
    Name := 'Kurzinfo Titel'; //Kurztext
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    GrossPriceAmount := 400; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice := 0;
    NetPriceAmount := 400; //Netto-Einzelpreis
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 400;

    with SubInvoiceLines.AddInvoiceLine do
    begin
      ID := '01.01'; //Positionsnummer
      Name := 'Kurzinfo Artikel 1'; //Kurztext
      Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
      Quantity := 4; //Menge
      UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
      SellersItemIdentification := 'A0815'; //Artikelnummer
      TaxPercent := 19.0; //MwSt
      TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
      GrossPriceAmount := 50; //Brutto-Einzelpreis
      DiscountOnTheGrossPrice := 0;
      NetPriceAmount := 50; //Netto-Einzelpreis
      BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
      BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
      LineAmount := 200;
    end;
    with SubInvoiceLines.AddInvoiceLine do
    begin
      ID := '01.02'; //Positionsnummer
      Name := 'Kurzinfo Artikel 2'; //Kurztext
      Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
      Quantity := 4; //Menge
      UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
      SellersItemIdentification := 'A0816'; //Artikelnummer
      TaxPercent := 19.0; //MwSt
      TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
      GrossPriceAmount := 50; //Brutto-Einzelpreis
      DiscountOnTheGrossPrice := 0;
      NetPriceAmount := 50; //Netto-Einzelpreis
      BaseQuantity := 0; //Preiseinheit 0 = wird nicht ausgegeben, entspricht default = 1
      BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
      LineAmount := 200;
    end;
    //Nachlass
    with AllowanceCharges.AddAllowanceCharge do
    begin
      ChargeIndicator := false;
      ReasonCodeAllowance := TInvoiceAllowanceOrChargeIdentCode.iacic_Discount;
      BaseAmount := 400.00;
      MultiplierFactorNumeric := 10; //10 Prozent auf 400 EUR
      Amount := 40.00;
      LineAmount := LineAmount - Amount; //Gesamtsumme der oberen InvoiceLine reduzieren
    end;
  end;

  inv.TaxAmountTotal := 68.40; //Summe der gesamten MwSt
  with inv.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxPercent := 19.0;
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    TaxableAmount := 360.0;
    TaxAmount := 68.40;
  end;

  inv.LineAmount := 360.0;         //Summe
  inv.TaxExclusiveAmount := 360.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 428.40; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 428.40;      //Summe Zahlbar MwSt
end;

end.
