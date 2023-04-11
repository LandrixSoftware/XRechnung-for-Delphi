{
Copyright (C) 2023 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 2.3.1

License
This file is not official part of the package XRechnung-for-Delphi.

This is provided as is, expressly without a warranty of any kind.
You use it at your own risc.
}

unit XRechnungUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Vcl.StdCtrls,Winapi.ShellApi,Winapi.ShlObj,WinApi.ActiveX
  ,Vcl.OleCtrls, SHDocVw, Vcl.ExtCtrls
  ,System.IOUtils,System.Win.COMObj,System.UITypes
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema
  ,intf.XRechnung, intf.Invoice
  ,intf.XRechnungValidationHelperJava
  ;

type
  TForm1 = class(TForm)
    btCreateInvoice: TButton;
    Memo2: TMemo;
    Memo3: TMemo;
    Label2: TLabel;
    WebBrowser2: TWebBrowser;
    rbPaymentTerms: TRadioGroup;
    cbAllowanceCharges: TCheckBox;
    Button4: TButton;
    Label3: TLabel;
    cbPrepaidAmount: TCheckBox;
    btX2ConvertHTML: TButton;
    Button1: TButton;
    cbAttachments: TCheckBox;
    cbDeliveriyInf: TCheckBox;
    rbFormat: TRadioGroup;
    Button2: TButton;
    Panel1: TPanel;
    Button3: TButton;
    Button5: TButton;
    procedure btCreateInvoiceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btX2ConvertHTMLClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    procedure Generate231(inv : TInvoice);
  public
    JavaRuntimeEnvironmentPath : String;
    ValidatorLibPath : String;
    ValidatorConfigurationPath : String;
    VisualizationLibPath : String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  hstr : String;
begin
  hstr := ExtractFileDir(Application.ExeName);
  hstr := ExtractFileDir(hstr);
  hstr := ExtractFileDir(hstr);
  hstr := ExtractFileDir(hstr)+PathDelim+'Distribution'+PathDelim;
  JavaRuntimeEnvironmentPath := hstr +'java'+PathDelim;
  ValidatorLibPath := hstr +'validator'+PathDelim;
  ValidatorConfigurationPath := hstr +'validator-configuration'+PathDelim;
  VisualizationLibPath := hstr +'visualization'+PathDelim;
  Width := 50;
  Top := 50;
  Width := Screen.WorkAreaWidth-100;
  Height := Screen.WorkAreaHeight-100;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  inv : TInvoice;
  suc : Boolean;
begin
  WebBrowser2.Navigate2('about:blank');
  Memo2.Clear;
  Memo3.Clear;
  btX2ConvertHTML.Visible := false;

  inv := TInvoice.Create;
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;
  inv.InvoiceDueDate := Date+30;
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Note := 'keine';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefüllt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstraße 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkäufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefüllt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstraße 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingCustomerParty.ContactName := 'Müller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PayeeFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PayeeFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := 'Zahlbar sofort ohne Abzug.';

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01.01'; //Positionsnummer
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 4; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    SellersItemIdentification := 'A0815'; //Artikelnummer
    TaxPercent := 0.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_AE_VATReverseCharge;
    PriceAmount := 50; //Einzelpreis
    //TODO Preiseinheiten
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 200;
  end;

  inv.TaxAmountTotal := 0.0;
  SetLength(inv.TaxAmountSubtotals,1); //1 MwSt-Saetze
  inv.TaxAmountSubtotals[0].TaxPercent := 0;
  inv.TaxAmountSubtotals[0].TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_AE_VATReverseCharge;
  inv.TaxAmountSubtotals[0].TaxExemptionReason := 'Hiermit erlaube ich mir folgende Rechnung für Bauleistungen zu stellen. Die Umsatzsteuer für diese Leistung schuldet nach §13b UStG der Leistungsempfänger.';
  inv.TaxAmountSubtotals[0].TaxableAmount := 200.0;
  inv.TaxAmountSubtotals[0].TaxAmount := 0.0;

  inv.LineAmount := 200.0;         //Summe
  inv.TaxExclusiveAmount := 200.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 200.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 200.00;      //Summe Zahlbar MwSt

  try
    Generate231(inv);
  finally
    inv.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  inv : TInvoice;
  error : String;
  od : TOpenDialog;
begin
  inv := TInvoice.Create;
  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;
    if not TXRechnungInvoiceAdapter.LoadFromFile(inv, od.FileName,error) then
      memo3.Lines.Text := error
    else
      ShowMessage('Eingelesen');
  finally
    od.Free;
    inv.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  od : TOpenDialog;
  cmdoutput,xmlresult,htmlresult : String;
  Doc : Variant;
begin
  btX2ConvertHTML.Visible := false;
  WebBrowser2.Navigate2('about:blank');

  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;

    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetValidatorLibPath(ValidatorLibPath)
        .SetValidatorConfigurationPath(ValidatorConfigurationPath)
        .ValidateFile(od.FileName,cmdoutput,xmlresult,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    Doc := WebBrowser2.Document;
    Doc.Clear;
    if htmlresult <> '' then
      Doc.Write(htmlresult)
    else
      Doc.Write('<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>');
    Doc.Close;

  finally
    od.Free;
  end;
end;

procedure TForm1.btCreateInvoiceClick(Sender: TObject);
var
  inv : TInvoice;
  suc : Boolean;
begin
  WebBrowser2.Navigate2('about:blank');
  Memo2.Clear;
  Memo3.Clear;
  btX2ConvertHTML.Visible := false;

  inv := TInvoice.Create;
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;
  inv.InvoiceDueDate := Date+30;         //Rechnungsdatum
  inv.InvoicePeriodStartDate := Date-30; //Leistungs-/Lieferzeitpunkt Beginn
  inv.InvoicePeriodEndDate := Date-1;    //Leistungs-/Lieferzeitpunkt Ende
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Note := 'keine';
  inv.PurchaseOrderReference := 'B0815'; //Bestell-Nr. optional
  inv.ProjectReference := 'PR456789';
  inv.ContractDocumentReference := 'V876543210';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstraße 1';
  inv.AccountingSupplierParty.Address.AdditionalStreetName := 'Hinterhaus'; //optional
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountrySubentity := 'Sachsen';     //optional
  inv.AccountingSupplierParty.Address.AddressLine := 'Gate 64';  //optional
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkäufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefüllt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstraße 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingCustomerParty.ContactName := 'Müller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  //Eine Gruppe von Informationselementen, die Informationen über die Anschrift liefern, an die
  //die Waren geliefert oder an der die Dienstleistungen erbracht werden. Die Gruppe ist nur zu
  //verwenden, wenn die Lieferanschrift von der Erwerberanschrift abweicht. Wenn die Waren
  //abgeholt werden, ist die Abholadresse die Lieferadresse. Eine vollständige gültige Anschrift
  //ist anzugeben.
  if cbDeliveriyInf.Checked then
  begin
    inv.DeliveryInformation.Name := 'Firma die es bekommt';
    inv.DeliveryInformation.Address.StreetName := 'Lieferstraße 1';
    inv.DeliveryInformation.Address.City := 'Lieferstadt';
    inv.DeliveryInformation.Address.PostalZone := '05678';
    inv.DeliveryInformation.Address.CountryCode := 'DE';
    inv.DeliveryInformation.ActualDeliveryDate := Date-1;
  end;

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PayeeFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PayeeFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  //verschiedene Zahlungsbedingungen, Verzugszinsen fehlt noch
  case rbPaymentTerms.ItemIndex of
    1 :
    begin
      inv.PaymentTermsType := iptt_Net;
      inv.PaymentTermNetNote := 'Zahlbar sofort ohne Abzug.';
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

  // Der Dateiname des angehängten Dokuments muss innerhalb einer
  //Rechnung eindeutig sein (nicht case-sensitiv). Die Dateinamenserweiterung (extension), in der meist der Typ der
  //Datei angegeben wird, ist dabei Teil des Dateinamens und wird bei der Bestimmung der Eindeutigkeit einbezogen.

  //Sofern das Binärobjekt vom Typ XML ist, darf das angehängte XML keine Elemente beinhalten, welche
  //wiederum ein eigenständiges XML-Dokument beinhaltet. Rechnungssteller und Rechnungsempfänger sollten sich
  //zur Sicherstellung der Verarbeitung bzgl. des zu nutzenden XML vorab abstimmen.
  if cbAttachments.Checked then
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
      EmbedDataFromFile(ExtractFilePath(ExtractFileDir(ExtractFileDir(Application.ExeName))) +'attachment.xlsx');
    end;
    //ohne Daten
    with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_application_vnd_openxmlformats_officedocument_spreadsheetml_sheet) do
    begin
      ID := 'attachment.xlsx';
      Filename := 'attachment.xlsx';
    end;
    //ohne Daten
    with inv.Attachments.AddAttachment(TInvoiceAttachmentType.iat_application_vnd_oasis_opendocument_spreadsheet) do
    begin
      ID := 'attachment.ods';
      Filename := 'attachment.ods';
    end;
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
      ExternalReference := 'http://meinserver.de/attachment-external.pdf'
    end;
  end;


  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '001'; //Positionsnummer
    //Note : String; //Hinweis
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 2; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    //TODO Artikelnummer Kaeufer
    SellersItemIdentification := 'A0815'; //Artikelnummer
    TaxPercent := 7.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    PriceAmount := 50; //Einzelpreis
    //TODO Preiseinheiten
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 100;
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
    PriceAmount := 100; //Einzelpreis
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 100;

    //Nachlass zur Position
    if cbAllowanceCharges.Checked then
    with AllowanceCharges.AddAllowanceCharge do
    begin
      ChargeIndicator := false;
      ReasonCodeAllowance := TInvoiceAllowanceOrChargeIdentCode.iacic_Discount;
      BaseAmount := 50.00;
      MultiplierFactorNumeric := 10; //10 Prozent auf 50 EUR
      Amount := 5.00;
      //Nicht erforderlich TaxPercent := 19.0;
      //Nicht erforderlich TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
      LineAmount := LineAmount - Amount;
    end;
  end;


  if cbAllowanceCharges.Checked then //Nachlaesse generieren
  begin
    with inv.AllowanceCharges.AddAllowanceCharge do
    begin
      //Haeufig genutzte Gruende

      //41 Bonus for works ahead of schedule - Bonus fuer fruehzeitig erfuellte Aufgaben
      //42 Other bonus - sonstiger Bonus
      //60 Manufacturer’s consumer discount - Verbrauchernachlass des Herstellers
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
      Reason := 'Zuschlag fuer Kommuikation';
      BaseAmount := 10.00;
      MultiplierFactorNumeric := 10; //10 Prozent auf 10 EUR
      Amount := 1.00;
      TaxPercent := 19.0;
      TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    end;
  end;

  inv.TaxAmountTotal := 26.0;
  SetLength(inv.TaxAmountSubtotals,2); //2 MwSt-Saetze
  inv.TaxAmountSubtotals[0].TaxPercent := 7.0;
  inv.TaxAmountSubtotals[0].TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
  inv.TaxAmountSubtotals[0].TaxableAmount := 100.0;
  inv.TaxAmountSubtotals[0].TaxAmount := 7.0;
  inv.TaxAmountSubtotals[1].TaxPercent := 19.0;
  inv.TaxAmountSubtotals[1].TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
  inv.TaxAmountSubtotals[1].TaxableAmount := 100.0;
  inv.TaxAmountSubtotals[1].TaxAmount := 19.0;

  inv.LineAmount := 200.0;         //Summe
  inv.TaxExclusiveAmount := 200.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 226.00; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 226.00;      //Summe Zahlbar MwSt

  if cbAllowanceCharges.Checked then
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
  //TODO Evtl könnte man das hier noch ergänzen, indem man die Abschlagsrechnungen als Anhang anhängt
  if cbPrepaidAmount.Checked then
  begin
    with inv.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
    begin
      ID := 'R2020-0001';
      IssueDate := Date-100; //Rechnungsdatum
    end;
    inv.PrepaidAmount := 100.00; //Euro angezahlt
    inv.PayableAmount := inv.PayableAmount - inv.PrepaidAmount;
  end;

  //TODO PayableRoundingAmount
  try
    Generate231(inv);
  finally
    inv.Free;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  inv : TInvoice;
  suc : Boolean;
begin
  ShowMessage('Die Validierungswarnung scheint derzeit noch normal zu sein.'+#10+'[UBL-CR-646]-A UBL invoice should not include the InvoiceLine SubInvoiceLine');

  WebBrowser2.Navigate2('about:blank');
  Memo2.Clear;
  Memo3.Clear;
  btX2ConvertHTML.Visible := false;

  inv := TInvoice.Create;
  inv.InvoiceNumber := 'R2020-0815';
  inv.InvoiceIssueDate := Date;
  inv.InvoiceDueDate := Date+30;
  inv.InvoicePeriodStartDate := Date-30;
  inv.InvoicePeriodEndDate := Date-1;
  inv.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice; //Schlussrechnung
  inv.InvoiceCurrencyCode := 'EUR';
  inv.TaxCurrencyCode := 'EUR';
  inv.BuyerReference := '04011000-12345-34'; //Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
  inv.Note := 'keine';

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefüllt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.Address.StreetName := 'Verkaeuferstraße 1';
  inv.AccountingSupplierParty.Address.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.Address.PostalZone := '01234';
  inv.AccountingSupplierParty.Address.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';
  //BT-34 Gibt die elektronische Adresse des Verkäufers an, an die die Antwort auf eine Rechnung gesendet werden kann.
  //Aktuell nur Unterstuetzung fuer schemeID=EM ElectronicMail
  //Weitere Codes auf Anfrage
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:eas_4#version
  inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefüllt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.Address.StreetName := 'Kaeuferstraße 1';
  inv.AccountingCustomerParty.Address.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.Address.PostalZone := '05678';
  inv.AccountingCustomerParty.Address.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingCustomerParty.ContactName := 'Müller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';
  inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'antwortaufrechnung@kunde.de'; //BT-49

  inv.PaymentMeansCode := ipmc_SEPACreditTransfer; //Ueberweisung
  inv.PaymentID := 'Verwendungszweck der Ueberweisung...R2020-0815';
  inv.PayeeFinancialAccount := 'DE75512108001245126199'; //dies ist eine nicht existerende aber valide IBAN als test dummy
  inv.PayeeFinancialAccountName := 'Fa. XY';
  //inv.PayeeFinancialInstitutionBranch := 'DEU...'; //BIC

  inv.PaymentTermsType := iptt_Net;
  inv.PaymentTermNetNote := 'Zahlbar sofort ohne Abzug.';

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '01'; //Positionsnummer
    //Note : String; //Hinweis
    Name := 'Kurzinfo Titel'; //Kurztext
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCodeHelper.MapUnitOfMeasure('Stk',suc); //Mengeneinheit
    TaxPercent := 19.0; //MwSt
    TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
    PriceAmount := 400; //Einzelpreis
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
      PriceAmount := 50; //Einzelpreis
      //TODO Preiseinheiten
      BaseQuantity := 0; //Preiseinheit
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
      PriceAmount := 50; //Einzelpreis
      //TODO Preiseinheiten
      BaseQuantity := 0; //Preiseinheit
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

  inv.TaxAmountTotal := 68.40;
  SetLength(inv.TaxAmountSubtotals,1); //1 MwSt-Saetze
  inv.TaxAmountSubtotals[0].TaxPercent := 19.0;
  inv.TaxAmountSubtotals[0].TaxCategory := TInvoiceDutyTaxFeeCategoryCode.idtfcc_S_StandardRate;
  inv.TaxAmountSubtotals[0].TaxableAmount := 360.0;
  inv.TaxAmountSubtotals[0].TaxAmount := 68.40;

  inv.LineAmount := 360.0;         //Summe
  inv.TaxExclusiveAmount := 360.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 428.40; //Summe inkl MwSt
  inv.AllowanceTotalAmount := 0; //Abzuege
  inv.ChargeTotalAmount := 0; //Zuschlaege
  inv.PrepaidAmount := 0; //Anzahlungen
  inv.PayableAmount := 428.40;      //Summe Zahlbar MwSt

  try
    Generate231(inv);
  finally
    inv.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  od : TOpenDialog;
  cmdoutput,xmlresult,htmlresult : String;
  Doc : Variant;
begin
  btX2ConvertHTML.Visible := false;
  WebBrowser2.Navigate2('about:blank');

  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;


    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetValidatorLibPath(ValidatorLibPath)
        .SetVisualizationLibPath(VisualizationLibPath)
        .VisualizeFile(od.FileName, (TXRechnungValidationHelper.GetXRechnungVersion(od.FileName) in [XRechnungVersion_220_UBL,XRechnungVersion_230_UBL]),cmdoutput,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    Doc := WebBrowser2.Document;
    Doc.Clear;
    if htmlresult <> '' then
      Doc.Write(htmlresult)
    else
      Doc.Write('<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>');
    Doc.Close;
  finally
    od.Free;
  end;
end;

procedure TForm1.Generate231(inv: TInvoice);
var
  xml,cmdoutput,xmlresult,htmlresult,error : String;
  Doc : Variant;
  invtest : TInvoice;
begin
  Memo3.Clear;
  if rbFormat.itemindex = 0 then
  begin
    TXRechnungInvoiceAdapter.SaveToXMLStr(inv,XRechnungVersion_230_UBL,xml);

    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetValidatorLibPath(ValidatorLibPath)
        .SetValidatorConfigurationPath(ValidatorConfigurationPath)
        .Validate(xml,cmdoutput,xmlresult,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    Doc := WebBrowser2.Document;
    Doc.Clear;
    if htmlresult <> '' then
      Doc.Write(htmlresult)
    else
      Doc.Write('<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>');
    Doc.Close;

    Memo2.Lines.Text := xml;
    Memo2.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'XRechnung-UBL-230.xml',TEncoding.UTF8);

    invtest := TInvoice.Create;
    try
      TXRechnungInvoiceAdapter.LoadFromXMLStr(invtest,xml,error);
      if error <> '' then
        MessageDlg('error loading XRechnung'+#10+error, mtError, [mbOK], 0);
    finally
      invtest.Free;
    end;

    btX2ConvertHTML.Visible := true;
  end
  else
  begin
    TXRechnungInvoiceAdapter.SaveToXMLStr(inv,XRechnungVersion_230_UNCEFACT,xml);

    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetValidatorLibPath(ValidatorLibPath)
        .SetValidatorConfigurationPath(ValidatorConfigurationPath)
        .Validate(xml,cmdoutput,xmlresult,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    Doc := WebBrowser2.Document;
    Doc.Clear;
    if htmlresult <> '' then
      Doc.Write(htmlresult)
    else
      Doc.Write('<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>');
    Doc.Close;

    Memo2.Lines.Text := xml;
    Memo2.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'XRechnung-UNCEFACT-230.xml',TEncoding.UTF8);

    invtest := TInvoice.Create;
    try
      TXRechnungInvoiceAdapter.LoadFromXMLStr(invtest,xml,error);
      if error <> '' then
        MessageDlg('error loading XRechnung'+#10+error, mtError, [mbOK], 0);
    finally
      invtest.Free;
    end;

    btX2ConvertHTML.Visible := true;
  end;
end;

procedure TForm1.btX2ConvertHTMLClick(Sender: TObject);
var
  xml,cmdoutput,htmlresult : String;
  Doc : Variant;
begin
  xml := Memo2.Lines.Text;

  GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
      .SetValidatorLibPath(ValidatorLibPath)
      .SetVisualizationLibPath(VisualizationLibPath)
      .Visualize(xml,rbFormat.itemindex=0,cmdoutput,htmlresult);

  Memo3.Lines.Text := cmdoutput;

  Doc := WebBrowser2.Document;
  Doc.Clear;
  if htmlresult <> '' then
    Doc.Write(htmlresult)
  else
    Doc.Write('<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>');
  Doc.Close;
end;

end.
