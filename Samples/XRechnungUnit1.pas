unit XRechnungUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Vcl.StdCtrls,Winapi.ShellApi,Winapi.ShlObj,WinApi.ActiveX
  ,System.IOUtils,System.Win.COMObj,System.UITypes
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema
  ,intf.XRechnung,intf.Invoice, Vcl.OleCtrls, SHDocVw, Vcl.ExtCtrls
  ;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button3: TButton;
    Memo2: TMemo;
    WebBrowser1: TWebBrowser;
    Memo3: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    WebBrowser2: TWebBrowser;
    rbPaymentTerms: TRadioGroup;
    cbAllowanceCharges: TCheckBox;
    Button4: TButton;
    Label3: TLabel;
    cbPrepaidAmount: TCheckBox;
    btX122ConvertHTML: TButton;
    btX200ConvertHTML: TButton;
    Button1: TButton;
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btX122ConvertHTMLClick(Sender: TObject);
    procedure btX200ConvertHTMLClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    function ExecAndWait(Filename, Params: string): Boolean;
    procedure Generate122(inv : TInvoice);
    procedure Generate200(inv : TInvoice);
  public
    toolsPath : String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btX122ConvertHTMLClick(Sender: TObject);
var
  Doc : Variant;
begin
  Memo3.Clear;
  if FileExists(toolsPath+'validator\libs\Saxon-HE-9.9.1-7.jar') then
  begin
    ExecAndWait(toolsPath+'jre\jdk8u265-b01-jre\bin\java','-jar '+toolsPath+'validator\libs\Saxon-HE-9.9.1-7.jar '+
               '-s:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.xml '+
               '-xsl:'+toolsPath+'visualization\xsl\ubl-invoice-xr.xsl '+
               '-o:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122-xr.xml');
    ExecAndWait(toolsPath+'jre\jdk8u265-b01-jre\bin\java','-jar '+toolsPath+'validator\libs\Saxon-HE-9.9.1-7.jar '+
               '-s:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122-xr.xml '+
               '-xsl:'+toolsPath+'visualization\xsl\xrechnung-html.xsl '+
               '-o:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.html');
    WebBrowser1.Navigate2('file:\\\'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.html');
  end else
  begin
    Doc := WebBrowser1.Document;
    Doc.Clear;
    Doc.Write('<html><body>Validation Tool nicht installiert. Siehe Verzeichnis ./Tools/Read.Me</body></html>');
    Doc.Close;
  end;
end;

procedure TForm1.btX200ConvertHTMLClick(Sender: TObject);
var
  Doc : Variant;
begin
  Memo3.Clear;
  if FileExists(toolsPath+'validator\libs\Saxon-HE-9.9.1-7.jar') then
  begin
    ExecAndWait(toolsPath+'jre\jdk8u265-b01-jre\bin\java','-jar '+toolsPath+'validator\libs\Saxon-HE-9.9.1-7.jar '+
               '-s:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.xml '+
               '-xsl:'+toolsPath+'visualization\xsl\ubl-invoice-xr.xsl '+
               '-o:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200-xr.xml');
    ExecAndWait(toolsPath+'jre\jdk8u265-b01-jre\bin\java','-jar '+toolsPath+'validator\libs\Saxon-HE-9.9.1-7.jar '+
               '-s:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200-xr.xml '+
               '-xsl:'+toolsPath+'visualization\xsl\xrechnung-html.xsl '+
               '-o:'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.html');
    WebBrowser2.Navigate2('file:\\\'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.html');
  end else
  begin
    Doc := WebBrowser2.Document;
    Doc.Clear;
    Doc.Write('<html><body>Validation Tool nicht installiert. Siehe Verzeichnis ./Tools/Read.Me</body></html>');
    Doc.Close;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  inv : TInvoice;
  suc : Boolean;
begin
  WebBrowser1.Navigate2('about:blank');
  WebBrowser2.Navigate2('about:blank');
  Memo1.Clear;
  Memo2.Clear;
  Memo3.Clear;
  pnStartDragX122.Visible := false;
  pnStartDragX200.Visible := false;
  btX122ConvertHTML.Visible := false;
  btX200ConvertHTML.Visible := false;

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
  inv.AccountingSupplierParty.StreetName := 'Verkaeuferstraße 1';
  inv.AccountingSupplierParty.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.PostalZone := '01234';
  inv.AccountingSupplierParty.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefüllt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.StreetName := 'Kaeuferstraße 1';
  inv.AccountingCustomerParty.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.PostalZone := '05678';
  inv.AccountingCustomerParty.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingCustomerParty.ContactName := 'Müller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';

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
    Generate122(inv);
    Generate200(inv);
  finally
    inv.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  inv : TInvoice;
  suc : Boolean;
begin
  WebBrowser1.Navigate2('about:blank');
  WebBrowser2.Navigate2('about:blank');
  Memo1.Clear;
  Memo2.Clear;
  Memo3.Clear;
  pnStartDragX122.Visible := false;
  pnStartDragX200.Visible := false;
  btX122ConvertHTML.Visible := false;
  btX200ConvertHTML.Visible := false;

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
  inv.PurchaseOrderReference := 'B0815'; //Bestell-Nr. optional

  inv.AccountingSupplierParty.Name := 'Verkaeufername';
  inv.AccountingSupplierParty.RegistrationName := 'Verkaeufername'; //Sollte ausgefuellt werden
  inv.AccountingSupplierParty.CompanyID :=  '';
  inv.AccountingSupplierParty.StreetName := 'Verkaeuferstraße 1';
  inv.AccountingSupplierParty.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.PostalZone := '01234';
  inv.AccountingSupplierParty.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefüllt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.StreetName := 'Kaeuferstraße 1';
  inv.AccountingCustomerParty.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.PostalZone := '05678';
  inv.AccountingCustomerParty.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingCustomerParty.ContactName := 'Müller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';

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
    Generate122(inv);
    Generate200(inv);
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

  WebBrowser1.Navigate2('about:blank');
  WebBrowser2.Navigate2('about:blank');
  Memo1.Clear;
  Memo2.Clear;
  Memo3.Clear;
  pnStartDragX122.Visible := false;
  pnStartDragX200.Visible := false;
  btX122ConvertHTML.Visible := false;
  btX200ConvertHTML.Visible := false;

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
  inv.AccountingSupplierParty.StreetName := 'Verkaeuferstraße 1';
  inv.AccountingSupplierParty.City := 'Verkaeuferstadt';
  inv.AccountingSupplierParty.PostalZone := '01234';
  inv.AccountingSupplierParty.CountryCode := 'DE';
  inv.AccountingSupplierParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingSupplierParty.ContactName := 'Meier';
  inv.AccountingSupplierParty.ContactTelephone := '030 0815';
  inv.AccountingSupplierParty.ContactElectronicMail := 'meier@company.com';

  inv.AccountingCustomerParty.Name := 'Kaeufername';
  inv.AccountingCustomerParty.RegistrationName := 'Kaeufername'; //Sollte ausgefüllt werden
  inv.AccountingCustomerParty.CompanyID :=  'HRB 456';
  inv.AccountingCustomerParty.StreetName := 'Kaeuferstraße 1';
  inv.AccountingCustomerParty.City := 'Kaeuferstadt';
  inv.AccountingCustomerParty.PostalZone := '05678';
  inv.AccountingCustomerParty.CountryCode := 'DE';
  inv.AccountingCustomerParty.VATCompanyID := 'DE12345678'; //TODO mehrere Steuer-IDs
  inv.AccountingCustomerParty.ContactName := 'Müller';
  inv.AccountingCustomerParty.ContactTelephone := '030 1508';
  inv.AccountingCustomerParty.ContactElectronicMail := 'mueller@kunde.de';

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
      LineAmount := LineAmount - Amount;
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
    Generate200(inv);
  finally
    inv.Free;
  end;
end;

function TForm1.ExecAndWait(Filename, Params: string): Boolean;
var
  SA: TSecurityAttributes;
  SI: TStartupInfoA;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  Handle:Boolean;
  ProcessExitCode : DWORD;
  ReadLine : AnsiString;
begin
  Result := false;

  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    Handle := CreateProcessA(nil, PAnsiChar(AnsiString(Filename+ ' ' + Params)),
                            nil, nil, True, 0, nil,
                            PAnsiChar(AnsiString(ExtractFileDir(Application.ExeName))), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            ReadLine := Copy(Buffer,0,BytesRead);
            Memo3.Lines.add(Trim(String(ReadLine)));
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
        Result := GetExitCodeProcess(pi.hProcess, ProcessExitCode);
        if Result then
          Result := ProcessExitCode = 0;
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  toolsPath := ExtractFileDir(Application.ExeName);
  toolsPath := ExtractFileDir(toolsPath);
  toolsPath := ExtractFileDir(toolsPath);
  toolsPath := ExtractFileDir(toolsPath)+PathDelim+'Tools'+PathDelim;
end;

procedure TForm1.Generate122(inv: TInvoice);
var
  hstr : String;
  Doc : Variant;
begin
  TXRechnungInvoiceAdapter.SaveToXMLStr(inv,XRechnungVersion_122,hstr);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_122,ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.xml');
  if FileExists(toolsPath+'validator\validationtool-1.4.0-java8-standalone.jar') then
  begin
    ExecAndWait(toolsPath+'jre\jdk8u265-b01-jre\bin\java','-jar '+toolsPath+'validator\validationtool-1.4.0-java8-standalone.jar -s '+toolsPath+'validator-configuration-122\scenarios.xml -h '+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.xml');
    WebBrowser1.Navigate2('file:\\\'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122-report.html');
  end else
  begin
    Doc := WebBrowser1.Document;
    Doc.Clear;
    Doc.Write('<html><body>Validation Tool nicht installiert. Siehe Verzeichnis ./Tools/Read.Me</body></html>');
    Doc.Close;
  end;
  Memo1.Lines.Text := hstr;
  pnStartDragX122.Visible := FileExists(ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.xml');
  btX122ConvertHTML.Visible := pnStartDragX122.Visible;
end;

procedure TForm1.Generate200(inv: TInvoice);
var
  hstr : String;
  Doc : Variant;
begin
  TXRechnungInvoiceAdapter.SaveToXMLStr(inv,XRechnungVersion_200,hstr);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_200,ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.xml');
  if FileExists(toolsPath+'validator\validationtool-1.4.0-java8-standalone.jar') then
  begin
    ExecAndWait(toolsPath+'jre\jdk8u265-b01-jre\bin\java','-jar '+toolsPath+'validator\validationtool-1.4.0-java8-standalone.jar -s '+toolsPath+'validator-configuration-200\scenarios.xml -h '+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.xml');
    WebBrowser2.Navigate2('file:\\\'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200-report.html');
  end else
  begin
    Doc := WebBrowser2.Document;
    Doc.Clear;
    Doc.Write('<html><body>Validation Tool nicht installiert. Siehe Verzeichnis ./Tools/Read.Me</body></html>');
    Doc.Close;
  end;
  Memo2.Lines.Text := hstr;
  pnStartDragX200.Visible := FileExists(ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.xml');
  btX200ConvertHTML.Visible := pnStartDragX200.Visible;
end;

end.
