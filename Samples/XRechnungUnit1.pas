unit XRechnungUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Vcl.StdCtrls,Winapi.ShellApi,Winapi.ShlObj,WinApi.ActiveX
  ,System.IOUtils
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,intf.MSXML2_TLB
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
    RadioGroup1: TRadioGroup;
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function ExecAndWait(Filename, Params: string): Boolean;
  public
    toolsPath : String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button3Click(Sender: TObject);
var
  inv : TInvoice;
  hstr : String;
begin
  WebBrowser1.Navigate2('about:blank');
  WebBrowser2.Navigate2('about:blank');
  Memo1.Clear;
  Memo2.Clear;
  Memo3.Clear;

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


  //verschiedene Zahlungsbedingungen, Verzugszinsen fehlt noch
  case RadioGroup1.ItemIndex of
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

  inv.TaxAmountTotal := 26.0;
  SetLength(inv.TaxAmountSubtotals,2); //2 MwSt-Saetze
  inv.TaxAmountSubtotals[0].TaxPercent := 7.0;
  inv.TaxAmountSubtotals[0].TaxableAmount := 100.0;
  inv.TaxAmountSubtotals[0].TaxAmount := 7.0;
  inv.TaxAmountSubtotals[1].TaxPercent := 19.0;
  inv.TaxAmountSubtotals[1].TaxableAmount := 100.0;
  inv.TaxAmountSubtotals[1].TaxAmount := 19.0;


  inv.LineAmount := 200.0;         //Summe
  inv.TaxExclusiveAmount := 200.00; //Summe ohne MwSt
  inv.TaxInclusiveAmount := 226.00; //Summe inkl MwSt
  //TODO AllowanceTotalAmount Abzuege
  //TODO ChargeTotalAmount Zuschlaege
  //TODO PrepaidAmount
  //TODO PayableRoundingAmount
  inv.PayableAmount := 226.00;      //Summe Zahlbar MwSt

  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '001'; //Positionsnummer
    //Note : String; //Hinweis
    Name := 'Kurzinfo Artikel 1'; //Kurztext
    Description := 'Langtext Artikel'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 2; //Menge
    UnitCode := TInvoiceUnitCode.iuc_piece; //Mengeneinheit
    //TODO Artikelnummer Kaeufer
    SellersItemIdentification := 'A0815'; //Artikelnummer
    TaxPercent := 7.0; //MwSt
    PriceAmount := 50; //Einzelpreis
    //TODO Preiseinheiten
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    //TODO Rabatte
    LineAmount := 100;
  end;
  with inv.InvoiceLines.AddInvoiceLine do
  begin
    ID := '002'; //Positionsnummer
    //Note : String; //Hinweis
    Name := 'Kurzinfo Artikel 2'; //Kurztext
    Description := 'Langtext Artikel 2'+#13#10+'Zeile 2'+#13#10+'Zeile 3'; //Laengere Beschreibung
    Quantity := 1; //Menge
    UnitCode := TInvoiceUnitCode.iuc_piece; //Mengeneinheit
    //TODO Artikelnummer Kaeufer
    SellersItemIdentification := 'A0816'; //Artikelnummer
    TaxPercent := 19.0; //MwSt
    PriceAmount := 100; //Einzelpreis
    BaseQuantity := 0; //Preiseinheit
    BaseQuantityUnitCode := TInvoiceUnitCode.iuc_None; //Preiseinheit Mengeneinheit
    LineAmount := 100;
  end;

  try
    TXRechnungInvoiceAdapter.SaveToXMLStr(inv,XRechnungVersion_122,hstr);
    TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_122,ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.xml');
    if FileExists(toolsPath+'validator\validationtool-1.3.1-java8-standalone.jar') then
    begin
      ExecAndWait(GetEnvironmentVariable('JAVA_HOME')+'\java','-jar '+toolsPath+'validator\validationtool-1.3.1-java8-standalone.jar -s '+toolsPath+'validator-configuration-122\scenarios.xml  -h '+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122.xml');
      WebBrowser1.Navigate2('file:\\\'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-122-report.html');
    end else
    begin
      var Doc : Variant := WebBrowser1.Document;
      Doc.Clear;
      Doc.Write('<html><body>Validation Tool nicht installiert. Siehe Verzeichnis ./Tools/Read.Me</body></html>');
      Doc.Close;
    end;
    Memo1.Lines.Text := hstr;

    TXRechnungInvoiceAdapter.SaveToXMLStr(inv,XRechnungVersion_200,hstr);
    TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_200,ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.xml');
    if FileExists(toolsPath+'validator\validationtool-1.3.1-java8-standalone.jar') then
    begin
      ExecAndWait(GetEnvironmentVariable('JAVA_HOME')+'\java','-jar '+toolsPath+'validator\validationtool-1.3.1-java8-standalone.jar -s '+toolsPath+'validator-configuration-200\scenarios.xml  -h '+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200.xml');
      WebBrowser2.Navigate2('file:\\\'+ExtractFilePath(Application.ExeName)+'XRechnung-UBL-200-report.html');
    end else
    begin
      var Doc : Variant := WebBrowser2.Document;
      Doc.Clear;
      Doc.Write('<html><body>Validation Tool nicht installiert. Siehe Verzeichnis ./Tools/Read.Me</body></html>');
      Doc.Close;
    end;
    Memo2.Lines.Text := hstr;
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

end.
