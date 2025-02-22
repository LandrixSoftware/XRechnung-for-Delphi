{
Copyright (C) 2025 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

License
This file is not official part of the package XRechnung-for-Delphi.

This is provided as is, expressly without a warranty of any kind.
You use it at your own risc.
}

unit XRechnungUnit1;

interface

{.$DEFINE USE_EDGE_BROWSER}

uses
  Windows, Messages, SysUtils, Variants,
  Classes,COMObj,StrUtils,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls,ShellApi,ShlObj,ActiveX
  ,OleCtrls, SHDocVw, ExtCtrls, ComCtrls
  ,xmldom,XMLDoc,XMLIntf,XMLSchema

  ,XRechnungUnit2TestCases
  ,intfXRechnung, intfInvoice
  ,intfXRechnungValidationHelperJava
  ;

type
  TForm1 = class(TForm)
    btCreateInvoice: TButton;
    Memo2: TMemo;
    Memo3: TMemo;
    rbPaymentTerms: TRadioGroup;
    cbAllowanceCharges: TCheckBox;
    Label3: TLabel;
    cbPrepaidAmount: TCheckBox;
    cbAttachments: TCheckBox;
    cbDeliveriyInf: TCheckBox;
    rbFormat: TRadioGroup;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    rbVersion: TRadioGroup;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Button9: TButton;
    ListBox1: TListBox;
    cbValidateWithJava: TCheckBox;
    WebBrowser2: TWebBrowser;
    procedure btCreateInvoiceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure rbVersionClick(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    WebBrowserContentFilename : String;
    WebBrowserContentFilenameHtml : String;
    WebBrowserContentFilenamePdf : String;
    procedure Generate(inv : TInvoice);
  private
    procedure ClearBrowser;
    procedure ShowFileInBrowser(const _Filename : String; _BrowserIdx : Integer);
    procedure ShowXMLAsHtml(_Content : String);
    procedure ShowXMLAsPdf(_Content : String);
  public
    DistributionBasePath : String;
    JavaRuntimeEnvironmentPath : String;
    ValidatorLibPath : String;
    ValidatorConfigurationPath : String;
    VisualizationLibPath : String;
    FopLibPath : String;
  public
    function ReadAllText(_Filename : String) : string;
    procedure WriteAllText(_Filename,_Value : String);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DistributionBasePath := ExtractFileDir(Application.ExeName);
  DistributionBasePath := ExtractFileDir(DistributionBasePath);
  DistributionBasePath := ExtractFileDir(DistributionBasePath);
  DistributionBasePath := ExtractFileDir(DistributionBasePath)+PathDelim+'Distribution'+PathDelim;
  JavaRuntimeEnvironmentPath := DistributionBasePath +'java'+PathDelim;
  ValidatorLibPath := DistributionBasePath +'validator'+PathDelim;
  ValidatorConfigurationPath := DistributionBasePath +'validator-configuration'+ifthen(rbVersion.ItemIndex = 0,'23x','30x')+PathDelim;
  VisualizationLibPath := DistributionBasePath +'visualization'+ifthen(rbVersion.ItemIndex = 0,'23x','30x')+PathDelim;
  FopLibPath := DistributionBasePath + 'apache-fop'+PathDelim;

  Left := 50;
  Top := 50;
  Width := Screen.WorkAreaWidth-100;
  Height := Screen.WorkAreaHeight-100;

  WebBrowserContentFilename := ExtractFilePath(Application.ExeName)+'content.html';
  WebBrowserContentFilenameHtml := ExtractFilePath(Application.ExeName)+'content2.html';
  WebBrowserContentFilenamePdf := ExtractFilePath(Application.ExeName)+'content.pdf';

  TabSheet2.TabVisible := false;
  TabSheet3.TabVisible := false;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  inv : TInvoice;
  version : TXRechnungVersion;
  error : String;
  od : TOpenDialog;
  xml,xmltest : String;
begin
  inv := TInvoice.Create;
  od := TOpenDialog.Create(nil);
  try
    od.Filter := 'XML-Dateien|*.xml';
    od.FilterIndex := 0;
    od.DefaultExt := '*.xml';
    if not od.Execute then
      exit;
    if not TXRechnungInvoiceAdapter.LoadFromFile(inv, od.FileName,error) then
      memo3.Lines.Text := error
    else
    if (MessageDlg('Eingelesen'+#10+'Soll die Eingabedatei mit der Bibliothek-Ausgabedatei verglichen werden?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      version := TXRechnungValidationHelper.GetXRechnungVersion(od.FileName);
      if version = XRechnungVersion_ReadingSupport_ZUGFeRDFacturX then
        version := XRechnungVersion_30x_UNCEFACT;

      xml := ReadAllText(od.FileName);
      TXRechnungInvoiceAdapter.SaveToXMLStr(inv,version,xmltest);
      if not SameText(xml,xmltest) then
      begin
        WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_original.xml',xml);
        WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_test.xml',xmltest);
        if MessageDlg('Testrechnung unterscheidet sich vom Original.'+#10+'Im Explorer anzeigen?', mtError, [mbYes,mbNo], 0) = mrYes then
          ShellExecute(0,'open','EXPLORER.EXE',PChar('/select,'+ExtractFilePath(Application.ExeName)+'xrechnung_original.xml'),'%SystemRoot%',SW_SHOWNORMAL);
      end;
    end;

    //Lesen von zusaetzlichen ZUGFeRD-Daten, die nicht im XRechnungs-Profil enthalten sind
    //Setzt Compilerschalter $DEFINE ZUGFeRD_Support in intf.XRechnung.pas voraus
    //invAdditionalData enthaelt als Ergebnis die komplette ZUGFeRD-Rechnung fuer eigene
    //Auswertungen
    //var invAdditionalData : TZUGFeRDAdditionalContent := TZUGFeRDAdditionalContent.Create;
    //try
    //  if not TXRechnungInvoiceAdapter.LoadFromFile(inv, od.FileName,error,invAdditionalData) then
    //    memo3.Lines.Text := error
    //  else
    //    ShowMessage('Eingelesen');
    //finally
    //  invAdditionalData.Free;
    //end;
  finally
    od.Free;
    inv.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  od : TOpenDialog;
  cmdoutput,xmlresult,htmlresult : String;
begin
  ClearBrowser;

  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;

    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetValidatorLibPath(ValidatorLibPath)
        .SetValidatorConfigurationPath(ValidatorConfigurationPath)
        .ValidateFile(od.FileName,cmdoutput,xmlresult,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    if htmlresult = '' then
      htmlresult := '<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
    WriteAllText(WebBrowserContentFilename,htmlresult);
    ShowFileInBrowser(WebBrowserContentFilename,1);
  finally
    od.Free;
  end;
end;

procedure TForm1.btCreateInvoiceClick(Sender: TObject);
var
  inv : TInvoice;
begin
  ClearBrowser;
  Memo2.Clear;
  Memo3.Clear;

  inv := TInvoice.Create;
  try
    TInvoiceTestCases.Gesamtbeispiel(inv,rbPaymentTerms.ItemIndex,
                          cbAllowanceCharges.Checked,cbPrepaidAmount.Checked,
                          cbAttachments.Checked,cbDeliveriyInf.Checked);

    Generate(inv);

  finally
    inv.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  od : TOpenDialog;
  cmdoutput,htmlresult : String;
begin
  ClearBrowser;

  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;

    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetValidatorLibPath(ValidatorLibPath)
        .SetVisualizationLibPath(VisualizationLibPath)
        .VisualizeFile(od.FileName, cmdoutput,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    if htmlresult = '' then
      htmlresult := '<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
    WriteAllText(WebBrowserContentFilename,htmlresult);
    ShowFileInBrowser(WebBrowserContentFilename,1);
  finally
    od.Free;
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  od : TOpenDialog;
  cmdoutput : String;
  pdfresult : TMemoryStream;
begin
  ClearBrowser;

  od := TOpenDialog.Create(nil);
  try
    if not od.Execute then
      exit;

    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetValidatorLibPath(ValidatorLibPath)
        .SetVisualizationLibPath(VisualizationLibPath)
        .SetFopLibPath(FopLibPath)
        .VisualizeFileAsPdf(od.FileName, cmdoutput,pdfresult);

    Memo3.Lines.Text := cmdoutput;

    if pdfresult <> nil then
    begin
      pdfresult.SaveToFile(WebBrowserContentFilenamePdf);
      pdfresult.Free;
      ShowFileInBrowser(WebBrowserContentFilenamePdf,3);
    end else
    begin
      WriteAllText(WebBrowserContentFilename,'<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>');
      ShowFileInBrowser(WebBrowserContentFilename,1);
    end;

  finally
    od.Free;
  end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  inv : TInvoice;
begin
  if ListBox1.ItemIndex < 0 then
    exit;

  ClearBrowser;
  Memo2.Clear;
  Memo3.Clear;

  inv := TInvoice.Create;

  try
    case ListBox1.ItemIndex of
      0 : TInvoiceTestCases.Kleinunternehmerregelung(inv);//Kleinunternehmerregelung
      1 : TInvoiceTestCases.Paragr13b(inv);//Paragr13b UStG
      2 : TInvoiceTestCases.Austauschteilesteuer(inv);//Austauschteilesteuer
      3 : TInvoiceTestCases.Differenzbesteuerung(inv);//Differenzbesteuerung
      4 : TInvoiceTestCases.TitelPositionsgruppen(inv);//Titel/Positionsgruppen
      5 : TInvoiceTestCases.Gutschrift(inv);
      6 : TInvoiceTestCases.Rechnungskorrektur(inv);
      7 : TInvoiceTestCases.MinimalbeispielB2BOhneLeitwegID(inv);
      8 : TInvoiceTestCases.PreiseinheitGroesser1(inv);
      9 : TInvoiceTestCases.Lastschrift(inv);
      10: TInvoiceTestCases.InnergemeinschaftlicheLieferungEUohneMehrwertsteuer(inv);
      11: TInvoiceTestCases.PayPalOderAndereOnlinezahlungsdienstleister(inv);
      12: TInvoiceTestCases.Kreditkarte(inv);
    end;

    Generate(inv);

  finally
    inv.Free;
  end;
end;

procedure TForm1.Generate(inv: TInvoice);
var
  xml,xmltest,cmdoutput,xmlresult,htmlresult,error : String;
  invtest : TInvoice;
  version : TXRechnungVersion;
begin
  Screen.Cursor := crHourglass;
  ClearBrowser;
  Memo3.Clear;

  try

  if rbFormat.itemindex = 0 then
    case rbVersion.ItemIndex of
      0 : version := XRechnungVersion_230_UBL_Deprecated;
      else version := XRechnungVersion_30x_UBL;
    end
  else
    case rbVersion.ItemIndex of
      0 : version := XRechnungVersion_230_UNCEFACT_Deprecated;
      else version := XRechnungVersion_30x_UNCEFACT;
    end;

  if not TXRechnungInvoiceAdapter.ConsistencyCheck(inv,version) then
  begin
    MessageDlg('Die Rechnung enthaelt fuer das XRechnung-Format ungueltige Werte', mtError, [mbOK], 0);
    exit;
  end;

  if rbFormat.itemindex = 0 then
  begin
    TXRechnungInvoiceAdapter.SaveToXMLStr(inv,version,xml);

    if cbValidateWithJava.Checked then
    begin
      GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
          .SetValidatorLibPath(ValidatorLibPath)
          .SetValidatorConfigurationPath(ValidatorConfigurationPath)
          .Validate(xml,cmdoutput,xmlresult,htmlresult);

      Memo3.Lines.Text := cmdoutput;

      if htmlresult <> '' then
      begin
        ShowXMLAsHtml(xml);
        ShowXMLAsPdf(xml);
      end else
        htmlresult := '<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
      WriteAllText(WebBrowserContentFilename,htmlresult);
      ShowFileInBrowser(WebBrowserContentFilename,1);
    end;
                                            
    Memo2.Lines.Text := xml;
    Memo2.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'XRechnung-UBL.xml');

    invtest := TInvoice.Create;
    try
      TXRechnungInvoiceAdapter.LoadFromXMLStr(invtest,xml,error);
      if error <> '' then
        MessageDlg('error loading XRechnung'+#10+error, mtError, [mbOK], 0);

      TXRechnungInvoiceAdapter.SaveToXMLStr(invtest,version,xmltest);
      if not SameText(xml,xmltest) then
      begin
        WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_original.xml',xml);
        WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_test.xml',xmltest);
        if MessageDlg('Testrechnung unterscheidet sich vom Original.'+#10+'Im Explorer anzeigen?', mtError, [mbYes,mbNo], 0) = mrYes then
          ShellExecute(0,'open','EXPLORER.EXE',PChar('/select,'+ExtractFilePath(Application.ExeName)+'xrechnung_original.xml'),'%SystemRoot%',SW_SHOWNORMAL);
      end;

    finally
      invtest.Free;
    end;

  end
  else
  begin
    TXRechnungInvoiceAdapter.SaveToXMLStr(inv,version,xml);

    if cbValidateWithJava.Checked then
    begin
      GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
          .SetValidatorLibPath(ValidatorLibPath)
          .SetValidatorConfigurationPath(ValidatorConfigurationPath)
          .Validate(xml,cmdoutput,xmlresult,htmlresult);

      Memo3.Lines.Text := cmdoutput;

      if htmlresult <> '' then
      begin
        ShowXMLAsHtml(xml);
        ShowXMLAsPdf(xml);
      end else
        htmlresult := '<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
      WriteAllText(WebBrowserContentFilename,htmlresult);
      ShowFileInBrowser(WebBrowserContentFilename,1);
    end;

    Memo2.Lines.Text := xml;
    Memo2.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'XRechnung-UNCEFACT.xml');

    invtest := TInvoice.Create;
    try
      TXRechnungInvoiceAdapter.LoadFromXMLStr(invtest,xml,error);
      if error <> '' then
        MessageDlg('error loading XRechnung'+#10+error, mtError, [mbOK], 0);

      TXRechnungInvoiceAdapter.SaveToXMLStr(invtest,version,xmltest);
      if not SameText(xml,xmltest) then
      begin
        WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_original.xml',xml);
        WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_test.xml',xmltest);
        if MessageDlg('Testrechnung unterscheidet sich vom Original.'+#10+'Im Explorer anzeigen?', mtError, [mbYes,mbNo], 0) = mrYes then
          ShellExecute(0,'open','EXPLORER.EXE',PChar('/select,'+ExtractFilePath(Application.ExeName)+'xrechnung_original.xml'),'%SystemRoot%',SW_SHOWNORMAL);
      end;

    finally
      invtest.Free;
    end;
  end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.rbVersionClick(Sender: TObject);
begin
  ValidatorConfigurationPath := DistributionBasePath +'validator-configuration'+ifthen(rbVersion.ItemIndex = 0,'23x','30x')+PathDelim;
  VisualizationLibPath := DistributionBasePath +'visualization'+ifthen(rbVersion.ItemIndex = 0,'23x','30x')+PathDelim;
end;

procedure TForm1.ClearBrowser;
begin
  WebBrowser2.Navigate('about:blank');
end;

procedure TForm1.ShowFileInBrowser(const _Filename: String;
  _BrowserIdx : Integer);
begin
  case _BrowserIdx of
    2 : ShellExecute(0,'open',PChar(_Filename),'','',0);
    3 : ShellExecute(0,'open',PChar(_Filename),'','',0);
    else WebBrowser2.Navigate('file:///'+_Filename);
  end;
end;

procedure TForm1.ShowXMLAsHtml(_Content: String);
var
  cmdoutput,htmlresult : String;
begin
  GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
      .SetValidatorLibPath(ValidatorLibPath)
      .SetVisualizationLibPath(VisualizationLibPath)
      .Visualize(_Content,cmdoutput,htmlresult);

  Memo3.Lines.Append(cmdoutput);

  if htmlresult = '' then
    htmlresult := '<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
  WriteAllText(WebBrowserContentFilenameHtml,htmlresult);
  ShellExecute(0,'open',PChar(WebBrowserContentFilenameHtml),'','',SW_SHOWNORMAL);
end;

procedure TForm1.ShowXMLAsPdf(_Content: String);
var
  cmdoutput : String;
  pdfresult : TMemoryStream;
begin
  GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
      .SetValidatorLibPath(ValidatorLibPath)
      .SetVisualizationLibPath(VisualizationLibPath)
      .SetFopLibPath(FopLibPath)
      .VisualizeAsPdf(_Content,cmdoutput,pdfresult);

  Memo3.Lines.Append(cmdoutput);

  if pdfresult <> nil then
  begin
    pdfresult.SaveToFile(WebBrowserContentFilenamePdf);
    pdfresult.Free;
    ShellExecute(0,'open',PChar(WebBrowserContentFilenamePdf),'','',SW_SHOWNORMAL);
  end;
end;

function TForm1.ReadAllText(_Filename: String): string;
var
  strl : TStringList;
begin
  strl := TStringList.Create;
  try
    strl.LoadFromFile(_Filename);
    Result := strl.Text;
  finally
    strl.Free;
  end;
end;

procedure TForm1.WriteAllText(_Filename, _Value: String);
var
  strl : TStringList;
begin
  strl := TStringList.Create;
  try
    strl.Text := _Value;
    strl.SaveToFile(_Filename);
  finally
    strl.Free;
  end;
end;

end.
