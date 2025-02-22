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

{$DEFINE USE_EDGE_BROWSER}

//https://Validool.org/
{$DEFINE USE_Valitool}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,System.IOUtils,System.Win.COMObj,System.UITypes,System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls,Winapi.ShellApi,Winapi.ShlObj,WinApi.ActiveX
  ,Vcl.OleCtrls, SHDocVw, Vcl.ExtCtrls, Vcl.ComCtrls
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema
  {$IFDEF USE_EDGE_BROWSER}
  ,Winapi.WebView2, Vcl.Edge
  {$ENDIF}
  ,XRechnungUnit2TestCases
  ,intf.XRechnung, intf.Invoice
  ,intf.XRechnungValidationHelperJava
  ;

{$IFDEF USE_Valitool}
  //Datei Valitool.inc mit
  //const Valitool_LICENSE = '...........';
  {$include Valitool.inc}
{$ENDIF}

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
    rbFormatVersion: TRadioGroup;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Button9: TButton;
    ListBox1: TListBox;
    cbValidateWithJava: TCheckBox;
    Button1: TButton;
    cbVisualizeWithJava: TCheckBox;
    TabSheet4: TTabSheet;
    procedure btCreateInvoiceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    WebBrowserContentFilename : String;
    WebBrowserContentFilenameHtml : String;
    WebBrowserContentFilenamePdf : String;
    WebBrowserContentFilenameValitoolPdf : String;
    procedure Generate(inv : TInvoice);
  private
    {$IFNDEF USE_EDGE_BROWSER}
    WebBrowser2: TWebBrowser;
    {$ELSE}
    EdgeBrowser1: TEdgeBrowser;
    EdgeBrowser2: TEdgeBrowser;
    EdgeBrowser3: TEdgeBrowser;
    EdgeBrowser4: TEdgeBrowser;
    {$IFEND}
    procedure ClearBrowser;
    procedure ShowFileInBrowser(const _Filename : String; _BrowserIdx : Integer);
    procedure ShowXMLAsHtml(_Content : String);
    procedure ShowXMLAsPdf(_Content : String);
    procedure ShowXMLAsHtmlMustang(_Filename : String);
    procedure ShowXMLAsPdfMustang(_Filename : String);
  public
    ValidXMLExamplesPath : String;
    DistributionBasePath : String;
    JavaRuntimeEnvironmentPath : String;
    ValidatorLibPath : String;
    ValidatorConfigurationPath : String;
    VisualizationLibPath : String;
    FopLibPath : String;
    MustangLibPath : String;
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
  ValidXMLExamplesPath := ExtractFileDir(DistributionBasePath)+PathDelim+'ValidXMLExamples'+PathDelim;
  DistributionBasePath := ExtractFileDir(DistributionBasePath)+PathDelim+'Distribution'+PathDelim;
  JavaRuntimeEnvironmentPath := DistributionBasePath +'java'+PathDelim;
  ValidatorLibPath := DistributionBasePath +'validator'+PathDelim;
  ValidatorConfigurationPath := DistributionBasePath +'validator-configuration30x'+PathDelim;
  VisualizationLibPath := DistributionBasePath +'visualization30x'+PathDelim;
  FopLibPath := DistributionBasePath + 'apache-fop'+PathDelim;
  MustangLibPath := DistributionBasePath + 'mustangproject'+PathDelim;

  ForceDirectories(ValidXMLExamplesPath);

  Left := 50;
  Top := 50;
  Width := Screen.WorkAreaWidth-100;
  Height := Screen.WorkAreaHeight-100;

  WebBrowserContentFilename := ExtractFilePath(Application.ExeName)+'content.html';
  WebBrowserContentFilenameHtml := ExtractFilePath(Application.ExeName)+'content2.html';
  WebBrowserContentFilenamePdf := ExtractFilePath(Application.ExeName)+'content.pdf';
  WebBrowserContentFilenameValitoolPdf := ExtractFilePath(Application.ExeName)+'content2.pdf';

  {$IFNDEF USE_EDGE_BROWSER}
  WebBrowser2 := TWebBrowser.Create(Self);
  WebBrowser2.SetParentComponent(TabSheet1);
  WebBrowser2.Align := alClient;
  TabSheet2.TabVisible := false;
  TabSheet3.TabVisible := false;
  MessageDlg('Fuer vollen Browsersupport bitte Edge nutzen: USE_EDGE_BROWSER', mtWarning, [mbOK], 0);
  {$ELSE}
  if not FileExists(ExtractFilePath(Application.ExeName)+'WebView2Loader.dll') then
    MessageDlg('WebView2Loader.dll nicht gefunden, Edge-Browser nicht nutzbar!', mtError, [mbOK], 0);
  EdgeBrowser1 := TEdgeBrowser.Create(Self);
  EdgeBrowser1.Name := 'EdgeBrowser1';
  EdgeBrowser1.SetParentComponent(TabSheet1);
  EdgeBrowser1.Align := alClient;
  EdgeBrowser2 := TEdgeBrowser.Create(Self);
  EdgeBrowser2.Name := 'EdgeBrowser2';
  EdgeBrowser2.SetParentComponent(TabSheet2);
  EdgeBrowser2.Align := alClient;
  EdgeBrowser3 := TEdgeBrowser.Create(Self);
  EdgeBrowser3.Name := 'EdgeBrowser3';
  EdgeBrowser3.SetParentComponent(TabSheet3);
  EdgeBrowser3.Align := alClient;
  EdgeBrowser4 := TEdgeBrowser.Create(Self);
  EdgeBrowser4.Name := 'EdgeBrowser4';
  EdgeBrowser4.SetParentComponent(TabSheet4);
  EdgeBrowser4.Align := alClient;
  {$IFEND}
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  inv : TInvoice;
begin
  inv := TInvoice.Create;
  TInvoiceTestCases.Kleinunternehmerregelung(inv);//Kleinunternehmerregelung
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Kleinunternehmerregelung-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Kleinunternehmerregelung-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Kleinunternehmerregelung-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Paragr13b(inv);//Paragr13b UStG
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Paragr13b-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Paragr13b-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Paragr13b-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Austauschteilesteuer(inv);//Austauschteilesteuer
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Austauschteilesteuer-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Austauschteilesteuer-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Austauschteilesteuer-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Differenzbesteuerung(inv);//Differenzbesteuerung
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Differenzbesteuerung-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Differenzbesteuerung-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Differenzbesteuerung-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.TitelPositionsgruppen(inv);//Titel/Positionsgruppen
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'TitelPositionsgruppen-ubl-30x.xml');
  //TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'TitelPositionsgruppen-ciiextended-232.xml');
  //TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'TitelPositionsgruppen-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gutschrift(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Gutschrift-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Gutschrift-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Gutschrift-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Rechnungskorrektur(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Rechnungskorrektur-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Rechnungskorrektur-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Rechnungskorrektur-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.MinimalbeispielB2BOhneLeitwegID(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'MinimalbeispielB2BOhneLeitwegID-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'MinimalbeispielB2BOhneLeitwegID-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'MinimalbeispielB2BOhneLeitwegID-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.PreiseinheitGroesser1(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'PreiseinheitGroesser1-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'PreiseinheitGroesser1-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'PreiseinheitGroesser1-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Lastschrift(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Lastschrift-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Lastschrift-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Lastschrift-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.InnergemeinschaftlicheLieferungEUohneMehrwertsteuer(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'LieferungEUohneMehrwertsteuer-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'LieferungEUohneMehrwertsteuer-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'LieferungEUohneMehrwertsteuer-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.PayPalOderAndereOnlinezahlungsdienstleister(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'PayPal-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'PayPal-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'PayPal-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Kreditkarte(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Kreditkarte-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Kreditkarte-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Kreditkarte-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.LeistungszeitraumJePosition(inv);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'LeistungszeitraumJePosition-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'LeistungszeitraumJePosition-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'LeistungszeitraumJePosition-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,0,false,false,false,false);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Gesamtbeispiel-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Gesamtbeispiel-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Gesamtbeispiel-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,1,false,false,false,false);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Gesamtbeispiel-Nettoziel-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Gesamtbeispiel-Nettoziel-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Gesamtbeispiel-Nettoziel-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,2,false,false,false,false);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Gesamtbeispiel-Skonto1-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Gesamtbeispiel-Skonto1-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Gesamtbeispiel-Skonto1-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,3,false,false,false,false);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Gesamtbeispiel-Skonto2-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Gesamtbeispiel-Skonto2-ciiextended-232.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Gesamtbeispiel-Skonto2-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,0,true,true,true,true);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Gesamtbeispiel-Alles-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Gesamtbeispiel-Alles-ciiextended-232.xml');
  inv.Free;
  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,0,true,true,true,true,true);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Gesamtbeispiel-Alles-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,3,true,true,true,true);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UBL,ValidXMLExamplesPath+'Gesamtbeispiel-Alles-Skonto2-ubl-30x.xml');
  TXRechnungInvoiceAdapter.SaveToFile(inv,ZUGFeRDExtendedVersion_232,ValidXMLExamplesPath+'Gesamtbeispiel-Alles-Skonto2-ciiextended-232.xml');
  inv.Free;
  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,3,true,true,true,true,true);
  TXRechnungInvoiceAdapter.SaveToFile(inv,XRechnungVersion_30x_UNCEFACT,ValidXMLExamplesPath+'Gesamtbeispiel-Alles-Skonto2-cii-30x.xml');
  inv.Free;

  {$IFDEF USE_Valitool}
  GetXRechnungValidationHelperJava
      .SetValitoolPath(DistributionBasePath+'Valitool-3.24.17A-SNAPSHOT\CLI\')
      .SetValitoolLicense(Valitool_LICENSE)
      .ValitoolValidateDirectory(ValidXMLExamplesPath);
  {$ENDIF}
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
        version := XRechnungVersion_30x_UNCEFACT
      else
      if version = XRechnungVersion_Unknown then
      begin
        MessageDlg('Unbekannte ZUGFeRD-Version', mtError, [mbOK], 0);
        exit;
      end;

      xml := TFile.ReadAllText(od.FileName,TEncoding.UTF8);
      TXRechnungInvoiceAdapter.SaveToXMLStr(inv,version,xmltest);
      if not SameStr(xml,xmltest) then
      begin
        TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_original.xml',xml,TEncoding.UTF8);
        TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_test.xml',xmltest,TEncoding.UTF8);
        if MessageDlg('Testrechnung unterscheidet sich vom Original.'+#10+'Im Explorer anzeigen?', mtError, [mbYes,mbNo], 0) = mrYes then
          ShellExecuteW(0,'open','EXPLORER.EXE',PChar('/select,'+ExtractFilePath(Application.ExeName)+'xrechnung_original.xml'),'%SystemRoot%',SW_SHOWNORMAL);
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

    case TXRechnungValidationHelper.GetXRechnungVersion(od.FileName) of
      XRechnungVersion_230_UBL_Deprecated,
      XRechnungVersion_230_UNCEFACT_Deprecated,
      XRechnungVersion_30x_UBL,
      XRechnungVersion_30x_UNCEFACT,
      ZUGFeRDExtendedVersion_232 :
        GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
            .SetValidatorLibPath(ValidatorLibPath)
            .SetValidatorConfigurationPath(DistributionBasePath +'validator-configuration23x'+PathDelim)
            .SetValidatorConfigurationPath(ValidatorConfigurationPath)
            .SetValidatorConfigurationPath(DistributionBasePath+'validator-configuration-zugferd232'+PathDelim)
            .ValidateFile(od.FileName,cmdoutput,xmlresult,htmlresult);
      else
      begin
        MessageDlg('Keine passenden Version gefunden.', mtError, [mbOK], 0);
        exit;
      end;
    end;

    Memo3.Lines.Text := cmdoutput;

    if htmlresult = '' then
      htmlresult := '<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
    TFile.WriteAllText(WebBrowserContentFilename,htmlresult,TEncoding.UTF8);
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
                          cbAllowanceCharges.Checked,
                          cbPrepaidAmount.Checked,
                          cbAttachments.Checked,
                          cbDeliveriyInf.Checked,
                          rbFormatVersion.ItemIndex = 1);

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
        .VisualizeFile(od.FileName,cmdoutput,htmlresult);

    Memo3.Lines.Text := cmdoutput;

    if htmlresult = '' then
      htmlresult := '<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
    TFile.WriteAllText(WebBrowserContentFilename,htmlresult,TEncoding.UTF8);
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
        .VisualizeFileAsPdf(od.FileName,cmdoutput,pdfresult);

    Memo3.Lines.Text := cmdoutput;

    if pdfresult <> nil then
    begin
      pdfresult.SaveToFile(WebBrowserContentFilenamePdf);
      pdfresult.Free;
      ShowFileInBrowser(WebBrowserContentFilenamePdf,3);
    end else
    begin
      TFile.WriteAllText(WebBrowserContentFilename,'<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>',TEncoding.UTF8);
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
      13: TInvoiceTestCases.LeistungszeitraumJePosition(inv);
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
  {$IFDEF USE_Valitool}
  pdfresult : TMemoryStream;
  {$ENDIF}
begin
  Screen.Cursor := crHourglass;
  ClearBrowser;
  Memo3.Clear;

  try

    case rbFormatVersion.ItemIndex of
      0 : version := XRechnungVersion_30x_UBL;
      2 : version := ZUGFeRDExtendedVersion_232;
      else version := XRechnungVersion_30x_UNCEFACT;
    end;

    if not TXRechnungInvoiceAdapter.ConsistencyCheck(inv,version) then
    begin
      MessageDlg('Die Rechnung enthaelt fuer das XRechnung-Format ungueltige Werte', mtError, [mbOK], 0);
      exit;
    end;

    case version of
    XRechnungVersion_30x_UBL :
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
          if cbVisualizeWithJava.Checked then
          begin
            ShowXMLAsHtml(xml);
            ShowXMLAsPdf(xml);
          end;
        end else
          htmlresult := '<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
        TFile.WriteAllText(WebBrowserContentFilename,htmlresult,TEncoding.UTF8);
        ShowFileInBrowser(WebBrowserContentFilename,1);
      end;

      Memo2.Lines.Text := xml;
      Memo2.Lines.WriteBOM := false;
      Memo2.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'XRechnung-UBL.xml',TEncoding.UTF8);

      invtest := TInvoice.Create;
      try
        TXRechnungInvoiceAdapter.LoadFromXMLStr(invtest,xml,error);
        if error <> '' then
          MessageDlg('error loading XRechnung'+#10+error, mtError, [mbOK], 0);

        TXRechnungInvoiceAdapter.SaveToXMLStr(invtest,version,xmltest);
        if not SameStr(xml,xmltest) then
        begin
          TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_original.xml',xml,TEncoding.UTF8);
          TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_test.xml',xmltest,TEncoding.UTF8);
          if MessageDlg('Testrechnung unterscheidet sich vom Original.'+#10+'Im Explorer anzeigen?', mtError, [mbYes,mbNo], 0) = mrYes then
            ShellExecuteW(0,'open','EXPLORER.EXE',PChar('/select,'+ExtractFilePath(Application.ExeName)+'xrechnung_original.xml'),'%SystemRoot%',SW_SHOWNORMAL);
        end;

      finally
        invtest.Free;
      end;

    end;
    XRechnungVersion_30x_UNCEFACT:
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
          if cbVisualizeWithJava.Checked then
          begin
            ShowXMLAsHtml(xml);
            ShowXMLAsPdf(xml);
          end;
        end else
          htmlresult := '<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
        TFile.WriteAllText(WebBrowserContentFilename,htmlresult,TEncoding.UTF8);
        ShowFileInBrowser(WebBrowserContentFilename,1);
      end;

      Memo2.Lines.Text := xml;
      Memo2.Lines.WriteBOM := false;
      Memo2.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'XRechnung-UNCEFACT.xml',TEncoding.UTF8);

      invtest := TInvoice.Create;
      try
        TXRechnungInvoiceAdapter.LoadFromXMLStr(invtest,xml,error);
        if error <> '' then
          MessageDlg('error loading XRechnung'+#10+error, mtError, [mbOK], 0);

        TXRechnungInvoiceAdapter.SaveToXMLStr(invtest,version,xmltest);
        if not SameStr(xml,xmltest) then
        begin
          TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_original.xml',xml,TEncoding.UTF8);
          TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'xrechnung_test.xml',xmltest,TEncoding.UTF8);
          if MessageDlg('Testrechnung unterscheidet sich vom Original.'+#10+'Im Explorer anzeigen?', mtError, [mbYes,mbNo], 0) = mrYes then
            ShellExecuteW(0,'open','EXPLORER.EXE',PChar('/select,'+ExtractFilePath(Application.ExeName)+'xrechnung_original.xml'),'%SystemRoot%',SW_SHOWNORMAL);
        end;

      finally
        invtest.Free;
      end;
    end;
    ZUGFeRDExtendedVersion_232 :
    begin
      TXRechnungInvoiceAdapter.SaveToXMLStr(inv,version,xml);

      Memo2.Lines.Text := xml;
      Memo2.Lines.WriteBOM := false;
      Memo2.Lines.SaveToFile(ExtractFilePath(Application.ExeName)+'ZUGFeRD-Extended.xml',TEncoding.UTF8);

      if cbValidateWithJava.Checked then
      begin
        GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
            .SetValidatorLibPath(ValidatorLibPath)
            .SetValidatorConfigurationPath(DistributionBasePath+'validator-configuration-zugferd232'+PathDelim)
            .ValidateFile(ExtractFilePath(Application.ExeName)+'ZUGFeRD-Extended.xml',cmdoutput,xmlresult,htmlresult);

//        GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
//            .SetMustangprojectLibPath(MustangLibPath)
//            .MustangValidateFile(ExtractFilePath(Application.ExeName)+'ZUGFeRD-Extended.xml',cmdoutput,xmlresult);

        Memo3.Lines.Text := cmdoutput;

        if htmlresult <> '' then
        begin
          if cbVisualizeWithJava.Checked then
          begin
            ShowXMLAsHtmlMustang(ExtractFilePath(Application.ExeName)+'ZUGFeRD-Extended.xml');
            ShowXMLAsPdfMustang(ExtractFilePath(Application.ExeName)+'ZUGFeRD-Extended.xml');
          end;
        end else
          htmlresult := '<html><body>Validation nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
        TFile.WriteAllText(WebBrowserContentFilename,htmlresult,TEncoding.UTF8);
        ShowFileInBrowser(WebBrowserContentFilename,1);
      end;

      invtest := TInvoice.Create;
      try
        TXRechnungInvoiceAdapter.LoadFromXMLStr(invtest,xml,error);
        if error <> '' then
          MessageDlg('error loading ZUGFeRD'+#10+error, mtError, [mbOK], 0);

        TXRechnungInvoiceAdapter.SaveToXMLStr(invtest,version,xmltest);
        if not SameStr(xml,xmltest) then
        begin
          TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'zugferd_original.xml',xml,TEncoding.UTF8);
          TFile.WriteAllText(ExtractFilePath(Application.ExeName)+'zugferd_test.xml',xmltest,TEncoding.UTF8);
          if MessageDlg('Testrechnung unterscheidet sich vom Original.'+#10+'Im Explorer anzeigen?', mtError, [mbYes,mbNo], 0) = mrYes then
            ShellExecuteW(0,'open','EXPLORER.EXE',PChar('/select,'+ExtractFilePath(Application.ExeName)+'zugferd_original.xml'),'%SystemRoot%',SW_SHOWNORMAL);
        end;
      finally
        invtest.Free;
      end;
    end;
    end;

    {$IFDEF USE_Valitool}
    if cbValidateWithJava.Checked then
    begin
      GetXRechnungValidationHelperJava
          .SetValitoolPath(DistributionBasePath+'Valitool-3.24.17A-SNAPSHOT\CLI\')
          .SetValitoolLicense(Valitool_LICENSE)
          .ValitoolValidate(xml,cmdoutput,xmlresult,pdfresult);

      Memo3.Lines.Text := cmdoutput;

      if pdfresult <> nil then
      begin
        pdfresult.SaveToFile(WebBrowserContentFilenameValitoolPdf);
        pdfresult.Free;
        ShowFileInBrowser(WebBrowserContentFilenameValitoolPdf,4);
      end;
    end;
    {$ENDIF}
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.ClearBrowser;
begin
  {$IFNDEF USE_EDGE_BROWSER}
  WebBrowser2.Navigate2('about:blank');
  {$ELSE}
  EdgeBrowser1.Navigate('about:blank');
  EdgeBrowser2.Navigate('about:blank');
  EdgeBrowser3.Navigate('about:blank');
  EdgeBrowser4.Navigate('about:blank');
  {$IFEND}
end;

procedure TForm1.ShowFileInBrowser(const _Filename: String;
  _BrowserIdx : Integer);
begin
  {$IFNDEF USE_EDGE_BROWSER}
  case _BrowserIdx of
    2 : ShellExecute(0,'open',PChar(_Filename),'','',0);
    3 : ShellExecute(0,'open',PChar(_Filename),'','',0);
    4 : ShellExecute(0,'open',PChar(_Filename),'','',0);
    else WebBrowser2.Navigate2('file:///'+_Filename);
  end;
  {$ELSE}
  case _BrowserIdx of
    2 : EdgeBrowser2.Navigate('file:///'+_Filename);
    3 : EdgeBrowser3.Navigate('file:///'+_Filename);
    4 : EdgeBrowser4.Navigate('file:///'+_Filename);
    else EdgeBrowser1.Navigate('file:///'+_Filename);
  end;
  {$IFEND}
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
  TFile.WriteAllText(WebBrowserContentFilenameHtml,htmlresult,TEncoding.UTF8);

{$IFDEF USE_EDGE_BROWSER}
  EdgeBrowser2.Navigate('file:///'+WebBrowserContentFilenameHtml);
{$ELSE}
  ShellExecute(0,'open',PChar(WebBrowserContentFilenameHtml),'','',SW_SHOWNORMAL);
{$IFEND}
end;

procedure TForm1.ShowXMLAsHtmlMustang(_Filename: String);
var
  cmdoutput,htmlresult : String;
begin
  GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
      .SetValidatorLibPath(ValidatorLibPath)
      .SetMustangprojectLibPath(MustangLibPath)
      .MustangVisualizeFile(_Filename,cmdoutput,htmlresult);

  Memo3.Lines.Append(cmdoutput);

  if htmlresult = '' then
    htmlresult := '<html><body>Visualisierung nicht erfolgreich. Siehe Verzeichnis ./Distribution/Read.Me</body></html>';
  TFile.WriteAllText(WebBrowserContentFilenameHtml,htmlresult,TEncoding.UTF8);

{$IFDEF USE_EDGE_BROWSER}
  EdgeBrowser2.Navigate('file:///'+WebBrowserContentFilenameHtml);
{$ELSE}
  ShellExecute(0,'open',PChar(WebBrowserContentFilenameHtml),'','',SW_SHOWNORMAL);
{$IFEND}
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
{$IFDEF USE_EDGE_BROWSER}
    EdgeBrowser3.Navigate('file:///'+WebBrowserContentFilenamePdf);
{$ELSE}
    ShellExecute(0,'open',PChar(WebBrowserContentFilenamePdf),'','',SW_SHOWNORMAL);
{$IFEND}
  end else
  begin
{$IFDEF USE_EDGE_BROWSER}
    EdgeBrowser3.Navigate('about:blank');
{$IFEND}
  end;
end;

procedure TForm1.ShowXMLAsPdfMustang(_Filename: String);
var
  cmdoutput : String;
  pdfresult : TMemoryStream;
begin
  GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
      .SetMustangprojectLibPath(MustangLibPath)
      .MustangVisualizeFileAsPdf(_Filename,cmdoutput,pdfresult);

  Memo3.Lines.Append(cmdoutput);

  if pdfresult <> nil then
  begin
    pdfresult.SaveToFile(WebBrowserContentFilenamePdf);
    pdfresult.Free;

    GetXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(JavaRuntimeEnvironmentPath)
        .SetMustangprojectLibPath(MustangLibPath)
        .MustangCombinePdfAndXML(WebBrowserContentFilenamePdf,_Filename,cmdoutput,pdfresult);

    Memo3.Lines.Append(cmdoutput);

    if pdfresult <> nil then
    begin
      pdfresult.SaveToFile(WebBrowserContentFilenamePdf);
      pdfresult.Free;
    end;

{$IFDEF USE_EDGE_BROWSER}
    EdgeBrowser3.Navigate('file:///'+WebBrowserContentFilenamePdf);
{$ELSE}
    ShellExecute(0,'open',PChar(WebBrowserContentFilenamePdf),'','',SW_SHOWNORMAL);
{$IFEND}
  end else
  begin
{$IFDEF USE_EDGE_BROWSER}
    EdgeBrowser3.Navigate('about:blank');
{$IFEND}
  end;
end;

end.
