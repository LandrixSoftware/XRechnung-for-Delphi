{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de

Portabler Generator fuer die gueltigen XML-Beispiele. Bildet die Sequenz aus
Samples\XRechnungUnit1.pas (TForm1.Button1Click) 1:1 nach, schreibt aber in ein
frei waehlbares Zielverzeichnis. Wird vom FreePascal-Paritaetstest genutzt und
ist zugleich unter Delphi compilierbar.
}

unit XRechnungGenerateExamples;

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
  {$codepage utf8}
{$ENDIF}

interface

// _OutPath muss mit Pfadtrenner enden. _AttachmentBasePath ist das Verzeichnis,
// in dem attachment.pdf/.png/.jpg/.csv liegen (Repo-Verzeichnis Samples\).
procedure GenerateAllExamples(const _OutPath, _AttachmentBasePath: String);

implementation

uses
  {$IFDEF FPC}SysUtils{$ELSE}System.SysUtils{$ENDIF}
  ,intf.Invoice
  ,intf.XRechnung
  ,XRechnungUnit2TestCases;

procedure GenerateAllExamples(const _OutPath, _AttachmentBasePath: String);
var
  inv : TInvoice;

  procedure Sv(_Version : TXRechnungVersion; const _Name : String);
  begin
    TXRechnungInvoiceAdapter.SaveToFile(inv,_Version,_OutPath+_Name);
  end;

  procedure EA;
  begin
    inv.AccountingSupplierParty.ElectronicAddressSellerBuyer := '9482348239847239874';
    inv.AccountingSupplierParty.ElectronicAddressSellerBuyerSchemeID := '0088';
    inv.AccountingCustomerParty.ElectronicAddressSellerBuyer := 'FR23342';
    inv.AccountingCustomerParty.ElectronicAddressSellerBuyerSchemeID := '0002';
  end;

begin
  TInvoiceTestCases.AttachmentBasePath := _AttachmentBasePath;

  inv := TInvoice.Create;
  TInvoiceTestCases.Kleinunternehmerregelung(inv);
  Sv(XRechnungVersion_30x_UBL,'Kleinunternehmerregelung-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Kleinunternehmerregelung-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Kleinunternehmerregelung-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Kleinunternehmerregelung-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Kleinunternehmerregelung-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Paragr13b(inv);
  Sv(XRechnungVersion_30x_UBL,'Paragr13b-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Paragr13b-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Paragr13b-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Paragr13b-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Paragr13b-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Austauschteilesteuer(inv);
  Sv(XRechnungVersion_30x_UBL,'Austauschteilesteuer-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Austauschteilesteuer-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Austauschteilesteuer-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Austauschteilesteuer-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Austauschteilesteuer-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Differenzbesteuerung(inv);
  Sv(XRechnungVersion_30x_UBL,'Differenzbesteuerung-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Differenzbesteuerung-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Differenzbesteuerung-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Differenzbesteuerung-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Differenzbesteuerung-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.TitelPositionsgruppen(inv);
  Sv(XRechnungVersion_30x_UBL,'TitelPositionsgruppen-ubl-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'TitelPositionsgruppen-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gutschrift(inv);
  Sv(XRechnungVersion_30x_UBL,'Gutschrift-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Gutschrift-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Gutschrift-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Gutschrift-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Gutschrift-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Rechnungskorrektur(inv);
  Sv(XRechnungVersion_30x_UBL,'Rechnungskorrektur-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Rechnungskorrektur-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Rechnungskorrektur-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Rechnungskorrektur-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Rechnungskorrektur-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.MinimalbeispielB2BOhneLeitwegID(inv);
  Sv(XRechnungVersion_30x_UBL,'MinimalbeispielB2BOhneLeitwegID-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'MinimalbeispielB2BOhneLeitwegID-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'MinimalbeispielB2BOhneLeitwegID-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'MinimalbeispielB2BOhneLeitwegID-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'MinimalbeispielB2BOhneLeitwegID-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.PreiseinheitGroesser1(inv);
  Sv(XRechnungVersion_30x_UBL,'PreiseinheitGroesser1-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'PreiseinheitGroesser1-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'PreiseinheitGroesser1-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'PreiseinheitGroesser1-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'PreiseinheitGroesser1-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Lastschrift(inv);
  Sv(XRechnungVersion_30x_UBL,'Lastschrift-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Lastschrift-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Lastschrift-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Lastschrift-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Lastschrift-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.InnergemeinschaftlicheLieferungEUohneMehrwertsteuer(inv);
  Sv(XRechnungVersion_30x_UBL,'LieferungEUohneMehrwertsteuer-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'LieferungEUohneMehrwertsteuer-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'LieferungEUohneMehrwertsteuer-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'LieferungEUohneMehrwertsteuer-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'LieferungEUohneMehrwertsteuer-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.PayPalOderAndereOnlinezahlungsdienstleister(inv);
  Sv(XRechnungVersion_30x_UBL,'PayPal-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'PayPal-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'PayPal-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'PayPal-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'PayPal-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Kreditkarte(inv);
  Sv(XRechnungVersion_30x_UBL,'Kreditkarte-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Kreditkarte-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Kreditkarte-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Kreditkarte-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Kreditkarte-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.LeistungszeitraumJePosition(inv);
  Sv(XRechnungVersion_30x_UBL,'LeistungszeitraumJePosition-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'LeistungszeitraumJePosition-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'LeistungszeitraumJePosition-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'LeistungszeitraumJePosition-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'LeistungszeitraumJePosition-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,0,false,false,false,false);
  Sv(XRechnungVersion_30x_UBL,'Gesamtbeispiel-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Gesamtbeispiel-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Gesamtbeispiel-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Gesamtbeispiel-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Gesamtbeispiel-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,1,false,false,false,false);
  Sv(XRechnungVersion_30x_UBL,'Gesamtbeispiel-Nettoziel-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Gesamtbeispiel-Nettoziel-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Gesamtbeispiel-Nettoziel-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Gesamtbeispiel-Nettoziel-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Gesamtbeispiel-Nettoziel-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,2,false,false,false,false);
  Sv(XRechnungVersion_30x_UBL,'Gesamtbeispiel-Skonto1-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Gesamtbeispiel-Skonto1-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Gesamtbeispiel-Skonto1-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Gesamtbeispiel-Skonto1-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Gesamtbeispiel-Skonto1-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,3,false,false,false,false);
  Sv(XRechnungVersion_30x_UBL,'Gesamtbeispiel-Skonto2-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Gesamtbeispiel-Skonto2-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Gesamtbeispiel-Skonto2-ciiextended-25.xml');
  Sv(XRechnungVersion_30x_UNCEFACT,'Gesamtbeispiel-Skonto2-cii-30x.xml');
  EA; Sv(PeppolBillingVersion_30,'Gesamtbeispiel-Skonto2-ubl-peppol.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,0,true,true,true,true);
  Sv(XRechnungVersion_30x_UBL,'Gesamtbeispiel-Alles-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Gesamtbeispiel-Alles-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Gesamtbeispiel-Alles-ciiextended-25.xml');
  EA; Sv(PeppolBillingVersion_30,'Gesamtbeispiel-Alles-ubl-peppol.xml');
  inv.Free;
  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,0,true,true,true,true,true);
  Sv(XRechnungVersion_30x_UNCEFACT,'Gesamtbeispiel-Alles-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,3,true,true,true,true);
  Sv(XRechnungVersion_30x_UBL,'Gesamtbeispiel-Alles-Skonto2-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Gesamtbeispiel-Alles-Skonto2-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Gesamtbeispiel-Alles-Skonto2-ciiextended-25.xml');
  EA; Sv(PeppolBillingVersion_30,'Gesamtbeispiel-Alles-Skonto2-ubl-peppol.xml');
  inv.Free;
  inv := TInvoice.Create;
  TInvoiceTestCases.Gesamtbeispiel(inv,3,true,true,true,true,true);
  Sv(XRechnungVersion_30x_UNCEFACT,'Gesamtbeispiel-Alles-Skonto2-cii-30x.xml');
  inv.Free;

  inv := TInvoice.Create;
  TInvoiceTestCases.VierNachkommastellen(inv);
  Sv(XRechnungVersion_30x_UBL,'Vier-Nachkommastellen-ubl-30x.xml');
  Sv(ZUGFeRDEN16931Version_250,'Vier-Nachkommastellen-ciiEN16931-25.xml');
  Sv(ZUGFeRDExtendedVersion_250,'Vier-Nachkommastellen-ciiextended-25.xml');
  EA; Sv(PeppolBillingVersion_30,'Vier-Nachkommastellen-ubl-peppol.xml');
  inv.Free;
  inv := TInvoice.Create;
  TInvoiceTestCases.VierNachkommastellen(inv);
  Sv(XRechnungVersion_30x_UNCEFACT,'Vier-Nachkommastellen-cii-30x.xml');
  inv.Free;
end;

end.
