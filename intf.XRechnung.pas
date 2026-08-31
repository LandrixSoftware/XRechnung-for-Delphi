{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}

unit intf.XRechnung;

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
  {$codepage utf8}
{$ENDIF}

interface

//setzt ZUGFeRD-for-Delphi voraus
//https://github.com/LandrixSoftware/ZUGFeRD-for-Delphi
{.$DEFINE ZUGFeRD_Support}

uses
  {$IFDEF FPC}
  SysUtils,Classes,Types,Math
  ,StrUtils,DateUtils,Contnrs
  ,intf.XRechnungXmlShim
  {$ELSE}
  System.SysUtils,System.Classes,System.Types,System.Math
  ,System.StrUtils,System.DateUtils,System.Contnrs
  ,Xml.XMLDoc,Xml.XMLIntf
  {$ENDIF}
  ,intf.XRechnungPdfExtract
  {$IFDEF ZUGFeRD_Support}
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDTradeAllowanceCharge
  ,intf.ZUGFeRDAllowanceReasonCodes
  ,intf.ZUGFeRDChargeReasonCodes
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDSubjectCodes
  ,intf.ZUGFeRDDesignatedProductClassificationClassCodes
  ,intf.ZUGFeRDHelper
  ,intf.ZUGFeRDParty
  ,intf.ZUGFeRDContact
  ,intf.ZUGFeRDTaxRegistration
  ,intf.ZUGFeRDElectronicAddress
  ,intf.ZUGFeRDElectronicAddressSchemeIdentifiers
  ,intf.ZUGFeRDAdditionalReferencedDocumentTypeCodes
  ,intf.ZUGFeRDTradeLineItem
  ,System.Generics.Collections
  {$ENDIF}
  ,intf.XRechnung_3_0
  ,intf.Invoice
  ;

type
  TXRechnungHelper = class(TObject)
  public
    class function DateFromStrUBLFormat(const _Val : String) : TDateTime;
    class function DateFromStrUNCEFACTFormat(const _Val : String) : TDateTime;
    class function DateToStrUBLFormat(const _Val : TDateTime) : String;
    class function DateToStrUNCEFACTFormat(const _Val : TDateTime) : String;
    class function AmountToStr(_Val : Currency) : String;
    class function AmountFromStr(_Val : String) : Currency;
    class function UnitPriceAmountToStr(_Val : Currency) : String;
    class function UnitPriceAmountFromStr(_Val : String) : Currency;
    class function FloatToStr(_Val : double; _DecimalPlaces : Integer = 2) : String;
    class function FloatFromStr(_Val : String) : double;
    class function PercentageToStr(_Val : double) : String;
    class function PercentageFromStr(_Val : String) : double;
    class function QuantityToStr(_Val : double) : String;
    class function QuantityFromStr(_Val : String) : double;
    class function InvoiceTypeCodeToStr(_Val : TInvoiceTypeCode) : String;
    class function InvoiceTypeCodeFromStr(const _Val : String) : TInvoiceTypeCode;
    class function InvoicePaymentMeansCodeToStr(_Val : TInvoicePaymentMeansCode) : String;
    class function InvoicePaymentMeansCodeFromStr(_Val : String) : TInvoicePaymentMeansCode;
    class function InvoiceUnitCodeToStr(_Val : TInvoiceUnitCode) : String;   //mehr Konvertierungen in Res\intf.XRechnung.unusedUnits.pas
    class function InvoiceUnitCodeFromStr(_Val : String) : TInvoiceUnitCode;   //mehr Konvertierungen in Res\intf.XRechnung.unusedUnits.pas
    class function InvoiceAllowanceOrChargeIdentCodeToStr(_Val : TInvoiceAllowanceOrChargeIdentCode) : String;
    class function InvoiceAllowanceOrChargeIdentCodeFromStr(_Val : String) : TInvoiceAllowanceOrChargeIdentCode;
    class function InvoiceSpecialServiceDescriptionCodeToStr(_Val : TInvoiceSpecialServiceDescriptionCode) : String;
    class function InvoiceSpecialServiceDescriptionCodeFromStr(_Val : String) : TInvoiceSpecialServiceDescriptionCode;
    class function InvoiceDutyTaxFeeCategoryCodeToStr(_Val : TInvoiceDutyTaxFeeCategoryCode) : String;
    class function InvoiceDutyTaxFeeCategoryCodeFromStr(_Val : String) : TInvoiceDutyTaxFeeCategoryCode;
    class function InvoiceAttachmentTypeToStr(_Val : TInvoiceAttachmentType) : String;
    class function InvoiceAttachmentTypeFromStr(_Val : String) : TInvoiceAttachmentType;
    class function InvoiceAttachmentTypeCodeToStr(_Val : TInvoiceAttachmentTypeCode) : String;
    class function InvoiceAttachmentTypeCodeFromStr(_Val : String) : TInvoiceAttachmentTypeCode;
    class function InvoiceNoteSubjectCodeToStr(_Val : TInvoiceNoteSubjectCode) : String;
    class function InvoiceNoteSubjectCodeFromStr(_Val : String) : TInvoiceNoteSubjectCode;
    class procedure ReadPaymentTerms(_Invoice: TInvoice; _PaymentTermsText: String);
  end;

  TXRechnungVersion = (XRechnungVersion_Unknown,
                       XRechnungVersion_30x_UBL,
                       XRechnungVersion_30x_UNCEFACT,
                       ZUGFeRDEN16931Version_250,
                       ZUGFeRDExtendedVersion_250,
                       PeppolBillingVersion_30,
                       ZUGFeRDExtendedVersion_1_NotSupported,
                       //aeltere XRechnung-Versionen und fremde EN16931-CIUS in UBL, z.B. xrechnung_1.2 oder 2.3.
                       //Schreiben ist seit 01.02.2024 nur noch als 3.0.x zulaessig, gelesen werden koennen sie
                       //weiterhin, die Feldstruktur ist identisch
                       XRechnungVersion_2x_ReadingOnly);

  TXRechnungValidationHelper = class(TObject)
  public
    class function GetXRechnungVersion(const _Filename : String) : TXRechnungVersion; overload;
    class function GetXRechnungVersion(_Xml : IXMLDocument) : TXRechnungVersion; overload;
    class function GetXRechnungVersion(_Stream: TStream) : TXRechnungVersion; overload;
    class function GetXRechnungVersionFromString(const _XML: String) : TXRechnungVersion;

    //First thoughts on the topic
    //class function Validate(_XSDFilename, _XmlFilename: String) : Boolean;
  end;

  {$IFDEF ZUGFeRD_Support}
  TZUGFeRDAdditionalContent = class
  public
    ZUGFeRDInvoice : TZUGFeRDInvoiceDescriptor;

    InvoiceeTradePartyFound : Boolean;
    InvoiceeTradeParty : TInvoiceAccountingParty;

    SpecifiedLogisticsServiceChargeFound : Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;
  {$ENDIF}

  TXRechnungInvoiceAdapter = class
  private
    class procedure SaveDocument(_Invoice: TInvoice;_Version : TXRechnungVersion; _Xml : IXMLDocument);
    class function  LoadFromXMLDocument(_Invoice: TInvoice; _XmlDocument: IXMLDocument; out _Error : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
  public const
    ccOK                       = 0;
    ccNoPaymentsCount          = 1;
    ccInvoiceTypeNotSupported  = 2;
    ccTooManyPrecedingInvoices = 3;
    ccTooManySEPADirectDebit   = 4;
    ccTooManyCreditCard        = 5;
    ccNoBT31BT30               = 6;
    ccNoBT31BT32               = 7;
    ccPrepaidPaymentNotSupported = 8;
    ccNoEMUnderPeppol          = 9;
  public
    class function ConsistencyCheck(_Invoice : TInvoice; _Version : TXRechnungVersion) : Boolean; overload;
    class function ConsistencyCheck(_Invoice : TInvoice; _Version : TXRechnungVersion; out _ErrorCode : Integer) : Boolean; overload;
    class procedure CorrectDueDateIfNotDefined(_Invoice : TInvoice);

    class procedure SaveToStream(_Invoice : TInvoice; _Version : TXRechnungVersion; _Stream : TStream);
    class procedure SaveToFile(_Invoice : TInvoice; _Version : TXRechnungVersion; const _Filename : String);
    class procedure SaveToXMLStr(_Invoice : TInvoice; _Version : TXRechnungVersion; out _XML : String);

    //LoadFromStream und LoadFromFile erkennen ein PDF an seiner Kennung und holen
    //die eingebettete Rechnung selbst heraus - ohne externe Werkzeuge. Fuer den
    //umgekehrten Fall (alle Anhaenge eines PDFs) siehe intf.XRechnungPdfExtract.
    class function  LoadFromStream(_Invoice : TInvoice; _Stream : TStream; out _Error : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
    class function  LoadFromFile(_Invoice : TInvoice; const _Filename : String; out _Error : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
    class function  LoadFromXMLStr(_Invoice : TInvoice; const _XML : String; out _Error : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;

    //Liest die Rechnung ausdruecklich aus einem ZUGFeRD-/Factur-X-PDF. Liefert
    //zusaetzlich den Namen des eingebetteten Anhangs (factur-x.xml, xrechnung.xml, ...).
    class function  LoadFromPdfStream(_Invoice : TInvoice; _Stream : TStream; out _Error : String; out _AttachmentName : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
    class function  LoadFromPdfFile(_Invoice : TInvoice; const _Filename : String; out _Error : String; out _AttachmentName : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
  end;

const
  ZUGFERD_INVOICE_PDF_FILENAME_FACTURX =
    'factur-x.xml';

  ZUGFERD_INVOICE_PDF_XMP_INFO =
    '<rdf:Description xmlns:fx="urn:factur-x:pdfa:CrossIndustryDocument:invoice:1p0#"' + #32 + 'rdf:about="">' + #10 +
    '<fx:DocumentType>INVOICE</fx:DocumentType>' + #10 +
    '<fx:DocumentFileName>factur-x.xml</fx:DocumentFileName>' + #10 +
    '<fx:Version>1.0</fx:Version>' + #10 +
    '<fx:ConformanceLevel>EXTENDED</fx:ConformanceLevel></rdf:Description>';

  ZUGFERD_INVOICE_PDF_SCHEMA =
    '<rdf:li rdf:parseType="Resource">' + #10 +
      '<pdfaSchema:schema>Factur-X PDFA Extension Schema</pdfaSchema:schema>' + #10 +
      '<pdfaSchema:namespaceURI>urn:factur-x:pdfa:CrossIndustryDocument:invoice:1p0#</pdfaSchema:namespaceURI>' + #10 +
      '<pdfaSchema:prefix>zf</pdfaSchema:prefix>' + #10 +
      '<pdfaSchema:property>' + #10 +
         '<rdf:Seq>' + #10 +
           '<rdf:li rdf:parseType="Resource">' + #10 +
              '<pdfaProperty:name>DocumentFileName</pdfaProperty:name>' + #10 +
              '<pdfaProperty:valueType>Text</pdfaProperty:valueType>' + #10 +
              '<pdfaProperty:category>external</pdfaProperty:category>' + #10 +
              '<pdfaProperty:description>name of the embedded XML invoice file</pdfaProperty:description>' + #10 +
           '</rdf:li>' + #10 +
           '<rdf:li rdf:parseType="Resource">' + #10 +
              '<pdfaProperty:name>DocumentType</pdfaProperty:name> ' + #10 +
              '<pdfaProperty:valueType>Text</pdfaProperty:valueType>' + #10 +
              '<pdfaProperty:category>external</pdfaProperty:category>' + #10 +
              '<pdfaProperty:description>INVOICE</pdfaProperty:description>' + #10 +
           '</rdf:li> ' + #10 +
           '<rdf:li rdf:parseType="Resource"> ' + #10 +
              '<pdfaProperty:name>Version</pdfaProperty:name>' + #10 +
              '<pdfaProperty:valueType>Text</pdfaProperty:valueType>' + #10 +
              '<pdfaProperty:category>external</pdfaProperty:category>' + #10 +
              '<pdfaProperty:description>The actual version of the ZUGFeRD data</pdfaProperty:description>' + #10 +
           '</rdf:li>' + #10 +
           '<rdf:li rdf:parseType="Resource">' + #10 +
              '<pdfaProperty:name>ConformanceLevel</pdfaProperty:name>' + #10 +
              '<pdfaProperty:valueType>Text</pdfaProperty:valueType>' + #10 +
              '<pdfaProperty:category>external</pdfaProperty:category>' + #10 +
              '<pdfaProperty:description>The conformance level of the ZUGFeRD data</pdfaProperty:description>' + #10 +
           '</rdf:li>' + #10 +
        '</rdf:Seq> ' + #10 +
     '</pdfaSchema:property>' + #10 +
  '</rdf:li>';

implementation

uses intf.XRechnungHelper;

{$IFDEF ZUGFeRD_Support}
type
  TZUGFeRDInvoiceAdapter = class
  private
    //Codewert eines Enums der ZUGFeRD-Bibliothek. Nicht gesetzte Werte und Codes, die die
    //Bibliothek nicht kennt (Unknown), ergeben einen Leerstring - so laesst sich eine
    //fehlende Angabe nicht mehr mit dem ersten Enum-Wert verwechseln.
    class function CodeFromEnum<TEnum>(const _Value : ZUGFeRDNullable<TEnum>) : String;
    class function UnitCodeFrom(const _Value : ZUGFeRDNullable<TZUGFeRDQuantityCodes>) : TInvoiceUnitCode;
    class function TaxCategoryFrom(const _Value : ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes>) : TInvoiceDutyTaxFeeCategoryCode;
    class procedure TransferAddress(_Target : TInvoiceAddress; _Source : TZUGFeRDParty);
    class procedure TransferParty(_Target : TInvoiceAccountingParty; _Source : TZUGFeRDParty;
      _Contact : TZUGFeRDContact; _TaxRegistrations : TObjectList<TZUGFeRDTaxRegistration>;
      _ElectronicAddress : TZUGFeRDElectronicAddress);
    class procedure TransferAllowanceCharge(_Target : TInvoiceAllowanceCharge;
      _Source : TZUGFeRDAbstractTradeAllowanceCharge);
    class procedure TransferPaymentTerms(_Invoice : TInvoice; _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor);
    class procedure TransferPaymentMeans(_Invoice : TInvoice; _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor);
    class procedure TransferInvoiceLine(_InvoiceLine : TInvoiceLine; _TradeLineItem : TZUGFeRDTradeLineItem);
    class function LoadFromInvoiceDescriptor(_Invoice: TInvoice; _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor; out _Error : String) : Boolean;
    class function LoadAdditionalContentFromXMLDocument(_AdditionalContent : TZUGFeRDAdditionalContent; _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor) : Boolean;
  public
    class function  LoadFromXMLDocument(_Invoice: TInvoice; _XmlDocument: IXMLDocument; out _Error : String; _AdditionalContent : TZUGFeRDAdditionalContent = nil) : Boolean;
    class function  LoadFromStream(_Invoice : TInvoice; _Stream : TStream; out _Error : String) : Boolean;
    class function  LoadFromFile(_Invoice : TInvoice; const _Filename : String; out _Error : String) : Boolean;
    class function  LoadFromXMLStr(_Invoice : TInvoice; const _XML : String; out _Error : String) : Boolean;
  end;
{$ENDIF}

{ TXRechnungInvoiceAdapter }

class procedure TXRechnungInvoiceAdapter.SaveToStream(_Invoice: TInvoice;
  _Version : TXRechnungVersion; _Stream: TStream);
var
  xml : IXMLDocument;
begin
  if _Invoice = nil then
    exit;
  if _Stream = nil then
    exit;

  xml := NewXMLDocument;
  try
    TXRechnungInvoiceAdapter.SaveDocument(_Invoice,_Version,xml);
    xml.SaveToStream(_Stream);
  finally
    xml := nil;
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveToXMLStr(_Invoice: TInvoice;
  _Version : TXRechnungVersion; out _XML: String);
var
  xml : IXMLDocument;
begin
  xml := NewXMLDocument;
  try
    TXRechnungInvoiceAdapter.SaveDocument(_Invoice,_Version,xml);
    xml.SaveToXML(_XML);
  finally
    xml := nil;
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveToFile(_Invoice: TInvoice;
  _Version : TXRechnungVersion;const _Filename: String);
var
  xml : IXMLDocument;
  hstrl : TStringList;
  xmlstring : String;
begin
  if _Invoice = nil then
    exit;
  if _Filename = '' then
    exit;
  if not DirectoryExists(ExtractFilePath(_Filename)) then
    exit;

  xml := NewXMLDocument;
  hstrl := TStringList.Create;
  try
    TXRechnungInvoiceAdapter.SaveDocument(_Invoice,_Version,xml);
    xml.SaveToXML(xmlstring);
    hstrl.Text := xmlstring;
    if hstrl.Count > 0 then
    if SameText(hstrl[0],'<?xml version="1.0"?>') then
      hstrl[0] := '<?xml version="1.0" encoding="UTF-8"?>';
    hstrl.WriteBOM := false;
    hstrl.SaveToFile(_Filename,TEncoding.UTF8);
  finally
    hstrl.Free;
    xml := nil;
  end;
end;

class function TXRechnungInvoiceAdapter.ConsistencyCheck(_Invoice: TInvoice;
  _Version: TXRechnungVersion): Boolean;
var
  lErrorCode : Integer;
begin
  Result := TXRechnungInvoiceAdapter.ConsistencyCheck(_Invoice, _Version, lErrorCode);
end;

class function TXRechnungInvoiceAdapter.ConsistencyCheck(_Invoice: TInvoice;
  _Version: TXRechnungVersion; out _ErrorCode: Integer): Boolean;
var
  lCount,i : Integer;
begin
  Result := true;
  _ErrorCode := ccOK;

  //Mindestens eine Zahlungsanweisung notwendig (bei ZUGFeRD nur im Profil EXTENDED)
  if (_Invoice.PaymentTypes.Count = 0) and
     (_Version <> TXRechnungVersion.ZUGFeRDEN16931Version_250) then
  begin
    _ErrorCode := ccNoPaymentsCount;
    Result := false;
    exit;
  end;

  //In XRechnung nicht unterstuetzte Rechnungsarten
  if (_Version in [TXRechnungVersion.XRechnungVersion_30x_UBL,
                   TXRechnungVersion.XRechnungVersion_30x_UNCEFACT]) then
  if (_Invoice.InvoiceTypeCode in [itc_DebitnoteRelatedToFinancialAdjustments,
                                   itc_SelfBilledCreditNote,
                                   itc_DebitNote,
                                   itc_PrepaymentInvoice,
                                   itc_Cancellation
                                   ]) then
  begin
    _ErrorCode := ccInvoiceTypeNotSupported;
    Result := false;
    exit;
  end;

  //Nur maximal eine Referenzrechnung in ZUGFeRD erlaubt
  if (_Version in [TXRechnungVersion.XRechnungVersion_30x_UNCEFACT]) then
  if _Invoice.PrecedingInvoiceReferences.Count > 1 then
  begin
    _ErrorCode := ccTooManyPrecedingInvoices;
    Result := false;
    exit;
  end;

  //Nur eine Lastschrift pro Rechnung
  lCount := 0;
  for i := 0 to _Invoice.PaymentTypes.Count-1 do
  if _Invoice.PaymentTypes[i].PaymentMeansCode = ipmc_SEPADirectDebit then
    inc(lCount);
  if lCount > 1 then
  begin
    _ErrorCode := ccTooManySEPADirectDebit;
    Result := false;
    exit;
  end;

  //Nur eine Kreditkarte pro Rechnung
  lCount := 0;
  for i := 0 to _Invoice.PaymentTypes.Count-1 do
  if _Invoice.PaymentTypes[i].PaymentMeansCode = ipmc_CreditCard then
    inc(lCount);
  if lCount > 1 then
  begin
    _ErrorCode := ccTooManyCreditCard;
    Result := false;
    exit;
  end;

  //Wenn der Verkaeufer keine UStId BT-31 und keine CompanyID BT-30 hat,
  //sollte CompanyID auf non-existent gesetzt werden
  if (_Invoice.AccountingSupplierParty.VATCompanyID = '') and
     (_Invoice.AccountingSupplierParty.CompanyID = '') then
  begin
    _ErrorCode := ccNoBT31BT30;
    Result := false;
    exit;
  end;

  //Eins von beiden BT-31 BT-32 muss angegeben werden
  if (_Invoice.AccountingSupplierParty.VATCompanyID = '') and
     (_Invoice.AccountingSupplierParty.VATCompanyNumber = '') then
  begin
    _ErrorCode := ccNoBT31BT32;
    Result := false;
    exit;
  end;

  //BG-DEX-09 THIRD PARTY PAYMENT Extension NUR XRechnung UBL !!!! https://blog.seeburger.com/de/xrechnung-2-3-1-gueltig-ab-dem-01-08-2023/
  if _Invoice.PrepaidPayments.Count > 0 then
  if not (_Version in [//TXRechnungVersion.XRechnungVersion_230_UBL_Deprecated, Version 2.3 wird nicht mehr gepflegt
                   TXRechnungVersion.XRechnungVersion_30x_UBL]) then
  begin
    _ErrorCode := ccPrepaidPaymentNotSupported;
    Result := false;
    exit;
  end;

  //Ein leeres Schema wird beim Schreiben durch 'EM' ersetzt, unter Peppol ist beides unzulaessig,
  //dort muss eine adressierbare Kennung aus der EAS-Codeliste angegeben werden
  if (_Version = PeppolBillingVersion_30) then
  if ((_Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer <> '') and
      ((_Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyerSchemeID = 'EM') or
       (_Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyerSchemeID = ''))) or
     ((_Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer <> '') and
      ((_Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyerSchemeID = 'EM') or
       (_Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyerSchemeID = ''))) then
  begin
    _ErrorCode := ccNoEMUnderPeppol;
    Result := false;
    exit;
  end;

  //Beide Steuernummern beim Kaeufer nicht vorgesehen
//  if (_Invoice.AccountingCustomerParty.VATCompanyID <> '') and
//     (_Invoice.AccountingCustomerParty.VATCompanyNumber <> '') then
//  begin
//    Result := false;
//    exit;
//  end;
end;

class procedure TXRechnungInvoiceAdapter.CorrectDueDateIfNotDefined(
  _Invoice: TInvoice);
begin
  //Zahlungsziel gesetzt? Wenn nein, dann gesetzliche 30 Tagefrist eintragen
  //Wenn Skonto hinter den 30 Tagen gewaehrt wird, dann laengste Frist + 1 Tag
  if _Invoice.InvoiceDueDate = 0 then
  begin
    _Invoice.InvoiceDueDate := _Invoice.InvoiceIssueDate + 30;
    case _Invoice.PaymentTermsType of
      iptt_CashDiscount1 : if _Invoice.InvoiceDueDate <= _Invoice.InvoiceIssueDate + _Invoice.PaymentTermCashDiscount1Days then
        _Invoice.InvoiceDueDate := _Invoice.InvoiceIssueDate + _Invoice.PaymentTermCashDiscount1Days + 1;
      iptt_CashDiscount2 : if _Invoice.InvoiceDueDate <= _Invoice.InvoiceIssueDate + _Invoice.PaymentTermCashDiscount2Days then
        _Invoice.InvoiceDueDate := _Invoice.InvoiceIssueDate + _Invoice.PaymentTermCashDiscount2Days + 1;
      iptt_CashDiscount3 : if _Invoice.InvoiceDueDate <= _Invoice.InvoiceIssueDate + _Invoice.PaymentTermCashDiscount3Days then
        _Invoice.InvoiceDueDate := _Invoice.InvoiceIssueDate + _Invoice.PaymentTermCashDiscount3Days + 1;
    end;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromFile(_Invoice: TInvoice;
  const _Filename: String; out _Error : String
  {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
var
  xml : IXMLDocument;
  attachmentName : String;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _Filename = '' then
    exit;
  if not FileExists(_Filename) then
  begin
    _Error := 'Datei nicht gefunden: ' + _Filename;
    exit;
  end;

  //ZUGFeRD-/Factur-X-PDF: die eingebettete Rechnung selbst herausholen
  if TXRechnungPdfExtractor.IsPdfFile(_Filename) then
  begin
    Result := TXRechnungInvoiceAdapter.LoadFromPdfFile(_Invoice,_Filename,_Error,attachmentName{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
    exit;
  end;

  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromFile(_Filename);
    Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
  finally
    xml := nil;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromStream(_Invoice: TInvoice;
  _Stream: TStream; out _Error : String
  {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
var
  xml : IXMLDocument;
  attachmentName : String;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _Stream = nil then
    exit;

  //ZUGFeRD-/Factur-X-PDF: die eingebettete Rechnung selbst herausholen
  if TXRechnungPdfExtractor.IsPdfStream(_Stream) then
  begin
    Result := TXRechnungInvoiceAdapter.LoadFromPdfStream(_Invoice,_Stream,_Error,attachmentName{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
    exit;
  end;

  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromStream(_Stream);
    Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
  finally
    xml := nil;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromPdfStream(_Invoice: TInvoice;
  _Stream: TStream; out _Error : String; out _AttachmentName : String
  {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
var
  xml : IXMLDocument;
  xmlBytes : TBytes;
  info : TXRechnungPdfExtractInfo;
  ms : TMemoryStream;
begin
  Result := false;
  _AttachmentName := '';
  if _Invoice = nil then
    exit;
  if _Stream = nil then
    exit;

  if not TXRechnungPdfExtractor.ExtractInvoiceFromStream(_Stream,xmlBytes,_AttachmentName,info) then
  begin
    if info.Error <> '' then
      _Error := info.Error
    else
      _Error := 'Im PDF wurde keine eingebettete Rechnung gefunden';
    exit;
  end;
  if Length(xmlBytes) = 0 then
  begin
    _Error := 'Die eingebettete Rechnung ist leer';
    exit;
  end;

  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(xmlBytes[0],Length(xmlBytes));
    ms.Position := 0;
    xml := TXMLDocument.Create(nil);
    try
      xml.LoadFromStream(ms);
      Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
    finally
      xml := nil;
    end;
  finally
    ms.Free;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromPdfFile(_Invoice: TInvoice;
  const _Filename: String; out _Error : String; out _AttachmentName : String
  {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
var
  fs : TFileStream;
begin
  Result := false;
  _AttachmentName := '';
  if _Invoice = nil then
    exit;
  if not FileExists(_Filename) then
  begin
    _Error := 'Datei nicht gefunden: ' + _Filename;
    exit;
  end;

  try
    fs := TFileStream.Create(_Filename,fmOpenRead or fmShareDenyWrite);
  except
    on E : Exception do
    begin
      _Error := 'Datei nicht lesbar: ' + E.Message;
      exit;
    end;
  end;
  try
    Result := TXRechnungInvoiceAdapter.LoadFromPdfStream(_Invoice,fs,_Error,_AttachmentName{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
  finally
    fs.Free;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromXMLDocument(
  _Invoice: TInvoice; _XmlDocument: IXMLDocument;
  out _Error: String
  {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}): Boolean;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _XmlDocument = nil then
    exit;

  case TXRechnungValidationHelper.GetXRechnungVersion(_XmlDocument) of
    XRechnungVersion_30x_UBL      : Result := TXRechnungInvoiceAdapter301.LoadDocumentUBL(_Invoice,_XmlDocument,_Error);
    XRechnungVersion_30x_UNCEFACT : Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    PeppolBillingVersion_30       : Result := TXRechnungInvoiceAdapter301.LoadDocumentUBL(_Invoice,_XmlDocument,_Error);
    //nur Lesen, geschrieben wird ausschliesslich 3.0.x
    XRechnungVersion_2x_ReadingOnly : Result := TXRechnungInvoiceAdapter301.LoadDocumentUBL(_Invoice,_XmlDocument,_Error);
    {$IFNDEF ZUGFeRD_Support}
    ZUGFeRDEN16931Version_250 : Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    ZUGFeRDExtendedVersion_250 : Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    {$ELSE}
    ZUGFeRDEN16931Version_250,
    ZUGFeRDExtendedVersion_250,
    ZUGFeRDExtendedVersion_1_NotSupported : Result := TZUGFeRDInvoiceAdapter.LoadFromXMLDocument(_Invoice,_XmlDocument,_Error,_AdditionalContent);
    {$ENDIF}
    else
    begin
      //Fallback fuer Rechnungen ohne oder mit unbekannter CustomizationID. Die Feldstruktur
      //entspricht EN16931, daher wird strukturbasiert nach dem Wurzelelement eingelesen,
      //andernfalls gibt es wenigstens einen Fehlertext.
      if (SameText(_XmlDocument.DocumentElement.NodeName,'Invoice') or
          SameText(_XmlDocument.DocumentElement.NodeName,'ubl:Invoice') or
          SameText(_XmlDocument.DocumentElement.NodeName,'ns0:Invoice') or
          SameText(_XmlDocument.DocumentElement.NodeName,'CreditNote') or
          SameText(_XmlDocument.DocumentElement.NodeName,'ubl:CreditNote') or
          SameText(_XmlDocument.DocumentElement.NodeName,'ns0:CreditNote')) then
        Result := TXRechnungInvoiceAdapter301.LoadDocumentUBL(_Invoice,_XmlDocument,_Error)
      else
      if (SameText(_XmlDocument.DocumentElement.NodeName,'CrossIndustryInvoice') or
          SameText(_XmlDocument.DocumentElement.NodeName,'rsm:CrossIndustryInvoice')) then
        Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error)
      else
        _Error := 'Nicht unterstuetztes Rechnungsformat (Wurzelelement '+_XmlDocument.DocumentElement.NodeName+')';
    end;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromXMLStr(_Invoice: TInvoice;
  const _XML: String; out _Error : String
  {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
var
  xml : IXMLDocument;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _XML = '' then
    exit;

  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromXML(_XML);
    Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
  finally
    xml := nil;
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveDocument(_Invoice: TInvoice;
  _Version : TXRechnungVersion; _Xml: IXMLDocument);
begin
  case _Version of
    XRechnungVersion_30x_UBL : TXRechnungInvoiceAdapter301.SaveDocumentUBL(_Invoice,_Xml,ipXRechnung);
    XRechnungVersion_30x_UNCEFACT : TXRechnungInvoiceAdapter301.SaveDocumentUNCEFACT(_Invoice,_Xml,ipXRechnung);
    ZUGFeRDEN16931Version_250 : TXRechnungInvoiceAdapter301.SaveDocumentUNCEFACT(_Invoice,_Xml,ipZUGFeRDEN16931);
    ZUGFeRDExtendedVersion_250 : TXRechnungInvoiceAdapter301.SaveDocumentUNCEFACT(_Invoice,_Xml,ipZUGFeRDExtended);
    PeppolBillingVersion_30 : TXRechnungInvoiceAdapter301.SaveDocumentUBL(_Invoice,_Xml,ipPeppol);
    else raise Exception.Create('Unkown version');
  end;
end;

{ TXRechnungHelper }

class function TXRechnungHelper.AmountFromStr(_Val: String): Currency;
var
  fs : TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator := '.';
  Result := StrToCurrDef(_Val,0,fs);
end;

class function TXRechnungHelper.AmountToStr(
  _Val: Currency): String;
begin
  Result := ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TXRechnungHelper.UnitPriceAmountFromStr(
  _Val: String): Currency;
var
  fs : TFormatSettings;
begin
  Result := 0;
  if _Val = '' then
    exit;
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator := '.';
  Result := StrToCurrDef(_Val,0,fs);
end;

class function TXRechnungHelper.UnitPriceAmountToStr(
  _Val: Currency): String;
var
  lRounded : Currency;
begin
  lRounded := RoundTo(_Val,-2);
  if _Val = lRounded then
    Result := ReplaceText(Format('%.2f',[_Val]),',','.')
  else
    Result := ReplaceText(Format('%.4f',[_Val]),',','.');
end;

class function TXRechnungHelper.DateFromStrUBLFormat(const _Val : String) : TDateTime;
begin
  Result := 0;
  if Length(_Val) <> 10 then
    exit;
  Result := EncodeDate(StrToIntDef(Copy(_Val,1,4),1899),StrToIntDef(Copy(_Val,6,2),12),StrToIntDef(Copy(_Val,9,2),30));
end;

class function TXRechnungHelper.DateFromStrUNCEFACTFormat(const _Val : String) : TDateTime;
begin
  Result := 0;
  if Length(_Val) <> 8 then
    exit;
  Result := EncodeDate(StrToIntDef(Copy(_Val,1,4),1899),StrToIntDef(Copy(_Val,5,2),12),StrToIntDef(Copy(_Val,7,2),30));
end;

class function TXRechnungHelper.DateToStrUBLFormat(
  const _Val: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd',_Val);
end;

class function TXRechnungHelper.DateToStrUNCEFACTFormat(
  const _Val: TDateTime): String;
begin
  Result := FormatDateTime('yyyymmdd',_Val);
end;

class function TXRechnungHelper.FloatFromStr(_Val: String): double;
var
  fs : TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator := '.';
  Result := StrToFloatDef(_Val,0,fs);
end;

class function TXRechnungHelper.FloatToStr(
  _Val: double; _DecimalPlaces : Integer = 2): String;
begin
  if _DecimalPlaces < 0 then
    _DecimalPlaces := 0;
  Result := ReplaceText(Format('%.'+IntToStr(_DecimalPlaces)+'f',[_Val]),',','.');
end;

class function TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeFromStr(
  _Val: String): TInvoiceAllowanceOrChargeIdentCode;
begin
  if SameText(_Val,'41') then
    Result := iacic_BonusForWorksAheadOfSchedule else
  if SameText(_Val,'42') then
    Result := iacic_OtherBonus else
  if SameText(_Val,'60') then
    Result := iacic_ManufacturersConsumerDiscount else
  if SameText(_Val,'62') then
    Result := iacic_DueToMilitaryStatus else
  if SameText(_Val,'63') then
    Result := iacic_DueToWorkAccident else
  if SameText(_Val,'64') then
    Result := iacic_SpecialAgreement else
  if SameText(_Val,'65') then
    Result := iacic_ProductionErrorDiscount else
  if SameText(_Val,'66') then
    Result := iacic_NewOutletDiscount else
  if SameText(_Val,'67') then
    Result := iacic_SampleDiscount else
  if SameText(_Val,'68') then
    Result := iacic_EndOfRangeDiscount else
  if SameText(_Val,'70') then
    Result := iacic_IncotermDiscount else
  if SameText(_Val,'71') then
    Result := iacic_PointOfSalesThresholdAllowance else
  if SameText(_Val,'88') then
    Result := iacic_MaterialSurchargeDeduction else
  if SameText(_Val,'95') then
    Result := iacic_Discount else
  if SameText(_Val,'100') then
    Result := iacic_SpecialRebate else
  if SameText(_Val,'102') then
    Result := iacic_FixedLongTerm else
  if SameText(_Val,'103') then
    Result := iacic_Temporary else
  if SameText(_Val,'104') then
    Result := iacic_Standard else
  if SameText(_Val,'105') then
    Result := iacic_YearlyTurnover else
  Result := iacic_None;
end;

class function TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeToStr(
  _Val: TInvoiceAllowanceOrChargeIdentCode): String;
begin
  case _Val of
    //iacic_HandlingCommission: Result :=                                '1';
    //iacic_AmendmentCommission: Result :=                               '2';
    //iacic_AcceptanceCommission: Result :=                              '3';
    //iacic_CommissionForObtainingAcceptance: Result :=                  '4';
    //iacic_CommissionOnDelivery: Result :=                              '5';
    //iacic_AdvisingCommission: Result :=                                '6';
    //iacic_ConfirmationCommission: Result :=                            '7';
    //iacic_DeferredPaymentCommission: Result :=                         '8';
    //iacic_CommissionForTakingUpDocuments: Result :=                    '9';
    //iacic_OpeningCommission: Result :=                                 '10';
    //iacic_FeeForPaymentUnderReserve: Result :=                         '11';
    //iacic_DiscrepancyFee: Result :=                                    '12';
    //iacic_DomicilationCommission: Result :=                            '13';
    //iacic_CommissionForReleaseOfGoods: Result :=                       '14';
    //iacic_CollectionCommission: Result :=                              '15';
    //iacic_NegotiationCommission: Result :=                             '16';
    //iacic_ReturnCommission: Result :=                                  '17';
    //iacic_BLSplittingCharges: Result :=                                '18';
    //iacic_TrustCommission: Result :=                                   '19';
    //iacic_TransferCommission: Result :=                                '20';
    //iacic_CommissionForOpeningIrrevocableDocumentaryCredits: Result := '21';
    //iacic_PreadviceCommission: Result :=                               '22';
    //iacic_SupervisoryCommission: Result :=                             '23';
    //iacic_ModelCharges: Result :=                                      '24';
    //iacic_RiskCommission: Result :=                                    '25';
    //iacic_GuaranteeCommission: Result :=                               '26';
    //iacic_ReimbursementCommission: Result :=                           '27';
    //iacic_StampDuty: Result :=                                         '28';
    //iacic_Brokerage: Result :=                                         '29';
    //iacic_BankCharges: Result :=                                       '30';
    //iacic_BankChargesInformation: Result :=                            '31';
    //iacic_CourierFee: Result :=                                        '32';
    //iacic_PhoneFee: Result :=                                          '33';
    //iacic_PostageFee: Result :=                                        '34';
    //iacic_SWIFTFee: Result :=                                          '35';
    //iacic_TelexFee: Result :=                                          '36';
    //iacic_PenaltyForLateDeliveryOfDocuments: Result :=                 '37';
    //iacic_PenaltyForLateDeliveryOfValuationOfWorks: Result :=          '38';
    //iacic_PenaltyForExecutionOfWorksBehindSchedule: Result :=          '39';
    //iacic_OtherPenalties: Result :=                                    '40';
    iacic_BonusForWorksAheadOfSchedule: Result :=                      '41';
    iacic_OtherBonus: Result :=                                        '42';
    //iacic_ProjectManagementCost: Result :=                             '44';
    //iacic_ProRataRetention: Result :=                                  '45';
    //iacic_ContractualRetention: Result :=                              '46';
    //iacic_OtherRetentions: Result :=                                   '47';
    //iacic_InterestOnArrears: Result :=                                 '48';
    //iacic_Interest: Result :=                                          '49';
    //iacic_ChargePerCreditCover: Result :=                              '50';
    //iacic_ChargePerUnusedCreditCover: Result :=                        '51';
    //iacic_MinimumCommission: Result :=                                 '52';
    //iacic_FactoringCommission: Result :=                               '53';
    //iacic_ChamberOfCommerceCharge: Result :=                           '54';
    //iacic_TransferCharges: Result :=                                   '55';
    //iacic_RepatriationCharges: Result :=                               '56';
    //iacic_MiscellaneousCharges: Result :=                              '57';
    //iacic_ForeignExchangeCharges: Result :=                            '58';
    //iacic_AgreedDebitInterestCharge: Result :=                         '59';
    iacic_ManufacturersConsumerDiscount: Result :=                     '60';
    //iacic_FaxAdviceCharge: Result :=                                   '61';
    iacic_DueToMilitaryStatus: Result :=                               '62';
    iacic_DueToWorkAccident: Result :=                                 '63';
    iacic_SpecialAgreement: Result :=                                  '64';
    iacic_ProductionErrorDiscount: Result :=                           '65';
    iacic_NewOutletDiscount: Result :=                                 '66';
    iacic_SampleDiscount: Result :=                                    '67';
    iacic_EndOfRangeDiscount: Result :=                                '68';
    //iacic_ChargeForACustomerSpecificFinish: Result :=                  '69';
    iacic_IncotermDiscount: Result :=                                  '70';
    iacic_PointOfSalesThresholdAllowance: Result :=                    '71';
    //iacic_TechnicalModificationCosts: Result :=                        '72';
    //iacic_JoborderProductionCosts: Result :=                           '73';
    //iacic_OffpremisesCosts: Result :=                                  '74';
    //iacic_AdditionalProcessingCosts: Result :=                         '75';
    //iacic_AttestingCharge: Result :=                                   '76';
    //iacic_RushDeliverySurcharge: Result :=                             '77';
    //iacic_SpecialConstructionCosts: Result :=                          '78';
    //iacic_FreightCharges: Result :=                                    '79';
    //iacic_PackingCharge: Result :=                                     '80';
    //iacic_RepairCharge: Result :=                                      '81';
    //iacic_LoadingCharge: Result :=                                     '82';
    //iacic_SetupCharge: Result :=                                       '83';
    //iacic_TestingCharge: Result :=                                     '84';
    //iacic_WarehousingCharge: Result :=                                 '85';
    //iacic_GoldSurcharge: Result :=                                     '86';
    //iacic_CopperSurcharge: Result :=                                   '87';
    iacic_MaterialSurchargeDeduction: Result :=                        '88';
    //iacic_LeadSurcharge: Result :=                                     '89';
    //iacic_PriceIndexSurcharge: Result :=                               '90';
    //iacic_PlatinumSurcharge: Result :=                                 '91';
    //iacic_SilverSurcharge: Result :=                                   '92';
    //iacic_WolframSurcharge: Result :=                                  '93';
    //iacic_AluminumSurcharge: Result :=                                 '94';
    iacic_Discount: Result :=                                          '95';
    //iacic_Insurance: Result :=                                         '96';
    //iacic_MinimumOrderMinimumBillingCharge: Result :=                  '97';
    //iacic_MaterialSurchargeSspecialMaterials: Result :=                '98';
    //iacic_Surcharge: Result :=                                         '99';
    iacic_SpecialRebate: Result :=                                     '100';
    //iacic_CarbonFootprintCharge: Result :=                             '101';
    iacic_FixedLongTerm: Result :=                                     '102';
    iacic_Temporary: Result :=                                         '103';
    iacic_Standard: Result :=                                          '104';
    iacic_YearlyTurnover: Result :=                                    '105';
    //iacic_WithheldTaxesAndSocialSecurityContributions: Result :=       '106';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoiceAttachmentTypeToStr(_Val: TInvoiceAttachmentType): String;
begin
  case _Val of
    iat_application_pdf: Result := 'application/pdf';
    iat_image_png: Result := 'image/png';
    iat_image_jpeg: Result := 'image/jpeg';
    iat_text_csv: Result := 'text/csv';
    iat_application_vnd_openxmlformats_officedocument_spreadsheetml_sheet: Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
    iat_application_vnd_oasis_opendocument_spreadsheet: Result := 'application/vnd.oasis.opendocument.spreadsheet';
    iat_application_xml: Result := 'application/xml';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoiceAttachmentTypeCodeFromStr(
  _Val: String): TInvoiceAttachmentTypeCode;
begin
  if SameText(_Val,'50') then
    Result := iatc_50 else
  if SameText(_Val,'130') then
    Result := iatc_130 else
  if SameText(_Val,'916') then
    Result := iatc_916
  else
    Result := iatc_None;
end;

class function TXRechnungHelper.InvoiceAttachmentTypeCodeToStr(
  _Val: TInvoiceAttachmentTypeCode): String;
begin
  case _Val of
    iatc_50: Result := '50';
    iatc_130: Result := '130';
    iatc_916: Result := '916';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoiceAttachmentTypeFromStr(_Val: String): TInvoiceAttachmentType;
begin
  if SameText(_Val,'application/pdf') then
    Result := iat_application_pdf else
  if SameText(_Val,'image/png') then
    Result := iat_image_png else
  if SameText(_Val,'image/jpeg') then
    Result := iat_image_jpeg else
  if SameText(_Val,'text/csv') then
    Result := iat_text_csv else
  if SameText(_Val,'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet') then
    Result := iat_application_vnd_openxmlformats_officedocument_spreadsheetml_sheet else
  if SameText(_Val,'application/vnd.oasis.opendocument.spreadsheet') then
    Result := iat_application_vnd_oasis_opendocument_spreadsheet else
  if SameText(_Val,'application/xml') then Result := iat_application_xml else
    Result := iat_application_None
end;

class function TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeFromStr(
  _Val: String): TInvoiceDutyTaxFeeCategoryCode;
begin
  if SameText(_Val,'AE') then
    Result := idtfcc_AE_VATReverseCharge
  else
  if SameText(_Val,'E') then
    Result := idtfcc_E_ExemptFromTax
  else
  if SameText(_Val,'G') then
    Result := idtfcc_G_FreeExportItemTaxNotCharged
  else
  if SameText(_Val,'K') then
    Result := idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices
  else
  if SameText(_Val,'L') then
    Result := idtfcc_L_CanaryIslandsGeneralIndirectTax
  else
  if SameText(_Val,'M') then
    Result := idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla
  else
  if SameText(_Val,'O') then
    Result := idtfcc_O_ServicesOutsideScopeOfTax
  else
  if SameText(_Val,'S') then
    Result := idtfcc_S_StandardRate
  else
  if SameText(_Val,'Z') then
    Result := idtfcc_Z_ZeroRatedGoods
  else
    Result := idtfcc_None;
end;

class function TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(_Val: TInvoiceDutyTaxFeeCategoryCode): String;
begin
  case _Val of
    //idtfcc_A_MixedTaxRate: Result := 'A';
    //idtfcc_AA_LowerRate: Result := 'AA';
    //idtfcc_AB_ExemptForResale: Result := 'AB';
    //idtfcc_AC_ValueAddedTaxVATNotNowDueForPayment: Result := 'AC';
    //idtfcc_AD_ValueAddedTaxVATDueFromAPreviousInvoice: Result := 'AD';
    idtfcc_AE_VATReverseCharge: Result := 'AE';
    //idtfcc_B_TransferredVAT: Result := 'B';
    //idtfcc_C_DutyPaidBySupplier: Result := 'C';
    //idtfcc_D_ValueAddedTaxVATMmarginSchemeTravelAgents: Result := 'D';
    idtfcc_E_ExemptFromTax: Result := 'E';
    //idtfcc_F_ValueAddedTaxVATMmarginSchemeSecondhandGoods: Result := 'F';
    idtfcc_G_FreeExportItemTaxNotCharged: Result := 'G';
    //idtfcc_H_HigherRate: Result := 'H';
    //idtfcc_I_ValueAddedTaxVATMarginSchemeWorksOfArt: Result := 'I';
    //idtfcc_J_ValueAddedTaxVATMarginSchemeCollectorsItemsAndAntiques: Result := 'J';
    idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices: Result := 'K';
    idtfcc_L_CanaryIslandsGeneralIndirectTax: Result := 'L';
    idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla: Result := 'M';
    idtfcc_O_ServicesOutsideScopeOfTax: Result := 'O';
    idtfcc_S_StandardRate: Result := 'S';
    idtfcc_Z_ZeroRatedGoods: Result := 'Z';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoiceNoteSubjectCodeFromStr(
  _Val: String): TInvoiceNoteSubjectCode;
begin
  if SameText(_Val,'AAI') then
    Result := insc_AAI
  else
  if SameText(_Val,'AAJ') then
    Result := insc_AAJ
  else
  if SameText(_Val,'AAK') then
    Result := insc_AAK
  else
  if SameText(_Val,'SUR') then
    Result := insc_SUR
  else
  if SameText(_Val,'REG') then
    Result := insc_REG
  else
  if SameText(_Val,'ABL') then
    Result := insc_ABL
  else
  if SameText(_Val,'TXD') then
    Result := insc_TXD
  else
  if SameText(_Val,'CUS') then
    Result := insc_CUS
  else
  if SameText(_Val,'PMT') then
    Result := insc_PMT
  else
    Result := insc_None;
end;

class function TXRechnungHelper.InvoiceNoteSubjectCodeToStr(
  _Val: TInvoiceNoteSubjectCode): String;
begin
  case _Val of
    insc_AAI: Result := 'AAI';
    insc_AAJ: Result := 'AAJ';
    insc_AAK: Result := 'AAK';
    insc_SUR: Result := 'SUR';
    insc_REG: Result := 'REG';
    insc_ABL: Result := 'ABL';
    insc_TXD: Result := 'TXD';
    insc_CUS: Result := 'CUS';
    insc_PMT: Result := 'PMT';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoicePaymentMeansCodeFromStr(
  _Val: String): TInvoicePaymentMeansCode;
begin
  if SameText(_Val,'2') then
    Result := ipmc_AutomatedClearingHouseCredit
  else
  if SameText(_Val,'3') then
    Result := ipmc_AutomatedClearingHouseDebit
  else
  if SameText(_Val,'4') then
    Result := ipmc_ACH_DemandDebitReversal
  else
  if SameText(_Val,'5') then
    Result := ipmc_ACH_DemandCreditReversal
  else
  if SameText(_Val,'6') then
    Result := ipmc_ACH_Demand_Credit
  else
  if SameText(_Val,'7') then
    Result := ipmc_ACH_Demand_Debit
  else
  if SameText(_Val,'8') then
    Result := ipmc_Hold
  else
  if SameText(_Val,'9') then
    Result := ipmc_NationalOrRegionalClearing
  else
  if SameText(_Val,'10') then
    Result := ipmc_InCash
  else
  if SameText(_Val,'11') then
    Result := ipmc_ACH_SavingsCreditReversal
  else
  if SameText(_Val,'12') then
    Result := ipmc_ACH_SavingsDebitReversal
  else
  if SameText(_Val,'13') then
    Result := ipmc_ACH_SavingsCredit
  else
  if SameText(_Val,'14') then
    Result := ipmc_ACH_SavingsDebit
  else
  if SameText(_Val,'15') then
    Result := ipmc_BookEntryCredit
  else
  if SameText(_Val,'16') then
    Result := ipmc_BookEntryDebit
  else
  if SameText(_Val,'17') then
    Result := ipmc_ACH_DemandCashConcentrationDisbursementCredit
  else
  if SameText(_Val,'18') then
    Result := ipmc_ACH_DemandCashConcentrationDisbursementDebit
  else
  if SameText(_Val,'19') then
    Result := ipmc_ACH_DemandCorporateTradePaymentCredit
  else
  if SameText(_Val,'20') then
    Result := ipmc_Cheque
  else
  if SameText(_Val,'21') then
    Result := ipmc_BankersDraft
  else
  if SameText(_Val,'22') then
    Result := ipmc_CertifiedBankerDraft
  else
  if SameText(_Val,'23') then
    Result := ipmc_BankChequeIssuedByEstablishment
  else
  if SameText(_Val,'24') then
    Result := ipmc_BillOfExchangeAwaitingAcceptance
  else
  if SameText(_Val,'25') then
    Result := ipmc_CertifiedCheque
  else
  if SameText(_Val,'26') then
    Result := ipmc_LocalCheque
  else
  if SameText(_Val,'27') then
    Result := ipmc_ACH_DemandCorporateTradePaymentDebit
  else
  if SameText(_Val,'28') then
    Result := ipmc_ACH_DemandCorporateTradeExchangeCredit
  else
  if SameText(_Val,'29') then
    Result := ipmc_ACH_DemandCorporateTradeExchangeDebit
  else
  if SameText(_Val,'30') then
    Result := ipmc_CreditTransfer
  else
  if SameText(_Val,'31') then
    Result := ipmc_DebitTransfer
  else
  if SameText(_Val,'32') then
    Result := ipmc_ACH_DemandCashConcentrationDisbursementPlusCredit
  else
  if SameText(_Val,'33') then
    Result := ipmc_ACH_DemandCashConcentrationDisbursementPlusDebit
  else
  if SameText(_Val,'34') then
    Result := ipmc_ACH_PrearrangedPaymentAndDeposit
  else
  if SameText(_Val,'35') then
    Result := ipmc_ACH_SavingsCashConcentrationDisbursementCredit
  else
  if SameText(_Val,'36') then
    Result := ipmc_ACH_SavingsCashConcentrationDisbursementDebit
  else
  if SameText(_Val,'37') then
    Result := ipmc_ACH_SavingsCorporateTradePaymentCredit
  else
  if SameText(_Val,'38') then
    Result := ipmc_ACH_SavingsCorporateTradePaymentDebit
  else
  if SameText(_Val,'39') then
    Result := ipmc_ACH_SavingsCorporateTradeExchangeCredit
  else
  if SameText(_Val,'40') then
    Result := ipmc_ACH_SavingsCorporateTradeExchangeDebit
  else
  if SameText(_Val,'41') then
    Result := ipmc_ACH_SavingsCashConcentrationDisbursementPlusCredit
  else
  if SameText(_Val,'42') then
    Result := ipmc_PaymentToBankAccount
  else
  if SameText(_Val,'43') then
    Result := ipmc_ACH_SavingsCashConcentrationDisbursementPlusDebit
  else
  if SameText(_Val,'44') then
    Result := ipmc_AcceptedBillOfExchange
  else
  if SameText(_Val,'45') then
    Result := ipmc_ReferencedHomeBankingCreditTransfer
  else
  if SameText(_Val,'46') then
    Result := ipmc_InterbankDebitTransfer
  else
  if SameText(_Val,'47') then
    Result := ipmc_HomeBankingDebitTransfer
  else
  if SameText(_Val,'48') then
    Result := ipmc_BankCard
  else
  if SameText(_Val,'49') then
    Result := ipmc_DirectDebit
  else
  if SameText(_Val,'50') then
    Result := ipmc_PaymentByPostgiro
  else
  if SameText(_Val,'51') then
    Result := ipmc_FR_Norme_6_97
  else
  if SameText(_Val,'52') then
    Result := ipmc_UrgentCommercialPayment
  else
  if SameText(_Val,'53') then
    Result := ipmc_UrgentTreasuryPayment
  else
  if SameText(_Val,'54') then
    Result := ipmc_CreditCard
  else
  if SameText(_Val,'55') then
    Result := ipmc_DebitCard
  else
  if SameText(_Val,'56') then
    Result := ipmc_Bankgiro
  else
  if SameText(_Val,'57') then
    Result := ipmc_StandingAgreement
  else
  if SameText(_Val,'58')  then
    Result := ipmc_SEPACreditTransfer
  else
  if SameText(_Val,'59')  then
    Result := ipmc_SEPADirectDebit
  else
  if SameText(_Val,'60')  then
    Result := ipmc_PromissoryNote
  else
  if SameText(_Val,'61')  then
    Result := ipmc_PromissoryNoteSignedByDebtor
  else
  if SameText(_Val,'62')  then
    Result := ipmc_PromissoryNoteSignedByDebtorEndorsedByBank
  else
  if SameText(_Val,'63')  then
    Result := ipmc_PromissoryNoteSignedByDebtorEndorsedByThirdParty
  else
  if SameText(_Val,'64')  then
    Result := ipmc_PromissoryNoteSignedByBank
  else
  if SameText(_Val,'65')  then
    Result := ipmc_PromissoryNoteSignedByBankEndorsedByAnotherBank
  else
  if SameText(_Val,'66')  then
    Result := ipmc_PromissoryNoteSignedByThirdParty
  else
  if SameText(_Val,'67')  then
    Result := ipmc_PromissoryNoteSignedByThirdPartyEndorsedByBank
  else
  if SameText(_Val,'68')  then
    Result := ipmc_OnlinePaymentService
  else
  if SameText(_Val,'69')  then
    Result := ipmc_TransferAdvice
  else
  if SameText(_Val,'70')  then
    Result := ipmc_BillDrawnByCrdtOnDebtor
  else
  if SameText(_Val,'74')  then
    Result := ipmc_BillDrawnByCrdtOnBank
  else
  if SameText(_Val,'75')  then
    Result := ipmc_BillDrawnByCrdtEndorsedByAnotherBank
  else
  if SameText(_Val,'76')  then
    Result := ipmc_BillDrawnByCrdtOnBankEndorsedByThirdParty
  else
  if SameText(_Val,'77')  then
    Result := ipmc_BillDrawnByCrdtOnThirdParty
  else
  if SameText(_Val,'78')  then
    Result := ipmc_BillDrawnByCrdtOnThirdPartyAcceptedAndEndorsedByBank
  else
  if SameText(_Val,'91')  then
    Result := ipmc_NotTransferableBankersDraft
  else
  if SameText(_Val,'92')  then
    Result := ipmc_NotTransferableLocalCheque
  else
  if SameText(_Val,'93')  then
    Result := ipmc_ReferenceGiro
  else
  if SameText(_Val,'94')  then
    Result := ipmc_UrgentGiro
  else
  if SameText(_Val,'95')  then
    Result := ipmc_FreeFormatGiro
  else
  if SameText(_Val,'96')  then
    Result := ipmc_RequestedMethodForPaymentWasNotUsed
  else
  if SameText(_Val,'97')  then
    Result := ipmc_ClearingBetweenPartners
  else
  if SameText(_Val,'ZZZ')  then
    Result := ipmc_MutuallyDefined
  else
  if SameText(_Val,'1')  then
    Result := ipmc_InstrumentNotDefined
  else
    Result := ipmc_NotImplemented;
end;

class function TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Val: TInvoicePaymentMeansCode): String;
begin
  case _Val of
    ipmc_InstrumentNotDefined :                                 Result := '1';
    ipmc_AutomatedClearingHouseCredit :                         Result := '2';
    ipmc_AutomatedClearingHouseDebit :                          Result := '3';
    ipmc_ACH_DemandDebitReversal :                              Result := '4';
    ipmc_ACH_DemandCreditReversal :                             Result := '5';
    ipmc_ACH_Demand_Credit :                                    Result := '6';
    ipmc_ACH_Demand_Debit :                                     Result := '7';
    ipmc_Hold :                                                 Result := '8';
    ipmc_NationalOrRegionalClearing :                           Result := '9';
    ipmc_InCash: Result := '10';
    ipmc_ACH_SavingsCreditReversal :                            Result := '11';
    ipmc_ACH_SavingsDebitReversal :                             Result := '12';
    ipmc_ACH_SavingsCredit :                                    Result := '13';
    ipmc_ACH_SavingsDebit :                                     Result := '14';
    ipmc_BookEntryCredit :                                      Result := '15';
    ipmc_BookEntryDebit :                                       Result := '16';
    ipmc_ACH_DemandCashConcentrationDisbursementCredit :        Result := '17';
    ipmc_ACH_DemandCashConcentrationDisbursementDebit :         Result := '18';
    ipmc_ACH_DemandCorporateTradePaymentCredit :                Result := '19';
    ipmc_Cheque: Result := '20';
    ipmc_BankersDraft :                                         Result := '21';
    ipmc_CertifiedBankerDraft :                                 Result := '22';
    ipmc_BankChequeIssuedByEstablishment :                      Result := '23';
    ipmc_BillOfExchangeAwaitingAcceptance :                     Result := '24';
    ipmc_CertifiedCheque :                                      Result := '25';
    ipmc_LocalCheque :                                          Result := '26';
    ipmc_ACH_DemandCorporateTradePaymentDebit :                 Result := '27';
    ipmc_ACH_DemandCorporateTradeExchangeCredit :               Result := '28';
    ipmc_ACH_DemandCorporateTradeExchangeDebit :                Result := '29';
    ipmc_CreditTransfer: Result := '30';
    ipmc_DebitTransfer :                                        Result := '31';
    ipmc_ACH_DemandCashConcentrationDisbursementPlusCredit :    Result := '32';
    ipmc_ACH_DemandCashConcentrationDisbursementPlusDebit :     Result := '33';
    ipmc_ACH_PrearrangedPaymentAndDeposit :                     Result := '34';
    ipmc_ACH_SavingsCashConcentrationDisbursementCredit :       Result := '35';
    ipmc_ACH_SavingsCashConcentrationDisbursementDebit :        Result := '36';
    ipmc_ACH_SavingsCorporateTradePaymentCredit :               Result := '37';
    ipmc_ACH_SavingsCorporateTradePaymentDebit :                Result := '38';
    ipmc_ACH_SavingsCorporateTradeExchangeCredit :              Result := '39';
    ipmc_ACH_SavingsCorporateTradeExchangeDebit :               Result := '40';
    ipmc_ACH_SavingsCashConcentrationDisbursementPlusCredit :   Result := '41';
    ipmc_PaymentToBankAccount :                                 Result := '42';
    ipmc_ACH_SavingsCashConcentrationDisbursementPlusDebit :    Result := '43';
    ipmc_AcceptedBillOfExchange :                               Result := '44';
    ipmc_ReferencedHomeBankingCreditTransfer :                  Result := '45';
    ipmc_InterbankDebitTransfer :                               Result := '46';
    ipmc_HomeBankingDebitTransfer :                             Result := '47';
    ipmc_BankCard :                                             Result := '48';
    ipmc_DirectDebit :                                          Result := '49';
    ipmc_PaymentByPostgiro :                                    Result := '50';
    ipmc_FR_Norme_6_97 :                                        Result := '51';
    ipmc_UrgentCommercialPayment :                              Result := '52';
    ipmc_UrgentTreasuryPayment :                                Result := '53';
    ipmc_CreditCard: Result := '54';
    ipmc_DebitCard :                                            Result := '55';
    ipmc_Bankgiro :                                             Result := '56';
    ipmc_StandingAgreement :                                    Result := '57';
    ipmc_SEPACreditTransfer: Result := '58';
    ipmc_SEPADirectDebit: Result := '59';
    ipmc_PromissoryNote :                                       Result := '60';
    ipmc_PromissoryNoteSignedByDebtor :                         Result := '61';
    ipmc_PromissoryNoteSignedByDebtorEndorsedByBank :           Result := '62';
    ipmc_PromissoryNoteSignedByDebtorEndorsedByThirdParty :     Result := '63';
    ipmc_PromissoryNoteSignedByBank :                           Result := '64';
    ipmc_PromissoryNoteSignedByBankEndorsedByAnotherBank :      Result := '65';
    ipmc_PromissoryNoteSignedByThirdParty :                     Result := '66';
    ipmc_PromissoryNoteSignedByThirdPartyEndorsedByBank :       Result := '67';
    ipmc_OnlinePaymentService: Result := '68';
    ipmc_TransferAdvice :                                       Result := '69';
    ipmc_BillDrawnByCrdtOnDebtor :                              Result := '70';
    ipmc_BillDrawnByCrdtOnBank :                                Result := '74';
    ipmc_BillDrawnByCrdtEndorsedByAnotherBank :                 Result := '75';
    ipmc_BillDrawnByCrdtOnBankEndorsedByThirdParty :            Result := '76';
    ipmc_BillDrawnByCrdtOnThirdParty :                          Result := '77';
    ipmc_BillDrawnByCrdtOnThirdPartyAcceptedAndEndorsedByBank : Result := '78';
    ipmc_NotTransferableBankersDraft :                          Result := '91';
    ipmc_NotTransferableLocalCheque :                           Result := '92';
    ipmc_ReferenceGiro :                                        Result := '93';
    ipmc_UrgentGiro :                                           Result := '94';
    ipmc_FreeFormatGiro :                                       Result := '95';
    ipmc_RequestedMethodForPaymentWasNotUsed :                  Result := '96';
    ipmc_ClearingBetweenPartners :                              Result := '97';
    ipmc_MutuallyDefined: Result := 'ZZZ';
    else Result := '1'; //ipmc_InstrumentNotDefined
  end;
end;

class function TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeFromStr(
  _Val: String): TInvoiceSpecialServiceDescriptionCode;
begin
  if SameText(_Val,'AA') then
    Result := issdc_AA_Advertising else
  if SameText(_Val,'AAA') then
    Result := issdc_AAA_Telecommunication else
  if SameText(_Val,'ABK') then
    Result := issdc_ABK_Miscellaneous else
  if SameText(_Val,'ABL') then
    Result := issdc_ABL_AdditionalPackaging else
  if SameText(_Val,'ADR') then
    Result := issdc_ADR_OtherServices else
  if SameText(_Val,'ADT') then
    Result := issdc_ADT_Pickup else
  if SameText(_Val,'FC') then
    Result := issdc_FC_FreightService else
  if SameText(_Val,'FI') then
    Result := issdc_FI_Financing else
  if SameText(_Val,'LA') then
    Result := issdc_LA_Labelling else
  if SameText(_Val,'PC') then
    Result := issdc_PC_Packing else
  Result := issdc_None;
end;

class function TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeToStr(
  _Val: TInvoiceSpecialServiceDescriptionCode): String;
begin
  case _Val of
    issdc_AA_Advertising: Result := 'AA';
    issdc_AAA_Telecommunication: Result := 'AAA';
    issdc_ABK_Miscellaneous: Result := 'ABK';
    issdc_ABL_AdditionalPackaging: Result := 'ABL';
    issdc_ADR_OtherServices: Result := 'ADR';
    issdc_ADT_Pickup: Result := 'ADT';
    issdc_FC_FreightService: Result := 'FC';
    issdc_FI_Financing: Result := 'FI';
    issdc_LA_Labelling: Result := 'LA';
    issdc_PC_Packing: Result := 'PC';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoiceTypeCodeFromStr(const _Val: String): TInvoiceTypeCode;
begin
  if SameText(_Val,'326') then
    Result := itc_PartialInvoice
  else
  if SameText(_Val,'380') then
    Result := itc_CommercialInvoice
  else
  if SameText(_Val,'384') then
    Result := itc_CorrectedInvoice
  else
  if SameText(_Val,'389') then
    Result := itc_SelfbilledInvoice
  else
  if SameText(_Val,'381') then
    Result := itc_CreditNote
  else
  if SameText(_Val,'875') then
    Result := itc_PartialConstructionInvoice
  else
  if SameText(_Val,'876') then
    Result := itc_PartialFinalConstructionInvoice
  else
  if SameText(_Val,'877') then
    Result := itc_FinalConstructionInvoice
  else
  //Nicht in XRechnung zugelassen, in ZUGFeRD/Factur-X aber moeglich
  if SameText(_Val,'84') then
    Result := itc_DebitnoteRelatedToFinancialAdjustments
  else
  if SameText(_Val,'261') then
    Result := itc_SelfBilledCreditNote
  else
  if SameText(_Val,'383') then
    Result := itc_DebitNote
  else
  if SameText(_Val,'386') then
    Result := itc_PrepaymentInvoice
  else
  if SameText(_Val,'457') then
    Result := itc_Cancellation
  else
    Result := itc_None;
end;

class function TXRechnungHelper.InvoiceTypeCodeToStr(_Val: TInvoiceTypeCode): String;
begin
  case _Val of
    itc_PartialInvoice: Result := '326';
    itc_CommercialInvoice: Result := '380';
    itc_CorrectedInvoice: Result := '384';
    itc_SelfbilledInvoice: Result := '389';
    itc_CreditNote: Result := '381';
    itc_PartialConstructionInvoice: Result := '875';
    itc_PartialFinalConstructionInvoice: Result := '876';
    itc_FinalConstructionInvoice: Result := '877';
    //Die folgenden Typen sind in XRechnung nicht zugelassen, kommen aber in
    //ZUGFeRD/Factur-X vor und gehen sonst beim Wiederschreiben verloren.
    itc_DebitnoteRelatedToFinancialAdjustments: Result := '84';
    itc_SelfBilledCreditNote: Result := '261';
    itc_DebitNote: Result := '383';
    itc_PrepaymentInvoice: Result := '386';
    itc_Cancellation: Result := '457';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoiceUnitCodeFromStr(
  _Val: String): TInvoiceUnitCode;
begin
  if SameText(_Val,'H87') then
    Result := iuc_piece else
  if SameText(_Val,'LS') then
    Result := iuc_flaterate else
  if SameText(_Val,'NAR') then
    Result := iuc_number_of_articles else
  if SameText(_Val,'SET') then
    Result := iuc_set else
  if SameText(_Val,'WEE') then
    Result := iuc_week else
  if SameText(_Val,'MON') then
    Result := iuc_month else
  if SameText(_Val,'DAY') then
    Result := iuc_day else
  if SameText(_Val,'TNE') then
    Result := iuc_tonne_metric_ton else
  if SameText(_Val,'MTK') then
    Result := iuc_square_metre else
  if SameText(_Val,'MTQ') then
    Result := iuc_cubic_metre else
  if SameText(_Val,'MTR') then
    Result := iuc_metre else
  if SameText(_Val,'MMK') then
    Result := iuc_square_millimetre else
  if SameText(_Val,'MMQ') then
    Result := iuc_cubic_millimetre else
  if SameText(_Val,'MMT') then
    Result := iuc_millimetre else
  if SameText(_Val,'MIN') then
    Result := iuc_minute_unit_of_time else
  if SameText(_Val,'SEC') then
    Result := iuc_second_unit_of_time else
  if SameText(_Val,'LTR') then
    Result := iuc_litre else
  if SameText(_Val,'MLT') then
    Result := iuc_millilitre else
  if SameText(_Val,'HUR') then
    Result := iuc_hour else
  if SameText(_Val,'GRM') then
    Result := iuc_gram else
  if SameText(_Val,'MGM') then
    Result := iuc_milligram else
  if SameText(_Val,'KGM') then
    Result := iuc_kilogram else
  if SameText(_Val,'KMT') then
    Result := iuc_kilometre else
  if SameText(_Val,'KWH') then
    Result := iuc_kilowatt_hour else
  if SameText(_Val,'P1') then
    Result := iuc_percent else
  if SameText(_Val,'XPK') then
    Result := iuc_packaging else
  Result := iuc_one; //C62
end;

class function TXRechnungHelper.InvoiceUnitCodeToStr(_Val: TInvoiceUnitCode): String;
begin
  //mehr Konvertierungen in Res\intf.XRechnung.unusedUnits.pas
  case _Val of
    iuc_one : Result := 'C62';
    iuc_piece : Result := 'H87';
    iuc_flaterate : Result := 'LS';
    iuc_number_of_articles : Result := 'NAR';
    iuc_set : Result := 'SET';
    iuc_week : Result := 'WEE';
    iuc_month : Result := 'MON';
    iuc_day : Result := 'DAY';
    iuc_tonne_metric_ton : Result := 'TNE';
    iuc_square_metre : Result := 'MTK';
    iuc_cubic_metre : Result := 'MTQ';
    iuc_metre : Result := 'MTR';
    iuc_square_millimetre : Result := 'MMK';
    iuc_cubic_millimetre : Result := 'MMQ';
    iuc_millimetre : Result := 'MMT';
    iuc_minute_unit_of_time : Result := 'MIN';
    iuc_second_unit_of_time : Result := 'SEC';
    iuc_millilitre : Result := 'MLT';
    iuc_litre : Result := 'LTR';
    iuc_hour : Result := 'HUR';
    iuc_kilogram : Result := 'KGM';
    iuc_gram : Result := 'GRM';
    iuc_milligram : Result := 'MGM';
    iuc_kilometre : Result := 'KMT';
    iuc_kilowatt_hour : Result := 'KWH';
    iuc_percent : Result := 'P1';
    iuc_packaging : Result := 'XPK';
  end;
end;

class function TXRechnungHelper.PercentageFromStr(_Val: String): double;
var
  fs : TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator := '.';
  Result := StrToFloatDef(_Val,0,fs);
end;

class function TXRechnungHelper.PercentageToStr(_Val: double): String;
begin
  Result := ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TXRechnungHelper.QuantityFromStr(_Val: String): double;
var
  fs : TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator := '.';
  Result := StrToFloatDef(_Val,0,fs);
end;

class function TXRechnungHelper.QuantityToStr(_Val: double): String;
begin
  Result := ReplaceText(Format('%.4f',[_Val]),',','.');
end;

class procedure TXRechnungHelper.ReadPaymentTerms(_Invoice: TInvoice;
  _PaymentTermsText: String);
var
  lPoshashtag, lPosBasis: Integer;
  lPaymentTermsList : TStringList;
  lPaymentTerm : String;
  i, lDays : Integer;
  lSkonto : double;
  lBasisbetrag : Currency;
begin
  if _PaymentTermsText = '' then
    exit;

  _Invoice.PaymentTermsType := iptt_Net;
  _Invoice.PaymentTermNetNote := _PaymentTermsText;

  if Pos('#SKONTO#',_PaymentTermsText) = 0 then
    exit;

  lPaymentTermsList := TStringList.Create;
  try
    lPaymentTermsList.Text := Trim(_PaymentTermsText);
    if lPaymentTermsList.Count = 0 then
      exit;
    for i := 0 to lPaymentTermsList.Count-1 do
    if (Pos('#SKONTO#', lPaymentTermsList[i]) = 1) then
    begin
      if _Invoice.PaymentTermsType = iptt_CashDiscount3 then
        break; //Mehr geht nicht
      _Invoice.PaymentTermsType := TInvoicePaymentTermsType(Integer(_Invoice.PaymentTermsType)+1);

      lPaymentTerm := Trim(lPaymentTermsList[i]);

      lPoshashtag := Pos('#', lPaymentTerm);
      Delete(lPaymentTerm, 1, lPoshashtag); // Entfernen des ersten '#'

      // Zerlegen der Werte
      lPoshashtag := Pos('#', lPaymentTerm);
      Delete(lPaymentTerm, 1, lPoshashtag); // Skonto entfernen
      lDays := StrToIntDef(Copy(lPaymentTerm, Pos('=', lPaymentTerm)+1, Pos('#', lPaymentTerm) - 1 - Pos('=', lPaymentTerm)), 0);

      lPoshashtag := Pos('#', lPaymentTerm);
      Delete(lPaymentTerm, 1, lPoshashtag); // Tage entfernen
      lSkonto := TXRechnungHelper.FloatFromStr(Copy(lPaymentTerm, Pos('=', lPaymentTerm) + 1, Pos('#', lPaymentTerm) - 1 - Pos('=', lPaymentTerm)));

      lPoshashtag := Pos('#', lPaymentTerm);
      Delete(lPaymentTerm, 1, lPoshashtag); // Prozent entfernen
      lPosBasis := Pos('BASISBETRAG=', lPaymentTerm);
      if lPosBasis = 1 then
      begin
        Delete(lPaymentTerm, 1, 12); // "BASISBETRAG=" entfernen
        if Length(lPaymentTerm)>0 then //# entfernen
        if lPaymentTerm[Length(lPaymentTerm)]='#' then
          Delete(lPaymentTerm,Length(lPaymentTerm),1);
        lBasisbetrag := TXRechnungHelper.AmountFromStr(lPaymentTerm);
      end
      else
        lBasisbetrag := 0;
      case _Invoice.PaymentTermsType of
        iptt_CashDiscount1 :
        begin
          _Invoice.PaymentTermCashDiscount1Days := lDays;
          _Invoice.PaymentTermCashDiscount1Percent := lSkonto;
          _Invoice.PaymentTermCashDiscount1Base := lBasisbetrag;
        end;
        iptt_CashDiscount2 :
        begin
          _Invoice.PaymentTermCashDiscount2Days := lDays;
          _Invoice.PaymentTermCashDiscount2Percent := lSkonto;
          _Invoice.PaymentTermCashDiscount2Base := lBasisbetrag;
        end;
        iptt_CashDiscount3 :
        begin
          _Invoice.PaymentTermCashDiscount3Days := lDays;
          _Invoice.PaymentTermCashDiscount3Percent := lSkonto;
          _Invoice.PaymentTermCashDiscount3Base := lBasisbetrag;
        end;
      end;
    end;
  finally
    lPaymentTermsList.Free;
  end;
end;

{ TXRechnungValidationHelper }

class function TXRechnungValidationHelper.GetXRechnungVersion(
  _Xml: IXMLDocument): TXRechnungVersion;
var
  node,node2 : IXMLNode;
begin
  Result := XRechnungVersion_Unknown;
  if _XML = nil then
    exit;
  if (SameText(_XML.DocumentElement.NodeName,'Invoice') or
      SameText(_XML.DocumentElement.NodeName,'ubl:Invoice') or
      SameText(_XML.DocumentElement.NodeName,'ns0:Invoice') or
      SameText(_XML.DocumentElement.NodeName,'CreditNote') or
      SameText(_XML.DocumentElement.NodeName,'ubl:CreditNote') or
      SameText(_XML.DocumentElement.NodeName,'ns0:CreditNote')) then
  begin
    if not (TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'cbc:CustomizationID',node) or
            TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'CustomizationID',node)) then
      exit;
    if Pos('xrechnung_3.0',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_30x_UBL
    else
    if Pos('billing:3.0',AnsiLowerCase(node.Text))>0 then
      Result := PeppolBillingVersion_30
    else
    //aeltere XRechnung-Versionen und fremde EN16931-CIUS, z.B.
    //urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3
    if Pos('urn:cen.eu:en16931:2017',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_2x_ReadingOnly;
  end else
  if (SameText(_XML.DocumentElement.NodeName,'CrossIndustryInvoice') or
      SameText(_XML.DocumentElement.NodeName,'rsm:CrossIndustryInvoice')) then
  begin
    if not (TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'rsm:ExchangedDocumentContext',node) or
            TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'ExchangedDocumentContext',node)) then
      exit;
    if not TXRechnungXMLHelper.FindChild(node,'ram:GuidelineSpecifiedDocumentContextParameter',node2) then
      exit;
    if not TXRechnungXMLHelper.FindChild(node2,'ram:ID',node) then
      exit;
    if Pos('xrechnung_3.0',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_30x_UNCEFACT
    else
    if SameText(node.Text,'urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended') then
      Result := ZUGFeRDExtendedVersion_250
    else
    if Pos('urn:cen.eu:en16931:2017',AnsiLowerCase(node.Text))>0 then
      Result := ZUGFeRDEN16931Version_250;
  end else
  if (SameText(_XML.DocumentElement.NodeName,'CrossIndustryDocument') or
      SameText(_XML.DocumentElement.NodeName,'rsm:CrossIndustryDocument')) then
  begin
    Result := ZUGFeRDExtendedVersion_1_NotSupported;
  end;
end;

//Holt die eingebettete Rechnung aus einem ZUGFeRD-/Factur-X-PDF und liefert sie
//als XML-Dokument. Die Position des Datenstroms bleibt unveraendert.
function TryLoadXmlFromPdfStream(_Stream : TStream; out _Xml : IXMLDocument) : Boolean;
var
  xmlBytes : TBytes;
  attachmentName : String;
  info : TXRechnungPdfExtractInfo;
  ms : TMemoryStream;
  savePos : Int64;
begin
  Result := false;
  _Xml := nil;
  if _Stream = nil then
    exit;
  savePos := _Stream.Position;
  try
    if not TXRechnungPdfExtractor.ExtractInvoiceFromStream(_Stream,xmlBytes,attachmentName,info) then
      exit;
    if Length(xmlBytes) = 0 then
      exit;
    ms := TMemoryStream.Create;
    try
      ms.WriteBuffer(xmlBytes[0],Length(xmlBytes));
      ms.Position := 0;
      _Xml := TXMLDocument.Create(nil);
      _Xml.LoadFromStream(ms);
      Result := true;
    finally
      ms.Free;
    end;
  finally
    _Stream.Position := savePos;
  end;
end;

class function TXRechnungValidationHelper.GetXRechnungVersion(
  const _Filename: String): TXRechnungVersion;
var
  xml : IXMLDocument;
  fs : TFileStream;
begin
  Result := XRechnungVersion_Unknown;
  if not FileExists(_Filename) then
    exit;

  //ZUGFeRD-/Factur-X-PDF: Version der eingebetteten Rechnung bestimmen
  if TXRechnungPdfExtractor.IsPdfFile(_Filename) then
  begin
    fs := TFileStream.Create(_Filename,fmOpenRead or fmShareDenyWrite);
    try
      if TryLoadXmlFromPdfStream(fs,xml) then
      try
        Result := TXRechnungValidationHelper.GetXRechnungVersion(xml);
      finally
        xml := nil;
      end;
    finally
      fs.Free;
    end;
    exit;
  end;

  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromFile(_Filename);
    Result := TXRechnungValidationHelper.GetXRechnungVersion(xml);
  finally
    xml := nil;
  end;
end;

class function TXRechnungValidationHelper.GetXRechnungVersion(
  _Stream: TStream): TXRechnungVersion;
var
  xml : IXMLDocument;
  currentStreamPosition : Int64;
begin
  Result := XRechnungVersion_Unknown;
  if (_Stream = nil) then
    exit;
  currentStreamPosition := _Stream.Position;

  //ZUGFeRD-/Factur-X-PDF: Version der eingebetteten Rechnung bestimmen
  if TXRechnungPdfExtractor.IsPdfStream(_Stream) then
  begin
    if TryLoadXmlFromPdfStream(_Stream,xml) then
    try
      Result := TXRechnungValidationHelper.GetXRechnungVersion(xml);
    finally
      xml := nil;
    end;
    _Stream.Position := currentStreamPosition;
    exit;
  end;

  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromStream(_Stream);
    _Stream.Position := currentStreamPosition;
    Result := TXRechnungValidationHelper.GetXRechnungVersion(xml);
  finally
    xml := nil;
  end;
end;

class function TXRechnungValidationHelper.GetXRechnungVersionFromString(
  const _XML: String): TXRechnungVersion;
var
  xml : IXMLDocument;
begin
  Result := XRechnungVersion_Unknown;
  if (_XML = '') then
    exit;
  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromXML(_XML);
    Result := TXRechnungValidationHelper.GetXRechnungVersion(xml);
  finally
    xml := nil;
  end;
end;

//class function TXRechnungValidationHelper.Validate(_XSDFilename, _XmlFilename: String): Boolean;
//var
//  FXMLDocument: IXMLDOMDocument2;
//  FXMLDOMSchema: IXMLDOMSchemaCollection2;
//  FXMLParserError: IXMLDOMParseError2;
//  s: string;
//  i: integer;
//begin
//  //https://en.delphipraxis.net/topic/7803-validating-an-xml-using-xsd/
//  //FileName:= '';
//  try
//    FXMLDocument := CreateOleObject('Msxml2.DOMDocument.6.0') as IXMLDomDocument2;
//    FXMLDOMSchema := CreateOleObject('Msxml2.XMLSchemaCache.6.0') as IXMLDOMSchemaCollection2;
//
//    FXMLDOMSchema.add('', _XSDFilename);
//    FXMLDocument.Async := false;
//    FXMLDocument.resolveExternals:= false;
//    FXMLDocument.validateOnParse := false;
//    FXMLDocument.setProperty('MultipleErrorMessages', true);
//    FXMLDocument.load(_XmlFilename);
//    FXMLDocument.schemas := FXMLDOMSchema;
//    FXMLParserError := FXMLDocument.validate as IXMLDOMParseError2;
//  finally
//    if (FXMLParserError.errorCode <> 0) then
//    begin
//      s:= '';
//      Result := false;
////        flk:= NewFLK_P;
////        flk.FNAME:= ReplaceFirstChar(ExtractFileName(xml_file),'V');
////        flk.FNAME_I:= ExtractFileName(xml_file);
////        with FXMLParserError.allErrors do
////          for i:= 0 to Length - 1 do
////            begin
////              with flk.PR.Add do
////                begin
////                  case Item[i].ErrorCode of
////                    -1072897535: OSHIB:= 903;
////                    -1072898028: OSHIB:= 902;
////                  end;
////                  BAS_EL:= Item[i].errorXPath;
////                  COMMENT:= StringReplace(Item[i].reason, #13#10, '', [rfReplaceAll]);
////                end;
////              s:= s + Format('ErrorCode: %d' + #13#10 + 'Reason: %s' + #13#10 +
////                             'SrcText: %s' + #13#10 + 'Line: %d' + #13#10 +
////                             'LinePos: %d' + #13#10 + 'FilePos: %d' + #13#10 +
////                             'XPath: %s', [Item[i].ErrorCode, Item[i].reason, Item[i].Srctext, Item[i].Line, Item[i].LinePos, Item[i].FilePos, Item[i].errorXPath])+ #13#10;
////            end;
////        FileName:= TempFolder + ReplaceFirstChar(ExtractFileName(xml_file),'V');
////        flk.OwnerDocument.LoadFromXML(XMLDoc.FormatXMLData(flk.OwnerDocument.XML.Text));
////        flk.OwnerDocument.SaveToFile(FileName);
//        //raise Exception.Create(ExtractFileName(xml_file) +
//        //  ExtractFileName(xsd_file) + #13#10 + s);
//    end else
//      Result := true;
//    FXMLParserError:= nil;
//    FXMLDOMSchema:= nil;
//    FXMLDocument:= nil;
//  end;
//end;

{$IFDEF ZUGFeRD_Support}
class function TZUGFeRDInvoiceAdapter.LoadFromStream(_Invoice : TInvoice;
        _Stream : TStream; out _Error : String) : Boolean;
var
  desc : TZUGFeRDInvoiceDescriptor;
begin
  Result := false;
  _Error := '';
  if _Invoice = nil then
    exit;
  if _Stream = nil then
    exit;

  desc := nil;
  try
    desc := TZUGFeRDInvoiceDescriptor.Load(_Stream);
    Result := TZUGFeRDInvoiceAdapter.LoadFromInvoiceDescriptor(_Invoice,desc,_Error);
  except
    on E:Exception do
    begin
      _Error := E.Message;
      Result := false;
    end;
  end;
  if desc <> nil then
    desc.Free;
end;

class function TZUGFeRDInvoiceAdapter.LoadFromFile(_Invoice : TInvoice;
  const _Filename : String; out _Error : String) : Boolean;
var
  stream : TFileStream;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _Filename = '' then
    exit;
  if not FileExists(_Filename) then
    exit;

  stream := TFileStream.Create(_Filename,fmOpenRead or fmShareDenyNone);
  try
    Result := TZUGFeRDInvoiceAdapter.LoadFromStream(_Invoice,stream,_Error);
  finally
    stream.Free;
  end;
end;

class function TZUGFeRDInvoiceAdapter.LoadFromXMLDocument(_Invoice: TInvoice;
  _XmlDocument: IXMLDocument; out _Error: String;
  _AdditionalContent : TZUGFeRDAdditionalContent = nil): Boolean;
var
  desc : TZUGFeRDInvoiceDescriptor;
begin
  Result := false;
  _Error := '';
  if _Invoice = nil then
    exit;
  if _XmlDocument = nil then
    exit;

  desc := nil;
  try
    desc := TZUGFeRDInvoiceDescriptor.Load(_XmlDocument);
    Result := TZUGFeRDInvoiceAdapter.LoadFromInvoiceDescriptor(_Invoice,desc,_Error);
    if _AdditionalContent <> nil then
    begin
      TZUGFeRDInvoiceAdapter.LoadAdditionalContentFromXMLDocument(_AdditionalContent,desc);
      //Der Descriptor geht in den Besitz von _AdditionalContent ueber
      _AdditionalContent.ZUGFeRDInvoice := desc;
      desc := nil;
    end;
  except
    on E:Exception do
    begin
      _Error := E.Message;
      Result := false;
    end;
  end;
  if desc <> nil then
    desc.Free;
end;

class function TZUGFeRDInvoiceAdapter.LoadFromXMLStr(_Invoice : TInvoice;
  const _XML : String; out _Error : String) : Boolean;
var
  stream : TStringStream;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _XML = '' then
    exit;

  stream := TStringStream.Create(_XML,TEncoding.UTF8);
  try
    Result := TZUGFeRDInvoiceAdapter.LoadFromStream(_Invoice,stream,_Error);
  finally
    stream.Free;
  end;
end;

class function TZUGFeRDInvoiceAdapter.CodeFromEnum<TEnum>(
  const _Value : ZUGFeRDNullable<TEnum>) : String;
begin
  Result := TEnumExtensions<TEnum>.EnumToString(_Value);
  //Codes ausserhalb der jeweiligen Codeliste liefert die ZUGFeRD-Bibliothek als
  //'Unknown'. Sie duerfen nicht als echter Code weitergereicht werden.
  if SameText(Result,'Unknown') then
    Result := '';
end;

class function TZUGFeRDInvoiceAdapter.UnitCodeFrom(
  const _Value : ZUGFeRDNullable<TZUGFeRDQuantityCodes>) : TInvoiceUnitCode;
begin
  Result := TXRechnungHelper.InvoiceUnitCodeFromStr(CodeFromEnum<TZUGFeRDQuantityCodes>(_Value));
end;

class function TZUGFeRDInvoiceAdapter.TaxCategoryFrom(
  const _Value : ZUGFeRDNullable<TZUGFeRDTaxCategoryCodes>) : TInvoiceDutyTaxFeeCategoryCode;
begin
  Result := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeFromStr(CodeFromEnum<TZUGFeRDTaxCategoryCodes>(_Value));
end;

class procedure TZUGFeRDInvoiceAdapter.TransferAddress(_Target : TInvoiceAddress;
  _Source : TZUGFeRDParty);
begin
  if (_Target = nil) or (_Source = nil) then
    exit;

  //Die ZUGFeRD-Bibliothek legt ram:LineOne in ContactName ab und ram:LineTwo in Street,
  //aber nur dann, wenn beide Zeilen belegt sind - sonst steht LineOne in Street.
  if _Source.ContactName = '' then
  begin
    _Target.StreetName := _Source.Street;
    _Target.AdditionalStreetName := '';
  end else
  begin
    _Target.StreetName := _Source.ContactName;
    _Target.AdditionalStreetName := _Source.Street;
  end;
  _Target.City := _Source.City;
  _Target.PostalZone := _Source.Postcode;
  _Target.CountrySubentity := _Source.CountrySubdivisionName;
  _Target.AddressLine := _Source.AddressLine3;
  _Target.CountryCode := CodeFromEnum<TZUGFeRDCountryCodes>(_Source.Country);
end;

class procedure TZUGFeRDInvoiceAdapter.TransferParty(_Target : TInvoiceAccountingParty;
  _Source : TZUGFeRDParty; _Contact : TZUGFeRDContact;
  _TaxRegistrations : TObjectList<TZUGFeRDTaxRegistration>;
  _ElectronicAddress : TZUGFeRDElectronicAddress);
var
  i : Integer;
begin
  if _Target = nil then
    exit;

  if _Source <> nil then
  begin
    if _Source.SpecifiedLegalOrganization <> nil then
    begin
      _Target.Name := _Source.SpecifiedLegalOrganization.TradingBusinessName; //BT-28, BT-45
      _Target.CompanyID := _Source.SpecifiedLegalOrganization.ID.ID; //BT-30, BT-47
    end;
    _Target.RegistrationName := _Source.Name; //BT-27, BT-44
    TransferAddress(_Target.Address,_Source);
    _Target.IdentifierSellerBuyer := _Source.ID.ID; //BT-29, BT-46
    //BT-29-0/BT-46-0 mit Schema BT-29-1/BT-46-1, in CII ram:GlobalID
    _Target.GlobalIdentifierSellerBuyer := _Source.GlobalID.ID;
    _Target.GlobalIdentifierSellerBuyerSchemeID :=
      CodeFromEnum<TZUGFeRDGlobalIDSchemeIdentifiers>(_Source.GlobalID.SchemeID);
    _Target.AdditionalLegalInformationSeller := _Source.Description; //BT-33, nur Verkaeufer
  end;

  if _TaxRegistrations <> nil then
  for i := 0 to _TaxRegistrations.Count-1 do
  if _TaxRegistrations[i].SchemeID = TZUGFeRDTaxRegistrationSchemeID.VA then
    _Target.VATCompanyID := _TaxRegistrations[i].No //BT-31, BT-48
  else
  if _TaxRegistrations[i].SchemeID = TZUGFeRDTaxRegistrationSchemeID.FC then
    _Target.VATCompanyNumber := _TaxRegistrations[i].No; //BT-32

  if _Contact <> nil then
  begin
    _Target.ContactName := _Contact.Name; //BT-41, BT-56
    _Target.ContactTelephone := _Contact.PhoneNo; //BT-42, BT-57
    _Target.ContactElectronicMail := _Contact.EmailAddress; //BT-43, BT-58
  end;

  if _ElectronicAddress <> nil then
  begin
    _Target.ElectronicAddressSellerBuyer := _ElectronicAddress.Address; //BT-34, BT-49
    //Das Schema ist in der ZUGFeRD-Bibliothek nicht optional und steht ohne
    //gelesene Adresse auf dem ersten Enum-Wert - dann darf es nicht uebernommen werden.
    if _ElectronicAddress.Address <> '' then
      _Target.ElectronicAddressSellerBuyerSchemeID := //BT-34-1, BT-49-1
        CodeFromEnum<TZUGFeRDElectronicAddressSchemeIdentifiers>(
          ZUGFeRDNullable<TZUGFeRDElectronicAddressSchemeIdentifiers>.Create(_ElectronicAddress.ElectronicAddressSchemeID));
  end;
end;

class procedure TZUGFeRDInvoiceAdapter.TransferAllowanceCharge(
  _Target : TInvoiceAllowanceCharge; _Source : TZUGFeRDAbstractTradeAllowanceCharge);
begin
  if (_Target = nil) or (_Source = nil) then
    exit;

  _Target.ChargeIndicator := _Source.ChargeIndicator;
  _Target.ReasonCodeAllowance := iacic_None;
  _Target.ReasonCodeCharge := issdc_None;
  //Unterschieden wird ueber die tatsaechliche Klasse und nicht ueber ChargeIndicator -
  //ein harter Cast auf die falsche Klasse waere sonst moeglich. Fehlt der Grundcode,
  //liefert CodeFromEnum einen Leerstring und daraus wird iacic_None/issdc_None.
  if _Source is TZUGFeRDTradeCharge then
    _Target.ReasonCodeCharge := TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeFromStr(
      CodeFromEnum<TZUGFeRDChargeReasonCodes>(TZUGFeRDTradeCharge(_Source).ReasonCode))
  else
  if _Source is TZUGFeRDTradeAllowance then
    _Target.ReasonCodeAllowance := TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeFromStr(
      CodeFromEnum<TZUGFeRDAllowanceReasonCodes>(TZUGFeRDTradeAllowance(_Source).ReasonCode));
  _Target.Reason := _Source.Reason;
  _Target.BaseAmount := _Source.BasisAmount.GetValueOrDefault(0);
  _Target.MultiplierFactorNumeric := _Source.ChargePercentage.GetValueOrDefault(0);
  _Target.Amount := _Source.ActualAmount;
  //Auf Positionsebene (BG-27/BG-28) gibt es keine eigene Umsatzsteuerangabe, die
  //CategoryTradeTax bleibt dort leer und liefert 0 bzw. idtfcc_None.
  if _Source.Tax <> nil then
  begin
    _Target.TaxPercent := _Source.Tax.Percent;
    _Target.TaxCategory := TaxCategoryFrom(_Source.Tax.CategoryCode);
  end else
  begin
    _Target.TaxPercent := 0;
    _Target.TaxCategory := idtfcc_None;
  end;
end;

class procedure TZUGFeRDInvoiceAdapter.TransferPaymentMeans(_Invoice : TInvoice;
  _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor);
var
  i : Integer;
  lPaymentMeansCode : TInvoicePaymentMeansCode;
  lPaymentMeansInformation : String;
begin
  _Invoice.PaymentID := _InvoiceDescriptor.PaymentReference; //BT-83

  if _InvoiceDescriptor.PaymentMeans = nil then
    exit;

  _Invoice.AccountingSupplierParty.BankAssignedCreditorIdentifier :=
    _InvoiceDescriptor.PaymentMeans.SEPACreditorIdentifier; //BT-90
  _Invoice.PaymentMandateID := _InvoiceDescriptor.PaymentMeans.SEPAMandateReference; //BT-89

  //Der Zahlungsweg wird ueber die UNTDID-4461-Kennung uebernommen, damit alle Codes
  //ankommen, die das Datenmodell kennt - nicht nur eine handverlesene Auswahl.
  lPaymentMeansCode := TXRechnungHelper.InvoicePaymentMeansCodeFromStr(
    CodeFromEnum<TZUGFeRDPaymentMeansTypeCodes>(_InvoiceDescriptor.PaymentMeans.TypeCode));
  if lPaymentMeansCode = ipmc_NotImplemented then
    exit;

  lPaymentMeansInformation := _InvoiceDescriptor.PaymentMeans.Information; //BT-82

  //Lastschrift (BG-19): das belastete Konto ist das des Kaeufers
  if lPaymentMeansCode in [ipmc_SEPADirectDebit,ipmc_DirectDebit] then
  begin
    for i := 0 to _InvoiceDescriptor.DebitorBankAccounts.Count-1 do
    with _Invoice.PaymentTypes.AddPaymentType do
    begin
      PaymentMeansCode := lPaymentMeansCode;
      PaymentMeansInformation := lPaymentMeansInformation;
      FinancialAccount := _InvoiceDescriptor.DebitorBankAccounts[i].IBAN; //BT-91
      FinancialAccountName := _InvoiceDescriptor.DebitorBankAccounts[i].Name;
      FinancialInstitutionBranch := _InvoiceDescriptor.DebitorBankAccounts[i].BIC;
    end;
    if _InvoiceDescriptor.DebitorBankAccounts.Count > 0 then
      exit;
  end else
  //Kartenzahlung (BG-18): Kartennummer und Karteninhaber statt Bankverbindung
  if (lPaymentMeansCode in [ipmc_BankCard,ipmc_CreditCard,ipmc_DebitCard]) and
     (_InvoiceDescriptor.PaymentMeans.FinancialCard <> nil) then
  begin
    with _Invoice.PaymentTypes.AddPaymentType do
    begin
      PaymentMeansCode := lPaymentMeansCode;
      PaymentMeansInformation := lPaymentMeansInformation;
      FinancialAccount := _InvoiceDescriptor.PaymentMeans.FinancialCard.Id; //BT-87
      FinancialAccountName := _InvoiceDescriptor.PaymentMeans.FinancialCard.CardholderName; //BT-88
    end;
    exit;
  end else
  begin
    //Ueberweisung (BG-17) und alles Weitere: das begueenstigte Konto ist das des Verkaeufers
    for i := 0 to _InvoiceDescriptor.CreditorBankAccounts.Count-1 do
    with _Invoice.PaymentTypes.AddPaymentType do
    begin
      PaymentMeansCode := lPaymentMeansCode;
      PaymentMeansInformation := lPaymentMeansInformation;
      FinancialAccount := _InvoiceDescriptor.CreditorBankAccounts[i].IBAN; //BT-84
      FinancialAccountName := _InvoiceDescriptor.CreditorBankAccounts[i].Name; //BT-85
      FinancialInstitutionBranch := _InvoiceDescriptor.CreditorBankAccounts[i].BIC; //BT-86
    end;
    if _InvoiceDescriptor.CreditorBankAccounts.Count > 0 then
      exit;
  end;

  //Zahlungswege ohne Kontoangabe (z.B. 1 = keine Angabe, 10 = Bar, 68 = Online)
  with _Invoice.PaymentTypes.AddPaymentType do
  begin
    PaymentMeansCode := lPaymentMeansCode;
    PaymentMeansInformation := lPaymentMeansInformation;
  end;
end;

class procedure TZUGFeRDInvoiceAdapter.TransferPaymentTerms(_Invoice : TInvoice;
  _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor);
var
  i : Integer;
begin
  _Invoice.PaymentTermsType := iptt_None;

  //Faelligkeitsdatum BT-9 aus der ersten Zahlungsbedingung ohne Skontosatz
  _Invoice.InvoiceDueDate := 0;
  for i := 0 to _InvoiceDescriptor.PaymentTermsList.Count-1 do
  if _InvoiceDescriptor.PaymentTermsList[i].Percentage.GetValueOrDefault(0) = 0.0 then
  begin
    _Invoice.InvoiceDueDate := _InvoiceDescriptor.PaymentTermsList[i].DueDate.GetValueOrDefault(0);
    break;
  end;

  for i := 0 to _InvoiceDescriptor.PaymentTermsList.Count-1 do
  begin
    //Sonderfall, Skonto XRechnung-Format in ZUGFeRD, eigentlich nicht erlaubt
    if _InvoiceDescriptor.PaymentTermsList.Count = 1 then
    if (_InvoiceDescriptor.PaymentTermsList[i].DueDays.GetValueOrDefault > 0) and
       (_InvoiceDescriptor.PaymentTermsList[i].DueDate.GetValueOrDefault > 0) and
       (_InvoiceDescriptor.PaymentTermsList[i].DueDate.GetValueOrDefault <> Trunc(_Invoice.InvoiceIssueDate)+_InvoiceDescriptor.PaymentTermsList[i].DueDays.GetValueOrDefault) then
    begin
      _Invoice.InvoiceDueDate := _InvoiceDescriptor.PaymentTermsList[i].DueDate;
      _Invoice.PaymentTermsType := iptt_CashDiscount1;
      _Invoice.PaymentTermCashDiscount1Days := Trunc(_InvoiceDescriptor.PaymentTermsList[i].DueDays.Value);
      _Invoice.PaymentTermCashDiscount1Percent := _InvoiceDescriptor.PaymentTermsList[i].Percentage;
      _Invoice.PaymentTermCashDiscount1Base := _InvoiceDescriptor.PaymentTermsList[i].BaseAmount;
      _Invoice.PaymentTermCashDiscount1ActualAmount := _InvoiceDescriptor.PaymentTermsList[i].ActualAmount;
      break;
    end;

    if (not _InvoiceDescriptor.PaymentTermsList[i].Percentage.HasValue) and
       (not _InvoiceDescriptor.PaymentTermsList[i].BaseAmount.HasValue) then
    begin
      if _Invoice.PaymentTermsType = iptt_None then
        _Invoice.PaymentTermsType := iptt_Net;
      if _InvoiceDescriptor.PaymentTermsList[i].DueDate.GetValueOrDefault > 0 then
        _Invoice.InvoiceDueDate := _InvoiceDescriptor.PaymentTermsList[i].DueDate
      else
      if _InvoiceDescriptor.PaymentTermsList[i].DueDays.HasValue then
        _Invoice.InvoiceDueDate := Trunc(_Invoice.InvoiceIssueDate)+ Trunc(_InvoiceDescriptor.PaymentTermsList[i].DueDays.Value);
      _Invoice.PaymentTermNetNote := _InvoiceDescriptor.PaymentTermsList[i].Description;
    end else
    if (_Invoice.PaymentTermsType in [iptt_None,iptt_Net]) then
    begin
      _Invoice.PaymentTermsType := iptt_CashDiscount1;
      if _InvoiceDescriptor.PaymentTermsList[i].DueDate.HasValue then
        _Invoice.PaymentTermCashDiscount1Days := DaysBetween(_Invoice.InvoiceIssueDate,_InvoiceDescriptor.PaymentTermsList[i].DueDate)
      else
      if _InvoiceDescriptor.PaymentTermsList[i].DueDays.HasValue then
        _Invoice.PaymentTermCashDiscount1Days := Trunc(_InvoiceDescriptor.PaymentTermsList[i].DueDays.Value);
      _Invoice.PaymentTermCashDiscount1Percent := _InvoiceDescriptor.PaymentTermsList[i].Percentage;
      _Invoice.PaymentTermCashDiscount1Base := _InvoiceDescriptor.PaymentTermsList[i].BaseAmount;
      _Invoice.PaymentTermCashDiscount1ActualAmount := _InvoiceDescriptor.PaymentTermsList[i].ActualAmount;
    end else
    if _Invoice.PaymentTermsType = iptt_CashDiscount1 then
    begin
      _Invoice.PaymentTermsType := iptt_CashDiscount2;
      if _InvoiceDescriptor.PaymentTermsList[i].DueDate.HasValue then
        _Invoice.PaymentTermCashDiscount2Days := DaysBetween(_Invoice.InvoiceIssueDate,_InvoiceDescriptor.PaymentTermsList[i].DueDate)
      else
      if _InvoiceDescriptor.PaymentTermsList[i].DueDays.HasValue then
        _Invoice.PaymentTermCashDiscount2Days := Trunc(_InvoiceDescriptor.PaymentTermsList[i].DueDays.Value);
      _Invoice.PaymentTermCashDiscount2Percent := _InvoiceDescriptor.PaymentTermsList[i].Percentage;
      _Invoice.PaymentTermCashDiscount2Base := _InvoiceDescriptor.PaymentTermsList[i].BaseAmount;
      _Invoice.PaymentTermCashDiscount2ActualAmount := _InvoiceDescriptor.PaymentTermsList[i].ActualAmount;
    end else
    if _Invoice.PaymentTermsType = iptt_CashDiscount2 then
    begin
      _Invoice.PaymentTermsType := iptt_CashDiscount3;
      if _InvoiceDescriptor.PaymentTermsList[i].DueDate.HasValue then
        _Invoice.PaymentTermCashDiscount3Days := DaysBetween(_Invoice.InvoiceIssueDate,_InvoiceDescriptor.PaymentTermsList[i].DueDate)
      else
      if _InvoiceDescriptor.PaymentTermsList[i].DueDays.HasValue then
        _Invoice.PaymentTermCashDiscount3Days := Trunc(_InvoiceDescriptor.PaymentTermsList[i].DueDays.Value);
      _Invoice.PaymentTermCashDiscount3Percent := _InvoiceDescriptor.PaymentTermsList[i].Percentage;
      _Invoice.PaymentTermCashDiscount3Base := _InvoiceDescriptor.PaymentTermsList[i].BaseAmount;
      _Invoice.PaymentTermCashDiscount3ActualAmount := _InvoiceDescriptor.PaymentTermsList[i].ActualAmount;
    end;
  end;
end;

class procedure TZUGFeRDInvoiceAdapter.TransferInvoiceLine(_InvoiceLine : TInvoiceLine;
  _TradeLineItem : TZUGFeRDTradeLineItem);
var
  j : Integer;
  firstDiscount : Boolean;
begin
  if (_InvoiceLine = nil) or (_TradeLineItem = nil) then
    exit;

  //Nur der erste Rabatt einer Position passt in BT-147, alle weiteren werden zu BG-27.
  //Der Zaehler gehoert je Position zurueckgesetzt, sonst erhaelt nur die erste Position
  //der Rechnung ihren Bruttopreis-Rabatt.
  firstDiscount := true;

  if _TradeLineItem.AssociatedDocument <> nil then
  begin
    _InvoiceLine.ID := _TradeLineItem.AssociatedDocument.LineID; //BT-126
    for j := 0 to _TradeLineItem.AssociatedDocument.Notes.Count-1 do
    begin
      if _InvoiceLine.Note <> '' then
        _InvoiceLine.Note := _InvoiceLine.Note + #13#10;
      _InvoiceLine.Note := _InvoiceLine.Note + _TradeLineItem.AssociatedDocument.Notes[j].Content; //BT-127
    end;
  end;
  if _TradeLineItem.GlobalID.ID <> '' then
  if _TradeLineItem.GlobalID.SchemeID.GetValueOrDefault(TZUGFeRDGlobalIDSchemeIdentifiers.Unknown) = EAN then
    _InvoiceLine.GlobalID_EAN_GTIN := _TradeLineItem.GlobalID.ID; //BT-157
  _InvoiceLine.Name := _TradeLineItem.Name; //BT-153
  _InvoiceLine.Description := _TradeLineItem.Description; //BT-154
  _InvoiceLine.Quantity := _TradeLineItem.BilledQuantity; //BT-129
  _InvoiceLine.UnitCode := UnitCodeFrom(_TradeLineItem.UnitCode); //BT-130
  _InvoiceLine.SellersItemIdentification := _TradeLineItem.SellerAssignedID; //BT-155
  _InvoiceLine.BuyersItemIdentification := _TradeLineItem.BuyerAssignedID; //BT-156
  if _TradeLineItem.BuyerOrderReferencedDocument <> nil then
  begin
    _InvoiceLine.OrderNumber := _TradeLineItem.BuyerOrderReferencedDocument.ID; //BT-X-21
    _InvoiceLine.OrderLineReference := _TradeLineItem.BuyerOrderReferencedDocument.LineID; //BT-132
  end;
  if _TradeLineItem.ReceivableSpecifiedTradeAccountingAccounts.Count > 0 then
    _InvoiceLine.BuyerAccountingReference := _TradeLineItem.ReceivableSpecifiedTradeAccountingAccounts.First.TradeAccountID; //BT-133
  _InvoiceLine.TaxPercent := _TradeLineItem.TaxPercent; //BT-152
  _InvoiceLine.TaxCategory := TaxCategoryFrom(_TradeLineItem.TaxCategoryCode); //BT-151
  _InvoiceLine.TaxExemptionReason := _TradeLineItem.TaxExemptionReason;
  _InvoiceLine.GrossPriceAmount := _TradeLineItem.GrossUnitPrice.GetValueOrDefault(0); //BT-148

  for j := 0 to _TradeLineItem.TradeAllowanceCharges.Count-1 do
  begin
    //wegen XRechnung UBL nur ein Item moeglich mit ChargeIndicator = false
    //weitere Felder aus TradeAllowanceCharge werden nach _InvoiceLine.AllowanceCharges
    //transferiert
    //z.B. liefern manche Lieferanten Rohstoffzuschlaege an dieser Stelle
    if (_TradeLineItem.TradeAllowanceCharges[j].ChargeIndicator = false) and firstDiscount then
    begin
      firstDiscount := false;
      _InvoiceLine.DiscountOnTheGrossPrice := _TradeLineItem.TradeAllowanceCharges[j].ActualAmount; //BT-147
    end else
      TransferAllowanceCharge(_InvoiceLine.AllowanceCharges.AddAllowanceCharge,
                              _TradeLineItem.TradeAllowanceCharges[j]);
  end;

  if _TradeLineItem.BillingPeriodStart.HasValue then
    _InvoiceLine.InvoiceLinePeriodStartDate := _TradeLineItem.BillingPeriodStart; //BT-134
  if _TradeLineItem.BillingPeriodEnd.HasValue then
    _InvoiceLine.InvoiceLinePeriodEndDate := _TradeLineItem.BillingPeriodEnd; //BT-135
  _InvoiceLine.NetPriceAmount := _TradeLineItem.NetUnitPrice.GetValueOrDefault(0); //BT-146
  _InvoiceLine.BaseQuantity := _TradeLineItem.NetQuantity.GetValueOrDefault(0); //BT-149
  //BT-150 gehoert zur Preiseinheit und hat eine eigene Mengeneinheit; ist sie nicht
  //angegeben, gilt die der abgerechneten Menge.
  if _TradeLineItem.NetUnitCode.HasValue then
    _InvoiceLine.BaseQuantityUnitCode := UnitCodeFrom(_TradeLineItem.NetUnitCode)
  else
    _InvoiceLine.BaseQuantityUnitCode := UnitCodeFrom(_TradeLineItem.UnitCode);
  _InvoiceLine.LineAmount := _TradeLineItem.LineTotalAmount.GetValueOrDefault(0); //BT-131

  for j := 0 to _TradeLineItem.SpecifiedTradeAllowanceCharges.Count-1 do
    TransferAllowanceCharge(_InvoiceLine.AllowanceCharges.AddAllowanceCharge,
                            _TradeLineItem.SpecifiedTradeAllowanceCharges[j]);

  for j := 0 to _TradeLineItem.ApplicableProductCharacteristics.Count-1 do
  with _InvoiceLine.ItemAttributes.AddItemAttribute do
  begin
    Name := _TradeLineItem.ApplicableProductCharacteristics[j].Description; //BT-160
    Value := _TradeLineItem.ApplicableProductCharacteristics[j].Value; //BT-161
  end;

  for j := 0 to _TradeLineItem.DesignedProductClassifications.Count-1 do
  with _InvoiceLine.ItemClassifications.AddItemClassification do
  begin
    ClassCode := _TradeLineItem.DesignedProductClassifications[j].ClassCode; //BT-158
    ListID := CodeFromEnum<TZUGFeRDDesignatedProductClassificationClassCodes>(
                _TradeLineItem.DesignedProductClassifications[j].ListID); //BT-158-1
    ListVersionID := _TradeLineItem.DesignedProductClassifications[j].ListVersionID; //BT-158-2
    ClassificationName := _TradeLineItem.DesignedProductClassifications[j].ClassName_; //nur EXTENDED
  end;

  _InvoiceLine.OriginTradeCountry := CodeFromEnum<TZUGFeRDCountryCodes>(_TradeLineItem.OriginTradeCountry); //BT-159
end;

class function TZUGFeRDInvoiceAdapter.LoadFromInvoiceDescriptor(
  _Invoice: TInvoice; _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  out _Error : String) : Boolean;
var
  i : Integer;
  lAttachment : TInvoiceAttachment;
begin
  Result := false;
  _Error := '';
  if _Invoice = nil then
    exit;
  if _InvoiceDescriptor = nil then
    exit;

  try
    if _InvoiceDescriptor.BusinessProcess <> '' then
      _Invoice.ProfileID := _InvoiceDescriptor.BusinessProcess; //BT-23
    _Invoice.InvoiceNumber := _InvoiceDescriptor.InvoiceNo; //BT-1
    _Invoice.InvoiceIssueDate := _InvoiceDescriptor.InvoiceDate.GetValueOrDefault(0); //BT-2
    _Invoice.InvoicePeriodStartDate := _InvoiceDescriptor.BillingPeriodStart.GetValueOrDefault(0); //BT-73
    _Invoice.InvoicePeriodEndDate := _InvoiceDescriptor.BillingPeriodEnd.GetValueOrDefault(0); //BT-74
    _Invoice.InvoiceTypeCode := TXRechnungHelper.InvoiceTypeCodeFromStr( //BT-3
      CodeFromEnum<TZUGFeRDInvoiceType>(ZUGFeRDNullable<TZUGFeRDInvoiceType>.Create(_InvoiceDescriptor.Type_)));
    _Invoice.InvoiceCurrencyCode := CodeFromEnum<TZUGFeRDCurrencyCodes>( //BT-5
      ZUGFeRDNullable<TZUGFeRDCurrencyCodes>.Create(_InvoiceDescriptor.Currency));
    _Invoice.TaxCurrencyCode := _Invoice.InvoiceCurrencyCode; //BT-6, die ZUGFeRD-Bibliothek fuehrt keine eigene Steuerwaehrung
    _Invoice.BuyerReference := _InvoiceDescriptor.ReferenceOrderNo; //BT-10

    for i := 0 to _InvoiceDescriptor.Notes.Count-1 do
    with _Invoice.Notes.AddNote do
    begin
      Content := _InvoiceDescriptor.Notes[i].Content; //BT-22
      SubjectCode := TXRechnungHelper.InvoiceNoteSubjectCodeFromStr( //BT-21
        CodeFromEnum<TZUGFeRDSubjectCodes>(_InvoiceDescriptor.Notes[i].SubjectCode));
    end;

    if _InvoiceDescriptor.SellerOrderReferencedDocument <> nil then
      _Invoice.SellerOrderReference := _InvoiceDescriptor.SellerOrderReferencedDocument.ID; //BT-14
    _Invoice.PurchaseOrderReference := _InvoiceDescriptor.OrderNo; //BT-13
    if _InvoiceDescriptor.SpecifiedProcuringProject <> nil then
      _Invoice.ProjectReference := _InvoiceDescriptor.SpecifiedProcuringProject.ID; //BT-11
    //BT-15 (Empfangsbestaetigung) kennt die ZUGFeRD-Bibliothek nicht
    if _InvoiceDescriptor.ContractReferencedDocument <> nil then
      _Invoice.ContractDocumentReference := _InvoiceDescriptor.ContractReferencedDocument.ID; //BT-12
    if _InvoiceDescriptor.DespatchAdviceReferencedDocument <> nil then
    begin
      _Invoice.DeliveryReceiptNumber := _InvoiceDescriptor.DespatchAdviceReferencedDocument.ID; //BT-16
      _Invoice.DeliveryReceiptDate := _InvoiceDescriptor.DespatchAdviceReferencedDocument.IssueDateTime.GetValueOrDefault(0); //BT-X-200
    end;
    if _InvoiceDescriptor.DeliveryNoteReferencedDocument <> nil then
    begin
      _Invoice.DeliveryReceiptNumberExtended := _InvoiceDescriptor.DeliveryNoteReferencedDocument.ID; //BT-X-202
      _Invoice.DeliveryReceiptDateExtended := _InvoiceDescriptor.DeliveryNoteReferencedDocument.IssueDateTime.GetValueOrDefault(0); //BT-X-203
    end;
    if _InvoiceDescriptor.ReceivableSpecifiedTradeAccountingAccounts.Count > 0 then
      _Invoice.BuyerAccountingReference := _InvoiceDescriptor.ReceivableSpecifiedTradeAccountingAccounts[0].TradeAccountID; //BT-19

    TransferParty(_Invoice.AccountingSupplierParty,_InvoiceDescriptor.Seller,
      _InvoiceDescriptor.SellerContact,_InvoiceDescriptor.SellerTaxRegistration,
      _InvoiceDescriptor.SellerElectronicAddress);
    TransferParty(_Invoice.AccountingCustomerParty,_InvoiceDescriptor.Buyer,
      _InvoiceDescriptor.BuyerContact,_InvoiceDescriptor.BuyerTaxRegistration,
      _InvoiceDescriptor.BuyerElectronicAddress);
    //BT-33 gibt es nur beim Verkaeufer
    _Invoice.AccountingCustomerParty.AdditionalLegalInformationSeller := '';

    if _InvoiceDescriptor.ShipTo <> nil then
    begin
      _Invoice.DeliveryInformation.Name := _InvoiceDescriptor.ShipTo.Name; //BT-70
      TransferAddress(_Invoice.DeliveryInformation.Address,_InvoiceDescriptor.ShipTo);
      //BT-71 steht in CII als ram:ID der ShipToTradeParty, ersatzweise als ram:GlobalID
      if _InvoiceDescriptor.ShipTo.ID.ID <> '' then
      begin
        _Invoice.DeliveryInformation.LocationIdentifier := _InvoiceDescriptor.ShipTo.ID.ID;
        _Invoice.DeliveryInformation.LocationIdentifierSchemeID :=
          CodeFromEnum<TZUGFeRDGlobalIDSchemeIdentifiers>(_InvoiceDescriptor.ShipTo.ID.SchemeID);
      end else
      begin
        _Invoice.DeliveryInformation.LocationIdentifier := _InvoiceDescriptor.ShipTo.GlobalID.ID;
        _Invoice.DeliveryInformation.LocationIdentifierSchemeID :=
          CodeFromEnum<TZUGFeRDGlobalIDSchemeIdentifiers>(_InvoiceDescriptor.ShipTo.GlobalID.SchemeID);
      end;
    end;
    _Invoice.DeliveryInformation.ActualDeliveryDate := _InvoiceDescriptor.ActualDeliveryDate.GetValueOrDefault(0); //BT-72

    TransferPaymentMeans(_Invoice,_InvoiceDescriptor);
    TransferPaymentTerms(_Invoice,_InvoiceDescriptor);

    for i := 0 to _InvoiceDescriptor.TradeLineItems.Count-1 do
      TransferInvoiceLine(_Invoice.InvoiceLines.AddInvoiceLine,_InvoiceDescriptor.TradeLineItems[i]);

    for i := 0 to _InvoiceDescriptor.AdditionalReferencedDocuments.Count-1 do
    begin
      lAttachment := TInvoiceAttachment.Create(iat_application_None);
      lAttachment.AttachmentType := TInvoiceAttachmentTypeHelper.GetTypeFromFilename(
        _InvoiceDescriptor.AdditionalReferencedDocuments[i].Filename);
      lAttachment.ID := _InvoiceDescriptor.AdditionalReferencedDocuments[i].ID; //BT-122
      lAttachment.DocumentDescription := _InvoiceDescriptor.AdditionalReferencedDocuments[i].Name; //BT-123
      lAttachment.Filename := _InvoiceDescriptor.AdditionalReferencedDocuments[i].Filename; //BT-125-2
      lAttachment.TypeCode := TXRechnungHelper.InvoiceAttachmentTypeCodeFromStr(
        CodeFromEnum<TZUGFeRDAdditionalReferencedDocumentTypeCode>(_InvoiceDescriptor.AdditionalReferencedDocuments[i].TypeCode));
      lAttachment.ExternalReference := _InvoiceDescriptor.AdditionalReferencedDocuments[i].URIID; //BT-124
      if _InvoiceDescriptor.AdditionalReferencedDocuments[i].AttachmentBinaryObject <> nil then
      begin
        lAttachment.Data.LoadFromStream(_InvoiceDescriptor.AdditionalReferencedDocuments[i].AttachmentBinaryObject);
        _InvoiceDescriptor.AdditionalReferencedDocuments[i].AttachmentBinaryObject.Position := 0;
      end;
      _Invoice.Attachments.Add(lAttachment);
    end;

    for i := 0 to _InvoiceDescriptor.TradeAllowanceCharges.Count-1 do
      TransferAllowanceCharge(_Invoice.AllowanceCharges.AddAllowanceCharge,
                              _InvoiceDescriptor.TradeAllowanceCharges[i]);

    //Achtung, CII-Format <= v2.2 maximal ein Element erlaubt, UBL-Format beliebig viele
    for i := 0 to _InvoiceDescriptor.InvoiceReferencedDocuments.Count-1 do
    if (_InvoiceDescriptor.InvoiceReferencedDocuments[i].ID <> '') and
       (_InvoiceDescriptor.InvoiceReferencedDocuments[i].IssueDateTime.GetValueOrDefault > 100) then
    with _Invoice.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
    begin
      ID := _InvoiceDescriptor.InvoiceReferencedDocuments[i].ID; //BT-25
      IssueDate := _InvoiceDescriptor.InvoiceReferencedDocuments[i].IssueDateTime.GetValueOrDefault(0); //BT-26
    end;

    _Invoice.TaxAmountTotal := _InvoiceDescriptor.TaxTotalAmount.GetValueOrDefault(0); //BT-110
    for i := 0 to _InvoiceDescriptor.Taxes.Count-1 do
    with _Invoice.TaxAmountSubtotals.AddTaxAmount do
    begin
      TaxableAmount := _InvoiceDescriptor.Taxes[i].BasisAmount; //BT-116
      TaxAmount := _InvoiceDescriptor.Taxes[i].TaxAmount; //BT-117
      TaxPercent := _InvoiceDescriptor.Taxes[i].Percent; //BT-119
      TaxCategory := TaxCategoryFrom(_InvoiceDescriptor.Taxes[i].CategoryCode); //BT-118
      TaxExemptionReason := _InvoiceDescriptor.Taxes[i].ExemptionReason; //BT-120
    end;

    _Invoice.LineAmount := _InvoiceDescriptor.LineTotalAmount.GetValueOrDefault(0); //BT-106
    _Invoice.TaxExclusiveAmount := _InvoiceDescriptor.TaxBasisAmount.GetValueOrDefault(0); //BT-109
    _Invoice.TaxInclusiveAmount := _InvoiceDescriptor.GrandTotalAmount.GetValueOrDefault(0); //BT-112
    _Invoice.AllowanceTotalAmount := _InvoiceDescriptor.AllowanceTotalAmount.GetValueOrDefault(0); //BT-107
    _Invoice.ChargeTotalAmount := _InvoiceDescriptor.ChargeTotalAmount.GetValueOrDefault(0); //BT-108
    _Invoice.PrepaidAmount := _InvoiceDescriptor.TotalPrepaidAmount.GetValueOrDefault(0); //BT-113
    _Invoice.PayableRoundingAmount := _InvoiceDescriptor.RoundingAmount.GetValueOrDefault(0); //BT-114
    _Invoice.PayableAmount := _InvoiceDescriptor.DuePayableAmount.GetValueOrDefault(0); //BT-115
    Result := True;
  except
    on E:Exception do
    begin
      _Error := E.Message;
      Result := false;
    end;
  end;
end;

class function TZUGFeRDInvoiceAdapter.LoadAdditionalContentFromXMLDocument(
  _AdditionalContent : TZUGFeRDAdditionalContent;
  _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor) : Boolean;
begin
  Result := false;
  if _AdditionalContent = nil then
    exit;
  if _InvoiceDescriptor = nil then
    exit;

  if _InvoiceDescriptor.Invoicee <> nil then
  begin
    _AdditionalContent.InvoiceeTradePartyFound := true;
    //Der Rechnungsempfaenger kennt in der ZUGFeRD-Bibliothek keinen eigenen
    //Ansprechpartner und keine elektronische Adresse.
    TransferParty(_AdditionalContent.InvoiceeTradeParty,_InvoiceDescriptor.Invoicee,
      nil,_InvoiceDescriptor.InvoiceeTaxRegistration,nil);
    //BT-33 gibt es nur beim Verkaeufer
    _AdditionalContent.InvoiceeTradeParty.AdditionalLegalInformationSeller := '';
  end;

  _AdditionalContent.SpecifiedLogisticsServiceChargeFound := _InvoiceDescriptor.ServiceCharges.Count > 0;
end;

{ TZUGFeRDAdditionalContent }

procedure TZUGFeRDAdditionalContent.Clear;
begin
  if Assigned(ZUGFeRDInvoice) then begin ZUGFeRDInvoice.Free; ZUGFeRDInvoice := nil; end;
  InvoiceeTradePartyFound := false;
  SpecifiedLogisticsServiceChargeFound := false;
end;

constructor TZUGFeRDAdditionalContent.Create;
begin
  ZUGFeRDInvoice := nil;
  InvoiceeTradeParty := TInvoiceAccountingParty.Create;
  Clear;
end;

destructor TZUGFeRDAdditionalContent.Destroy;
begin
  if Assigned(ZUGFeRDInvoice) then begin ZUGFeRDInvoice.Free; ZUGFeRDInvoice := nil; end;
  if Assigned(InvoiceeTradeParty) then begin InvoiceeTradeParty.Free; InvoiceeTradeParty := nil; end;
  inherited;
end;

{$ENDIF}

end.

