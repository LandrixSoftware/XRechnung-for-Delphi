{
License XRechnung-for-Delphi

Copyright (C) 2025 Landrix Software GmbH & Co. KG
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

unit intfXRechnung;

interface

//setzt ZUGFeRD-for-Delphi voraus
//https://github.com/LandrixSoftware/ZUGFeRD-for-Delphi
{.$DEFINE ZUGFeRD_Support}

uses
  SysUtils,Classes,Types,ActiveX
  ,StrUtils,DateUtils,Contnrs
  ,XMLDoc,XMLIntf
  {$IFDEF ZUGFeRD_Support}
  ,intf.ZUGFeRDInvoiceDescriptor
  ,intf.ZUGFeRDCurrencyCodes
  ,intf.ZUGFeRDTaxRegistrationSchemeID
  ,intf.ZUGFeRDInvoiceTypes
  ,intf.ZUGFeRDCountryCodes
  ,intf.ZUGFeRDPaymentMeansTypeCodes
  ,intf.ZUGFeRDTaxCategoryCodes
  ,intf.ZUGFeRDSpecialServiceDescriptionCodes
  ,intf.ZUGFeRDAllowanceOrChargeIdentificationCodes
  ,intf.ZUGFeRDQuantityCodes
  ,intf.ZUGFeRDGlobalIDSchemeIdentifiers
  ,intf.ZUGFeRDSubjectCodes
  {$ENDIF}
  ,intfXRechnung_2_3
  ,intfXRechnung_3_0
  ,intfInvoice
  ;

type
  TXRechnungHelper = class(TObject)
  private
    class function PrepareValue(const _Val : String) : String;
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
                       XRechnungVersion_230_UBL_Deprecated,
                       XRechnungVersion_230_UNCEFACT_Deprecated,
                       XRechnungVersion_30x_UBL,
                       XRechnungVersion_30x_UNCEFACT,
                       ZUGFeRDExtendedVersion_232,
                       XRechnungVersion_ReadingSupport_ZUGFeRDFacturX,
                       ZUGFeRDExtendedVersion_1_NotSupported);

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
  public
    class function ConsistencyCheck(_Invoice : TInvoice; _Version : TXRechnungVersion) : Boolean;
    class procedure CorrectDueDateIfNotDefined(_Invoice : TInvoice);

    class procedure SaveToStream(_Invoice : TInvoice; _Version : TXRechnungVersion; _Stream : TStream);
    class procedure SaveToFile(_Invoice : TInvoice; _Version : TXRechnungVersion; const _Filename : String);
    class procedure SaveToXMLStr(_Invoice : TInvoice; _Version : TXRechnungVersion; out _XML : String);

    class function  LoadFromStream(_Invoice : TInvoice; _Stream : TStream; out _Error : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
    class function  LoadFromFile(_Invoice : TInvoice; const _Filename : String; out _Error : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
    class function  LoadFromXMLStr(_Invoice : TInvoice; const _XML : String; out _Error : String {$IFDEF ZUGFeRD_Support};_AdditionalContent : TZUGFeRDAdditionalContent = nil{$ENDIF}) : Boolean;
  end;

implementation

uses intfXRechnungHelper;

var
  xml : IXMLDocument;

{$IFDEF ZUGFeRD_Support}
type
  TZUGFeRDInvoiceAdapter = class
  private
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
begin
  if _Invoice = nil then
    exit;
  if _Stream = nil then
    exit;


  xml.XML.Clear;
  try
    TXRechnungInvoiceAdapter.SaveDocument(_Invoice,_Version,xml);
    xml.SaveToStream(_Stream);
  finally
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveToXMLStr(_Invoice: TInvoice;
  _Version : TXRechnungVersion; out _XML: String);
begin
  xml.XML.Clear;
  try
    TXRechnungInvoiceAdapter.SaveDocument(_Invoice,_Version,xml);
    xml.SaveToXML(_XML);
  finally
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveToFile(_Invoice: TInvoice;
  _Version : TXRechnungVersion;const _Filename: String);
var
  xml : IXMLDocument;
begin
  if _Invoice = nil then
    exit;
  if _Filename = '' then
    exit;
  if not DirectoryExists(ExtractFilePath(_Filename)) then
    exit;

  xml.XML.Clear;
  try
    TXRechnungInvoiceAdapter.SaveDocument(_Invoice,_Version,xml);
    xml.SaveToFile(_Filename);
  finally
  end;
end;

class function TXRechnungInvoiceAdapter.ConsistencyCheck(_Invoice: TInvoice;
  _Version: TXRechnungVersion): Boolean;
var
  lCount,i : Integer;
begin
  Result := true;

  //Mindestens eine Zahlungsanweisung notwendig (bei ZUGFeRD nur im Profil EXTENDED)
  if (_Invoice.PaymentTypes.Count = 0) and
     (_Version <> XRechnungVersion_ReadingSupport_ZUGFeRDFacturX) then
  begin
    Result := false;
    exit;
  end;

  //In XRechnung nicht unterstuetzte Rechnungsarten
  if (_Version in [XRechnungVersion_230_UBL_Deprecated,
                   XRechnungVersion_230_UNCEFACT_Deprecated,
                   XRechnungVersion_30x_UBL,
                   XRechnungVersion_30x_UNCEFACT]) then
  if (_Invoice.InvoiceTypeCode in [itc_DebitnoteRelatedToFinancialAdjustments,
                                   itc_SelfBilledCreditNote,
                                   itc_DebitNote,
                                   itc_PrepaymentInvoice,
                                   itc_Cancellation
                                   ]) then
  begin
    Result := false;
    exit;
  end;

  //Nur maximal eine Referenzrechnung in ZUGFeRD erlaubt
  if (_Version in [XRechnungVersion_230_UNCEFACT_Deprecated,
                   XRechnungVersion_30x_UNCEFACT]) then
  if _Invoice.PrecedingInvoiceReferences.Count > 1 then
  begin
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
    Result := false;
    exit;
  end;

  //Wenn der Verkaeufer keine UStId BT-31 und keine CompanyID BT-30 hat,
  //sollte CompanyID auf non-existent gesetzt werden
  if (_Invoice.AccountingSupplierParty.VATCompanyID = '') and
     (_Invoice.AccountingSupplierParty.CompanyID = '') then
  begin
    Result := false;
    exit;
  end;

  //Eins von beiden BT-31 BT-32 muss angegeben werden
  if (_Invoice.AccountingSupplierParty.VATCompanyID = '') and
     (_Invoice.AccountingSupplierParty.VATCompanyNumber = '') then
  begin
    Result := false;
    exit;
  end;

  //BG-DEX-09 THIRD PARTY PAYMENT Extension NUR XRechnung UBL !!!! https://blog.seeburger.com/de/xrechnung-2-3-1-gueltig-ab-dem-01-08-2023/
  if _Invoice.PrepaidPayments.Count > 0 then
  if not (_Version in [//TXRechnungVersion.XRechnungVersion_230_UBL_Deprecated, Version 2.3 wird nicht mehr gepflegt
                   XRechnungVersion_30x_UBL]) then
  begin
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
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _Filename = '' then
    exit;
  if not FileExists(_Filename) then
    exit;

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
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _Stream = nil then
    exit;

  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromStream(_Stream);
    Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error{$IFDEF ZUGFeRD_Support},_AdditionalContent{$ENDIF});
  finally
    xml := nil;
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
    XRechnungVersion_230_UBL_Deprecated      : Result := TXRechnungInvoiceAdapter230.LoadDocumentUBL(_Invoice,_XmlDocument,_Error);
    XRechnungVersion_30x_UBL      : Result := TXRechnungInvoiceAdapter301.LoadDocumentUBL(_Invoice,_XmlDocument,_Error);
    XRechnungVersion_230_UNCEFACT_Deprecated : Result := TXRechnungInvoiceAdapter230.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    XRechnungVersion_30x_UNCEFACT : Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    ZUGFeRDExtendedVersion_232 : Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    {$IFNDEF ZUGFeRD_Support}
    XRechnungVersion_ReadingSupport_ZUGFeRDFacturX : Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    {$ELSE}
    XRechnungVersion_ReadingSupport_ZUGFeRDFacturX,
    ZUGFeRDExtendedVersion_1_NotSupported : Result := TZUGFeRDInvoiceAdapter.LoadFromXMLDocument(_Invoice,_XmlDocument,_Error,_AdditionalContent);
    {$ENDIF}
    else exit;
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
    XRechnungVersion_230_UBL_Deprecated : TXRechnungInvoiceAdapter230.SaveDocumentUBL(_Invoice,_Xml);
    XRechnungVersion_30x_UBL : TXRechnungInvoiceAdapter301.SaveDocumentUBL(_Invoice,_Xml);
    XRechnungVersion_230_UNCEFACT_Deprecated : TXRechnungInvoiceAdapter230.SaveDocumentUNCEFACT(_Invoice,_Xml);
    XRechnungVersion_30x_UNCEFACT : TXRechnungInvoiceAdapter301.SaveDocumentUNCEFACT(_Invoice,_Xml,true);
    ZUGFeRDExtendedVersion_232 : TXRechnungInvoiceAdapter301.SaveDocumentUNCEFACT(_Invoice,_Xml,false);
    else raise Exception.Create('XRechnung - wrong version');
  end;
end;

{ TXRechnungHelper }

class function TXRechnungHelper.AmountFromStr(_Val: String): Currency;
begin
  Result := StrToCurrDef(TXRechnungHelper.PrepareValue(_Val),0);
end;

class function TXRechnungHelper.AmountToStr(
  _Val: Currency): String;
begin
  Result := StringReplace(Format('%.2f',[_Val]),',','.',[rfIgnoreCase,rfReplaceAll]);
end;

class function TXRechnungHelper.UnitPriceAmountFromStr(
  _Val: String): Currency;
begin
  Result := 0;
  if _Val = '' then
    exit;
  Result := StrToCurrDef(TXRechnungHelper.PrepareValue(_Val),0);
end;

class function TXRechnungHelper.UnitPriceAmountToStr(
  _Val: Currency): String;
begin
  Result := StringReplace(Format('%.2f',[_Val]),',','.',[rfIgnoreCase,rfReplaceAll]);
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
begin
  Result := StrToFloatDef(TXRechnungHelper.PrepareValue(_Val),0);
end;

class function TXRechnungHelper.FloatToStr(
  _Val: double; _DecimalPlaces : Integer = 2): String;
begin
  if _DecimalPlaces < 0 then
    _DecimalPlaces := 0;
  Result := StringReplace(Format('%.'+IntToStr(_DecimalPlaces)+'f',[_Val]),',','.',[rfIgnoreCase,rfReplaceAll]);
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
  if SameText(_Val,'HUR') then
    Result := iuc_hour else
  if SameText(_Val,'GRM') then
    Result := iuc_gram else
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
    iuc_litre : Result := 'LTR';
    iuc_hour : Result := 'HUR';
    iuc_kilogram : Result := 'KGM';
    iuc_gram : Result := 'GRM';
    iuc_kilometre : Result := 'KMT';
    iuc_kilowatt_hour : Result := 'KWH';
    iuc_percent : Result := 'P1';
    iuc_packaging : Result := 'XPK';
  end;
end;

class function TXRechnungHelper.PercentageFromStr(_Val: String): double;
begin
  Result := StrToFloatDef(TXRechnungHelper.PrepareValue(_Val),0);
end;

class function TXRechnungHelper.PercentageToStr(_Val: double): String;
begin
  Result := StringReplace(Format('%.2f',[_Val]),',','.',[rfIgnoreCase,rfReplaceAll]);
end;

class function TXRechnungHelper.QuantityFromStr(_Val: String): double;
begin
  Result := StrToFloatDef(TXRechnungHelper.PrepareValue(_Val),0);
end;

class function TXRechnungHelper.QuantityToStr(_Val: double): String;
begin
  Result := StringReplace(Format('%.4f',[_Val]),',','.',[rfIgnoreCase,rfReplaceAll]);
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

class function TXRechnungHelper.PrepareValue(const _Val: String): String;
begin
  Result := _Val;
  if DecimalSeparator = '.' then
    exit;
  Result := StringReplace(_Val, '.', DecimalSeparator, [rfReplaceAll]);
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
    if not TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'cbc:CustomizationID',node) then
      exit;
    if Pos('xrechnung_2.3',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_230_UBL_Deprecated
    else
    if Pos('xrechnung_3.0',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_30x_UBL;
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
    if Pos('xrechnung_2.3',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_230_UNCEFACT_Deprecated
    else
    if Pos('xrechnung_3.0',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_30x_UNCEFACT
    else
    if SameText(node.Text,'urn:cen.eu:en16931:2017#conformant#urn:factur-x.eu:1p0:extended') then
      Result := ZUGFeRDExtendedVersion_232
    else
    if Pos('urn:cen.eu:en16931:2017',AnsiLowerCase(node.Text))>0 then
      Result := XRechnungVersion_ReadingSupport_ZUGFeRDFacturX;
  end else
  if (SameText(_XML.DocumentElement.NodeName,'CrossIndustryDocument') or
      SameText(_XML.DocumentElement.NodeName,'rsm:CrossIndustryDocument')) then
  begin
    Result := ZUGFeRDExtendedVersion_1_NotSupported;
  end;
end;

class function TXRechnungValidationHelper.GetXRechnungVersion(
  const _Filename: String): TXRechnungVersion;
var
  xml : IXMLDocument;
begin
  Result := XRechnungVersion_Unknown;
  if not FileExists(_Filename) then
    exit;
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
  if _Invoice = nil then
    exit;
  if _Stream = nil then
    exit;

  desc := TZUGFeRDInvoiceDescriptor.Load(_Stream);
  try
    Result := TZUGFeRDInvoiceAdapter.LoadFromInvoiceDescriptor(_Invoice,desc,_Error);
  finally
    desc.Free;
  end;
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
  if not System.SysUtils.FileExists(_Filename) then
    exit;

  stream := TFileStream.Create(_Filename,fmOpenRead or fmShareDenyNone);
  try
    TZUGFeRDInvoiceAdapter.LoadFromStream(_Invoice,stream,_Error);
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
  if _Invoice = nil then
    exit;
  if _XmlDocument = nil then
    exit;

  desc := TZUGFeRDInvoiceDescriptor.Load(_XmlDocument);
  try
    Result := TZUGFeRDInvoiceAdapter.LoadFromInvoiceDescriptor(_Invoice,desc,_Error);
    if _AdditionalContent <> nil then
    begin
      TZUGFeRDInvoiceAdapter.LoadAdditionalContentFromXMLDocument(_AdditionalContent,desc);
      _AdditionalContent.ZUGFeRDInvoice := desc;
      desc := nil;
    end;
  finally
    if desc <> nil then
      desc.Free;
  end;
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
    TZUGFeRDInvoiceAdapter.LoadFromStream(_Invoice,stream,_Error);
  finally
    stream.Free;
  end;
end;

class function TZUGFeRDInvoiceAdapter.LoadFromInvoiceDescriptor(
  _Invoice: TInvoice; _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor;
  out _Error : String) : Boolean;
var
  i,j : Integer;
  firstDiscount : Boolean;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _InvoiceDescriptor = nil then
    exit;

  firstDiscount := true;

  _Invoice.InvoiceNumber := _InvoiceDescriptor.InvoiceNo;
  _Invoice.InvoiceIssueDate := _InvoiceDescriptor.InvoiceDate;
  _Invoice.InvoiceDueDate := 0;
  for i := 0 to _InvoiceDescriptor.PaymentTermsList.Count-1 do
  if (_InvoiceDescriptor.PaymentTermsList[i].Percentage.Value = 0.0) then
  begin
    _Invoice.InvoiceDueDate := _InvoiceDescriptor.PaymentTermsList[i].DueDate;
    break;
  end;
  _Invoice.InvoicePeriodStartDate := _InvoiceDescriptor.BillingPeriodStart;
  _Invoice.InvoicePeriodEndDate := _InvoiceDescriptor.BillingPeriodEnd;
  case _InvoiceDescriptor.Type_ of
    DebitnoteRelatedToFinancialAdjustments: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_DebitnoteRelatedToFinancialAdjustments;
    SelfBilledCreditNote: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_SelfBilledCreditNote;
    PartialInvoice: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_PartialInvoice;
    Invoice: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_CommercialInvoice;
    CreditNote: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_CreditNote;
    DebitNote: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_DebitNote;
    Correction: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_CorrectedInvoice;
    PrepaymentInvoice: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_PrepaymentInvoice;
    SelfBilledInvoice: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_SelfbilledInvoice;
    //InvoiceInformation: ;
    //CorrectionOld: ;
    Cancellation: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_Cancellation;
    PartialConstructionInvoice: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_PartialConstructionInvoice;
    PartialFinalConstructionInvoice: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_PartialFinalConstructionInvoice;
    FinalConstructionInvoice: _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_FinalConstructionInvoice;
    else _Invoice.InvoiceTypeCode := TInvoiceTypeCode.itc_None;
  end;
  _Invoice.InvoiceCurrencyCode := TZUGFeRDCurrencyCodesExtensions.EnumToString(_InvoiceDescriptor.Currency);
  _Invoice.TaxCurrencyCode := _Invoice.InvoiceCurrencyCode; //TODO fehlt in ZUGFeRD-Lib
  _Invoice.BuyerReference := _InvoiceDescriptor.ReferenceOrderNo;
  for i := 0 to _InvoiceDescriptor.Notes.Count-1 do
  begin
    _Invoice.Notes.AddNote.Content := _InvoiceDescriptor.Notes[i].Content;
    case _InvoiceDescriptor.Notes[i].SubjectCode of
      //TZUGFeRDSubjectCodes.AAC :_Invoice.Notes.Last.SubjectCode :=
      TZUGFeRDSubjectCodes.AAI :_Invoice.Notes.Last.SubjectCode := insc_AAI;
      TZUGFeRDSubjectCodes.AAJ :_Invoice.Notes.Last.SubjectCode := insc_AAJ;
      //TZUGFeRDSubjectCodes.ABN :_Invoice.Notes.Last.SubjectCode :=
      TZUGFeRDSubjectCodes.AAK :_Invoice.Notes.Last.SubjectCode := insc_AAK;
      //TZUGFeRDSubjectCodes.ACB :_Invoice.Notes.Last.SubjectCode :=
      //TZUGFeRDSubjectCodes.ADU :_Invoice.Notes.Last.SubjectCode :=
      TZUGFeRDSubjectCodes.PMT :_Invoice.Notes.Last.SubjectCode := insc_PMT;
      //TZUGFeRDSubjectCodes.PRF :_Invoice.Notes.Last.SubjectCode :=
      TZUGFeRDSubjectCodes.REG :_Invoice.Notes.Last.SubjectCode := insc_REG;
      TZUGFeRDSubjectCodes.SUR :_Invoice.Notes.Last.SubjectCode := insc_SUR;
      TZUGFeRDSubjectCodes.TXD :_Invoice.Notes.Last.SubjectCode := insc_TXD;
    end; //TODO insc_ABL, insc_CUS
  end;
  if _InvoiceDescriptor.SellerOrderReferencedDocument <> nil then
    _Invoice.SellerOrderReference := _InvoiceDescriptor.SellerOrderReferencedDocument.ID;
  _Invoice.PurchaseOrderReference := _InvoiceDescriptor.OrderNo;
  if _InvoiceDescriptor.SpecifiedProcuringProject <> nil then
    _Invoice.ProjectReference := _InvoiceDescriptor.SpecifiedProcuringProject.ID;
  if _InvoiceDescriptor.ContractReferencedDocument <> nil then
    _Invoice.ContractDocumentReference := _InvoiceDescriptor.ContractReferencedDocument.ID;
  if _InvoiceDescriptor.DespatchAdviceReferencedDocument <> nil then
    _Invoice.DeliveryReceiptNumber := _InvoiceDescriptor.DespatchAdviceReferencedDocument.ID;
  //Seller
  if _InvoiceDescriptor.Seller <> nil then
  begin
    if _InvoiceDescriptor.Seller.SpecifiedLegalOrganization <> nil then
    begin
      _Invoice.AccountingSupplierParty.Name := _InvoiceDescriptor.Seller.SpecifiedLegalOrganization.TradingBusinessName;
      _Invoice.AccountingSupplierParty.CompanyID := _InvoiceDescriptor.Seller.SpecifiedLegalOrganization.ID.ID;
    end;
    _Invoice.AccountingSupplierParty.RegistrationName := _InvoiceDescriptor.Seller.Name;
    if _InvoiceDescriptor.Seller.ContactName = '' then
    begin
      _Invoice.AccountingSupplierParty.Address.StreetName := _InvoiceDescriptor.Seller.Street;
      _Invoice.AccountingSupplierParty.Address.AdditionalStreetName := '';
    end else
    begin
      _Invoice.AccountingSupplierParty.Address.StreetName := _InvoiceDescriptor.Seller.ContactName;
      _Invoice.AccountingSupplierParty.Address.AdditionalStreetName := _InvoiceDescriptor.Seller.Street;
    end;
    _Invoice.AccountingSupplierParty.Address.City := _InvoiceDescriptor.Seller.City;
    _Invoice.AccountingSupplierParty.Address.PostalZone := _InvoiceDescriptor.Seller.Postcode;
    _Invoice.AccountingSupplierParty.Address.CountrySubentity := _InvoiceDescriptor.Seller.CountrySubdivisionName;
    _Invoice.AccountingSupplierParty.Address.AddressLine := _InvoiceDescriptor.Seller.AddressLine3;
    _Invoice.AccountingSupplierParty.Address.CountryCode := TZUGFeRDCountryCodesExtensions.EnumToString(_InvoiceDescriptor.Seller.Country);
    _Invoice.AccountingSupplierParty.IdentifierSellerBuyer := _InvoiceDescriptor.Seller.ID.ID;
  end;
  for i := 0 to _InvoiceDescriptor.SellerTaxRegistration.Count-1 do
  if _InvoiceDescriptor.SellerTaxRegistration[i].SchemeID = TZUGFeRDTaxRegistrationSchemeID.VA then
    _Invoice.AccountingSupplierParty.VATCompanyID := _InvoiceDescriptor.SellerTaxRegistration[i].No
  else
  if _InvoiceDescriptor.SellerTaxRegistration[i].SchemeID = TZUGFeRDTaxRegistrationSchemeID.FC then
    _Invoice.AccountingSupplierParty.VATCompanyNumber := _InvoiceDescriptor.SellerTaxRegistration[i].No;
  if _InvoiceDescriptor.SellerContact <> nil then
  begin
    _Invoice.AccountingSupplierParty.ContactName := _InvoiceDescriptor.SellerContact.Name;
    _Invoice.AccountingSupplierParty.ContactTelephone := _InvoiceDescriptor.SellerContact.PhoneNo;
    _Invoice.AccountingSupplierParty.ContactElectronicMail := _InvoiceDescriptor.SellerContact.EmailAddress;
  end;
  _Invoice.AccountingSupplierParty.AdditionalLegalInformationSeller := _InvoiceDescriptor.Seller.Description;
  _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer := _InvoiceDescriptor.SellerElectronicAddress.Address;
  //Buyer
  if _InvoiceDescriptor.Buyer <> nil then
  begin
    if _InvoiceDescriptor.Buyer.SpecifiedLegalOrganization <> nil then
    begin
      _Invoice.AccountingCustomerParty.Name := _InvoiceDescriptor.Buyer.SpecifiedLegalOrganization.TradingBusinessName;
      _Invoice.AccountingCustomerParty.CompanyID := _InvoiceDescriptor.Buyer.SpecifiedLegalOrganization.ID.ID;
    end;
    _Invoice.AccountingCustomerParty.RegistrationName := _InvoiceDescriptor.Buyer.Name;
    if _InvoiceDescriptor.Buyer.ContactName = '' then
    begin
      _Invoice.AccountingCustomerParty.Address.StreetName := _InvoiceDescriptor.Buyer.Street;
      _Invoice.AccountingCustomerParty.Address.AdditionalStreetName := '';
    end else
    begin
      _Invoice.AccountingCustomerParty.Address.StreetName := _InvoiceDescriptor.Buyer.ContactName;
      _Invoice.AccountingCustomerParty.Address.AdditionalStreetName := _InvoiceDescriptor.Buyer.Street;
    end;
    _Invoice.AccountingCustomerParty.Address.City := _InvoiceDescriptor.Buyer.City;
    _Invoice.AccountingCustomerParty.Address.PostalZone := _InvoiceDescriptor.Buyer.Postcode;
    _Invoice.AccountingCustomerParty.Address.CountrySubentity := _InvoiceDescriptor.Buyer.CountrySubdivisionName;
    _Invoice.AccountingCustomerParty.Address.AddressLine := _InvoiceDescriptor.Buyer.AddressLine3;
    _Invoice.AccountingCustomerParty.Address.CountryCode := TZUGFeRDCountryCodesExtensions.EnumToString(_InvoiceDescriptor.Buyer.Country);
    _Invoice.AccountingCustomerParty.IdentifierSellerBuyer := _InvoiceDescriptor.Buyer.ID.ID;
  end;
  for i := 0 to _InvoiceDescriptor.BuyerTaxRegistration.Count-1 do
  if _InvoiceDescriptor.BuyerTaxRegistration[i].SchemeID = TZUGFeRDTaxRegistrationSchemeID.VA then
    _Invoice.AccountingCustomerParty.VATCompanyID := _InvoiceDescriptor.BuyerTaxRegistration[i].No
  else
  if _InvoiceDescriptor.BuyerTaxRegistration[i].SchemeID = TZUGFeRDTaxRegistrationSchemeID.FC then
    _Invoice.AccountingCustomerParty.VATCompanyNumber := _InvoiceDescriptor.BuyerTaxRegistration[i].No;
  if _InvoiceDescriptor.BuyerContact <> nil then
  begin
    _Invoice.AccountingCustomerParty.ContactName := _InvoiceDescriptor.BuyerContact.Name;
    _Invoice.AccountingCustomerParty.ContactTelephone := _InvoiceDescriptor.BuyerContact.PhoneNo;
    _Invoice.AccountingCustomerParty.ContactElectronicMail := _InvoiceDescriptor.BuyerContact.EmailAddress;
    _Invoice.AccountingCustomerParty.AdditionalLegalInformationSeller := ''; //TODO fehlt in ZUGFeRD-Lib
  end;
  _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer := _InvoiceDescriptor.BuyerElectronicAddress.Address;

  if _InvoiceDescriptor.ShipTo <> nil then
  begin
    _Invoice.DeliveryInformation.Name := _InvoiceDescriptor.ShipTo.Name;
    if _InvoiceDescriptor.ShipTo.ContactName = '' then
    begin
      _Invoice.DeliveryInformation.Address.StreetName := _InvoiceDescriptor.ShipTo.Street;
      _Invoice.DeliveryInformation.Address.AdditionalStreetName := '';
    end else
    begin
      _Invoice.DeliveryInformation.Address.StreetName := _InvoiceDescriptor.ShipTo.ContactName;
      _Invoice.DeliveryInformation.Address.AdditionalStreetName := _InvoiceDescriptor.ShipTo.Street;
    end;
    _Invoice.DeliveryInformation.Address.City := _InvoiceDescriptor.ShipTo.City;
    _Invoice.DeliveryInformation.Address.PostalZone := _InvoiceDescriptor.ShipTo.Postcode;
    _Invoice.DeliveryInformation.Address.CountrySubentity := _InvoiceDescriptor.ShipTo.CountrySubdivisionName;
    _Invoice.DeliveryInformation.Address.AddressLine := _InvoiceDescriptor.ShipTo.AddressLine3;
    _Invoice.DeliveryInformation.Address.CountryCode := TZUGFeRDCountryCodesExtensions.EnumToString(_InvoiceDescriptor.ShipTo.Country);
  end;
  _Invoice.DeliveryInformation.ActualDeliveryDate := _InvoiceDescriptor.ActualDeliveryDate.GetValueOrDefault(0);

  var lPaymentMeansCode : TInvoicePaymentMeansCode := ipmc_NotImplemented;
  if _InvoiceDescriptor.PaymentMeans <> nil then
  case _InvoiceDescriptor.PaymentMeans.TypeCode of
    InCash: lPaymentMeansCode := ipmc_InCash;
    Cheque: lPaymentMeansCode := ipmc_Cheque;
    CreditTransfer: lPaymentMeansCode := ipmc_CreditTransfer;
    //TODO Fehlt : _Invoice.PaymentMeansCode := ipmc_CreditCard; 54
    SEPACreditTransfer: lPaymentMeansCode := ipmc_SEPACreditTransfer;
    SEPADirectDebit: lPaymentMeansCode := ipmc_SEPADirectDebit;
    NotDefined: lPaymentMeansCode := ipmc_InstrumentNotDefined;
    //TODO Fehlt 68
    //    AutomatedClearingHouseDebit: ;
    //    DebitTransfer: ;
    //    PaymentToBankAccount: ;
    //    BankCard: 48
    //    StandingAgreement: ;
    //    ClearingBetweenPartners: ;
  end;
  _Invoice.PaymentID := _InvoiceDescriptor.PaymentReference;
  if _InvoiceDescriptor.PaymentMeans <> nil then
    _Invoice.AccountingSupplierParty.BankAssignedCreditorIdentifier := _InvoiceDescriptor.PaymentMeans.SEPACreditorIdentifier;
  if lPaymentMeansCode = ipmc_SEPADirectDebit then
  begin
    for i := 0 to _InvoiceDescriptor.DebitorBankAccounts.Count-1 do
    with _Invoice.PaymentTypes.AddPaymentType do
    begin
      PaymentMeansCode := lPaymentMeansCode;
      FinancialAccount := _InvoiceDescriptor.DebitorBankAccounts[i].IBAN;
      FinancialAccountName := _InvoiceDescriptor.DebitorBankAccounts[i].Name;
      FinancialInstitutionBranch := _InvoiceDescriptor.DebitorBankAccounts[i].BIC;
    end;
  end else
  if lPaymentMeansCode = ipmc_InstrumentNotDefined then
    _Invoice.PaymentTypes.AddPaymentType.PaymentMeansCode := ipmc_InstrumentNotDefined
  else
  if lPaymentMeansCode <> ipmc_NotImplemented then
  begin
    for i := 0 to _InvoiceDescriptor.CreditorBankAccounts.Count-1 do
    with _Invoice.PaymentTypes.AddPaymentType do
    begin
      PaymentMeansCode := lPaymentMeansCode;
      FinancialAccount := _InvoiceDescriptor.CreditorBankAccounts[i].IBAN;
      FinancialAccountName := _InvoiceDescriptor.CreditorBankAccounts[i].Name;
      FinancialInstitutionBranch := _InvoiceDescriptor.CreditorBankAccounts[i].BIC;
    end;
  end;

  _Invoice.PaymentMandateID := _InvoiceDescriptor.PaymentMeans.SEPAMandateReference;

  _Invoice.PaymentTermsType := iptt_None;
  for i := 0 to _InvoiceDescriptor.PaymentTermsList.Count-1 do
  begin
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
        _Invoice.PaymentTermCashDiscount1Days := Trunc(_InvoiceDescriptor.PaymentTermsList[i].DueDate.Value);
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

  for i := 0 to _InvoiceDescriptor.TradeLineItems.Count-1 do
  begin
    var lInvoiceLine : TInvoiceLine := _Invoice.InvoiceLines.AddInvoiceLine;
    if _InvoiceDescriptor.TradeLineItems[i].AssociatedDocument <> nil then
    begin
      lInvoiceLine.ID := _InvoiceDescriptor.TradeLineItems[i].AssociatedDocument.LineID;
      for j := 0 to _InvoiceDescriptor.TradeLineItems[i].AssociatedDocument.Notes.Count-1 do
      begin
        if lInvoiceLine.Note <> '' then
          lInvoiceLine.Note := lInvoiceLine.Note + #13#10;
        lInvoiceLine.Note := lInvoiceLine.Note + _InvoiceDescriptor.TradeLineItems[i].AssociatedDocument.Notes[j].Content;
      end;
    end;
    if _InvoiceDescriptor.TradeLineItems[i].GlobalID.ID <> '' then
    if _InvoiceDescriptor.TradeLineItems[i].GlobalID.SchemeID = EAN then
      lInvoiceLine.GlobalID_EAN_GTIN := _InvoiceDescriptor.TradeLineItems[i].GlobalID.ID;
    lInvoiceLine.Name := _InvoiceDescriptor.TradeLineItems[i].Name;
    lInvoiceLine.Description := _InvoiceDescriptor.TradeLineItems[i].Description;
    lInvoiceLine.Quantity := _InvoiceDescriptor.TradeLineItems[i].BilledQuantity;
    //lInvoiceLine.UnitCode := TXRechnungHelper.InvoiceUnitCodeFromStr(TZUGFeRDQuantityCodesExtensions.EnumToString(_InvoiceDescriptor.TradeLineItems[i].BilledQuantityUnitCode));
    lInvoiceLine.UnitCode := TXRechnungHelper.InvoiceUnitCodeFromStr(TZUGFeRDQuantityCodesExtensions.EnumToString(_InvoiceDescriptor.TradeLineItems[i].UnitCode));
    lInvoiceLine.SellersItemIdentification := _InvoiceDescriptor.TradeLineItems[i].SellerAssignedID;
    lInvoiceLine.BuyersItemIdentification := _InvoiceDescriptor.TradeLineItems[i].BuyerAssignedID;
    if _InvoiceDescriptor.TradeLineItems[i].BuyerOrderReferencedDocument <> nil then
      lInvoiceLine.OrderLineReference := _InvoiceDescriptor.TradeLineItems[i].BuyerOrderReferencedDocument.LineID;
    if _InvoiceDescriptor.TradeLineItems[i].ReceivableSpecifiedTradeAccountingAccounts.Count > 0 then
      lInvoiceLine.BuyerAccountingReference := _InvoiceDescriptor.TradeLineItems[i].ReceivableSpecifiedTradeAccountingAccounts.First.TradeAccountID;
    lInvoiceLine.TaxPercent := _InvoiceDescriptor.TradeLineItems[i].TaxPercent;
    case _InvoiceDescriptor.TradeLineItems[i].TaxCategoryCode of
      TZUGFeRDTaxCategoryCodes.AE : lInvoiceLine.TaxCategory := idtfcc_AE_VATReverseCharge;
      TZUGFeRDTaxCategoryCodes.E : lInvoiceLine.TaxCategory := idtfcc_E_ExemptFromTax;
      TZUGFeRDTaxCategoryCodes.G : lInvoiceLine.TaxCategory := idtfcc_G_FreeExportItemTaxNotCharged;
      TZUGFeRDTaxCategoryCodes.K : lInvoiceLine.TaxCategory := idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices;
      TZUGFeRDTaxCategoryCodes.L : lInvoiceLine.TaxCategory := idtfcc_L_CanaryIslandsGeneralIndirectTax;
      TZUGFeRDTaxCategoryCodes.M : lInvoiceLine.TaxCategory := idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla;
      TZUGFeRDTaxCategoryCodes.O : lInvoiceLine.TaxCategory := idtfcc_O_ServicesOutsideScopeOfTax;
      TZUGFeRDTaxCategoryCodes.S : lInvoiceLine.TaxCategory := idtfcc_S_StandardRate;
      TZUGFeRDTaxCategoryCodes.Z : lInvoiceLine.TaxCategory := idtfcc_Z_ZeroRatedGoods;
      else lInvoiceLine.TaxCategory := idtfcc_None; //TODO weitere Category Types von ZUGFeRD
    end;
    lInvoiceLine.GrossPriceAmount := _InvoiceDescriptor.TradeLineItems[i].GrossUnitPrice.GetValueOrDefault(0);
    for j := 0 to _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges.Count-1 do
    begin
      //wegen XRechnung UBL nur ein Item moeglich mit ChargeIndicator = false
      //weitere Felder aus TradeAllowanceCharge werden nach lInvoiceLine.AllowanceCharges
      //transferiert
      //z.B. liefern manche Lieferanten Rohstoffzuschlaege an dieser Stelle
      if (_InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].ChargeIndicator = false) and firstDiscount then
      begin
        firstDiscount := false;
        lInvoiceLine.DiscountOnTheGrossPrice := _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].ActualAmount;
      end else
      begin
        with lInvoiceLine.AllowanceCharges.AddAllowanceCharge do
        begin
          ChargeIndicator := _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].ChargeIndicator;
          ReasonCodeAllowance := iacic_None;
          ReasonCodeCharge := issdc_None;
          if ChargeIndicator then
            case _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].ReasonCodeCharge of
              AA_Advertising : ReasonCodeCharge := issdc_AA_Advertising;
              AAA_Telecommunication : ReasonCodeCharge := issdc_AAA_Telecommunication;
              ABK_Miscellaneous : ReasonCodeCharge := issdc_ABK_Miscellaneous;
              ABL_AdditionalPackaging : ReasonCodeCharge := issdc_ABL_AdditionalPackaging;
              ADR_OtherServices : ReasonCodeCharge := issdc_ADR_OtherServices;
              ADT_Pickup : ReasonCodeCharge := issdc_ADT_Pickup;
              FC_FreightService : ReasonCodeCharge := issdc_FC_FreightService;
              FI_Financing : ReasonCodeCharge := issdc_FI_Financing;
              LA_Labelling : ReasonCodeCharge := issdc_LA_Labelling;
              PC_Packing  : ReasonCodeCharge := issdc_PC_Packing;
              else ReasonCodeCharge := issdc_None;
            end
          else
            case _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].ReasonCodeAllowance of
              BonusForWorksAheadOfSchedule : ReasonCodeAllowance := iacic_BonusForWorksAheadOfSchedule;
              OtherBonus : ReasonCodeAllowance := iacic_OtherBonus;
              ManufacturersConsumerDiscount : ReasonCodeAllowance :=  iacic_ManufacturersConsumerDiscount;
              DueToMilitaryStatus : ReasonCodeAllowance :=  iacic_DueToMilitaryStatus;
              DueToWorkAccident : ReasonCodeAllowance :=  iacic_DueToWorkAccident;
              SpecialAgreement : ReasonCodeAllowance :=  iacic_SpecialAgreement;
              ProductionErrorDiscount : ReasonCodeAllowance :=  iacic_ProductionErrorDiscount;
              NewOutletDiscount : ReasonCodeAllowance :=  iacic_NewOutletDiscount;
              SampleDiscount : ReasonCodeAllowance :=  iacic_SampleDiscount;
              EndOfRangeDiscount : ReasonCodeAllowance :=  iacic_EndOfRangeDiscount;
              IncotermDiscount : ReasonCodeAllowance :=  iacic_IncotermDiscount;
              PointOfSalesThresholdAllowance : ReasonCodeAllowance :=  iacic_PointOfSalesThresholdAllowance;
              MaterialSurchargeDeduction : ReasonCodeAllowance :=  iacic_MaterialSurchargeDeduction;
              Discount : ReasonCodeAllowance :=  iacic_Discount;
              SpecialRebate : ReasonCodeAllowance :=  iacic_SpecialRebate;
              FixedLongTerm : ReasonCodeAllowance :=  iacic_FixedLongTerm;
              Temporary : ReasonCodeAllowance :=  iacic_Temporary;
              Standard : ReasonCodeAllowance :=  iacic_Standard;
              YearlyTurnover : ReasonCodeAllowance :=  iacic_YearlyTurnover;
              else ReasonCodeAllowance := iacic_None;
            end;
          Reason := _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].Reason;
          BaseAmount := _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].BasisAmount;
          MultiplierFactorNumeric := _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].ChargePercentage;
          Amount := _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges[j].ActualAmount;
          TaxPercent := 0; //Nicht in Position vorhanden
          TaxCategory := idtfcc_None; //Nicht in Position vorhanden
        end;
      end;
    end;
    if _InvoiceDescriptor.TradeLineItems[i].BillingPeriodStart.HasValue then
      lInvoiceLine.InvoiceLinePeriodStartDate := _InvoiceDescriptor.TradeLineItems[i].BillingPeriodStart;
    if _InvoiceDescriptor.TradeLineItems[i].BillingPeriodEnd.HasValue then
      lInvoiceLine.InvoiceLinePeriodEndDate := _InvoiceDescriptor.TradeLineItems[i].BillingPeriodEnd;
    lInvoiceLine.NetPriceAmount := _InvoiceDescriptor.TradeLineItems[i].NetUnitPrice.GetValueOrDefault(0);
    lInvoiceLine.BaseQuantity := _InvoiceDescriptor.TradeLineItems[i].NetQuantity.GetValueOrDefault(0);
    lInvoiceLine.BaseQuantityUnitCode := TXRechnungHelper.InvoiceUnitCodeFromStr(TZUGFeRDQuantityCodesExtensions.EnumToString(_InvoiceDescriptor.TradeLineItems[i].UnitCode));
    lInvoiceLine.LineAmount := _InvoiceDescriptor.TradeLineItems[i].LineTotalAmount.GetValueOrDefault(0);
    for j := 0 to _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges.Count-1 do
    with lInvoiceLine.AllowanceCharges.AddAllowanceCharge do
    begin
      ChargeIndicator := _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges[j].ChargeIndicator;
      ReasonCodeAllowance := iacic_None;
      ReasonCodeCharge := issdc_None;
      if ChargeIndicator then
        case _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges[j].ReasonCodeCharge of
          AA_Advertising : ReasonCodeCharge := issdc_AA_Advertising;
          AAA_Telecommunication : ReasonCodeCharge := issdc_AAA_Telecommunication;
          ABK_Miscellaneous : ReasonCodeCharge := issdc_ABK_Miscellaneous;
          ABL_AdditionalPackaging : ReasonCodeCharge := issdc_ABL_AdditionalPackaging;
          ADR_OtherServices : ReasonCodeCharge := issdc_ADR_OtherServices;
          ADT_Pickup : ReasonCodeCharge := issdc_ADT_Pickup;
          FC_FreightService : ReasonCodeCharge := issdc_FC_FreightService;
          FI_Financing : ReasonCodeCharge := issdc_FI_Financing;
          LA_Labelling : ReasonCodeCharge := issdc_LA_Labelling;
          PC_Packing  : ReasonCodeCharge := issdc_PC_Packing;
          else ReasonCodeCharge := issdc_None;
        end
      else
        case _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges[j].ReasonCodeAllowance of
          BonusForWorksAheadOfSchedule : ReasonCodeAllowance := iacic_BonusForWorksAheadOfSchedule;
          OtherBonus : ReasonCodeAllowance := iacic_OtherBonus;
          ManufacturersConsumerDiscount : ReasonCodeAllowance :=  iacic_ManufacturersConsumerDiscount;
          DueToMilitaryStatus : ReasonCodeAllowance :=  iacic_DueToMilitaryStatus;
          DueToWorkAccident : ReasonCodeAllowance :=  iacic_DueToWorkAccident;
          SpecialAgreement : ReasonCodeAllowance :=  iacic_SpecialAgreement;
          ProductionErrorDiscount : ReasonCodeAllowance :=  iacic_ProductionErrorDiscount;
          NewOutletDiscount : ReasonCodeAllowance :=  iacic_NewOutletDiscount;
          SampleDiscount : ReasonCodeAllowance :=  iacic_SampleDiscount;
          EndOfRangeDiscount : ReasonCodeAllowance :=  iacic_EndOfRangeDiscount;
          IncotermDiscount : ReasonCodeAllowance :=  iacic_IncotermDiscount;
          PointOfSalesThresholdAllowance : ReasonCodeAllowance :=  iacic_PointOfSalesThresholdAllowance;
          MaterialSurchargeDeduction : ReasonCodeAllowance :=  iacic_MaterialSurchargeDeduction;
          Discount : ReasonCodeAllowance :=  iacic_Discount;
          SpecialRebate : ReasonCodeAllowance :=  iacic_SpecialRebate;
          FixedLongTerm : ReasonCodeAllowance :=  iacic_FixedLongTerm;
          Temporary : ReasonCodeAllowance :=  iacic_Temporary;
          Standard : ReasonCodeAllowance :=  iacic_Standard;
          YearlyTurnover : ReasonCodeAllowance :=  iacic_YearlyTurnover;
          else ReasonCodeAllowance := iacic_None;
        end;
      Reason := _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges[j].Reason;
      BaseAmount := _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges[j].BasisAmount;
      MultiplierFactorNumeric := _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges[j].ChargePercentage;
      Amount := _InvoiceDescriptor.TradeLineItems[i].SpecifiedTradeAllowanceCharges[j].ActualAmount;
      TaxPercent := 0; //Nicht in Position vorhanden
      TaxCategory := idtfcc_None; //Nicht in Position vorhanden
    end;

    for j := 0 to _InvoiceDescriptor.TradeLineItems[i].ApplicableProductCharacteristics.Count-1 do
    with lInvoiceLine.ItemAttributes.AddItemAttribute do
    begin
      Name := _InvoiceDescriptor.TradeLineItems[i].ApplicableProductCharacteristics[j].Description;
      Value := _InvoiceDescriptor.TradeLineItems[i].ApplicableProductCharacteristics[j].Value;
    end;
  end;

  for i := 0 to _InvoiceDescriptor.AdditionalReferencedDocuments.Count-1 do
  begin
    var lAttachment : TInvoiceAttachment := TInvoiceAttachment.Create(iat_application_None);
    lAttachment.ID := _InvoiceDescriptor.AdditionalReferencedDocuments[i].ID;
    //TODO fehlt in ZUGFeRD lAttachment.ExternalReference
    lAttachment.DocumentDescription := _InvoiceDescriptor.AdditionalReferencedDocuments[i].Name;
    lAttachment.Filename := _InvoiceDescriptor.AdditionalReferencedDocuments[i].Filename;
    lAttachment.AttachmentType := TInvoiceAttachmentTypeHelper.GetTypeFromFilename(_InvoiceDescriptor.AdditionalReferencedDocuments[i].Filename);
    if _InvoiceDescriptor.AdditionalReferencedDocuments[i].AttachmentBinaryObject<> nil then
    begin
      lAttachment.Data.LoadFromStream(_InvoiceDescriptor.AdditionalReferencedDocuments[i].AttachmentBinaryObject);
      _InvoiceDescriptor.AdditionalReferencedDocuments[i].AttachmentBinaryObject.Position := 0;
    end;
    lAttachment.ExternalReference := '';
    _Invoice.Attachments.Add(lAttachment);
  end;

  for i := 0 to _InvoiceDescriptor.TradeAllowanceCharges.Count-1 do
  with _Invoice.AllowanceCharges.AddAllowanceCharge do
  begin
    ChargeIndicator := _InvoiceDescriptor.TradeAllowanceCharges[i].ChargeIndicator;
    ReasonCodeAllowance := iacic_None;
    ReasonCodeCharge := issdc_None;
    if ChargeIndicator then
      case _InvoiceDescriptor.TradeAllowanceCharges[i].ReasonCodeCharge of
        AA_Advertising : ReasonCodeCharge := issdc_AA_Advertising;
        AAA_Telecommunication : ReasonCodeCharge := issdc_AAA_Telecommunication;
        ABK_Miscellaneous : ReasonCodeCharge := issdc_ABK_Miscellaneous;
        ABL_AdditionalPackaging : ReasonCodeCharge := issdc_ABL_AdditionalPackaging;
        ADR_OtherServices : ReasonCodeCharge := issdc_ADR_OtherServices;
        ADT_Pickup : ReasonCodeCharge := issdc_ADT_Pickup;
        FC_FreightService : ReasonCodeCharge := issdc_FC_FreightService;
        FI_Financing : ReasonCodeCharge := issdc_FI_Financing;
        LA_Labelling : ReasonCodeCharge := issdc_LA_Labelling;
        PC_Packing  : ReasonCodeCharge := issdc_PC_Packing;
        else ReasonCodeCharge := issdc_None;
      end
    else
      case _InvoiceDescriptor.TradeAllowanceCharges[i].ReasonCodeAllowance of
        BonusForWorksAheadOfSchedule : ReasonCodeAllowance := iacic_BonusForWorksAheadOfSchedule;
        OtherBonus : ReasonCodeAllowance := iacic_OtherBonus;
        ManufacturersConsumerDiscount : ReasonCodeAllowance :=  iacic_ManufacturersConsumerDiscount;
        DueToMilitaryStatus : ReasonCodeAllowance :=  iacic_DueToMilitaryStatus;
        DueToWorkAccident : ReasonCodeAllowance :=  iacic_DueToWorkAccident;
        SpecialAgreement : ReasonCodeAllowance :=  iacic_SpecialAgreement;
        ProductionErrorDiscount : ReasonCodeAllowance :=  iacic_ProductionErrorDiscount;
        NewOutletDiscount : ReasonCodeAllowance :=  iacic_NewOutletDiscount;
        SampleDiscount : ReasonCodeAllowance :=  iacic_SampleDiscount;
        EndOfRangeDiscount : ReasonCodeAllowance :=  iacic_EndOfRangeDiscount;
        IncotermDiscount : ReasonCodeAllowance :=  iacic_IncotermDiscount;
        PointOfSalesThresholdAllowance : ReasonCodeAllowance :=  iacic_PointOfSalesThresholdAllowance;
        MaterialSurchargeDeduction : ReasonCodeAllowance :=  iacic_MaterialSurchargeDeduction;
        Discount : ReasonCodeAllowance :=  iacic_Discount;
        SpecialRebate : ReasonCodeAllowance :=  iacic_SpecialRebate;
        FixedLongTerm : ReasonCodeAllowance :=  iacic_FixedLongTerm;
        Temporary : ReasonCodeAllowance :=  iacic_Temporary;
        Standard : ReasonCodeAllowance :=  iacic_Standard;
        YearlyTurnover : ReasonCodeAllowance :=  iacic_YearlyTurnover;
        else ReasonCodeAllowance := iacic_None;
      end;
    Reason := _InvoiceDescriptor.TradeAllowanceCharges[i].Reason;
    BaseAmount := _InvoiceDescriptor.TradeAllowanceCharges[i].BasisAmount;
    MultiplierFactorNumeric := _InvoiceDescriptor.TradeAllowanceCharges[i].ChargePercentage;
    Amount := _InvoiceDescriptor.TradeAllowanceCharges[i].ActualAmount;
    TaxPercent := _InvoiceDescriptor.TradeAllowanceCharges[i].Tax.Percent;
    case _InvoiceDescriptor.TradeAllowanceCharges[i].Tax.CategoryCode of
      TZUGFeRDTaxCategoryCodes.AE : TaxCategory := idtfcc_AE_VATReverseCharge;
      TZUGFeRDTaxCategoryCodes.E : TaxCategory := idtfcc_E_ExemptFromTax;
      TZUGFeRDTaxCategoryCodes.G : TaxCategory := idtfcc_G_FreeExportItemTaxNotCharged;
      TZUGFeRDTaxCategoryCodes.K : TaxCategory := idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices;
      TZUGFeRDTaxCategoryCodes.L : TaxCategory := idtfcc_L_CanaryIslandsGeneralIndirectTax;
      TZUGFeRDTaxCategoryCodes.M : TaxCategory := idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla;
      TZUGFeRDTaxCategoryCodes.O : TaxCategory := idtfcc_O_ServicesOutsideScopeOfTax;
      TZUGFeRDTaxCategoryCodes.S : TaxCategory := idtfcc_S_StandardRate;
      TZUGFeRDTaxCategoryCodes.Z : TaxCategory := idtfcc_Z_ZeroRatedGoods;
      else TaxCategory := idtfcc_None; //TODO weitere Category Types von ZUGFeRD
    end;
  end;

  //Achtung, CII-Format <= v2.2 maximal ein Element erlaubt, UBL-Format beliebig viele
  for i := 0 to _InvoiceDescriptor.InvoiceReferencedDocuments.Count-1 do
  if (_InvoiceDescriptor.InvoiceReferencedDocuments[i].ID <> '') and
     (_InvoiceDescriptor.InvoiceReferencedDocuments[i].IssueDateTime.GetValueOrDefault > 100) then
  with _Invoice.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
  begin
    ID := _InvoiceDescriptor.InvoiceReferencedDocuments[i].ID;
    IssueDate := _InvoiceDescriptor.InvoiceReferencedDocuments[i].IssueDateTime.GetValueOrDefault(0);
  end;

  _Invoice.TaxAmountTotal := _InvoiceDescriptor.TaxTotalAmount.GetValueOrDefault(0);
  for i := 0 to _InvoiceDescriptor.Taxes.Count-1 do
  with _Invoice.TaxAmountSubtotals.AddTaxAmount do
  begin
    TaxableAmount := _InvoiceDescriptor.Taxes[i].BasisAmount;
    TaxAmount := _InvoiceDescriptor.Taxes[i].TaxAmount;
    TaxPercent := _InvoiceDescriptor.Taxes[i].Percent;
    //TODO DEFAULT VAT_InvoiceDescriptor.Taxes[i].TypeCode
    case _InvoiceDescriptor.Taxes[i].CategoryCode of
      TZUGFeRDTaxCategoryCodes.AE : TaxCategory := idtfcc_AE_VATReverseCharge;
      TZUGFeRDTaxCategoryCodes.E : TaxCategory := idtfcc_E_ExemptFromTax;
      TZUGFeRDTaxCategoryCodes.G : TaxCategory := idtfcc_G_FreeExportItemTaxNotCharged;
      TZUGFeRDTaxCategoryCodes.K : TaxCategory := idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices;
      TZUGFeRDTaxCategoryCodes.L : TaxCategory := idtfcc_L_CanaryIslandsGeneralIndirectTax;
      TZUGFeRDTaxCategoryCodes.M : TaxCategory := idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla;
      TZUGFeRDTaxCategoryCodes.O : TaxCategory := idtfcc_O_ServicesOutsideScopeOfTax;
      TZUGFeRDTaxCategoryCodes.S : TaxCategory := idtfcc_S_StandardRate;
      TZUGFeRDTaxCategoryCodes.Z : TaxCategory := idtfcc_Z_ZeroRatedGoods;
      else TaxCategory := idtfcc_None; //TODO weitere Category Types von ZUGFeRD
    end;
    TaxExemptionReason := _InvoiceDescriptor.Taxes[i].ExemptionReason;
  end;

  _Invoice.LineAmount := _InvoiceDescriptor.LineTotalAmount.GetValueOrDefault(0);
  _Invoice.TaxExclusiveAmount := _InvoiceDescriptor.TaxBasisAmount.GetValueOrDefault(0);
  _Invoice.TaxInclusiveAmount := _InvoiceDescriptor.GrandTotalAmount.GetValueOrDefault(0);
  _Invoice.AllowanceTotalAmount := _InvoiceDescriptor.AllowanceTotalAmount.GetValueOrDefault(0);
  _Invoice.ChargeTotalAmount := _InvoiceDescriptor.ChargeTotalAmount.GetValueOrDefault(0);
  _Invoice.PrepaidAmount := _InvoiceDescriptor.TotalPrepaidAmount.GetValueOrDefault(0);
  _Invoice.PayableRoundingAmount := _InvoiceDescriptor.RoundingAmount.GetValueOrDefault(0);
  _Invoice.PayableAmount := _InvoiceDescriptor.DuePayableAmount.GetValueOrDefault(0);
  Result := True;
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

    if _InvoiceDescriptor.Invoicee.SpecifiedLegalOrganization <> nil then
    begin
      _AdditionalContent.InvoiceeTradeParty.Name := _InvoiceDescriptor.Invoicee.SpecifiedLegalOrganization.TradingBusinessName;
      _AdditionalContent.InvoiceeTradeParty.CompanyID := _InvoiceDescriptor.Invoicee.SpecifiedLegalOrganization.ID.ID;
    end;
    _AdditionalContent.InvoiceeTradeParty.RegistrationName := _InvoiceDescriptor.Invoicee.Name;
    if _InvoiceDescriptor.Invoicee.ContactName = '' then
    begin
      _AdditionalContent.InvoiceeTradeParty.Address.StreetName := _InvoiceDescriptor.Invoicee.Street;
      _AdditionalContent.InvoiceeTradeParty.Address.AdditionalStreetName := '';
    end else
    begin
      _AdditionalContent.InvoiceeTradeParty.Address.StreetName := _InvoiceDescriptor.Invoicee.ContactName;
      _AdditionalContent.InvoiceeTradeParty.Address.AdditionalStreetName := _InvoiceDescriptor.Invoicee.Street;
    end;
    _AdditionalContent.InvoiceeTradeParty.Address.City := _InvoiceDescriptor.Invoicee.City;
    _AdditionalContent.InvoiceeTradeParty.Address.PostalZone := _InvoiceDescriptor.Invoicee.Postcode;
    _AdditionalContent.InvoiceeTradeParty.Address.CountrySubentity := _InvoiceDescriptor.Invoicee.CountrySubdivisionName;
    _AdditionalContent.InvoiceeTradeParty.Address.AddressLine := _InvoiceDescriptor.Invoicee.AddressLine3;
    _AdditionalContent.InvoiceeTradeParty.Address.CountryCode := TZUGFeRDCountryCodesExtensions.EnumToString(_InvoiceDescriptor.Invoicee.Country);
    _AdditionalContent.InvoiceeTradeParty.IdentifierSellerBuyer := _InvoiceDescriptor.Invoicee.ID.ID;
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

initialization
  CoInitialize(nil);
  xml := NewXMLDocument;

end.

