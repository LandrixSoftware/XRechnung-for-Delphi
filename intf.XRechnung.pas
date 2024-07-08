{
License XRechnung-for-Delphi

Copyright (C) 2024 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.1

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
}

unit intf.XRechnung;

interface

//setzt ZUGFeRD-for-Delphi voraus
//https://github.com/LandrixSoftware/ZUGFeRD-for-Delphi
{.$DEFINE ZUGFeRD_Support}

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants,System.StrUtils,System.Generics.Collections
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,intf.XRechnungMSXML2_TLB
  {$IFDEF USE_OXMLDomVendor},OXmlDOMVendor{$ENDIF}
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
  {$ENDIF}
  ,intf.XRechnung_2_3
  ,intf.XRechnung_3_0
  ,intf.Invoice
  ;

//UBL-Format
//https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice

//https://portal3.gefeg.com/invoice/tthome/index/617afdc4-623f-44e0-a05b-5b878840e508

//validieren von XRechnung
//https://ecosio.com/de/peppol-und-xml-dokumente-online-validieren/

//https://www.e-rechnung-bund.de/wp-content/uploads/2023/04/Uebersichtslisten-Eingabefelder-OZG-RE.pdf

type
  TXRechnungHelper = class(TObject)
  public
    class function DateFromStrUBLFormat(const _Val : String) : TDateTime;
    class function DateFromStrUNCEFACTFormat(const _Val : String) : TDateTime;
    class function DateToStrUBLFormat(const _Val : TDateTime) : String;
    class function DateToStrUNCEFACTFormat(const _Val : TDateTime) : String;
//    class function StrToCurr(_Val : String) : Currency;
//    class function StrToFloat(_Val : String) : double;
    class function AmountToStr(_Val : Currency) : String;
    class function AmountFromStr(_Val : String) : Currency;
    class function UnitPriceAmountToStr(_Val : Currency) : String;
    class function UnitPriceAmountFromStr(_Val : String) : Currency;
    class function FloatToStr(_Val : double) : String;
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
  end;

  TXRechnungVersion = (XRechnungVersion_Unknown,
                       XRechnungVersion_230_UBL,
                       XRechnungVersion_230_UNCEFACT,
                       XRechnungVersion_30x_UBL,
                       XRechnungVersion_30x_UNCEFACT,
                       XRechnungVersion_ReadingSupport_ZUGFeRDFacturX);

  TXRechnungValidationHelper = class(TObject)
  public type
    TValidationError = record
    public
      Reason : String;
      SrcText : String;
    end;
  public
    class function GetXRechnungVersion(const _Filename : String) : TXRechnungVersion; overload;
    class function GetXRechnungVersion(_Xml : IXMLDocument) : TXRechnungVersion; overload;
    class function GetXRechnungVersion(const _Stream: TStream) : TXRechnungVersion; overload;
  end;

  TXRechnungInvoiceAdapter = class
  private
    class procedure SaveDocument(_Invoice: TInvoice;_Version : TXRechnungVersion; _Xml : IXMLDocument);
    class function  LoadFromXMLDocument(_Invoice: TInvoice; _XmlDocument: IXMLDocument; out _Error : String) : Boolean;
  public
    class function ConsistencyCheck(_Invoice : TInvoice; _Version : TXRechnungVersion) : Boolean;

    class procedure SaveToStream(_Invoice : TInvoice; _Version : TXRechnungVersion; _Stream : TStream);
    class procedure SaveToFile(_Invoice : TInvoice; _Version : TXRechnungVersion; const _Filename : String);
    class procedure SaveToXMLStr(_Invoice : TInvoice; _Version : TXRechnungVersion; out _XML : String);

    class function  LoadFromStream(_Invoice : TInvoice; _Stream : TStream; out _Error : String) : Boolean;
    class function  LoadFromFile(_Invoice : TInvoice; const _Filename : String; out _Error : String) : Boolean;
    class function  LoadFromXMLStr(_Invoice : TInvoice; const _XML : String; out _Error : String) : Boolean;
  end;

  {$IFDEF ZUGFeRD_Support}
  TZUGFeRDInvoiceAdapter  = class
  private
    class function  LoadFromInvoiceDescriptor(_Invoice: TInvoice; _InvoiceDescriptor: TZUGFeRDInvoiceDescriptor; out _Error : String) : Boolean;
  public
    class function  LoadFromXMLDocument(_Invoice: TInvoice; _XmlDocument: IXMLDocument; out _Error : String) : Boolean;
    class function  LoadFromStream(_Invoice : TInvoice; _Stream : TStream; out _Error : String) : Boolean;
    class function  LoadFromFile(_Invoice : TInvoice; const _Filename : String; out _Error : String) : Boolean;
    class function  LoadFromXMLStr(_Invoice : TInvoice; const _XML : String; out _Error : String) : Boolean;
  end;
  {$ENDIF}

implementation

uses intf.XRechnungHelper;

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
begin
  if _Invoice = nil then
    exit;
  if _Filename = '' then
    exit;
  if not System.SysUtils.DirectoryExists(ExtractFilePath(_Filename)) then
    exit;

  xml := NewXMLDocument;
  try
    TXRechnungInvoiceAdapter.SaveDocument(_Invoice,_Version,xml);
    xml.SaveToFile(_Filename);
  finally
    xml := nil;
  end;
end;

class function TXRechnungInvoiceAdapter.ConsistencyCheck(_Invoice: TInvoice;
  _Version: TXRechnungVersion): Boolean;
begin
  Result := true;

  //Beide Felder sind in UBL nicht möglich
  if (_Version in [TXRechnungVersion.XRechnungVersion_230_UBL,
                   TXRechnungVersion.XRechnungVersion_30x_UBL]) then
  if (_Invoice.PurchaseOrderReference <> '') and
     (_Invoice.SellerOrderReference <> '') then
  begin
    Result := false;
    exit;
  end;

  //In XRechnung nicht unterstützte Rechnungsarten
  if (_Version in [TXRechnungVersion.XRechnungVersion_230_UBL,
                   TXRechnungVersion.XRechnungVersion_230_UNCEFACT,
                   TXRechnungVersion.XRechnungVersion_30x_UBL,
                   TXRechnungVersion.XRechnungVersion_30x_UNCEFACT]) then
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
  if (_Version in [TXRechnungVersion.XRechnungVersion_230_UNCEFACT,
                   TXRechnungVersion.XRechnungVersion_30x_UNCEFACT]) then
  if _Invoice.PrecedingInvoiceReferences.Count > 1 then
  begin
    Result := false;
    exit;
  end;

end;

class function TXRechnungInvoiceAdapter.LoadFromFile(_Invoice: TInvoice;
  const _Filename: String; out _Error : String) : Boolean;
var
  xml : IXMLDocument;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _Filename = '' then
    exit;
  if not System.SysUtils.FileExists(_Filename) then
    exit;

  xml := TXMLDocument.Create(nil);
  try
    xml.LoadFromFile(_Filename);
    Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error);
  finally
    xml := nil;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromStream(_Invoice: TInvoice;
  _Stream: TStream; out _Error : String) : Boolean;
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
    Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error);
  finally
    xml := nil;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromXMLDocument(
  _Invoice: TInvoice; _XmlDocument: IXMLDocument;
  out _Error: String): Boolean;
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _XmlDocument = nil then
    exit;

  case TXRechnungValidationHelper.GetXRechnungVersion(_XmlDocument) of
    XRechnungVersion_230_UBL      : Result := TXRechnungInvoiceAdapter230.LoadDocumentUBL(_Invoice,_XmlDocument,_Error);
    XRechnungVersion_30x_UBL      : Result := TXRechnungInvoiceAdapter301.LoadDocumentUBL(_Invoice,_XmlDocument,_Error);
    {$IFNDEF ZUGFeRD_Support}
    XRechnungVersion_230_UNCEFACT : Result := TXRechnungInvoiceAdapter230.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    XRechnungVersion_30x_UNCEFACT,
    XRechnungVersion_ReadingSupport_ZUGFeRDFacturX : Result := TXRechnungInvoiceAdapter301.LoadDocumentUNCEFACT(_Invoice,_XmlDocument,_Error);
    else exit;
    {$ELSE}
    else Result := TZUGFeRDInvoiceAdapter.LoadFromXMLDocument(_Invoice,_XmlDocument,_Error);
    {$ENDIF}
  end;
end;

class function TXRechnungInvoiceAdapter.LoadFromXMLStr(_Invoice: TInvoice;
  const _XML: String; out _Error : String) : Boolean;
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
    Result := TXRechnungInvoiceAdapter.LoadFromXMLDocument(_Invoice,xml,_Error);
  finally
    xml := nil;
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveDocument(_Invoice: TInvoice;
  _Version : TXRechnungVersion; _Xml: IXMLDocument);
begin
  case _Version of
    XRechnungVersion_230_UBL : TXRechnungInvoiceAdapter230.SaveDocumentUBL(_Invoice,_Xml);
    XRechnungVersion_30x_UBL : TXRechnungInvoiceAdapter301.SaveDocumentUBL(_Invoice,_Xml);
    XRechnungVersion_230_UNCEFACT : TXRechnungInvoiceAdapter230.SaveDocumentUNCEFACT(_Invoice,_Xml);
    XRechnungVersion_30x_UNCEFACT : TXRechnungInvoiceAdapter301.SaveDocumentUNCEFACT(_Invoice,_Xml);
    else raise Exception.Create('XRechnung - wrong version');
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
  Result := System.StrUtils.ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TXRechnungHelper.UnitPriceAmountFromStr(
  _Val: String): Currency;
var
  fs : TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator := '.';
  Result := StrToCurrDef(_Val,0,fs);
end;

class function TXRechnungHelper.UnitPriceAmountToStr(
  _Val: Currency): String;
begin
  Result := System.StrUtils.ReplaceText(Format('%.4f',[_Val]),',','.');
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
  _Val: double): String;
begin
  Result := System.StrUtils.ReplaceText(Format('%.2f',[_Val]),',','.');
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
    //idtfcc_O_ServicesOutsideScopeOfTax: Result := 'O';
    idtfcc_S_StandardRate: Result := 'S';
    idtfcc_Z_ZeroRatedGoods: Result := 'Z';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoicePaymentMeansCodeFromStr(
  _Val: String): TInvoicePaymentMeansCode;
begin
  if SameText(_Val,'10') then
    Result := ipmc_InCash
  else
  if SameText(_Val,'20') then
    Result := ipmc_Cheque
  else
  if SameText(_Val,'30') then
    Result := ipmc_CreditTransfer
  else
  if SameText(_Val,'54') then
    Result := ipmc_CreditCard
  else
  if SameText(_Val,'58')  then
    Result := ipmc_SEPACreditTransfer
  else
  if SameText(_Val,'59')  then
    Result := ipmc_SEPADirectDebit
  else
  if SameText(_Val,'1')  then
    Result := ipmc_InstrumentNotDefined
  else
    Result := ipmc_NotImplemented;
end;

class function TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Val: TInvoicePaymentMeansCode): String;
begin
  case _Val of
    ipmc_InCash: Result := '10';
    ipmc_Cheque: Result := '20';
    ipmc_CreditTransfer: Result := '30';
    ipmc_CreditCard: Result := '54';
    ipmc_SEPACreditTransfer: Result := '58';
    ipmc_SEPADirectDebit: Result := '59';
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
  if SameText(_Val,'KGM') then
    Result := iuc_kilogram else
  if SameText(_Val,'KMT') then
    Result := iuc_kilometre else
  if SameText(_Val,'KWH') then
    Result := iuc_kilowatt_hour else
  Result := iuc_one; //C62
end;

class function TXRechnungHelper.InvoiceUnitCodeToStr(_Val: TInvoiceUnitCode): String;
begin
  //mehr Konvertierungen in Res\intf.XRechnung.unusedUnits.pas
  case _Val of
    iuc_one : Result := 'C62';
    iuc_piece : Result := 'H87';
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
    iuc_kilometre : Result := 'KMT';
    iuc_kilowatt_hour : Result := 'KWH';
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
  Result := System.StrUtils.ReplaceText(Format('%.2f',[_Val]),',','.');
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
  Result := System.StrUtils.ReplaceText(Format('%.4f',[_Val]),',','.');
end;

//class function TXRechnungHelper.StrToCurr(
//  _Val: String): Currency;
//begin
//  _Val := System.StrUtils.ReplaceText(_Val,'.',FormatSettings.DecimalSeparator);
//  result := StrToCurrDef(_Val,0);
//end;
//
//class function TXRechnungHelper.StrToFloat(
//  _Val: String): double;
//begin
//  _Val := System.StrUtils.ReplaceText(_Val,'.',FormatSettings.DecimalSeparator);
//  result := StrToFloatDef(_Val,0);
//end;

{ TXRechnungValidationHelper }

class function TXRechnungValidationHelper.GetXRechnungVersion(
  _Xml: IXMLDocument): TXRechnungVersion;
var
  node,node2 : IXMLNode;
begin
  Result := XRechnungVersion_Unknown;
  if _XML = nil then
    exit;
  if (SameText(_XML.DocumentElement.NodeName,'Invoice') or SameText(_XML.DocumentElement.NodeName,'ubl:Invoice')) then
  begin
    if not TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'cbc:CustomizationID',node) then
      exit;
    if node.Text.EndsWith('xrechnung_2.3',true) then
      Result := XRechnungVersion_230_UBL
    else
    if node.Text.EndsWith('xrechnung_3.0',true) then
      Result := XRechnungVersion_30x_UBL;
  end else
  if (SameText(_XML.DocumentElement.NodeName,'CrossIndustryInvoice') or SameText(_XML.DocumentElement.NodeName,'rsm:CrossIndustryInvoice')) then
  begin
    if not (TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'rsm:ExchangedDocumentContext',node) or
            TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'ExchangedDocumentContext',node)) then
      exit;
    if not TXRechnungXMLHelper.FindChild(node,'ram:GuidelineSpecifiedDocumentContextParameter',node2) then
      exit;
    if not TXRechnungXMLHelper.FindChild(node2,'ram:ID',node) then
      exit;
    if node.Text.EndsWith('xrechnung_2.3',true) then
      Result := XRechnungVersion_230_UNCEFACT
    else
    if node.Text.EndsWith('xrechnung_3.0',true) then
      Result := XRechnungVersion_30x_UNCEFACT
    else
    if node.Text.StartsWith('urn:cen.eu:en16931:2017',true) then
      Result := XRechnungVersion_ReadingSupport_ZUGFeRDFacturX;
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
  const _Stream: TStream): TXRechnungVersion;
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
  _XmlDocument: IXMLDocument; out _Error: String): Boolean;
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
  finally
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
begin
  Result := false;
  if _Invoice = nil then
    exit;
  if _InvoiceDescriptor = nil then
    exit;

  _Invoice.InvoiceNumber := _InvoiceDescriptor.InvoiceNo;
  _Invoice.InvoiceIssueDate := _InvoiceDescriptor.InvoiceDate;
  _Invoice.InvoiceDueDate := 0;
  for i := 0 to _InvoiceDescriptor.PaymentTermsList.Count-1 do
  if (_InvoiceDescriptor.PaymentTermsList[i].ApplicableTradePaymentDiscountTerms.CalculationPercent = 0.0) then
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
    if _Invoice.Note <> '' then
      _Invoice.Note := _Invoice.Note + #13#10;
    _Invoice.Note := _Invoice.Note + _InvoiceDescriptor.Notes[i].Content;
  end;
  if _InvoiceDescriptor.SellerOrderReferencedDocument <> nil then
    _Invoice.SellerOrderReference := _InvoiceDescriptor.SellerOrderReferencedDocument.ID;
  _Invoice.PurchaseOrderReference := _InvoiceDescriptor.OrderNo;
  if _InvoiceDescriptor.SpecifiedProcuringProject <> nil then 
    _Invoice.ProjectReference := _InvoiceDescriptor.SpecifiedProcuringProject.ID;
  if _InvoiceDescriptor.ContractReferencedDocument <> nil then
    _Invoice.ContractDocumentReference := _InvoiceDescriptor.ContractReferencedDocument.ID;
  if _InvoiceDescriptor.DeliveryNoteReferencedDocument <> nil then
    _Invoice.DeliveryReceiptNumber := _InvoiceDescriptor.DeliveryNoteReferencedDocument.ID;
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
  _Invoice.AccountingSupplierParty.AdditionalLegalInformationSeller := ''; //TODO fehlt in ZUGFeRD-Lib
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

  case _InvoiceDescriptor.PaymentMeans.TypeCode of
    SEPACreditTransfer: _Invoice.PaymentMeansCode := ipmc_SEPACreditTransfer;
    NotDefined: _Invoice.PaymentMeansCode := ipmc_InstrumentNotDefined;
    else _Invoice.PaymentMeansCode := ipmc_NotImplemented;
    //    Unknown: ;
    //    AutomatedClearingHouseDebit: ;
    //    InCash: ;
    //    Cheque: ;
    //    CreditTransfer: ;
    //    DebitTransfer: ;
    //    PaymentToBankAccount: ;
    //    BankCard: ;
    //    DirectDebit: ;
    //    StandingAgreement: ;
    //    SEPADirectDebit: ;
    //    ClearingBetweenPartners: ;
  end;
  _Invoice.PaymentID := _InvoiceDescriptor.PaymentReference;
  //TODO Mehrere Bankverbindungen
  if _InvoiceDescriptor.CreditorBankAccounts.Count > 0 then
  begin
    _Invoice.PayeeFinancialAccount := _InvoiceDescriptor.CreditorBankAccounts[0].IBAN;
    _Invoice.PayeeFinancialAccountName := _InvoiceDescriptor.CreditorBankAccounts[0].Name;
    _Invoice.PayeeFinancialInstitutionBranch := _InvoiceDescriptor.CreditorBankAccounts[0].BIC;
  end;

  //TODO #SKONTO Type
  _Invoice.PaymentTermsType := iptt_None;
  for i := 0 to _InvoiceDescriptor.PaymentTermsList.Count-1 do
  begin
    if (_InvoiceDescriptor.PaymentTermsList[i].ApplicableTradePaymentDiscountTerms.CalculationPercent = 0) and
       (_InvoiceDescriptor.PaymentTermsList[i].ApplicableTradePaymentDiscountTerms.BasisAmount = 0) then
    begin
      if _Invoice.PaymentTermsType = iptt_None then
        _Invoice.PaymentTermsType := iptt_Net;
      _Invoice.InvoiceDueDate := _InvoiceDescriptor.PaymentTermsList[i].DueDate;
      _Invoice.PaymentTermNetNote := _InvoiceDescriptor.PaymentTermsList[i].Description;
    end else
    if (_Invoice.PaymentTermsType in [iptt_None,iptt_Net]) then
    begin
      _Invoice.PaymentTermsType := iptt_CashDiscount1;
      //TODO _Invoice.PaymentTermCashDiscount1Days :=
      _Invoice.PaymentTermCashDiscount1Percent := _InvoiceDescriptor.PaymentTermsList[i].ApplicableTradePaymentDiscountTerms.CalculationPercent;
      _Invoice.PaymentTermCashDiscount1Base := _InvoiceDescriptor.PaymentTermsList[i].ApplicableTradePaymentDiscountTerms.BasisAmount;
    end else
    if _Invoice.PaymentTermsType = iptt_CashDiscount1 then
    begin
      _Invoice.PaymentTermsType := iptt_CashDiscount2;
      //TODO _Invoice.PaymentTermCashDiscount2Days :=
      _Invoice.PaymentTermCashDiscount2Percent := _InvoiceDescriptor.PaymentTermsList[i].ApplicableTradePaymentDiscountTerms.CalculationPercent;
      _Invoice.PaymentTermCashDiscount2Base := _InvoiceDescriptor.PaymentTermsList[i].ApplicableTradePaymentDiscountTerms.BasisAmount;
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
    lInvoiceLine.Name := _InvoiceDescriptor.TradeLineItems[i].Name;
    lInvoiceLine.Description := _InvoiceDescriptor.TradeLineItems[i].Description;
    lInvoiceLine.Quantity := _InvoiceDescriptor.TradeLineItems[i].BilledQuantity;
    lInvoiceLine.UnitCode := TXRechnungHelper.InvoiceUnitCodeFromStr(TZUGFeRDQuantityCodesExtensions.EnumToString(_InvoiceDescriptor.TradeLineItems[i].UnitCode));
    lInvoiceLine.SellersItemIdentification := _InvoiceDescriptor.TradeLineItems[i].SellerAssignedID;
    lInvoiceLine.TaxPercent := _InvoiceDescriptor.TradeLineItems[i].TaxPercent;
    case _InvoiceDescriptor.TradeLineItems[i].TaxCategoryCode of
      TZUGFeRDTaxCategoryCodes.AE : lInvoiceLine.TaxCategory := idtfcc_AE_VATReverseCharge;
      TZUGFeRDTaxCategoryCodes.E : lInvoiceLine.TaxCategory := idtfcc_E_ExemptFromTax;
      TZUGFeRDTaxCategoryCodes.G : lInvoiceLine.TaxCategory := idtfcc_G_FreeExportItemTaxNotCharged;
      TZUGFeRDTaxCategoryCodes.K : lInvoiceLine.TaxCategory := idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices;
      TZUGFeRDTaxCategoryCodes.L : lInvoiceLine.TaxCategory := idtfcc_L_CanaryIslandsGeneralIndirectTax;
      TZUGFeRDTaxCategoryCodes.M : lInvoiceLine.TaxCategory := idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla;
      TZUGFeRDTaxCategoryCodes.S : lInvoiceLine.TaxCategory := idtfcc_S_StandardRate;
      TZUGFeRDTaxCategoryCodes.Z : lInvoiceLine.TaxCategory := idtfcc_Z_ZeroRatedGoods;
      else lInvoiceLine.TaxCategory := idtfcc_None; //TODO weitere Category Types von ZUGFeRD
    end;
    lInvoiceLine.PriceAmount := _InvoiceDescriptor.TradeLineItems[i].NetUnitPrice.GetValueOrDefault(0);
    lInvoiceLine.BaseQuantity := _InvoiceDescriptor.TradeLineItems[i].UnitQuantity.GetValueOrDefault(0);
    lInvoiceLine.BaseQuantityUnitCode := lInvoiceLine.UnitCode;
    lInvoiceLine.LineAmount := _InvoiceDescriptor.TradeLineItems[i].LineTotalAmount.GetValueOrDefault(0);
    for j := 0 to _InvoiceDescriptor.TradeLineItems[i].TradeAllowanceCharges.Count-1 do
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

  for i := 0 to _InvoiceDescriptor.AdditionalReferencedDocuments.Count-1 do
  begin
    var lAttachment : TInvoiceAttachment := TInvoiceAttachment.Create(iat_application_None);
    lAttachment.ID := _InvoiceDescriptor.AdditionalReferencedDocuments[i].ID;
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
      TZUGFeRDTaxCategoryCodes.S : TaxCategory := idtfcc_S_StandardRate;
      TZUGFeRDTaxCategoryCodes.Z : TaxCategory := idtfcc_Z_ZeroRatedGoods;
      else TaxCategory := idtfcc_None; //TODO weitere Category Types von ZUGFeRD
    end;
  end;

  //Achtung, CII-Format maximal ein Element erlaubt, UBL-Format beliebig viele
  if _InvoiceDescriptor.InvoiceReferencedDocument <> nil then
  with _Invoice.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
  begin
    ID := _InvoiceDescriptor.InvoiceReferencedDocument.ID;
    IssueDate := _InvoiceDescriptor.InvoiceReferencedDocument.IssueDateTime.GetValueOrDefault(0);
  end;

  _Invoice.TaxAmountTotal := _InvoiceDescriptor.TaxTotalAmount.GetValueOrDefault(0);
  SetLength(_Invoice.TaxAmountSubtotals,_InvoiceDescriptor.Taxes.Count);
  for i := 0 to _InvoiceDescriptor.Taxes.Count-1 do
  begin
    _Invoice.TaxAmountSubtotals[i].TaxableAmount := _InvoiceDescriptor.Taxes[i].BasisAmount;
    //TODO fehlt in ZUGFeRD Lib _Invoice.TaxAmountSubtotals[i].TaxAmount := _InvoiceDescriptor.Taxes[i].TaxAmount
    _Invoice.TaxAmountSubtotals[i].TaxPercent := _InvoiceDescriptor.Taxes[i].Percent;
    //TODO DEFAULT VAT_InvoiceDescriptor.Taxes[i].TypeCode
    case _InvoiceDescriptor.Taxes[i].CategoryCode of
      TZUGFeRDTaxCategoryCodes.AE : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_AE_VATReverseCharge;
      TZUGFeRDTaxCategoryCodes.E : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_E_ExemptFromTax;
      TZUGFeRDTaxCategoryCodes.G : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_G_FreeExportItemTaxNotCharged;
      TZUGFeRDTaxCategoryCodes.K : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices;
      TZUGFeRDTaxCategoryCodes.L : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_L_CanaryIslandsGeneralIndirectTax;
      TZUGFeRDTaxCategoryCodes.M : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla;
      TZUGFeRDTaxCategoryCodes.S : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_S_StandardRate;
      TZUGFeRDTaxCategoryCodes.Z : _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_Z_ZeroRatedGoods;
      else _Invoice.TaxAmountSubtotals[i].TaxCategory := idtfcc_None; //TODO weitere Category Types von ZUGFeRD
    end;
    _Invoice.TaxAmountSubtotals[i].TaxExemptionReason := _InvoiceDescriptor.Taxes[i].ExemptionReason;
  end;

  _Invoice.LineAmount := _InvoiceDescriptor.LineTotalAmount.GetValueOrDefault(0);
  _Invoice.TaxExclusiveAmount := _InvoiceDescriptor.TaxBasisAmount.GetValueOrDefault(0);
  _Invoice.TaxInclusiveAmount := _InvoiceDescriptor.GrandTotalAmount.GetValueOrDefault(0);
  _Invoice.AllowanceTotalAmount := _InvoiceDescriptor.AllowanceTotalAmount.GetValueOrDefault(0);
  _Invoice.ChargeTotalAmount := _InvoiceDescriptor.ChargeTotalAmount.GetValueOrDefault(0);
  _Invoice.PrepaidAmount := _InvoiceDescriptor.TotalPrepaidAmount.GetValueOrDefault(0);
  _Invoice.PayableAmount := _InvoiceDescriptor.DuePayableAmount.GetValueOrDefault(0);
  Result := True;
end;
{$ENDIF}

end.

