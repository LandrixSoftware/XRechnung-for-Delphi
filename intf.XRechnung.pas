{
License XRechnung-for-Delphi

Copyright (C) 2021 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 1.3.2

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

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants,System.StrUtils,System.Generics.Collections
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,intf.MSXML2_TLB
  {$IFDEF USE_OXMLDomVendor},OXmlDOMVendor{$ENDIF}
  ,intf.Invoice
  ;

//https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice/tree/

//https://portal3.gefeg.com/invoice/tthome/index/617afdc4-623f-44e0-a05b-5b878840e508

//validieren von XRechnung
//https://ecosio.com/de/peppol-und-xml-dokumente-online-validieren/

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
                       XRechnungVersion_122,
                       XRechnungVersion_211_UBL,
                       XRechnungVersion_211_UNCEFACT,
                       XRechnungVersion_ReadingSupport_ZUGFeRDFacturX_2xx);

  TXRechnungValidationHelper = class(TObject)
  public type
    TValidationError = record
    public
      Reason : String;
      SrcText : String;
    end;
  private
    //class function LoadXSDFromResource(const _ResourceName : String) : String;
  public
    class function GetXRechnungVersion(const _Filename : String) : TXRechnungVersion; overload;
    class function GetXRechnungVersion(_Xml : IXMLDocument) : TXRechnungVersion; overload;
    //class function ValidateXRechnung(const _XML : String; out _Error : TValidationError) : Boolean;
  end;

  TXRechnungInvoiceAdapter = class
  private
    class procedure SaveDocument(_Invoice: TInvoice;_Version : TXRechnungVersion; _Xml : IXMLDocument);
    class procedure SaveDocumentUNCEFACT(_Invoice: TInvoice;_Xml : IXMLDocument);
    class procedure SaveDocumentUBL(_Invoice: TInvoice;_Version : TXRechnungVersion; _Xml : IXMLDocument);
    class function LoadDocumentUNCEFACT(_Invoice: TInvoice;_Xml : IXMLDocument; out _Error : String) : Boolean;
    class function LoadDocumentUBL(_Invoice: TInvoice;_Version : TXRechnungVersion; _Xml : IXMLDocument; out _Error : String) : Boolean;
  public
    class procedure SaveToStream(_Invoice : TInvoice; _Version : TXRechnungVersion; _Stream : TStream);
    class procedure SaveToFile(_Invoice : TInvoice; _Version : TXRechnungVersion; const _Filename : String);
    class procedure SaveToXMLStr(_Invoice : TInvoice; _Version : TXRechnungVersion; out _XML : String);
    class function LoadFromStream(_Invoice : TInvoice; _Stream : TStream; out _Error : String) : Boolean;
    class function LoadFromFile(_Invoice : TInvoice; const _Filename : String; out _Error : String) : Boolean;
    class function LoadFromXMLStr(_Invoice : TInvoice; const _XML : String; out _Error : String) : Boolean;
  end;

implementation

//{$R intf.XRechnungSchema.res}

type
  TXRechnungXMLHelper = class(TObject)
  public type
    TXMLLoadCallback = procedure(Node : IXMLNode) of object;
  public
    class procedure LoadFromChilds(const _NodeName : String; _Node : IXMLNode; _Callback : TXMLLoadCallback);
    class function FindChild(_Node : IXMLNode; const _NodeName : String; out _Result : IXMLNode) : Boolean;
    class function SelectNode(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNode): Boolean;
    class function SelectNodes(_XnRoot: IXMLDOMNode; const _NodePath: String; out _Result : IXMLDOMNodeList): Boolean;
    class function SelectNodeText(_XnRoot: IXMLDOMNode; const _NodePath: String): String;
    class function FindNode(_XnRoot: IXMLDOMNode; const _NodePath: String): Boolean;
    class function PrepareDocumentForXPathQuerys(_Xml : IXMLDocument) : IXMLDOMDocument2;
  end;

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

class function TXRechnungInvoiceAdapter.LoadDocumentUBL(_Invoice: TInvoice;
  _Version: TXRechnungVersion; _Xml: IXMLDocument; out _Error : String) : Boolean;
var
  xml : IXMLDOMDocument2;
  node,node2 : IXMLDOMNode;
  nodes : IXMLDOMNodeList;
  i : Integer;
begin
  Result := false;
  _Error := '';
  try
    xml := TXRechnungXMLHelper.PrepareDocumentForXPathQuerys(_Xml);
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:ID',node) then
      _Invoice.InvoiceNumber := node.Text;
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:IssueDate',node) then
      _Invoice.InvoiceIssueDate := TXRechnungHelper.DateFromStrUBLFormat(node.Text);
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:DueDate',node) then
      _Invoice.InvoiceDueDate := TXRechnungHelper.DateFromStrUBLFormat(node.Text);
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:InvoiceTypeCode',node) then
      _Invoice.InvoiceTypeCode := TXRechnungHelper.InvoiceTypeCodeFromStr(node.Text);
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:Note',node) then
      _Invoice.Note := node.Text;
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:DocumentCurrencyCode',node) then
      _Invoice.InvoiceCurrencyCode := node.Text;
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:TaxCurrencyCode',node) then
      _Invoice.TaxCurrencyCode := node.Text;
    if TXRechnungXMLHelper.SelectNode(xml,'//cbc:BuyerReference',node) then
      _Invoice.BuyerReference := node.Text;
    if TXRechnungXMLHelper.SelectNode(xml,'//cac:InvoicePeriod',node) then
    begin
      _Invoice.InvoicePeriodStartDate := TXRechnungHelper.DateFromStrUBLFormat(TXRechnungXMLHelper.SelectNodeText(node,'//cbc:StartDate'));
      _Invoice.InvoicePeriodEndDate := TXRechnungHelper.DateFromStrUBLFormat(TXRechnungXMLHelper.SelectNodeText(node,'//cbc:EndDate'));
    end;
    if TXRechnungXMLHelper.SelectNode(xml,'//cac:OrderReference/cbc:ID',node) then
      _Invoice.PurchaseOrderReference := node.Text;
    if TXRechnungXMLHelper.SelectNodes(xml,'//cac:BillingReference/cac:InvoiceDocumentReference',nodes) then
    for i := 0  to nodes.length-1 do
    with _Invoice.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
    begin
      ID := TXRechnungXMLHelper.SelectNodeText(nodes.item[i],'.//cbc:ID');
      IssueDate := TXRechnungHelper.DateFromStrUBLFormat(TXRechnungXMLHelper.SelectNodeText(nodes.item[i],'.//cbc:IssueDate'));
    end;
    if TXRechnungXMLHelper.SelectNode(xml,'//cac:ContractDocumentReference/cbc:ID',node) then
      _Invoice.ContractDocumentReference := node.Text;
    if TXRechnungXMLHelper.SelectNodes(xml,'//cac:AdditionalDocumentReference',nodes) then
    for i := 0  to nodes.length-1 do
    with _Invoice.Attachments.AddAttachment(iat_application_None) do
    begin
      ID := TXRechnungXMLHelper.SelectNodeText(nodes.item[i],'.//cbc:ID');
      DocumentDescription := TXRechnungXMLHelper.SelectNodeText(nodes.item[i],'.//cbc:DocumentDescription');
      if TXRechnungXMLHelper.SelectNode(nodes.item[i],'.//cac:Attachment',node) then
      begin
        if TXRechnungXMLHelper.FindNode(node,'.//cac:ExternalReference/cbc:URI') then
          ExternalReference := TXRechnungXMLHelper.SelectNodeText(node,'.//cac:ExternalReference/cbc:URI')
        else
        if TXRechnungXMLHelper.SelectNode(node,'.//cbc:EmbeddedDocumentBinaryObject',node2) then
        begin
          AttachmentType := TXRechnungHelper.InvoiceAttachmentTypeFromStr(node2.Attributes.getNamedItem('mimeCode').Text);
          Filename := node2.Attributes.getNamedItem('filename').Text;
          SetDataFromBase64(node2.Text);
        end;
      end;
    end;
    if TXRechnungXMLHelper.SelectNode(xml,'//cac:ProjectReference/cbc:ID',node) then
      _Invoice.ProjectReference := node.Text;

//  with xRoot.AddChild('cac:AccountingSupplierParty').AddChild('cac:Party') do
//  begin
//    if _Invoice.AccountingSupplierParty.IdentifierSellerBuyer <> '' then
//    with AddChild('cac:PartyIdentification') do
//    begin
//      AddChild('cbc:ID').Text := _Invoice.AccountingSupplierParty.IdentifierSellerBuyer;
//    end;
//    with AddChild('cac:PartyName') do
//    begin
//      AddChild('cbc:Name').Text := _Invoice.AccountingSupplierParty.Name;
//    end;
//    with AddChild('cac:PostalAddress') do
//    begin
//      AddChild('cbc:StreetName').Text := _Invoice.AccountingSupplierParty.Address.StreetName;
//      if _Invoice.AccountingSupplierParty.Address.AdditionalStreetName <> '' then
//        AddChild('cbc:AdditionalStreetName').Text := _Invoice.AccountingSupplierParty.Address.AdditionalStreetName;
//      AddChild('cbc:CityName').Text := _Invoice.AccountingSupplierParty.Address.City;
//      AddChild('cbc:PostalZone').Text := _Invoice.AccountingSupplierParty.Address.PostalZone;
//      if _Invoice.AccountingSupplierParty.Address.CountrySubentity <> '' then
//        AddChild('cbc:CountrySubentity').Text := _Invoice.AccountingSupplierParty.Address.CountrySubentity;
//      if _Invoice.AccountingSupplierParty.Address.AddressLine <> '' then
//        AddChild('cac:AddressLine').AddChild('cbc:Line').Text := _Invoice.AccountingSupplierParty.Address.AddressLine;
//      AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.AccountingSupplierParty.Address.CountryCode;
//    end;
//    if _Invoice.AccountingSupplierParty.VATCompanyID <> '' then
//    with AddChild('cac:PartyTaxScheme') do
//    begin
//      AddChild('cbc:CompanyID').Text := _Invoice.AccountingSupplierParty.VATCompanyID;
//      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
//    end;
//    with AddChild('cac:PartyLegalEntity') do
//    begin
//      AddChild('cbc:RegistrationName').Text := _Invoice.AccountingSupplierParty.RegistrationName;
//      AddChild('cbc:CompanyID').Text := _Invoice.AccountingSupplierParty.CompanyID;
//    end;
//    with AddChild('cac:Contact') do
//    begin
//      AddChild('cbc:Name').Text := _Invoice.AccountingSupplierParty.ContactName;
//      AddChild('cbc:Telephone').Text := _Invoice.AccountingSupplierParty.ContactTelephone;
//      AddChild('cbc:ElectronicMail').Text := _Invoice.AccountingSupplierParty.ContactElectronicMail;
//    end;
//  end;
//
//  with xRoot.AddChild('cac:AccountingCustomerParty').AddChild('cac:Party') do
//  begin
//    if _Invoice.AccountingCustomerParty.IdentifierSellerBuyer <> '' then
//    with AddChild('cac:PartyIdentification') do
//    begin
//      AddChild('cbc:ID').Text := _Invoice.AccountingCustomerParty.IdentifierSellerBuyer;
//    end;
//    with AddChild('cac:PartyName') do
//    begin
//      AddChild('cbc:Name').Text := _Invoice.AccountingCustomerParty.Name;
//    end;
//    with AddChild('cac:PostalAddress') do
//    begin
//      AddChild('cbc:StreetName').Text := _Invoice.AccountingCustomerParty.Address.StreetName;
//      if _Invoice.AccountingCustomerParty.Address.AdditionalStreetName <> '' then
//        AddChild('cbc:AdditionalStreetName').Text := _Invoice.AccountingCustomerParty.Address.AdditionalStreetName;
//      AddChild('cbc:CityName').Text := _Invoice.AccountingCustomerParty.Address.City;
//      AddChild('cbc:PostalZone').Text := _Invoice.AccountingCustomerParty.Address.PostalZone;
//      if _Invoice.AccountingCustomerParty.Address.CountrySubentity <> '' then
//        AddChild('cbc:CountrySubentity').Text := _Invoice.AccountingCustomerParty.Address.CountrySubentity;
//      if _Invoice.AccountingCustomerParty.Address.AddressLine <> '' then
//        AddChild('cac:AddressLine').AddChild('cbc:Line').Text := _Invoice.AccountingCustomerParty.Address.AddressLine;
//      AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.AccountingCustomerParty.Address.CountryCode;
//    end;
//    if _Invoice.AccountingCustomerParty.VATCompanyID <> '' then
//    with AddChild('cac:PartyTaxScheme') do
//    begin
//      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.VATCompanyID;
//      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
//    end;
//    with AddChild('cac:PartyLegalEntity') do
//    begin
//      AddChild('cbc:RegistrationName').Text := _Invoice.AccountingCustomerParty.RegistrationName;
//      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.CompanyID;
//      //TODO <cbc:CompanyLegalForm>123/456/7890, HRA-Eintrag in []</cbc:CompanyLegalForm>
//    end;
//    with AddChild('cac:Contact') do
//    begin
//      AddChild('cbc:Name').Text := _Invoice.AccountingCustomerParty.ContactName;
//      AddChild('cbc:Telephone').Text := _Invoice.AccountingCustomerParty.ContactTelephone;
//      AddChild('cbc:ElectronicMail').Text := _Invoice.AccountingCustomerParty.ContactElectronicMail;
//    end;
//  end;
//
//  if (_Invoice.DeliveryInformation.ActualDeliveryDate > 0) or
//     (_Invoice.DeliveryInformation.Address.CountryCode <> '') or
//     (_Invoice.DeliveryInformation.Name <> '') then
//  with xRoot.AddChild('cac:Delivery') do
//  begin
//    if (_Invoice.DeliveryInformation.ActualDeliveryDate > 0) then
//      AddChild('cbc:ActualDeliveryDate').Text := TXRechnungHelper.DateToStrUBLFormat(_Invoice.DeliveryInformation.ActualDeliveryDate);
//    with AddChild('cac:DeliveryLocation') do
//    begin
//      //if (_Invoice.DeliveryInformation.LocationIdentifier <> '') then
//      //  AddChild('cbc:ID').Text := _Invoice.DeliveryInformation.LocationIdentifier; //TODO schemeID https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice/cac-Delivery/cac-DeliveryLocation/cbc-ID/
//      with AddChild('cac:Address') do
//      begin
//        AddChild('cbc:StreetName').Text := _Invoice.DeliveryInformation.Address.StreetName;
//        if _Invoice.DeliveryInformation.Address.AdditionalStreetName <> '' then
//          AddChild('cbc:AdditionalStreetName').Text := _Invoice.DeliveryInformation.Address.AdditionalStreetName;
//        AddChild('cbc:CityName').Text := _Invoice.DeliveryInformation.Address.City;
//        AddChild('cbc:PostalZone').Text := _Invoice.DeliveryInformation.Address.PostalZone;
//        if _Invoice.DeliveryInformation.Address.CountrySubentity <> '' then
//          AddChild('cbc:CountrySubentity').Text := _Invoice.DeliveryInformation.Address.CountrySubentity;
//        if _Invoice.DeliveryInformation.Address.AddressLine <> '' then
//          AddChild('cac:AddressLine').AddChild('cbc:Line').Text := _Invoice.DeliveryInformation.Address.AddressLine;
//        AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.DeliveryInformation.Address.CountryCode;
//      end;
//    end;
//    if (_Invoice.DeliveryInformation.Name <> '') then
//      AddChild('cac:DeliveryParty').AddChild('cac:PartyName').AddChild('cbc:Name').Text := _Invoice.DeliveryInformation.Name;
//  end;
//
//  if (_Invoice.PaymentMeansCode <> ipmc_None) and (_Invoice.PayeeFinancialAccount <> '') then
//  with xRoot.AddChild('cac:PaymentMeans') do
//  begin
//    AddChild('cbc:PaymentMeansCode').Text := TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Invoice.PaymentMeansCode);
//    if _Invoice.PaymentID <> '' then
//      AddChild('cbc:PaymentID').Text := _Invoice.PaymentID;
//    with AddChild('cac:PayeeFinancialAccount') do
//    begin
//      AddChild('cbc:ID').Text := _Invoice.PayeeFinancialAccount;
//      if _Invoice.PayeeFinancialAccountName <> '' then
//        AddChild('cbc:Name').Text := _Invoice.PayeeFinancialAccountName;
//      if _Invoice.PayeeFinancialInstitutionBranch <> '' then
//        AddChild('cac:FinancialInstitutionBranch').AddChild('cbc:ID').Text := _Invoice.PayeeFinancialInstitutionBranch;
//    end;
//  end;
//
//  case _Invoice.PaymentTermsType of
//    iptt_Net:
//      with xRoot.AddChild('cac:PaymentTerms') do
//      begin
//        AddChild('cbc:Note').Text := System.StrUtils.ReplaceText(_Invoice.PaymentTermNetNote,'#',' ');
//      end;
//    iptt_CashDiscount1:
//      with xRoot.AddChild('cac:PaymentTerms') do
//      begin
//        AddChild('cbc:Note').Text := Format('#SKONTO#TAGE=%d#PROZENT=%s#',
//          [_Invoice.PaymentTermCashDiscount1Days,
//           TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount1Percent)])+
//          IfThen(_Invoice.PaymentTermCashDiscount1Base <> 0,'BASISBETRAG='+
//            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','')+
//          IfThen(_Version = XRechnungVersion_201_UBL,#13#10,'');
//      end;
//    iptt_CashDiscount2:
//    begin
//      with xRoot.AddChild('cac:PaymentTerms') do
//      begin
//        AddChild('cbc:Note').Text := Format('#SKONTO#TAGE=%d#PROZENT=%s#',
//          [_Invoice.PaymentTermCashDiscount1Days,
//           TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount1Percent)])+
//          IfThen(_Invoice.PaymentTermCashDiscount1Base <> 0,'BASISBETRAG='+
//            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','')+
//          #13#10+
//          Format('#SKONTO#TAGE=%d#PROZENT=%s#',
//          [_Invoice.PaymentTermCashDiscount2Days,
//           TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount2Percent)])+
//          IfThen(_Invoice.PaymentTermCashDiscount2Base <> 0,'BASISBETRAG='+
//            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount2Base)+'#','')+
//          IfThen(_Version = XRechnungVersion_201_UBL,#13#10,'');
//      end;
//    end;
//  end;
//
//  for allowanceCharge in _Invoice.AllowanceCharges do
//  with xRoot.AddChild('cac:AllowanceCharge') do
//  begin
//    AddChild('cbc:ChargeIndicator').Text := LowerCase(BoolToStr(allowanceCharge.ChargeIndicator,true));
//    AddChild('cbc:AllowanceChargeReasonCode').Text :=
//             IfThen(allowanceCharge.ChargeIndicator,
//             TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeToStr(allowanceCharge.ReasonCodeCharge),
//             TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeToStr(allowanceCharge.ReasonCodeAllowance));
//    AddChild('cbc:AllowanceChargeReason').Text := allowanceCharge.Reason;
//    AddChild('cbc:MultiplierFactorNumeric').Text := TXRechnungHelper.FloatToStr(allowanceCharge.MultiplierFactorNumeric);
//    with AddChild('cbc:Amount') do
//    begin
//      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//      Text := TXRechnungHelper.AmountToStr(allowanceCharge.Amount);
//    end;
//    with AddChild('cbc:BaseAmount') do
//    begin
//      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//      Text := TXRechnungHelper.AmountToStr(allowanceCharge.BaseAmount);
//    end;
//    with AddChild('cac:TaxCategory') do
//    begin
//      AddChild('cbc:ID').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(allowanceCharge.TaxCategory);
//      AddChild('cbc:Percent').Text := TXRechnungHelper.PercentageToStr(allowanceCharge.TaxPercent);
//      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
//    end;
//  end;
//
//  with xRoot.AddChild('cac:TaxTotal') do
//  begin
//    with AddChild('cbc:TaxAmount') do
//    begin
//      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//      Text := TXRechnungHelper.AmountToStr(_Invoice.TaxAmountTotal);
//    end;
//    for taxSubtotal in _Invoice.TaxAmountSubtotals do
//    with AddChild('cac:TaxSubtotal') do
//    begin
//      with AddChild('cbc:TaxableAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(taxSubtotal.TaxableAmount);
//      end;
//      with AddChild('cbc:TaxAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(taxSubtotal.TaxAmount);
//      end;
//      with AddChild('cac:TaxCategory') do
//      begin
//        AddChild('cbc:ID').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(taxSubtotal.TaxCategory);
//        AddChild('cbc:Percent').Text := TXRechnungHelper.PercentageToStr(taxSubtotal.TaxPercent);
//        if taxSubtotal.TaxExemptionReason <> '' then
//          AddChild('cbc:TaxExemptionReason').Text := taxSubtotal.TaxExemptionReason;
//        AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
//      end;
//    end;
//  end;
//
//  with xRoot.AddChild('cac:LegalMonetaryTotal') do
//  begin
//      with AddChild('cbc:LineExtensionAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(_Invoice.LineAmount);
//      end;
//      with AddChild('cbc:TaxExclusiveAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(_Invoice.TaxExclusiveAmount);
//      end;
//      with AddChild('cbc:TaxInclusiveAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(_Invoice.TaxInclusiveAmount);
//      end;
//      with AddChild('cbc:AllowanceTotalAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(_Invoice.AllowanceTotalAmount);
//      end;
//      with AddChild('cbc:ChargeTotalAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(_Invoice.ChargeTotalAmount);
//      end;
//      with AddChild('cbc:PrepaidAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(_Invoice.PrepaidAmount);
//      end;
//      //      <cbc:PayableRoundingAmount currencyID="EUR">0</cbc:PayableRoundingAmount>
//      with AddChild('cbc:PayableAmount') do
//      begin
//        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
//        Text := TXRechnungHelper.AmountToStr(_Invoice.PayableAmount);
//      end;
//  end;
//
//  for i := 0 to _Invoice.InvoiceLines.Count-1 do
//    InternalAddInvoiceLine(_Invoice.InvoiceLines[i],xRoot.AddChild('cac:InvoiceLine'));

    Result := true;
  except
    on E:Exception do _Error := E.ClassName+' '+E.Message;
  end;
end;

class function TXRechnungInvoiceAdapter.LoadDocumentUNCEFACT(_Invoice: TInvoice;
  _Xml: IXMLDocument; out _Error : String) : Boolean;
var
  xml : IXMLDOMDocument2;
  node,node2,node3,node4,nodeSupplyChainTradeTransaction,
  nodeApplicableHeaderTradeAgreement : IXMLDOMNode;
  nodes : IXMLDOMNodeList;
  i : Integer;
  paymentTermsText : String;
  paymentTerms,paymentTerm : TArray<String>;

  procedure InternalReadInvoiceLine(_Invoiceline : TInvoiceLine; _Node : IXMLDOMNode);
  var
    nodei,node2i,node3i : IXMLDOMNode;
    nodesi : IXMLDOMNodeList;
    ii : Integer;
  begin
    if TXRechnungXMLHelper.SelectNode(_Node,'.//ram:AssociatedDocumentLineDocument',node2i) then
    begin
      _Invoiceline.ID := TXRechnungXMLHelper.SelectNodeText(node2i,'.//ram:LineID');
      if TXRechnungXMLHelper.SelectNode(node2i,'.//ram:IncludedNote',nodei) then
        _Invoiceline.Note := TXRechnungXMLHelper.SelectNodeText(nodei,'.//ram:Content');
    end;
    if TXRechnungXMLHelper.SelectNode(_Node,'.//ram:SpecifiedTradeProduct',node2i) then
    begin
      _Invoiceline.SellersItemIdentification := TXRechnungXMLHelper.SelectNodeText(node2i,'.//ram:SellerAssignedID');
      _Invoiceline.Name := TXRechnungXMLHelper.SelectNodeText(node2i,'.//ram:Name');
      _Invoiceline.Description := TXRechnungXMLHelper.SelectNodeText(node2i,'.//ram:Description');
    end;
    if TXRechnungXMLHelper.SelectNode(_Node,'.//ram:SpecifiedLineTradeAgreement',node2i) then
    begin
//        <ram:BuyerOrderReferencedDocument>
//            <ram:LineID>6171175.1</ram:LineID>
//        </ram:BuyerOrderReferencedDocument>
//        <cac:OrderLineReference>
//            <cbc:LineID>6171175.1</cbc:LineID>
//        </cac:OrderLineReference>
      if TXRechnungXMLHelper.SelectNode(node2i,'.//ram:NetPriceProductTradePrice',node3i) then
      begin
        _Invoiceline.PriceAmount := TXRechnungHelper.UnitPriceAmountFromStr(TXRechnungXMLHelper.SelectNodeText(node3i,'.//ram:ChargeAmount'));
        if TXRechnungXMLHelper.SelectNode(node3i,'.//ram:BaseQuantity',nodei) then
        begin
          _Invoiceline.BaseQuantityUnitCode := TXRechnungHelper.InvoiceUnitCodeFromStr(nodei.attributes.getNamedItem('unitCode').Text);
          _Invoiceline.BaseQuantity := StrToIntDef(nodei.text,0);
        end;
      end;
    end;
    if TXRechnungXMLHelper.SelectNode(_Node,'.//ram:SpecifiedLineTradeDelivery',node2i) then
    if TXRechnungXMLHelper.SelectNode(node2i,'.//ram:BilledQuantity',nodei) then
    begin
      _Invoiceline.UnitCode := TXRechnungHelper.InvoiceUnitCodeFromStr(nodei.attributes.getNamedItem('unitCode').Text);
      _Invoiceline.Quantity := TXRechnungHelper.QuantityFromStr(nodei.text);
    end;
    if TXRechnungXMLHelper.SelectNode(_Node,'.//ram:SpecifiedLineTradeSettlement',node2i) then
    begin
      if TXRechnungXMLHelper.SelectNode(node2i,'.//ram:ApplicableTradeTax',nodei) then
      begin
        _Invoiceline.TaxCategory := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeFromStr(TXRechnungXMLHelper.SelectNodeText(nodei,'.//ram:CategoryCode'));
        _Invoiceline.TaxPercent := TXRechnungHelper.PercentageFromStr(TXRechnungXMLHelper.SelectNodeText(nodei,'.//ram:RateApplicablePercent'));
      end;
      if TXRechnungXMLHelper.SelectNodes(node2i,'.//ram:SpecifiedTradeAllowanceCharge',nodesi) then
      for ii := 0 to nodesi.length-1 do
      with _Invoiceline.AllowanceCharges.AddAllowanceCharge do
      begin
        if TXRechnungXMLHelper.SelectNode(nodesi[i],'.//ram:ChargeIndicator',node2i) then
        if TXRechnungXMLHelper.SelectNode(node2i,'.//udt:Indicator',nodei) then
          ChargeIndicator := StrToBoolDef(nodei.text,false);
        MultiplierFactorNumeric := TXRechnungHelper.FloatFromStr(TXRechnungXMLHelper.SelectNodeText(nodesi[i],'.//ram:CalculationPercent'));
        BaseAmount  := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(nodesi[i],'.//ram:BasisAmount'));
        Amount  := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(nodesi[i],'.//ram:ActualAmount'));
        if ChargeIndicator then
          ReasonCodeCharge := TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeFromStr(TXRechnungXMLHelper.SelectNodeText(nodesi[i],'.//ram:ReasonCode'))
        else
          ReasonCodeAllowance := TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeFromStr(TXRechnungXMLHelper.SelectNodeText(nodesi[i],'.//ram:ReasonCode'));
        Reason := TXRechnungXMLHelper.SelectNodeText(nodesi[i],'.//ram:Reason');
      end;
      if TXRechnungXMLHelper.SelectNode(node2i,'.//ram:SpecifiedTradeSettlementLineMonetarySummation',nodei) then
      begin
        _Invoiceline.LineAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(nodei,'.//ram:LineTotalAmount'));
      end;
    end;
    if TXRechnungXMLHelper.SelectNodes(_Node,'.//ram:IncludedSupplyChainTradeLineItem',nodesi) then
    if nodesi.length > 0 then
      raise Exception.Create('Reading SubInvoiceLines in UNCEFACT not implemented');
  end;

begin
  Result := false;
  _Error := '';
  try
    xml := TXRechnungXMLHelper.PrepareDocumentForXPathQuerys(_Xml);
    if TXRechnungXMLHelper.SelectNode(xml,'//rsm:ExchangedDocument/ram:ID',node) then
      _Invoice.InvoiceNumber := node.Text;
    if TXRechnungXMLHelper.SelectNode(xml,'//rsm:ExchangedDocument/ram:IssueDateTime/udt:DateTimeString',node) then
      _Invoice.InvoiceIssueDate := TXRechnungHelper.DateFromStrUNCEFACTFormat(node.Text);
    if TXRechnungXMLHelper.SelectNode(xml,'//rsm:ExchangedDocument/ram:TypeCode',node) then
      _Invoice.InvoiceTypeCode := TXRechnungHelper.InvoiceTypeCodeFromStr(node.Text);
    if TXRechnungXMLHelper.SelectNode(xml,'//rsm:ExchangedDocument/ram:IncludedNote/ram:Content',node) then
      _Invoice.Note := node.Text;

    if not TXRechnungXMLHelper.SelectNode(xml,'//rsm:SupplyChainTradeTransaction',nodeSupplyChainTradeTransaction) then
      exit;

    if TXRechnungXMLHelper.SelectNodes(nodeSupplyChainTradeTransaction,'.//ram:IncludedSupplyChainTradeLineItem',nodes) then
    for i := 0 to nodes.length-1 do
      InternalReadInvoiceLine(_Invoice.InvoiceLines.AddInvoiceLine,nodes[i]);

    if TXRechnungXMLHelper.SelectNode(nodeSupplyChainTradeTransaction,'.//ram:ApplicableHeaderTradeAgreement',nodeApplicableHeaderTradeAgreement) then
    begin
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:BuyerReference',node) then
        _Invoice.BuyerReference := node.text;

      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:SellerTradeParty',node2) then
      begin
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:ID',node) then
          _Invoice.AccountingSupplierParty.IdentifierSellerBuyer := node.text;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:Name',node) then
          _Invoice.AccountingSupplierParty.RegistrationName := node.text;

        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:SpecifiedLegalOrganization',node3) then
        begin
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:ID',node) then
            _Invoice.AccountingSupplierParty.CompanyID := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:TradingBusinessName',node) then
            _Invoice.AccountingSupplierParty.Name := node.text;
        end;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:DefinedTradeContact',node3) then
        begin
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:PersonName',node) then
            _Invoice.AccountingSupplierParty.ContactName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:TelephoneUniversalCommunication',node4) then
          if TXRechnungXMLHelper.SelectNode(node4,'.//ram:CompleteNumber',node) then
            _Invoice.AccountingSupplierParty.ContactTelephone := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:EmailURIUniversalCommunication',node4) then
          if TXRechnungXMLHelper.SelectNode(node4,'.//ram:URIID',node) then
            _Invoice.AccountingSupplierParty.ContactElectronicMail := node.text;
        end;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:PostalTradeAddress',node3) then
        begin
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:PostcodeCode',node) then
            _Invoice.AccountingSupplierParty.Address.PostalZone := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineOne',node) then
            _Invoice.AccountingSupplierParty.Address.StreetName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineTwo',node) then
            _Invoice.AccountingSupplierParty.Address.AdditionalStreetName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineThree',node) then
            _Invoice.AccountingSupplierParty.Address.AddressLine := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CityName',node) then
            _Invoice.AccountingSupplierParty.Address.City := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CountryID',node) then
            _Invoice.AccountingSupplierParty.Address.CountryCode := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CountrySubDivisionName',node) then
            _Invoice.AccountingSupplierParty.Address.CountrySubentity := node.text;
        end;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:SpecifiedTaxRegistration',node3) then
        if TXRechnungXMLHelper.SelectNode(node3,'.//ram:ID',node4) then
          _Invoice.AccountingSupplierParty.VATCompanyID := node4.text;
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:BuyerTradeParty',node2) then
      begin
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:ID',node) then
          _Invoice.AccountingCustomerParty.IdentifierSellerBuyer := node.text;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:Name',node) then
          _Invoice.AccountingCustomerParty.RegistrationName := node.text;

        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:SpecifiedLegalOrganization',node3) then
        begin
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:ID',node) then
            _Invoice.AccountingCustomerParty.CompanyID := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:TradingBusinessName',node) then
            _Invoice.AccountingCustomerParty.Name := node.text;
        end;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:DefinedTradeContact',node3) then
        begin
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:PersonName',node) then
            _Invoice.AccountingCustomerParty.ContactName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:TelephoneUniversalCommunication',node4) then
          if TXRechnungXMLHelper.SelectNode(node4,'.//ram:CompleteNumber',node) then
            _Invoice.AccountingCustomerParty.ContactTelephone := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:EmailURIUniversalCommunication',node4) then
          if TXRechnungXMLHelper.SelectNode(node4,'.//ram:URIID',node) then
            _Invoice.AccountingCustomerParty.ContactElectronicMail := node.text;
        end;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:PostalTradeAddress',node3) then
        begin
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:PostcodeCode',node) then
            _Invoice.AccountingCustomerParty.Address.PostalZone := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineOne',node) then
            _Invoice.AccountingCustomerParty.Address.StreetName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineTwo',node) then
            _Invoice.AccountingCustomerParty.Address.AdditionalStreetName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineThree',node) then
            _Invoice.AccountingCustomerParty.Address.AddressLine := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CityName',node) then
            _Invoice.AccountingCustomerParty.Address.City := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CountryID',node) then
            _Invoice.AccountingCustomerParty.Address.CountryCode := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CountrySubDivisionName',node) then
            _Invoice.AccountingCustomerParty.Address.CountrySubentity := node.text;
        end;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:SpecifiedTaxRegistration',node3) then
        if TXRechnungXMLHelper.SelectNode(node3,'.//ram:ID',node4) then
          _Invoice.AccountingCustomerParty.VATCompanyID := node4.text;
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:BuyerOrderReferencedDocument',node2) then
      if TXRechnungXMLHelper.SelectNode(node2,'.//ram:IssuerAssignedID',node) then
        _Invoice.PurchaseOrderReference := node.text;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:ContractReferencedDocument',node2) then
      if TXRechnungXMLHelper.SelectNode(node2,'.//ram:IssuerAssignedID',node) then
        _Invoice.ContractDocumentReference := node.text;

      if TXRechnungXMLHelper.SelectNodes(nodeApplicableHeaderTradeAgreement,'.//ram:AdditionalReferencedDocument',nodes) then
      for i := 0  to nodes.length-1 do
      with _Invoice.Attachments.AddAttachment(iat_application_None) do
      begin
        ID := TXRechnungXMLHelper.SelectNodeText(nodes.item[i],'.//ram:IssuerAssignedID');
        DocumentDescription := TXRechnungXMLHelper.SelectNodeText(nodes.item[i],'.//ram:Name');
        if TXRechnungXMLHelper.FindNode(nodes.item[i],'.//ram:URIID') then
          ExternalReference := TXRechnungXMLHelper.SelectNodeText(nodes.item[i],'.//ram:URIID')
        else
        if TXRechnungXMLHelper.SelectNode(nodes.item[i],'.//ram:AttachmentBinaryObject',node) then
        begin
          AttachmentType := TXRechnungHelper.InvoiceAttachmentTypeFromStr(node.Attributes.getNamedItem('mimeCode').Text);
          Filename := node.Attributes.getNamedItem('filename').Text;
          SetDataFromBase64(node.Text);
        end;
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:SpecifiedProcuringProject',node2) then
      begin
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:ID',node) then
          _Invoice.ProjectReference := node.text;
      end;
    end;
    if TXRechnungXMLHelper.SelectNode(nodeSupplyChainTradeTransaction,'.//ram:ApplicableHeaderTradeDelivery',nodeApplicableHeaderTradeAgreement) then
    begin
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:ShipToTradeParty',node2) then
      begin
        _Invoice.DeliveryInformation.Name := TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:Name');
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:PostalTradeAddress',node3) then
        begin
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:PostcodeCode',node) then
            _Invoice.DeliveryInformation.Address.PostalZone := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineOne',node) then
            _Invoice.DeliveryInformation.Address.StreetName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineTwo',node) then
            _Invoice.DeliveryInformation.Address.AdditionalStreetName := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:LineThree',node) then
            _Invoice.DeliveryInformation.Address.AddressLine := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CityName',node) then
            _Invoice.DeliveryInformation.Address.City := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CountryID',node) then
            _Invoice.DeliveryInformation.Address.CountryCode := node.text;
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:CountrySubDivisionName',node) then
            _Invoice.DeliveryInformation.Address.CountrySubentity := node.text;
        end;
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:ActualDeliverySupplyChainEvent',node2) then
      if TXRechnungXMLHelper.SelectNode(node2,'.//ram:OccurrenceDateTime',node3) then
      if TXRechnungXMLHelper.SelectNode(node3,'.//udt:DateTimeString',node) then
        _Invoice.DeliveryInformation.ActualDeliveryDate := TXRechnungHelper.DateFromStrUNCEFACTFormat(node.Text);
    end;
    if TXRechnungXMLHelper.SelectNode(nodeSupplyChainTradeTransaction,'.//ram:ApplicableHeaderTradeSettlement',nodeApplicableHeaderTradeAgreement) then
    begin
      _Invoice.PaymentID := TXRechnungXMLHelper.SelectNodeText(nodeApplicableHeaderTradeAgreement,'.//ram:PaymentReference');
      _Invoice.InvoiceCurrencyCode := TXRechnungXMLHelper.SelectNodeText(nodeApplicableHeaderTradeAgreement,'.//ram:InvoiceCurrencyCode');
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:SpecifiedTradeSettlementPaymentMeans',node2) then
      begin
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:TypeCode',node) then
          _Invoice.PaymentMeansCode := TXRechnungHelper.InvoicePaymentMeansCodeFromStr(node.text);
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:PayeePartyCreditorFinancialAccount',node3) then
        begin
          _Invoice.PayeeFinancialAccount := TXRechnungXMLHelper.SelectNodeText(node3,'.//ram:IBANID');
          _Invoice.PayeeFinancialAccountName := TXRechnungXMLHelper.SelectNodeText(node3,'.//ram:AccountName');
        end;
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:PayeeSpecifiedCreditorFinancialInstitution',node3) then
          _Invoice.PayeeFinancialInstitutionBranch := TXRechnungXMLHelper.SelectNodeText(node3,'.//ram:BICID');
      end;
      if TXRechnungXMLHelper.SelectNodes(nodeApplicableHeaderTradeAgreement,'.//ram:ApplicableTradeTax',nodes) then
      for i := 0 to nodes.length-1 do
      begin
        SetLength(_Invoice.TaxAmountSubtotals,Length(_Invoice.TaxAmountSubtotals)+1);
        _Invoice.TaxAmountSubtotals[Length(_Invoice.TaxAmountSubtotals)-1].TaxAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:CalculatedAmount'));
        _Invoice.TaxAmountSubtotals[Length(_Invoice.TaxAmountSubtotals)-1].TaxExemptionReason := TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:ExemptionReason');
        _Invoice.TaxAmountSubtotals[Length(_Invoice.TaxAmountSubtotals)-1].TaxableAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:BasisAmount'));
        _Invoice.TaxAmountSubtotals[Length(_Invoice.TaxAmountSubtotals)-1].TaxCategory := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:CategoryCode'));
        _Invoice.TaxAmountSubtotals[Length(_Invoice.TaxAmountSubtotals)-1].TaxPercent := TXRechnungHelper.PercentageFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:RateApplicablePercent'));
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:BillingSpecifiedPeriod',node2) then
      begin
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:StartDateTime',node) then
          _Invoice.InvoicePeriodStartDate := TXRechnungHelper.DateFromStrUNCEFACTFormat(TXRechnungXMLHelper.SelectNodeText(node,'.//udt:DateTimeString'));
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:EndDateTime',node) then
          _Invoice.InvoicePeriodEndDate := TXRechnungHelper.DateFromStrUNCEFACTFormat(TXRechnungXMLHelper.SelectNodeText(node,'.//udt:DateTimeString'));
      end;
      if TXRechnungXMLHelper.SelectNodes(nodeApplicableHeaderTradeAgreement,'.//ram:SpecifiedTradeAllowanceCharge',nodes) then
      for i := 0 to nodes.length-1 do
      with _Invoice.AllowanceCharges.AddAllowanceCharge do
      begin
        if TXRechnungXMLHelper.SelectNode(nodes[i],'.//ram:ChargeIndicator',node2) then
        if TXRechnungXMLHelper.SelectNode(node2,'.//udt:Indicator',node) then
          ChargeIndicator := StrToBoolDef(node.text,false);
        MultiplierFactorNumeric := TXRechnungHelper.FloatFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:CalculationPercent'));
        BaseAmount  := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:BasisAmount'));
        Amount  := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:ActualAmount'));
        if ChargeIndicator then
          ReasonCodeCharge := TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:ReasonCode'))
        else
          ReasonCodeAllowance := TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeFromStr(TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:ReasonCode'));
        Reason := TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:Reason');
        if TXRechnungXMLHelper.SelectNode(nodes[i],'.//ram:CategoryTradeTax',node2) then
        begin
          TaxCategory := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:CategoryCode'));
          TaxPercent := TXRechnungHelper.PercentageFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:RateApplicablePercent'));
        end;
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:SpecifiedTradePaymentTerms',node2) then
      begin
        paymentTermsText := TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:Description');
        if Pos('#SKONTO#',paymentTermsText) = 0 then
        begin
          _Invoice.PaymentTermsType := iptt_Net;
          _Invoice.PaymentTermNetNote := paymentTermsText;
        end else
        if (Pos('#SKONTO#',paymentTermsText) = 1) and (Pos(#10,Trim(paymentTermsText)) > 1) then //zweimal Skonto
        begin
          _Invoice.PaymentTermsType := iptt_CashDiscount2;
          paymentTerms := paymentTermsText.Split([#10]);
          if (Length(paymentTerms) >= 2) then if (Pos('#SKONTO#',paymentTerms[1]) = 1) then
          begin
            paymentTerm := paymentTerms[1].Split(['#']); //0 Leer, 1 Skonto, 2 Tage, 3 Prozent, 4 Leer o. Basiswert
            if (Length(paymentTerm) >= 5) then
            begin
              Delete(paymentTerm[2],1,5);
              _Invoice.PaymentTermCashDiscount2Days := StrToIntDef(paymentTerm[2],0);
              Delete(paymentTerm[3],1,8);
              _Invoice.PaymentTermCashDiscount2Percent := TXRechnungHelper.FloatFromStr(paymentTerm[3]);
              if Pos('BASISBETRAG=',paymentTerm[4])=1 then
              begin
                Delete(paymentTerm[4],1,12);
                _Invoice.PaymentTermCashDiscount2Base := TXRechnungHelper.AmountFromStr(paymentTerm[4]);
              end
              else
                _Invoice.PaymentTermCashDiscount2Base := 0;
            end;
          end else
            _Invoice.PaymentTermsType := iptt_CashDiscount1;
          if (Length(paymentTerms) >= 1) then if (Pos('#SKONTO#',paymentTerms[0]) = 1) then
          begin
            paymentTerm := paymentTerms[0].Split(['#']); //0 Leer, 1 Skonto, 2 Tage, 3 Prozent, 4 Leer o. Basiswert
            if (Length(paymentTerm) >= 5) then
            begin
              Delete(paymentTerm[2],1,5);
              _Invoice.PaymentTermCashDiscount1Days := StrToIntDef(paymentTerm[2],0);
              Delete(paymentTerm[3],1,8);
              _Invoice.PaymentTermCashDiscount1Percent := TXRechnungHelper.FloatFromStr(paymentTerm[3]);
              if Pos('BASISBETRAG=',paymentTerm[4])=1 then
              begin
                Delete(paymentTerm[4],1,12);
                _Invoice.PaymentTermCashDiscount1Base := TXRechnungHelper.AmountFromStr(paymentTerm[4]);
              end
              else
                _Invoice.PaymentTermCashDiscount1Base := 0;
            end;
          end else
            _Invoice.PaymentTermsType := iptt_Net;
        end else
        if Pos('#SKONTO#',paymentTermsText) = 1 then //einmal Skonto
        begin
          _Invoice.PaymentTermsType := iptt_CashDiscount1;
          paymentTerm := paymentTermsText.Split(['#']); //0 Leer, 1 Skonto, 2 Tage, 3 Prozent, 4 Leer o. Basiswert
          if (Length(paymentTerm) >= 5) then
          begin
            Delete(paymentTerm[2],1,5);
            _Invoice.PaymentTermCashDiscount1Days := StrToIntDef(paymentTerm[2],0);
            Delete(paymentTerm[3],1,8);
            _Invoice.PaymentTermCashDiscount1Percent := TXRechnungHelper.FloatFromStr(paymentTerm[3]);
            if Pos('BASISBETRAG=',paymentTerm[4])=1 then
            begin
              Delete(paymentTerm[4],1,12);
              _Invoice.PaymentTermCashDiscount1Base := TXRechnungHelper.AmountFromStr(paymentTerm[4]);
            end
            else
              _Invoice.PaymentTermCashDiscount1Base := 0;
          end;
        end else
        begin
          _Invoice.PaymentTermsType := iptt_None;
          _Invoice.PaymentTermNetNote := paymentTermsText;
        end;

        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:DueDateDateTime',node3) then
        if TXRechnungXMLHelper.SelectNode(node3,'.//udt:DateTimeString',node) then
          _Invoice.InvoiceDueDate := TXRechnungHelper.DateFromStrUNCEFACTFormat(node.text);
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:SpecifiedTradeSettlementHeaderMonetarySummation',node2) then
      begin
        _Invoice.LineAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:LineTotalAmount'));
        _Invoice.ChargeTotalAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:ChargeTotalAmount'));
        _Invoice.AllowanceTotalAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:AllowanceTotalAmount'));
        _Invoice.TaxExclusiveAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:TaxBasisTotalAmount'));
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:TaxTotalAmount',node) then
        begin
          _Invoice.TaxCurrencyCode := node.attributes.getNamedItem('currencyID').Text;
          _Invoice.TaxAmountTotal := TXRechnungHelper.AmountFromStr(node.text);
        end;
        _Invoice.TaxInclusiveAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:GrandTotalAmount'));
        _Invoice.PrepaidAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:TotalPrepaidAmount'));
        _Invoice.PayableAmount := TXRechnungHelper.AmountFromStr(TXRechnungXMLHelper.SelectNodeText(node2,'.//ram:DuePayableAmount'));
      end;
      if TXRechnungXMLHelper.SelectNodes(nodeApplicableHeaderTradeAgreement,'.//ram:InvoiceReferencedDocument',nodes) then
      for i := 0 to nodes.length-1 do
      with _Invoice.PrecedingInvoiceReferences.AddPrecedingInvoiceReference do
      begin
        ID := TXRechnungXMLHelper.SelectNodeText(nodes[i],'.//ram:IssuerAssignedID');
        if TXRechnungXMLHelper.SelectNode(nodes[i],'.//ram:FormattedIssueDateTime',node2) then
        if TXRechnungXMLHelper.SelectNode(node2,'.//qdt:DateTimeString',node) then
          IssueDate := TXRechnungHelper.DateFromStrUNCEFACTFormat(node.text);
      end;

    end;
    Result := true;
  except
    on E:Exception do _Error := E.ClassName+' '+E.Message;
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
    case TXRechnungValidationHelper.GetXRechnungVersion(xml) of
      XRechnungVersion_122          : Result := LoadDocumentUBL(_Invoice,XRechnungVersion_122,xml,_Error);
      XRechnungVersion_211_UBL      : Result := LoadDocumentUBL(_Invoice,XRechnungVersion_211_UBL,xml,_Error);
      XRechnungVersion_211_UNCEFACT,
      XRechnungVersion_ReadingSupport_ZUGFeRDFacturX_2xx : Result := LoadDocumentUNCEFACT(_Invoice,xml,_Error);
      else exit;
    end;
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
    case TXRechnungValidationHelper.GetXRechnungVersion(xml) of
      XRechnungVersion_122          : Result := LoadDocumentUBL(_Invoice,XRechnungVersion_122,xml,_Error);
      XRechnungVersion_211_UBL      : Result := LoadDocumentUBL(_Invoice,XRechnungVersion_211_UBL,xml,_Error);
      XRechnungVersion_211_UNCEFACT,
      XRechnungVersion_ReadingSupport_ZUGFeRDFacturX_2xx : Result := LoadDocumentUNCEFACT(_Invoice,xml,_Error);
      else exit;
    end;
  finally
    xml := nil;
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
    case TXRechnungValidationHelper.GetXRechnungVersion(xml) of
      XRechnungVersion_122          : Result := LoadDocumentUBL(_Invoice,XRechnungVersion_122,xml,_Error);
      XRechnungVersion_211_UBL      : Result := LoadDocumentUBL(_Invoice,XRechnungVersion_211_UBL,xml,_Error);
      XRechnungVersion_211_UNCEFACT,
      XRechnungVersion_ReadingSupport_ZUGFeRDFacturX_2xx : Result := LoadDocumentUNCEFACT(_Invoice,xml,_Error);
      else exit;
    end;
  finally
    xml := nil;
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveDocument(_Invoice: TInvoice;
  _Version : TXRechnungVersion; _Xml: IXMLDocument);
begin
  case _Version of
    XRechnungVersion_122,
    XRechnungVersion_211_UBL : SaveDocumentUBL(_Invoice,_Version,_Xml);
    XRechnungVersion_211_UNCEFACT : SaveDocumentUNCEFACT(_Invoice,_Xml);
    else raise Exception.Create('XRechnung - wrong version');
  end;
end;

class procedure TXRechnungInvoiceAdapter.SaveDocumentUBL(_Invoice: TInvoice;
  _Version : TXRechnungVersion; _Xml: IXMLDocument);
var
  xRoot : IXMLNode;
  allowanceCharge : TInvoiceAllowanceCharge;
  taxSubtotal : TInvoiceTaxAmount;
  i : Integer;
  precedingInvoiceReference : TInvoicePrecedingInvoiceReference;

  function InternalExtensionEnabled : Boolean;
  //var a : Integer;
  begin
    Result := false;
    if _Invoice.InvoiceLines.Count > 0 then
    begin
      Result := true;
      exit;
    end;
    //for a := 0 to _Invoice.Attachments.Count-1 do
    //if _Invoice.Attachments[a].AttachmentType = TInvoiceAttachmentType.iat_application_xml then
    //begin
    //  Result := true;
    //  exit;
    //end;
  end;

  procedure InternalAddInvoiceLine(_Invoiceline : TInvoiceLine; _Node : IXMLNode);
  var subinvoiceline : TInvoiceLine;
    allowanceCharge : TInvoiceAllowanceCharge;
  begin
  with _Node do
  begin
    AddChild('cbc:ID').Text := _Invoiceline.ID;
    if _Invoiceline.Note <> '' then
      AddChild('cbc:Note').Text := _Invoiceline.Note;
    with AddChild('cbc:InvoicedQuantity') do
    begin
      Attributes['unitCode'] := TXRechnungHelper.InvoiceUnitCodeToStr(_Invoiceline.UnitCode);
      Text := TXRechnungHelper.QuantityToStr(_Invoiceline.Quantity);
    end;
    with AddChild('cbc:LineExtensionAmount') do
    begin
      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
      Text := TXRechnungHelper.AmountToStr(_Invoiceline.LineAmount);
    end;
    //  <cac:DocumentReference>
    //     <cbc:ID/>
    //     <cbc:DocumentType>916</cbc:DocumentType>
    //  </cac:DocumentReference>
    for allowanceCharge in _Invoiceline.AllowanceCharges do
    with AddChild('cac:AllowanceCharge') do
    begin
      AddChild('cbc:ChargeIndicator').Text := LowerCase(BoolToStr(allowanceCharge.ChargeIndicator,true));
      AddChild('cbc:AllowanceChargeReasonCode').Text :=
               IfThen(allowanceCharge.ChargeIndicator,
               TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeToStr(allowanceCharge.ReasonCodeCharge),
               TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeToStr(allowanceCharge.ReasonCodeAllowance));
      AddChild('cbc:AllowanceChargeReason').Text := allowanceCharge.Reason;
      AddChild('cbc:MultiplierFactorNumeric').Text := TXRechnungHelper.FloatToStr(allowanceCharge.MultiplierFactorNumeric);
      with AddChild('cbc:Amount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(allowanceCharge.Amount);
      end;
      with AddChild('cbc:BaseAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(allowanceCharge.BaseAmount);
      end;
    end;
    with AddChild('cac:Item') do
    begin
      AddChild('cbc:Description').Text := _Invoiceline.Description;
      AddChild('cbc:Name').Text := _Invoiceline.Name;
      //   <cac:BuyersItemIdentification>
      //      <cbc:ID/>
      //   </cac:BuyersItemIdentification>
      AddChild('cac:SellersItemIdentification').AddChild('cbc:ID').Text := _Invoiceline.SellersItemIdentification;
      //<cac:StandardItemIdentification>
      //      <cbc:ID schemeID="0001"/>
      //   </cac:StandardItemIdentification>
      with AddChild('cac:ClassifiedTaxCategory') do
      begin
        AddChild('cbc:ID').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(_Invoiceline.TaxCategory);
        AddChild('cbc:Percent').Text := TXRechnungHelper.PercentageToStr(_Invoiceline.TaxPercent);
        AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
      end;
    end;
    with AddChild('cac:Price') do
    begin
      with AddChild('cbc:PriceAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.UnitPriceAmountToStr(_Invoiceline.PriceAmount);
      end;
      if (_Invoiceline.BaseQuantity <> 0) and (_Invoiceline.BaseQuantityUnitCode <> iuc_None) then
      with AddChild('cbc:BaseQuantity') do
      begin
        Attributes['unitCode'] := TXRechnungHelper.InvoiceUnitCodeToStr(_Invoiceline.BaseQuantityUnitCode);
        Text := IntToStr(_Invoiceline.BaseQuantity);
      end;
    end;
    if _Version = XRechnungVersion_211_UBL then
    for subinvoiceline in _Invoiceline.SubInvoiceLines do
      InternalAddInvoiceLine(subinvoiceline,_Node.AddChild('cac:SubInvoiceLine'));
  end;
  end;

begin
  {$IFDEF USE_OXMLDomVendor}TXMLDocument(_Xml).DOMVendor := Xml.xmldom.GetDOMVendor(sOXmlDOMVendor);{$ENDIF}
  //Result := xmldoc.GetDocBinding('rsm:CrossIndustryInvoice', TXMLCrossIndustryDocumentType) as IXMLCrossIndustryDocumentType;
  TXMLDocument(_Xml).Options := TXMLDocument(_Xml).Options + [doNodeAutoIndent];
  _Xml.Active := True;
  _Xml.Version := '1.0';
  _Xml.StandAlone := 'yes';
  _Xml.Encoding := 'UTF-8';

  _Xml.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull];

  xRoot := _Xml.AddChild('ubl:Invoice');

  xRoot.DeclareNamespace('ubl','urn:oasis:names:specification:ubl:schema:xsd:Invoice-2');
  xRoot.DeclareNamespace('cac','urn:oasis:names:specification:ubl:schema:xsd:CommonAggregateComponents-2');
  xRoot.DeclareNamespace('cbc','urn:oasis:names:specification:ubl:schema:xsd:CommonBasicComponents-2');

  if _Version = XRechnungVersion_122 then
  begin
    xRoot.DeclareNamespace('xsi','http://www.w3.org/2001/XMLSchema-instance');
    xRoot.Attributes['xsi:schemaLocation'] := 'urn:oasis:names:specification:ubl:schema:xsd:Invoice-2 http://docs.oasis-open.org/ubl/os-UBL-2.1/xsd/maindoc/UBL-Invoice-2.1.xsd';
    xRoot.AddChild('cbc:CustomizationID').Text := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_1.2';
  end else
  begin
    xRoot.AddChild('cbc:CustomizationID').Text := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.1'+
         IfThen(InternalExtensionEnabled,'#conformant#urn:xoev-de:kosit:extension:xrechnung_2.1','');
  end;

  xRoot.AddChild('cbc:ID').Text := _Invoice.InvoiceNumber;
  xRoot.AddChild('cbc:IssueDate').Text := TXRechnungHelper.DateToStrUBLFormat(_Invoice.InvoiceIssueDate);
  if _Invoice.InvoiceDueDate > 100 then xRoot.AddChild('cbc:DueDate').Text := TXRechnungHelper.DateToStrUBLFormat(_Invoice.InvoiceDueDate);
  xRoot.AddChild('cbc:InvoiceTypeCode').Text := TXRechnungHelper.InvoiceTypeCodeToStr(_Invoice.InvoiceTypeCode);
  if _Invoice.Note <> '' then
    xRoot.AddChild('cbc:Note').Text := _Invoice.Note;
  xRoot.AddChild('cbc:DocumentCurrencyCode').Text := _Invoice.InvoiceCurrencyCode;
  xRoot.AddChild('cbc:TaxCurrencyCode').Text := _Invoice.TaxCurrencyCode;
  xRoot.AddChild('cbc:BuyerReference').Text := _Invoice.BuyerReference;
  if (_Invoice.InvoicePeriodStartDate > 100) and (_Invoice.InvoicePeriodEndDate > 100) then
  with xRoot.AddChild('cac:InvoicePeriod') do
  begin
    AddChild('cbc:StartDate').Text := TXRechnungHelper.DateToStrUBLFormat(_Invoice.InvoicePeriodStartDate);
    AddChild('cbc:EndDate').Text := TXRechnungHelper.DateToStrUBLFormat(_Invoice.InvoicePeriodEndDate);
  end;
  if _Invoice.PurchaseOrderReference <> '' then
    xRoot.AddChild('cac:OrderReference').AddChild('cbc:ID').Text := _Invoice.PurchaseOrderReference;
  for precedingInvoiceReference in _Invoice.PrecedingInvoiceReferences do
  with xRoot.AddChild('cac:BillingReference').AddChild('cac:InvoiceDocumentReference') do
  begin
    AddChild('cbc:ID').Text := precedingInvoiceReference.ID;
    AddChild('cbc:IssueDate').Text := TXRechnungHelper.DateToStrUBLFormat(precedingInvoiceReference.IssueDate);
  end;
  if _Invoice.ContractDocumentReference <> '' then
    xRoot.AddChild('cac:ContractDocumentReference').AddChild('cbc:ID').Text := _Invoice.ContractDocumentReference;

  for i := 0 to _Invoice.Attachments.Count -1 do
  begin
    if _Invoice.Attachments[i].AttachmentType = TInvoiceAttachmentType.iat_application_xml then
    if _Version = XRechnungVersion_122 then
      continue; //xml attachment not allowed in v1.2.2');

    with xRoot.AddChild('cac:AdditionalDocumentReference') do
    begin
      AddChild('cbc:ID').Text := _Invoice.Attachments[i].ID;
      if _Invoice.Attachments[i].DocumentDescription <> '' then
        AddChild('cbc:DocumentDescription').Text := _Invoice.Attachments[i].DocumentDescription;
      with AddChild('cac:Attachment') do
      begin
        if _Invoice.Attachments[i].ExternalReference <> '' then
        begin
          AddChild('cac:ExternalReference').AddChild('cbc:URI').Text := _Invoice.Attachments[i].ExternalReference;
        end else
        with AddChild('cbc:EmbeddedDocumentBinaryObject') do
        begin
          Attributes['mimeCode'] := TXRechnungHelper.InvoiceAttachmentTypeToStr(_Invoice.Attachments[i].AttachmentType);
          Attributes['filename'] := _Invoice.Attachments[i].Filename;
          Text := _Invoice.Attachments[i].GetDataAsBase64;
        end;
      end;
    end;
  end;

  if _Invoice.ProjectReference <> '' then
    xRoot.AddChild('cac:ProjectReference').AddChild('cbc:ID').Text := _Invoice.ProjectReference;

  with xRoot.AddChild('cac:AccountingSupplierParty').AddChild('cac:Party') do
  begin
    if _Invoice.AccountingSupplierParty.IdentifierSellerBuyer <> '' then
    with AddChild('cac:PartyIdentification') do
    begin
      AddChild('cbc:ID').Text := _Invoice.AccountingSupplierParty.IdentifierSellerBuyer;
    end;
    with AddChild('cac:PartyName') do
    begin
      AddChild('cbc:Name').Text := _Invoice.AccountingSupplierParty.Name;
    end;
    with AddChild('cac:PostalAddress') do
    begin
      AddChild('cbc:StreetName').Text := _Invoice.AccountingSupplierParty.Address.StreetName;
      if _Invoice.AccountingSupplierParty.Address.AdditionalStreetName <> '' then
        AddChild('cbc:AdditionalStreetName').Text := _Invoice.AccountingSupplierParty.Address.AdditionalStreetName;
      AddChild('cbc:CityName').Text := _Invoice.AccountingSupplierParty.Address.City;
      AddChild('cbc:PostalZone').Text := _Invoice.AccountingSupplierParty.Address.PostalZone;
      if _Invoice.AccountingSupplierParty.Address.CountrySubentity <> '' then
        AddChild('cbc:CountrySubentity').Text := _Invoice.AccountingSupplierParty.Address.CountrySubentity;
      if _Invoice.AccountingSupplierParty.Address.AddressLine <> '' then
        AddChild('cac:AddressLine').AddChild('cbc:Line').Text := _Invoice.AccountingSupplierParty.Address.AddressLine;
      AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.AccountingSupplierParty.Address.CountryCode;
    end;
    if _Invoice.AccountingSupplierParty.VATCompanyID <> '' then
    with AddChild('cac:PartyTaxScheme') do
    begin
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingSupplierParty.VATCompanyID;
      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
    end;
    with AddChild('cac:PartyLegalEntity') do
    begin
      AddChild('cbc:RegistrationName').Text := _Invoice.AccountingSupplierParty.RegistrationName;
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingSupplierParty.CompanyID;
    end;
    with AddChild('cac:Contact') do
    begin
      AddChild('cbc:Name').Text := _Invoice.AccountingSupplierParty.ContactName;
      AddChild('cbc:Telephone').Text := _Invoice.AccountingSupplierParty.ContactTelephone;
      AddChild('cbc:ElectronicMail').Text := _Invoice.AccountingSupplierParty.ContactElectronicMail;
    end;
  end;

  with xRoot.AddChild('cac:AccountingCustomerParty').AddChild('cac:Party') do
  begin
    if _Invoice.AccountingCustomerParty.IdentifierSellerBuyer <> '' then
    with AddChild('cac:PartyIdentification') do
    begin
      AddChild('cbc:ID').Text := _Invoice.AccountingCustomerParty.IdentifierSellerBuyer;
    end;
    with AddChild('cac:PartyName') do
    begin
      AddChild('cbc:Name').Text := _Invoice.AccountingCustomerParty.Name;
    end;
    with AddChild('cac:PostalAddress') do
    begin
      AddChild('cbc:StreetName').Text := _Invoice.AccountingCustomerParty.Address.StreetName;
      if _Invoice.AccountingCustomerParty.Address.AdditionalStreetName <> '' then
        AddChild('cbc:AdditionalStreetName').Text := _Invoice.AccountingCustomerParty.Address.AdditionalStreetName;
      AddChild('cbc:CityName').Text := _Invoice.AccountingCustomerParty.Address.City;
      AddChild('cbc:PostalZone').Text := _Invoice.AccountingCustomerParty.Address.PostalZone;
      if _Invoice.AccountingCustomerParty.Address.CountrySubentity <> '' then
        AddChild('cbc:CountrySubentity').Text := _Invoice.AccountingCustomerParty.Address.CountrySubentity;
      if _Invoice.AccountingCustomerParty.Address.AddressLine <> '' then
        AddChild('cac:AddressLine').AddChild('cbc:Line').Text := _Invoice.AccountingCustomerParty.Address.AddressLine;
      AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.AccountingCustomerParty.Address.CountryCode;
    end;
    if _Invoice.AccountingCustomerParty.VATCompanyID <> '' then
    with AddChild('cac:PartyTaxScheme') do
    begin
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.VATCompanyID;
      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
    end;
    with AddChild('cac:PartyLegalEntity') do
    begin
      AddChild('cbc:RegistrationName').Text := _Invoice.AccountingCustomerParty.RegistrationName;
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.CompanyID;
      //TODO <cbc:CompanyLegalForm>123/456/7890, HRA-Eintrag in []</cbc:CompanyLegalForm>
    end;
    with AddChild('cac:Contact') do
    begin
      AddChild('cbc:Name').Text := _Invoice.AccountingCustomerParty.ContactName;
      AddChild('cbc:Telephone').Text := _Invoice.AccountingCustomerParty.ContactTelephone;
      AddChild('cbc:ElectronicMail').Text := _Invoice.AccountingCustomerParty.ContactElectronicMail;
    end;
  end;

  if (_Invoice.DeliveryInformation.ActualDeliveryDate > 0) or
     (_Invoice.DeliveryInformation.Address.CountryCode <> '') or
     (_Invoice.DeliveryInformation.Name <> '') then
  with xRoot.AddChild('cac:Delivery') do
  begin
    if (_Invoice.DeliveryInformation.ActualDeliveryDate > 0) then
      AddChild('cbc:ActualDeliveryDate').Text := TXRechnungHelper.DateToStrUBLFormat(_Invoice.DeliveryInformation.ActualDeliveryDate);
    with AddChild('cac:DeliveryLocation') do
    begin
      //if (_Invoice.DeliveryInformation.LocationIdentifier <> '') then
      //  AddChild('cbc:ID').Text := _Invoice.DeliveryInformation.LocationIdentifier; //TODO schemeID https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice/cac-Delivery/cac-DeliveryLocation/cbc-ID/
      with AddChild('cac:Address') do
      begin
        AddChild('cbc:StreetName').Text := _Invoice.DeliveryInformation.Address.StreetName;
        if _Invoice.DeliveryInformation.Address.AdditionalStreetName <> '' then
          AddChild('cbc:AdditionalStreetName').Text := _Invoice.DeliveryInformation.Address.AdditionalStreetName;
        AddChild('cbc:CityName').Text := _Invoice.DeliveryInformation.Address.City;
        AddChild('cbc:PostalZone').Text := _Invoice.DeliveryInformation.Address.PostalZone;
        if _Invoice.DeliveryInformation.Address.CountrySubentity <> '' then
          AddChild('cbc:CountrySubentity').Text := _Invoice.DeliveryInformation.Address.CountrySubentity;
        if _Invoice.DeliveryInformation.Address.AddressLine <> '' then
          AddChild('cac:AddressLine').AddChild('cbc:Line').Text := _Invoice.DeliveryInformation.Address.AddressLine;
        AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.DeliveryInformation.Address.CountryCode;
      end;
    end;
    if (_Invoice.DeliveryInformation.Name <> '') then
      AddChild('cac:DeliveryParty').AddChild('cac:PartyName').AddChild('cbc:Name').Text := _Invoice.DeliveryInformation.Name;
  end;

  if (_Invoice.PaymentMeansCode <> ipmc_None) and (_Invoice.PayeeFinancialAccount <> '') then
  with xRoot.AddChild('cac:PaymentMeans') do
  begin
    AddChild('cbc:PaymentMeansCode').Text := TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Invoice.PaymentMeansCode);
    if _Invoice.PaymentID <> '' then
      AddChild('cbc:PaymentID').Text := _Invoice.PaymentID;
    with AddChild('cac:PayeeFinancialAccount') do
    begin
      AddChild('cbc:ID').Text := _Invoice.PayeeFinancialAccount;
      if _Invoice.PayeeFinancialAccountName <> '' then
        AddChild('cbc:Name').Text := _Invoice.PayeeFinancialAccountName;
      if _Invoice.PayeeFinancialInstitutionBranch <> '' then
        AddChild('cac:FinancialInstitutionBranch').AddChild('cbc:ID').Text := _Invoice.PayeeFinancialInstitutionBranch;
    end;
  end;

  case _Invoice.PaymentTermsType of
    iptt_Net:
      with xRoot.AddChild('cac:PaymentTerms') do
      begin
        AddChild('cbc:Note').Text := System.StrUtils.ReplaceText(_Invoice.PaymentTermNetNote,'#',' ');
      end;
    iptt_CashDiscount1:
      with xRoot.AddChild('cac:PaymentTerms') do
      begin
        AddChild('cbc:Note').Text := Format('#SKONTO#TAGE=%d#PROZENT=%s#',
          [_Invoice.PaymentTermCashDiscount1Days,
           TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount1Percent)])+
          IfThen(_Invoice.PaymentTermCashDiscount1Base <> 0,'BASISBETRAG='+
            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','')+
          IfThen(_Version = XRechnungVersion_211_UBL,#13#10,'');
      end;
    iptt_CashDiscount2:
    begin
      with xRoot.AddChild('cac:PaymentTerms') do
      begin
        AddChild('cbc:Note').Text := Format('#SKONTO#TAGE=%d#PROZENT=%s#',
          [_Invoice.PaymentTermCashDiscount1Days,
           TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount1Percent)])+
          IfThen(_Invoice.PaymentTermCashDiscount1Base <> 0,'BASISBETRAG='+
            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','')+
          #13#10+
          Format('#SKONTO#TAGE=%d#PROZENT=%s#',
          [_Invoice.PaymentTermCashDiscount2Days,
           TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount2Percent)])+
          IfThen(_Invoice.PaymentTermCashDiscount2Base <> 0,'BASISBETRAG='+
            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount2Base)+'#','')+
          IfThen(_Version = XRechnungVersion_211_UBL,#13#10,'');
      end;
    end;
  end;

  for allowanceCharge in _Invoice.AllowanceCharges do
  with xRoot.AddChild('cac:AllowanceCharge') do
  begin
    AddChild('cbc:ChargeIndicator').Text := LowerCase(BoolToStr(allowanceCharge.ChargeIndicator,true));
    AddChild('cbc:AllowanceChargeReasonCode').Text :=
             IfThen(allowanceCharge.ChargeIndicator,
             TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeToStr(allowanceCharge.ReasonCodeCharge),
             TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeToStr(allowanceCharge.ReasonCodeAllowance));
    AddChild('cbc:AllowanceChargeReason').Text := allowanceCharge.Reason;
    AddChild('cbc:MultiplierFactorNumeric').Text := TXRechnungHelper.FloatToStr(allowanceCharge.MultiplierFactorNumeric);
    with AddChild('cbc:Amount') do
    begin
      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
      Text := TXRechnungHelper.AmountToStr(allowanceCharge.Amount);
    end;
    with AddChild('cbc:BaseAmount') do
    begin
      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
      Text := TXRechnungHelper.AmountToStr(allowanceCharge.BaseAmount);
    end;
    with AddChild('cac:TaxCategory') do
    begin
      AddChild('cbc:ID').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(allowanceCharge.TaxCategory);
      AddChild('cbc:Percent').Text := TXRechnungHelper.PercentageToStr(allowanceCharge.TaxPercent);
      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
    end;
  end;

  with xRoot.AddChild('cac:TaxTotal') do
  begin
    with AddChild('cbc:TaxAmount') do
    begin
      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
      Text := TXRechnungHelper.AmountToStr(_Invoice.TaxAmountTotal);
    end;
    for taxSubtotal in _Invoice.TaxAmountSubtotals do
    with AddChild('cac:TaxSubtotal') do
    begin
      with AddChild('cbc:TaxableAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(taxSubtotal.TaxableAmount);
      end;
      with AddChild('cbc:TaxAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(taxSubtotal.TaxAmount);
      end;
      with AddChild('cac:TaxCategory') do
      begin
        AddChild('cbc:ID').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(taxSubtotal.TaxCategory);
        AddChild('cbc:Percent').Text := TXRechnungHelper.PercentageToStr(taxSubtotal.TaxPercent);
        if taxSubtotal.TaxExemptionReason <> '' then
          AddChild('cbc:TaxExemptionReason').Text := taxSubtotal.TaxExemptionReason;
        AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
      end;
    end;
  end;

  with xRoot.AddChild('cac:LegalMonetaryTotal') do
  begin
      with AddChild('cbc:LineExtensionAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.LineAmount);
      end;
      with AddChild('cbc:TaxExclusiveAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.TaxExclusiveAmount);
      end;
      with AddChild('cbc:TaxInclusiveAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.TaxInclusiveAmount);
      end;
      with AddChild('cbc:AllowanceTotalAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.AllowanceTotalAmount);
      end;
      with AddChild('cbc:ChargeTotalAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.ChargeTotalAmount);
      end;
      with AddChild('cbc:PrepaidAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.PrepaidAmount);
      end;
      //      <cbc:PayableRoundingAmount currencyID="EUR">0</cbc:PayableRoundingAmount>
      with AddChild('cbc:PayableAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.PayableAmount);
      end;
  end;

  for i := 0 to _Invoice.InvoiceLines.Count-1 do
    InternalAddInvoiceLine(_Invoice.InvoiceLines[i],xRoot.AddChild('cac:InvoiceLine'));
end;

class procedure TXRechnungInvoiceAdapter.SaveDocumentUNCEFACT(_Invoice: TInvoice; _Xml: IXMLDocument);
var
  xRoot : IXMLNode;
  allowanceCharge : TInvoiceAllowanceCharge;
  taxSubtotal : TInvoiceTaxAmount;
  i : Integer;
  precedingInvoiceReference : TInvoicePrecedingInvoiceReference;

  procedure InternalAddInvoiceLine(_Invoiceline : TInvoiceLine; _Node : IXMLNode);
  var
    allowanceCharge : TInvoiceAllowanceCharge;
  begin
  with _Node do
  begin
    with AddChild('ram:AssociatedDocumentLineDocument') do
    begin
      AddChild('ram:LineID').Text := _Invoiceline.ID;
      if _Invoiceline.Note <> '' then
        AddChild('ram:IncludedNote').AddChild('ram:Content').Text := _Invoiceline.Note;
    end;
    with AddChild('ram:SpecifiedTradeProduct') do
    begin
      AddChild('ram:SellerAssignedID').Text := _Invoiceline.SellersItemIdentification;
      AddChild('ram:Name').Text := _Invoiceline.Name;
      AddChild('ram:Description').Text := _Invoiceline.Description;
    end;
    with AddChild('ram:SpecifiedLineTradeAgreement') do
    begin
//        <ram:BuyerOrderReferencedDocument>
//            <ram:LineID>6171175.1</ram:LineID>
//        </ram:BuyerOrderReferencedDocument>
//        <cac:OrderLineReference>
//            <cbc:LineID>6171175.1</cbc:LineID>
//        </cac:OrderLineReference>
      with AddChild('ram:NetPriceProductTradePrice') do
      begin
        AddChild('ram:ChargeAmount').Text := TXRechnungHelper.UnitPriceAmountToStr(_Invoiceline.PriceAmount);
        if (_Invoiceline.BaseQuantity <> 0) and (_Invoiceline.BaseQuantityUnitCode <> iuc_None) then
        with AddChild('ram:BaseQuantity') do
        begin
          Attributes['unitCode'] := TXRechnungHelper.InvoiceUnitCodeToStr(_Invoiceline.BaseQuantityUnitCode);
          Text := IntToStr(_Invoiceline.BaseQuantity);
        end;
      end;
    end;
    with AddChild('ram:SpecifiedLineTradeDelivery').AddChild('ram:BilledQuantity') do
    begin
      Attributes['unitCode'] := TXRechnungHelper.InvoiceUnitCodeToStr(_Invoiceline.UnitCode);
      Text := TXRechnungHelper.QuantityToStr(_Invoiceline.Quantity);
    end;
    with AddChild('ram:SpecifiedLineTradeSettlement') do
    begin
      with AddChild('ram:ApplicableTradeTax') do
      begin
        AddChild('ram:TypeCode').Text := 'VAT';
        AddChild('ram:CategoryCode').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(_Invoiceline.TaxCategory);
        AddChild('ram:RateApplicablePercent').Text := TXRechnungHelper.PercentageToStr(_Invoiceline.TaxPercent);
      end;
      for allowanceCharge in _Invoiceline.AllowanceCharges do
      with AddChild('ram:SpecifiedTradeAllowanceCharge') do
      begin
        AddChild('ram:ChargeIndicator').AddChild('udt:Indicator').Text := LowerCase(BoolToStr(allowanceCharge.ChargeIndicator,true));
        AddChild('ram:CalculationPercent').Text := TXRechnungHelper.FloatToStr(allowanceCharge.MultiplierFactorNumeric);
        AddChild('ram:BasisAmount').Text := TXRechnungHelper.AmountToStr(allowanceCharge.BaseAmount);
        AddChild('ram:ActualAmount').Text := TXRechnungHelper.AmountToStr(allowanceCharge.Amount);
        AddChild('ram:ReasonCode').Text :=
                 IfThen(allowanceCharge.ChargeIndicator,
                 TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeToStr(allowanceCharge.ReasonCodeCharge),
                 TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeToStr(allowanceCharge.ReasonCodeAllowance));
        AddChild('ram:Reason').Text := allowanceCharge.Reason;
      end;
      with AddChild('ram:SpecifiedTradeSettlementLineMonetarySummation') do
      begin
        AddChild('ram:LineTotalAmount').Text := TXRechnungHelper.AmountToStr(_Invoiceline.LineAmount);
      end;
    end;
    if _Invoiceline.SubInvoiceLines.Count > 0 then
      raise Exception.Create('SubInvoiceLines in UNCEFACT not implemented');
  end;
  end;

begin
  {$IFDEF USE_OXMLDomVendor}TXMLDocument(_Xml).DOMVendor := Xml.xmldom.GetDOMVendor(sOXmlDOMVendor);{$ENDIF}
  //Result := xmldoc.GetDocBinding('rsm:CrossIndustryInvoice', TXMLCrossIndustryDocumentType) as IXMLCrossIndustryDocumentType;
  TXMLDocument(_Xml).Options := TXMLDocument(_Xml).Options + [doNodeAutoIndent];
  _Xml.Active := True;
  _Xml.Version := '1.0';
  _Xml.StandAlone := 'yes';
  _Xml.Encoding := 'UTF-8';

  _Xml.Options := [doNodeAutoCreate, doNodeAutoIndent, doAttrNull];

  xRoot := _Xml.AddChild('rsm:CrossIndustryInvoice');

  xRoot.DeclareNamespace('rsm','urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100');
  xRoot.DeclareNamespace('ram','urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100');
  xRoot.DeclareNamespace('udt','urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100');
  xRoot.DeclareNamespace('qdt','urn:un:unece:uncefact:data:standard:QualifiedDataType:100');

  xRoot.AddChild('rsm:ExchangedDocumentContext')
       .AddChild('ram:GuidelineSpecifiedDocumentContextParameter')
       .AddChild('ram:ID').Text := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.1';

  with xRoot.AddChild('rsm:ExchangedDocument') do
  begin
    AddChild('ram:ID').Text := _Invoice.InvoiceNumber;
    AddChild('ram:TypeCode').Text := TXRechnungHelper.InvoiceTypeCodeToStr(_Invoice.InvoiceTypeCode);
    with AddChild('ram:IssueDateTime').AddChild('udt:DateTimeString') do
    begin
      Attributes['format'] := '102';
      Text := TXRechnungHelper.DateToStrUNCEFACTFormat(_Invoice.InvoiceIssueDate);
    end;
    if _Invoice.Note <> '' then
    with AddChild('ram:IncludedNote') do
    begin
      AddChild('ram:Content').Text := _Invoice.Note;
      //TODO <ram:SubjectCode>ADU</ram:SubjectCode>, bei UBL auch
    end;
  end;

  with xRoot.AddChild('rsm:SupplyChainTradeTransaction') do
  begin
    for i := 0 to _Invoice.InvoiceLines.Count-1 do
      InternalAddInvoiceLine(_Invoice.InvoiceLines[i],AddChild('ram:IncludedSupplyChainTradeLineItem'));

    with AddChild('ram:ApplicableHeaderTradeAgreement') do
    begin
      AddChild('ram:BuyerReference').Text := _Invoice.BuyerReference;

      with AddChild('ram:SellerTradeParty') do
      begin
        if _Invoice.AccountingSupplierParty.IdentifierSellerBuyer <> '' then
          AddChild('ram:ID').Text := _Invoice.AccountingSupplierParty.IdentifierSellerBuyer;
        AddChild('ram:Name').Text := _Invoice.AccountingSupplierParty.RegistrationName;
        //TODO <ram:Description>123/456/7890, HRA-Eintrag in []</ram:Description>
        //<cbc:CompanyLegalForm>123/456/7890, HRA-Eintrag in []</cbc:CompanyLegalForm>
        with AddChild('ram:SpecifiedLegalOrganization') do
        begin
          AddChild('ram:ID').Text := _Invoice.AccountingSupplierParty.CompanyID;
          AddChild('ram:TradingBusinessName').Text := _Invoice.AccountingSupplierParty.Name;
        end;
        with AddChild('ram:DefinedTradeContact') do
        begin
          AddChild('ram:PersonName').Text := _Invoice.AccountingSupplierParty.ContactName;
          AddChild('ram:TelephoneUniversalCommunication').AddChild('ram:CompleteNumber').Text := _Invoice.AccountingSupplierParty.ContactTelephone;
          AddChild('ram:EmailURIUniversalCommunication').AddChild('ram:URIID').Text := _Invoice.AccountingSupplierParty.ContactElectronicMail;
        end;
        with AddChild('ram:PostalTradeAddress') do
        begin
          AddChild('ram:PostcodeCode').Text := _Invoice.AccountingSupplierParty.Address.PostalZone;
          AddChild('ram:LineOne').Text := _Invoice.AccountingSupplierParty.Address.StreetName;
          if _Invoice.AccountingSupplierParty.Address.AdditionalStreetName <> '' then
            AddChild('ram:LineTwo').Text := _Invoice.AccountingSupplierParty.Address.AdditionalStreetName;
          if _Invoice.AccountingSupplierParty.Address.AddressLine <> '' then
            AddChild('ram:LineThree').Text := _Invoice.AccountingSupplierParty.Address.AddressLine;
          AddChild('ram:CityName').Text := _Invoice.AccountingSupplierParty.Address.City;
          AddChild('ram:CountryID').Text := _Invoice.AccountingSupplierParty.Address.CountryCode;
          if _Invoice.AccountingSupplierParty.Address.CountrySubentity <> '' then
            AddChild('ram:CountrySubDivisionName').Text := _Invoice.AccountingSupplierParty.Address.CountrySubentity;
        end;
        if _Invoice.AccountingSupplierParty.VATCompanyID <> '' then
        with AddChild('ram:SpecifiedTaxRegistration').AddChild('ram:ID') do
        begin
          Attributes['schemeID'] := 'VA';
          Text := _Invoice.AccountingSupplierParty.VATCompanyID;
        end;
        //TODO FC bei Steuernummer
      end;
      with AddChild('ram:BuyerTradeParty') do
      begin
        if _Invoice.AccountingCustomerParty.IdentifierSellerBuyer <> '' then
          AddChild('ram:ID').Text := _Invoice.AccountingCustomerParty.IdentifierSellerBuyer;
        AddChild('ram:Name').Text := _Invoice.AccountingCustomerParty.RegistrationName;

        with AddChild('ram:SpecifiedLegalOrganization') do
        begin
          AddChild('ram:ID').Text := _Invoice.AccountingCustomerParty.CompanyID;
          AddChild('ram:TradingBusinessName').Text := _Invoice.AccountingCustomerParty.Name;
        end;
        with AddChild('ram:DefinedTradeContact') do
        begin
          AddChild('ram:PersonName').Text := _Invoice.AccountingCustomerParty.ContactName;
          AddChild('ram:TelephoneUniversalCommunication').AddChild('ram:CompleteNumber').Text := _Invoice.AccountingCustomerParty.ContactTelephone;
          AddChild('ram:EmailURIUniversalCommunication').AddChild('ram:URIID').Text := _Invoice.AccountingCustomerParty.ContactElectronicMail;
        end;
        with AddChild('ram:PostalTradeAddress') do
        begin
          AddChild('ram:PostcodeCode').Text := _Invoice.AccountingCustomerParty.Address.PostalZone;
          AddChild('ram:LineOne').Text := _Invoice.AccountingCustomerParty.Address.StreetName;
          if _Invoice.AccountingCustomerParty.Address.AdditionalStreetName <> '' then
            AddChild('ram:LineTwo').Text := _Invoice.AccountingCustomerParty.Address.AdditionalStreetName;
          if _Invoice.AccountingCustomerParty.Address.AddressLine <> '' then
            AddChild('ram:LineThree').Text := _Invoice.AccountingCustomerParty.Address.AddressLine;
          AddChild('ram:CityName').Text := _Invoice.AccountingCustomerParty.Address.City;
          AddChild('ram:CountryID').Text := _Invoice.AccountingCustomerParty.Address.CountryCode;
          if _Invoice.AccountingCustomerParty.Address.CountrySubentity <> '' then
            AddChild('ram:CountrySubDivisionName').Text := _Invoice.AccountingCustomerParty.Address.CountrySubentity;
        end;
        if _Invoice.AccountingCustomerParty.VATCompanyID <> '' then
        with AddChild('ram:SpecifiedTaxRegistration').AddChild('ram:ID') do
        begin
          Attributes['schemeID'] := 'VA';
          Text := _Invoice.AccountingCustomerParty.VATCompanyID;
        end;
      end;
      if _Invoice.PurchaseOrderReference <> '' then
        AddChild('ram:BuyerOrderReferencedDocument').AddChild('ram:IssuerAssignedID').Text := _Invoice.PurchaseOrderReference;
      if _Invoice.ContractDocumentReference <> '' then
        AddChild('ram:ContractReferencedDocument').AddChild('ram:IssuerAssignedID').Text := _Invoice.ContractDocumentReference;
      if _Invoice.ProjectReference <> '' then
      for i := 0 to _Invoice.Attachments.Count -1 do
      begin
        with AddChild('ram:AdditionalReferencedDocument') do
        begin
          AddChild('ram:IssuerAssignedID').Text := _Invoice.Attachments[i].ID;
          if _Invoice.Attachments[i].ExternalReference <> '' then
            AddChild('ram:URIID').Text := _Invoice.Attachments[i].ExternalReference;
          AddChild('ram:TypeCode').Text := '916';
          if _Invoice.Attachments[i].DocumentDescription <> '' then
            AddChild('ram:Name').Text := _Invoice.Attachments[i].DocumentDescription;
          if _Invoice.Attachments[i].ExternalReference = '' then
          with AddChild('ram:AttachmentBinaryObject') do
          begin
            Attributes['mimeCode'] := TXRechnungHelper.InvoiceAttachmentTypeToStr(_Invoice.Attachments[i].AttachmentType);
            Attributes['filename'] := _Invoice.Attachments[i].Filename;
            Text := _Invoice.Attachments[i].GetDataAsBase64;
          end;
        end;
      end;
      with AddChild('ram:SpecifiedProcuringProject') do
      begin
        AddChild('ram:ID').Text := _Invoice.ProjectReference;
        AddChild('ram:Name').Text := 'Project reference';
      end;
    end;
    with AddChild('ram:ApplicableHeaderTradeDelivery') do
    if (_Invoice.DeliveryInformation.ActualDeliveryDate > 0) or
       (_Invoice.DeliveryInformation.Address.CountryCode <> '') or
       (_Invoice.DeliveryInformation.Name <> '') then
    begin
      with AddChild('ram:ShipToTradeParty') do
      begin
        AddChild('ram:Name').Text := _Invoice.DeliveryInformation.Name;
        with AddChild('ram:PostalTradeAddress') do
        begin
          AddChild('ram:PostcodeCode').Text := _Invoice.DeliveryInformation.Address.PostalZone;
          AddChild('ram:LineOne').Text := _Invoice.DeliveryInformation.Address.StreetName;
          if _Invoice.DeliveryInformation.Address.AdditionalStreetName <> '' then
            AddChild('ram:LineTwo').Text := _Invoice.DeliveryInformation.Address.AdditionalStreetName;
          if _Invoice.DeliveryInformation.Address.AddressLine <> '' then
            AddChild('ram:LineThree').Text := _Invoice.DeliveryInformation.Address.AddressLine;
          AddChild('ram:CityName').Text := _Invoice.DeliveryInformation.Address.City;
          AddChild('ram:CountryID').Text := _Invoice.DeliveryInformation.Address.CountryCode;
          if _Invoice.DeliveryInformation.Address.CountrySubentity <> '' then
            AddChild('ram:CountrySubDivisionName').Text := _Invoice.DeliveryInformation.Address.CountrySubentity;
        end;
      end;
      if (_Invoice.DeliveryInformation.ActualDeliveryDate > 0) then
      with AddChild('ram:ActualDeliverySupplyChainEvent')
           .AddChild('ram:OccurrenceDateTime')
           .AddChild('udt:DateTimeString') do
      begin
        Attributes['format'] := '102';
        Text := TXRechnungHelper.DateToStrUNCEFACTFormat(_Invoice.DeliveryInformation.ActualDeliveryDate);
      end;
    end;
    with AddChild('ram:ApplicableHeaderTradeSettlement') do
    begin
      if _Invoice.PaymentID <> '' then
        AddChild('ram:PaymentReference').Text := _Invoice.PaymentID;
      //zuviel AddChild('ram:TaxCurrencyCode').Text := _Invoice.TaxCurrencyCode;
      AddChild('ram:InvoiceCurrencyCode').Text := _Invoice.InvoiceCurrencyCode;
      if (_Invoice.PaymentMeansCode <> ipmc_None) and (_Invoice.PayeeFinancialAccount <> '') then
      with AddChild('ram:SpecifiedTradeSettlementPaymentMeans') do
      begin
        AddChild('ram:TypeCode').Text := TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Invoice.PaymentMeansCode);
        with AddChild('ram:PayeePartyCreditorFinancialAccount') do
        begin
          AddChild('ram:IBANID').Text := _Invoice.PayeeFinancialAccount;
          if _Invoice.PayeeFinancialAccountName <> '' then
            AddChild('ram:AccountName').Text := _Invoice.PayeeFinancialAccountName;
        end;
        if _Invoice.PayeeFinancialInstitutionBranch <> '' then
        with AddChild('ram:PayeeSpecifiedCreditorFinancialInstitution') do
        begin
          AddChild('ram:BICID').Text := _Invoice.PayeeFinancialInstitutionBranch;
          //TODO <ram:Name>Name der Bank</ram:Name>
        end;
      end;
      for taxSubtotal in _Invoice.TaxAmountSubtotals do
      with AddChild('ram:ApplicableTradeTax') do
      begin
        AddChild('ram:CalculatedAmount').Text := TXRechnungHelper.AmountToStr(taxSubtotal.TaxAmount);
        AddChild('ram:TypeCode').Text := 'VAT';
        if taxSubtotal.TaxExemptionReason <> '' then
          AddChild('ram:ExemptionReason').Text := taxSubtotal.TaxExemptionReason;
        AddChild('ram:BasisAmount').Text := TXRechnungHelper.AmountToStr(taxSubtotal.TaxableAmount);
        AddChild('ram:CategoryCode').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(taxSubtotal.TaxCategory);
        AddChild('ram:RateApplicablePercent').Text := TXRechnungHelper.PercentageToStr(taxSubtotal.TaxPercent);
      end;
      if (_Invoice.InvoicePeriodStartDate > 100) and (_Invoice.InvoicePeriodEndDate > 100) then
      with AddChild('ram:BillingSpecifiedPeriod') do
      begin
        with AddChild('ram:StartDateTime').AddChild('udt:DateTimeString') do
        begin
          Attributes['format'] := '102';
          Text := TXRechnungHelper.DateToStrUNCEFACTFormat(_Invoice.InvoicePeriodStartDate);
        end;
        with AddChild('ram:EndDateTime').AddChild('udt:DateTimeString') do
        begin
          Attributes['format'] := '102';
          Text := TXRechnungHelper.DateToStrUNCEFACTFormat(_Invoice.InvoicePeriodEndDate);
        end;
      end;
      for allowanceCharge in _Invoice.AllowanceCharges do
      with AddChild('ram:SpecifiedTradeAllowanceCharge') do
      begin
        AddChild('ram:ChargeIndicator').AddChild('udt:Indicator').Text := LowerCase(BoolToStr(allowanceCharge.ChargeIndicator,true));
        AddChild('ram:CalculationPercent').Text := TXRechnungHelper.FloatToStr(allowanceCharge.MultiplierFactorNumeric);
        AddChild('ram:BasisAmount').Text := TXRechnungHelper.AmountToStr(allowanceCharge.BaseAmount);
        AddChild('ram:ActualAmount').Text := TXRechnungHelper.AmountToStr(allowanceCharge.Amount);
        AddChild('ram:ReasonCode').Text :=
                 IfThen(allowanceCharge.ChargeIndicator,
                 TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeToStr(allowanceCharge.ReasonCodeCharge),
                 TXRechnungHelper.InvoiceAllowanceOrChargeIdentCodeToStr(allowanceCharge.ReasonCodeAllowance));
        AddChild('ram:Reason').Text := allowanceCharge.Reason;
        with AddChild('ram:CategoryTradeTax') do
        begin
          AddChild('ram:TypeCode').Text := 'VAT';
          AddChild('ram:CategoryCode').Text := TXRechnungHelper.InvoiceDutyTaxFeeCategoryCodeToStr(allowanceCharge.TaxCategory);
          AddChild('ram:RateApplicablePercent').Text := TXRechnungHelper.PercentageToStr(allowanceCharge.TaxPercent);
        end;
      end;
      with AddChild('ram:SpecifiedTradePaymentTerms') do
      begin
        case _Invoice.PaymentTermsType of
          iptt_Net:
            AddChild('ram:Description').Text := System.StrUtils.ReplaceText(_Invoice.PaymentTermNetNote,'#',' ');
          iptt_CashDiscount1:
            AddChild('ram:Description').Text := Format('#SKONTO#TAGE=%d#PROZENT=%s#',
                [_Invoice.PaymentTermCashDiscount1Days,
                 TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount1Percent)])+
                IfThen(_Invoice.PaymentTermCashDiscount1Base <> 0,'BASISBETRAG='+
                  TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','')+
                #13#10;
          iptt_CashDiscount2:
          begin
            AddChild('ram:Description').Text := Format('#SKONTO#TAGE=%d#PROZENT=%s#',
                [_Invoice.PaymentTermCashDiscount1Days,
                 TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount1Percent)])+
                IfThen(_Invoice.PaymentTermCashDiscount1Base <> 0,'BASISBETRAG='+
                  TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','')+
                #13#10+
                Format('#SKONTO#TAGE=%d#PROZENT=%s#',
                [_Invoice.PaymentTermCashDiscount2Days,
                 TXRechnungHelper.FloatToStr(_Invoice.PaymentTermCashDiscount2Percent)])+
                IfThen(_Invoice.PaymentTermCashDiscount2Base <> 0,'BASISBETRAG='+
                  TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount2Base)+'#','')+
                #13#10;
            end;
        end;
        if _Invoice.InvoiceDueDate > 100 then
        with AddChild('ram:DueDateDateTime').AddChild('udt:DateTimeString') do
        begin
          Attributes['format'] := '102';
          Text := TXRechnungHelper.DateToStrUNCEFACTFormat(_Invoice.InvoiceDueDate);
        end;
      end;
      with AddChild('ram:SpecifiedTradeSettlementHeaderMonetarySummation') do
      begin
        AddChild('ram:LineTotalAmount').Text := TXRechnungHelper.AmountToStr(_Invoice.LineAmount);
        AddChild('ram:ChargeTotalAmount').Text :=  TXRechnungHelper.AmountToStr(_Invoice.ChargeTotalAmount);
        AddChild('ram:AllowanceTotalAmount').Text := TXRechnungHelper.AmountToStr(_Invoice.AllowanceTotalAmount);
        AddChild('ram:TaxBasisTotalAmount').Text := TXRechnungHelper.AmountToStr(_Invoice.TaxExclusiveAmount);
        with AddChild('ram:TaxTotalAmount') do
        begin
          Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
          Text :=  TXRechnungHelper.AmountToStr(_Invoice.TaxAmountTotal);
        end;
        //        <ram:RoundingAmount>0</ram:RoundingAmount>
        AddChild('ram:GrandTotalAmount').Text := TXRechnungHelper.AmountToStr(_Invoice.TaxInclusiveAmount);
        AddChild('ram:TotalPrepaidAmount').Text := TXRechnungHelper.AmountToStr(_Invoice.PrepaidAmount);
        AddChild('ram:DuePayableAmount').Text := TXRechnungHelper.AmountToStr(_Invoice.PayableAmount);
      end;
      for precedingInvoiceReference in _Invoice.PrecedingInvoiceReferences do
      with AddChild('ram:InvoiceReferencedDocument') do
      begin
        AddChild('ram:IssuerAssignedID').Text := precedingInvoiceReference.ID;
        with AddChild('ram:FormattedIssueDateTime').AddChild('qdt:DateTimeString') do
        begin
          Attributes['format'] := '102';
          Text := TXRechnungHelper.DateToStrUNCEFACTFormat(precedingInvoiceReference.IssueDate);
        end;
      end;

    end;
  end;
end;

{ TXRechnungXMLHelper }

class function TXRechnungXMLHelper.SelectNodes(_XnRoot: IXMLDOMNode;
  const _NodePath: String; out _Result : IXMLDOMNodeList): Boolean;
begin
  Result := false;
  _Result := nil;
  if not Assigned(_XnRoot) then
    exit;
  _Result := _XnRoot.selectNodes(_NodePath);
  Result := _Result <> nil;
end;

class function TXRechnungXMLHelper.SelectNodeText(_XnRoot: IXMLDOMNode; const _NodePath: String): String;
var
  node : IXMLDOMNode;
begin
  Result := '';
  if TXRechnungXMLHelper.SelectNode(_XnRoot,_NodePath,node) then
    Result := node.Text;
end;

class function TXRechnungXMLHelper.SelectNode(_XnRoot: IXMLDOMNode;
  const _NodePath: String; out _Result : IXMLDOMNode): Boolean;
begin
  Result := false;
  _Result := nil;
  if not Assigned(_XnRoot) then
    exit;
  _Result := _XnRoot.selectSingleNode(_NodePath);
  Result := _Result <> nil;
end;

class function TXRechnungXMLHelper.FindChild(_Node: IXMLNode; const _NodeName: String;
  out _Result: IXMLNode): Boolean;
begin
  Result := false;
  if _Node = nil then
    exit;
  _Result := _Node.ChildNodes.FindNode(_NodeName,'');
  Result := _Result <> nil;
end;

class procedure TXRechnungXMLHelper.LoadFromChilds(const _NodeName: String; _Node: IXMLNode;
  _Callback: TXMLLoadCallback);
var
  Node : IXMLNode;
begin
  Node := _Node.ChildNodes.FindNode(_NodeName,'');
  if Node = nil then
    exit;
  _Callback(Node);
end;

class function TXRechnungXMLHelper.PrepareDocumentForXPathQuerys(_Xml: IXMLDocument): IXMLDOMDocument2;
var
  hList: IDOMNodeList;
  i: Integer;
  s, sNSN, sNSUri: string;
  sNsLine: string;
begin
  Result := nil;
  if not _Xml.Active then
    exit;

  hList := (_Xml.DOMDocument as IDOMNodeSelect).selectNodes('//namespace::*');
  try
    for i := 0 to hList.length - 1 do
    begin
      sNSN := StringReplace(hList.item[i].nodeName, 'xmlns:', '', []);
      if sNSN = 'xml' then
      begin  // wenn es als xmlns:xml hinzugefuegt wird bekommt man die meldung das der Namespacename xml nicht verwendet werden darf...
        sNSN := 'xmlns:MyXml';
        sNSUri := hList.item[i].nodeValue;
      end
      else
      if sNSN = 'xmlns' then
      begin  // den Default Namespace mit einem Namen versehen, damit XPath drauf zugreifen kann.
        sNSN := 'xmlns:dn';
        sNSUri := hList.item[i].nodeValue;
      end
      else
      begin  // alle anderen Namespace auch fuer XPath bekannt machen
        sNSN := hList.item[i].nodeName;
        sNSUri := hList.item[i].nodeValue;
      end;
      s := sNSN + '="'+sNSUri+'"';
      if ContainsText(sNsLine, s) then
        continue;
      sNsLine := ' '+s + sNsLine;
    end;
    sNsLine := trim(sNsLine);
  finally
    hList := nil;
  end;

  Result := CoDOMDocument60.Create;
  Result.loadXML(_Xml.XML.Text);
  Result.setProperty('SelectionLanguage', 'XPath');  // ab 4.0 ist SelectionLanguage eh immer XPath
  Result.setProperty('SelectionNamespaces', sNsLine) ;
end;

class function TXRechnungXMLHelper.FindNode(_XnRoot: IXMLDOMNode; const _NodePath: String): Boolean;
var
  Node : IXMLDOMNode;
begin
  Result := TXRechnungXMLHelper.SelectNode(_XnRoot,_NodePath,Node);
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
    //iacic_YearlyTurnover: Result :=                                    '105';
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
  if SameText(_Val,'58')  then
    Result := ipmc_SEPACreditTransfer
  else
    Result := ipmc_None;
end;

class function TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Val: TInvoicePaymentMeansCode): String;
begin
  case _Val of
    ipmc_SEPACreditTransfer: Result := '58';
    else Result := '';
  end;
end;

class function TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeFromStr(
  _Val: String): TInvoiceSpecialServiceDescriptionCode;
begin
  if SameText(_Val,'AAA') then
    Result := issdc_AAA_Telecommunication else
  if SameText(_Val,'ABK') then
    Result := issdc_ABK_Miscellaneous else
  if SameText(_Val,'PC') then
    Result := issdc_PC_Packing else
  Result := issdc_None;
end;

class function TXRechnungHelper.InvoiceSpecialServiceDescriptionCodeToStr(
  _Val: TInvoiceSpecialServiceDescriptionCode): String;
begin
  case _Val of
    issdc_AAA_Telecommunication: Result := 'AAA';
    issdc_ABK_Miscellaneous: Result := 'ABK';
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
    if node.Text.EndsWith('xrechnung_2.1',true) then
      Result := XRechnungVersion_211_UBL
    else
    if node.Text.EndsWith('xrechnung_1.2',true) then
      Result := XRechnungVersion_122;
  end else
  if (SameText(_XML.DocumentElement.NodeName,'CrossIndustryInvoice') or SameText(_XML.DocumentElement.NodeName,'rsm:CrossIndustryInvoice')) then
  begin
    if not TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'rsm:ExchangedDocumentContext',node) then
      exit;
    if not TXRechnungXMLHelper.FindChild(node,'ram:GuidelineSpecifiedDocumentContextParameter',node2) then
      exit;
    if not TXRechnungXMLHelper.FindChild(node2,'ram:ID',node) then
      exit;
    if node.Text.EndsWith('xrechnung_2.1',true) then
      Result := XRechnungVersion_211_UNCEFACT
    else
    if node.Text.StartsWith('urn:cen.eu:en16931:2017',true) then
      Result := XRechnungVersion_ReadingSupport_ZUGFeRDFacturX_2xx;
  end;
end;

class function TXRechnungValidationHelper.GetXRechnungVersion(const _Filename: String): TXRechnungVersion;
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

//class function TXRechnungValidationHelper.LoadXSDFromResource(
//  const _ResourceName: String): String;
//var
//  res : TResourceStream;
//  str : TStringStream;
//begin
//  res := TResourceStream.Create(hInstance, _ResourceName, RT_RCDATA);
//  str := TStringStream.Create;
//  try
//    str.LoadFromStream(res);
//    result := str.DataString;
//  finally
//    str.Free;
//    res.Free;
//  end;
//end;

//class function TXRechnungValidationHelper.ValidateXRechnung(
//  const _XML: String; out _Error: TValidationError): Boolean;
//var
//  xml, xsd: IXMLDOMDocument2;
//  cache: IXMLDOMSchemaCollection;
//  error: IXMLDOMParseError;
//begin
//  Result := false;
//  cache := CoXMLSchemaCache60.Create;
//  try
//    xsd := CoDOMDocument60.Create;
//    xsd.Async := False;
//    xsd.loadXML(TXRechnungValidationHelper.LoadXSDFromResource('XRECHNUNG200UBL'));
//    cache.add('urn:un:unece:uncefact:data:standard:UnqualifiedDataType:100', xsd);
//
//    xsd := CoDOMDocument60.Create;
//    xsd.Async := False;
//    xsd.loadXML(TXRechnungValidationHelper.LoadXSDFromResource('XRechnung21BASICQualifiedDataType'));
//    cache.add('urn:un:unece:uncefact:data:standard:QualifiedDataType:100', xsd);
//
//    xsd := CoDOMDocument60.Create;
//    xsd.Async := False;
//    xsd.loadXML(TXRechnungValidationHelper.LoadXSDFromResource('XRechnung21BASICReusableAggregateBusinessInformationEntity'));
//    cache.add('urn:un:unece:uncefact:data:standard:ReusableAggregateBusinessInformationEntity:100', xsd);
//
//    xsd := CoDOMDocument60.Create;
//    xsd.Async := False;
//    xsd.loadXML(TXRechnungValidationHelper.LoadXSDFromResource('XRechnung21BASIC'));
//    cache.add('urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100', xsd);
//
//    xml := CoDOMDocument60.Create;
//    xml.async := False;
//    xml.schemas := cache;
//    xml.validateOnParse := true;
//
//    Result := xml.loadXML(_XML);
//    if not Result then
//    begin
//      _Error.Reason := xml.parseError.reason;
//      _Error.SrcText := xml.parseError.srcText;
//    end;
//  except
//    on E:Exception do begin _Error.Reason := E.Message; _Error.SrcText := E.ClassName; end;
//  end;
//end;

end.

