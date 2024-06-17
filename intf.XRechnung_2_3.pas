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

unit intf.XRechnung_2_3;

interface

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants,System.StrUtils,System.Generics.Collections
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema,intf.XRechnungMSXML2_TLB
  {$IFDEF USE_OXMLDomVendor},OXmlDOMVendor{$ENDIF}
  ,intf.Invoice
  ,intf.XRechnungHelper
  ;

type
  TXRechnungInvoiceAdapter230 = class
  public
    class procedure SaveDocumentUNCEFACT(_Invoice: TInvoice;_Xml : IXMLDocument);
    class procedure SaveDocumentUBL(_Invoice: TInvoice;_Xml : IXMLDocument);
    class function LoadDocumentUNCEFACT(_Invoice: TInvoice;_Xml : IXMLDocument; out _Error : String) : Boolean;
    class function LoadDocumentUBL(_Invoice: TInvoice;_Xml : IXMLDocument; out _Error : String) : Boolean;
  end;

implementation

uses
  intf.XRechnung;

{ TXRechnungInvoiceAdapter230 }

class function TXRechnungInvoiceAdapter230.LoadDocumentUBL(_Invoice: TInvoice;
  _Xml: IXMLDocument; out _Error : String) : Boolean;
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
//    if _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer <> '' then
//    with AddChild('cbc:EndpointID') do
//    begin
//      Attributes['schemeID'] := 'EM';
//      Text := _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer;
//    end;
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
//      AddChild('cbc:CompanyID').Text := _Invoice.AccountingSupplierParty.VATCompanyID; VATCompanyName
//      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT'; FC
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
//    if _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer <> '' then
//    with AddChild('cbc:EndpointID') do
//    begin
//      Attributes['schemeID'] := 'EM';
//      Text := _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer;
//    end;
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
//      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.VATCompanyID; VATCompanyName
//      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT'; FC
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

class function TXRechnungInvoiceAdapter230.LoadDocumentUNCEFACT(_Invoice: TInvoice;
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
    if TXRechnungXMLHelper.SelectNode(xml,'//*[local-name()="ExchangedDocument"]/ram:ID',node) then
      _Invoice.InvoiceNumber := node.Text;
    if TXRechnungXMLHelper.SelectNode(xml,'//*[local-name()="ExchangedDocument"]/ram:IssueDateTime/udt:DateTimeString',node) then
      _Invoice.InvoiceIssueDate := TXRechnungHelper.DateFromStrUNCEFACTFormat(node.Text);
    if TXRechnungXMLHelper.SelectNode(xml,'//*[local-name()="ExchangedDocument"]/ram:TypeCode',node) then
      _Invoice.InvoiceTypeCode := TXRechnungHelper.InvoiceTypeCodeFromStr(node.Text);
    if TXRechnungXMLHelper.SelectNode(xml,'//*[local-name()="ExchangedDocument"]/ram:IncludedNote/ram:Content',node) then
      _Invoice.Note := node.Text;

    if not TXRechnungXMLHelper.SelectNode(xml,'//*[local-name()="SupplyChainTradeTransaction"]',nodeSupplyChainTradeTransaction) then
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
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:URIUniversalCommunication',node3) then
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:URIID',node) then
            _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer := node.text;
        if TXRechnungXMLHelper.SelectNodes(node2,'.//ram:SpecifiedTaxRegistration',nodes) then
        for i := 0  to nodes.length-1 do
        if TXRechnungXMLHelper.SelectNode(nodes[i],'.//ram:ID',node3) then
        begin
          if SameText(node3.attributes.getNamedItem('schemeID').Text,'VA') then
            _Invoice.AccountingSupplierParty.VATCompanyID := node3.text
          else
          if SameText(node3.attributes.getNamedItem('schemeID').Text,'FC') then
            _Invoice.AccountingSupplierParty.VATCompanyNumber := node3.text;
        end;
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
        if TXRechnungXMLHelper.SelectNode(node2,'.//ram:URIUniversalCommunication',node3) then
          if TXRechnungXMLHelper.SelectNode(node3,'.//ram:URIID',node) then
            _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer := node.text;
        if TXRechnungXMLHelper.SelectNodes(node2,'.//ram:SpecifiedTaxRegistration',nodes) then
        for i := 0  to nodes.length-1 do
        if TXRechnungXMLHelper.SelectNode(nodes[i],'.//ram:ID',node3) then
        begin
          if SameText(node3.attributes.getNamedItem('schemeID').Text,'VA') then
            _Invoice.AccountingCustomerParty.VATCompanyID := node3.text
          else
          if SameText(node3.attributes.getNamedItem('schemeID').Text,'FC') then
            _Invoice.AccountingCustomerParty.VATCompanyNumber := node3.text;
        end;
      end;
      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:SellerOrderReferencedDocument',node2) then
      if TXRechnungXMLHelper.SelectNode(node2,'.//ram:IssuerAssignedID',node) then
        _Invoice.SellerOrderReference := node.text;
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

      if TXRechnungXMLHelper.SelectNode(nodeApplicableHeaderTradeAgreement,'.//ram:DeliveryNoteReferencedDocument',node2) then
      if TXRechnungXMLHelper.SelectNode(node2,'.//ram:IssuerAssignedID',node3) then
        _Invoice.DeliveryReceiptNumber := Node3.text;
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

class procedure TXRechnungInvoiceAdapter230.SaveDocumentUBL(_Invoice: TInvoice;
  _Xml: IXMLDocument);
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
    //if _Version in [XRechnungVersion_220_UBL,XRechnungVersion_230_UBL] then
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

  xRoot.AddChild('cbc:CustomizationID').Text := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3'+
           IfThen(InternalExtensionEnabled,'#conformant#urn:xoev-de:kosit:extension:xrechnung_2.3','');

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
  if _Invoice.DeliveryReceiptNumber <> '' then
    xRoot.AddChild('cac:DespatchDocumentReference').AddChild('cbc:ID').Text := _Invoice.DeliveryReceiptNumber;
  if _Invoice.ContractDocumentReference <> '' then
    xRoot.AddChild('cac:ContractDocumentReference').AddChild('cbc:ID').Text := _Invoice.ContractDocumentReference;

  for i := 0 to _Invoice.Attachments.Count -1 do
  begin
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
    if _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer <> '' then
    with AddChild('cbc:EndpointID') do
    begin
      Attributes['schemeID'] := 'EM';
      Text := _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer;
    end;
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
    if _Invoice.AccountingSupplierParty.VATCompanyNumber <> '' then
    with AddChild('cac:PartyTaxScheme') do
    begin
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingSupplierParty.VATCompanyNumber;
      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'FC';
    end;
    with AddChild('cac:PartyLegalEntity') do
    begin
      AddChild('cbc:RegistrationName').Text := _Invoice.AccountingSupplierParty.RegistrationName;
      if _Invoice.AccountingSupplierParty.CompanyID <> '' then
        AddChild('cbc:CompanyID').Text := _Invoice.AccountingSupplierParty.CompanyID;
      if not _Invoice.AccountingSupplierParty.AdditionalLegalInformationSeller.IsEmpty then
        AddChild('cbc:CompanyLegalForm').Text := _Invoice.AccountingSupplierParty.AdditionalLegalInformationSeller;
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
    if _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer <> '' then
    with AddChild('cbc:EndpointID') do
    begin
      Attributes['schemeID'] := 'EM';
      Text := _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer;
    end;
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
    if _Invoice.AccountingCustomerParty.VATCompanyNumber <> '' then
    with AddChild('cac:PartyTaxScheme') do
    begin
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.VATCompanyNumber;
      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'FC';
    end;
    with AddChild('cac:PartyLegalEntity') do
    begin
      AddChild('cbc:RegistrationName').Text := _Invoice.AccountingCustomerParty.RegistrationName;
      if _Invoice.AccountingCustomerParty.CompanyID <> '' then
        AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.CompanyID;
    end;
    if (_Invoice.AccountingCustomerParty.ContactName <> '') or
       (_Invoice.AccountingCustomerParty.ContactTelephone <> '') or
       (_Invoice.AccountingCustomerParty.ContactElectronicMail <> '') then
    with AddChild('cac:Contact') do
    begin
      if (_Invoice.AccountingCustomerParty.ContactName <> '') then
        AddChild('cbc:Name').Text := _Invoice.AccountingCustomerParty.ContactName;
      if (_Invoice.AccountingCustomerParty.ContactTelephone <> '') then
        AddChild('cbc:Telephone').Text := _Invoice.AccountingCustomerParty.ContactTelephone;
      if (_Invoice.AccountingCustomerParty.ContactElectronicMail <> '') then
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

  with xRoot.AddChild('cac:PaymentMeans') do
  begin
    AddChild('cbc:PaymentMeansCode').Text := TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Invoice.PaymentMeansCode);
    if _Invoice.PaymentID <> '' then
      AddChild('cbc:PaymentID').Text := _Invoice.PaymentID;
    if (_Invoice.PayeeFinancialAccount <> '') then
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
            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','')+#13#10;
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
            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount2Base)+'#','')+#13#10;
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

class procedure TXRechnungInvoiceAdapter230.SaveDocumentUNCEFACT(
  _Invoice: TInvoice; _Xml: IXMLDocument);
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
       .AddChild('ram:ID').Text := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.3';

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
        if _Invoice.AccountingSupplierParty.AdditionalLegalInformationSeller <> '' then
          AddChild('ram:Description').Text := _Invoice.AccountingSupplierParty.AdditionalLegalInformationSeller;
        with AddChild('ram:SpecifiedLegalOrganization') do
        begin
          if _Invoice.AccountingSupplierParty.CompanyID <> '' then
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
        if _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer <> '' then
        with AddChild('ram:URIUniversalCommunication').AddChild('ram:URIID') do
        begin
          Attributes['schemeID'] := 'EM';
          Text := _Invoice.AccountingSupplierParty.ElectronicAddressSellerBuyer;
        end;
        if _Invoice.AccountingSupplierParty.VATCompanyID <> '' then
        with AddChild('ram:SpecifiedTaxRegistration').AddChild('ram:ID') do
        begin
          Attributes['schemeID'] := 'VA';
          Text := _Invoice.AccountingSupplierParty.VATCompanyID;
        end;
        if _Invoice.AccountingSupplierParty.VATCompanyNumber <> '' then
        with AddChild('ram:SpecifiedTaxRegistration').AddChild('ram:ID') do
        begin
          Attributes['schemeID'] := 'FC';
          Text := _Invoice.AccountingSupplierParty.VATCompanyNumber;
        end;
      end;
      with AddChild('ram:BuyerTradeParty') do
      begin
        if _Invoice.AccountingCustomerParty.IdentifierSellerBuyer <> '' then
          AddChild('ram:ID').Text := _Invoice.AccountingCustomerParty.IdentifierSellerBuyer;
        AddChild('ram:Name').Text := _Invoice.AccountingCustomerParty.RegistrationName;

        with AddChild('ram:SpecifiedLegalOrganization') do
        begin
          if _Invoice.AccountingCustomerParty.CompanyID <> '' then
            AddChild('ram:ID').Text := _Invoice.AccountingCustomerParty.CompanyID;
          AddChild('ram:TradingBusinessName').Text := _Invoice.AccountingCustomerParty.Name;
        end;
        if (_Invoice.AccountingCustomerParty.ContactName <> '') or
           (_Invoice.AccountingCustomerParty.ContactTelephone <> '') or
           (_Invoice.AccountingCustomerParty.ContactElectronicMail <> '') then
        with AddChild('ram:DefinedTradeContact') do
        begin
          if (_Invoice.AccountingCustomerParty.ContactName <> '') then
            AddChild('ram:PersonName').Text := _Invoice.AccountingCustomerParty.ContactName;
          if (_Invoice.AccountingCustomerParty.ContactTelephone <> '') then
            AddChild('ram:TelephoneUniversalCommunication').AddChild('ram:CompleteNumber').Text := _Invoice.AccountingCustomerParty.ContactTelephone;
          if (_Invoice.AccountingCustomerParty.ContactElectronicMail <> '') then
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
        if _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer <> '' then
        with AddChild('ram:URIUniversalCommunication').AddChild('ram:URIID') do
        begin
          Attributes['schemeID'] := 'EM';
          Text := _Invoice.AccountingCustomerParty.ElectronicAddressSellerBuyer;
        end;
        if _Invoice.AccountingCustomerParty.VATCompanyID <> '' then
        with AddChild('ram:SpecifiedTaxRegistration').AddChild('ram:ID') do
        begin
          Attributes['schemeID'] := 'VA';
          Text := _Invoice.AccountingCustomerParty.VATCompanyID;
        end;
        if _Invoice.AccountingCustomerParty.VATCompanyNumber <> '' then
        with AddChild('ram:SpecifiedTaxRegistration').AddChild('ram:ID') do
        begin
          Attributes['schemeID'] := 'FC';
          Text := _Invoice.AccountingCustomerParty.VATCompanyNumber;
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
    begin
      if (_Invoice.DeliveryInformation.Address.CountryCode <> '') or
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
      end;
      if (_Invoice.DeliveryInformation.ActualDeliveryDate > 0) then
      with AddChild('ram:ActualDeliverySupplyChainEvent')
           .AddChild('ram:OccurrenceDateTime')
           .AddChild('udt:DateTimeString') do
      begin
        Attributes['format'] := '102';
        Text := TXRechnungHelper.DateToStrUNCEFACTFormat(_Invoice.DeliveryInformation.ActualDeliveryDate);
      end;
      if _Invoice.DeliveryReceiptNumber <> '' then
      with AddChild('ram:DeliveryNoteReferencedDocument')
           .AddChild('ram:IssuerAssignedID') do
      begin
        Text := _Invoice.DeliveryReceiptNumber;
      end;
    end;
    with AddChild('ram:ApplicableHeaderTradeSettlement') do
    begin
      if _Invoice.PaymentID <> '' then
        AddChild('ram:PaymentReference').Text := _Invoice.PaymentID;
      //zuviel AddChild('ram:TaxCurrencyCode').Text := _Invoice.TaxCurrencyCode;
      AddChild('ram:InvoiceCurrencyCode').Text := _Invoice.InvoiceCurrencyCode;
      with AddChild('ram:SpecifiedTradeSettlementPaymentMeans') do
      begin
        AddChild('ram:TypeCode').Text := TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Invoice.PaymentMeansCode);
        if (_Invoice.PayeeFinancialAccount <> '') then
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


end.

