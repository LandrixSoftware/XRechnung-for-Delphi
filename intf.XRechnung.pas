unit intf.XRechnung;

interface

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants,System.StrUtils,System.Generics.Collections
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema//,intf.MSXML2_TLB
  {$IFDEF USE_OXMLDomVendor},OXmlDOMVendor{$ENDIF}
  ,intf.Invoice
  ;

type
  TXRechnungXMLHelper = class(TObject)
  public type
    TXMLLoadCallback = procedure(Node : IXMLNode) of object;
  public
    class procedure LoadFromChilds(const _NodeName : String; _Node : IXMLNode; _Callback : TXMLLoadCallback);
    class function FindChild(_Node : IXMLNode; const _NodeName : String; out _Result : IXMLNode) : Boolean;
  end;

  TXRechnungHelper = class(TObject)
  public
//    class function DateFromStr(const _Val : String) : TDateTime;
    class function DateToStr(const _Val : TDateTime) : String;
//    class function StrToCurr(_Val : String) : Currency;
//    class function StrToFloat(_Val : String) : double;
    class function AmountToStr(_Val : Currency) : String;
    class function UnitPriceAmountToStr(_Val : Currency) : String;
    class function FloatToStr(_Val : double) : String;
    class function PercentageToStr(_Val : double) : String;
    class function QuantityToStr(_Val : double) : String;
    class function InvoiceTypeCodeToStr(_Val : TInvoiceTypeCode) : String;
    class function InvoicePaymentMeansCodeToStr(_Val : TInvoicePaymentMeansCode) : String;
    class function InvoiceUnitCodeToStr(_Val : TInvoiceUnitCode) : String;   //mehr Konvertierungen in Res\intf.XRechnung.unusedUnits.pas
  end;

  TXRechnungValidationHelper = class(TObject)
  public type
    TXRechnungVersion = (XRechnungVersion_Unkown,XRechnungVersion_122,XRechnungVersion_200);
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
    class procedure SaveDocument(_Invoice: TInvoice;_Version : TXRechnungValidationHelper.TXRechnungVersion; _Xml : IXMLDocument);
  public
    class procedure SaveToStream(_Invoice : TInvoice; _Version : TXRechnungValidationHelper.TXRechnungVersion; _Stream : TStream);
    class procedure SaveToFile(_Invoice : TInvoice; _Version : TXRechnungValidationHelper.TXRechnungVersion; const _Filename : String);
    class procedure SaveToXMLStr(_Invoice : TInvoice; _Version : TXRechnungValidationHelper.TXRechnungVersion; out _XML : String);
  end;

implementation

//{$R intf.XRechnungSchema.res}

{ TXRechnungInvoiceAdapter }

class procedure TXRechnungInvoiceAdapter.SaveToStream(_Invoice: TInvoice;
  _Version : TXRechnungValidationHelper.TXRechnungVersion; _Stream: TStream);
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
  _Version : TXRechnungValidationHelper.TXRechnungVersion; out _XML: String);
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
  _Version : TXRechnungValidationHelper.TXRechnungVersion;const _Filename: String);
var
  xml : IXMLDocument;
begin
  if _Invoice = nil then
    exit;
  if _Filename.IsEmpty then
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

class procedure TXRechnungInvoiceAdapter.SaveDocument(_Invoice: TInvoice;
  _Version : TXRechnungValidationHelper.TXRechnungVersion; _Xml: IXMLDocument);
var
  xRoot : IXMLNode;
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
    xRoot.AddChild('cbc:CustomizationID').Text := 'urn:cen.eu:en16931:2017#compliant#urn:xoev-de:kosit:standard:xrechnung_2.0';

  xRoot.AddChild('cbc:ID').Text := _Invoice.InvoiceNumber;
  xRoot.AddChild('cbc:IssueDate').Text := TXRechnungHelper.DateToStr(_Invoice.InvoiceIssueDate);
  if _Invoice.InvoiceDueDate > 100 then xRoot.AddChild('cbc:DueDate').Text := TXRechnungHelper.DateToStr(_Invoice.InvoiceDueDate);
  xRoot.AddChild('cbc:InvoiceTypeCode').Text := TXRechnungHelper.InvoiceTypeCodeToStr(_Invoice.InvoiceTypeCode);
  if not _Invoice.Note.IsEmpty then
    xRoot.AddChild('cbc:Note').Text := _Invoice.Note;
  xRoot.AddChild('cbc:DocumentCurrencyCode').Text := _Invoice.InvoiceCurrencyCode;
  xRoot.AddChild('cbc:TaxCurrencyCode').Text := _Invoice.TaxCurrencyCode;
  xRoot.AddChild('cbc:BuyerReference').Text := _Invoice.BuyerReference;
  if (_Invoice.InvoicePeriodStartDate > 100) and (_Invoice.InvoicePeriodEndDate > 100) then
  with xRoot.AddChild('cac:InvoicePeriod') do
  begin
    AddChild('cbc:StartDate').Text := TXRechnungHelper.DateToStr(_Invoice.InvoicePeriodStartDate);
    AddChild('cbc:EndDate').Text := TXRechnungHelper.DateToStr(_Invoice.InvoicePeriodEndDate);
  end;

  with xRoot.AddChild('cac:AccountingSupplierParty').AddChild('cac:Party') do
  begin
    with AddChild('cac:PartyName') do
    begin
      AddChild('cbc:Name').Text := _Invoice.AccountingSupplierParty.Name;
    end;
    with AddChild('cac:PostalAddress') do
    begin
      AddChild('cbc:StreetName').Text := _Invoice.AccountingSupplierParty.StreetName;
      AddChild('cbc:CityName').Text := _Invoice.AccountingSupplierParty.City;
      AddChild('cbc:PostalZone').Text := _Invoice.AccountingSupplierParty.PostalZone;
      AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.AccountingSupplierParty.CountryCode;
    end;
    if not _Invoice.AccountingSupplierParty.VATCompanyID.IsEmpty then
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
    with AddChild('cac:PartyName') do
    begin
      AddChild('cbc:Name').Text := _Invoice.AccountingCustomerParty.Name;
    end;
    with AddChild('cac:PostalAddress') do
    begin
      AddChild('cbc:StreetName').Text := _Invoice.AccountingCustomerParty.StreetName;
      AddChild('cbc:CityName').Text := _Invoice.AccountingCustomerParty.City;
      AddChild('cbc:PostalZone').Text := _Invoice.AccountingCustomerParty.PostalZone;
      AddChild('cac:Country').AddChild('cbc:IdentificationCode').Text := _Invoice.AccountingCustomerParty.CountryCode;
    end;
    if not _Invoice.AccountingCustomerParty.VATCompanyID.IsEmpty then
    with AddChild('cac:PartyTaxScheme') do
    begin
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.VATCompanyID;
      AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
    end;
    with AddChild('cac:PartyLegalEntity') do
    begin
      AddChild('cbc:RegistrationName').Text := _Invoice.AccountingCustomerParty.RegistrationName;
      AddChild('cbc:CompanyID').Text := _Invoice.AccountingCustomerParty.CompanyID;
    end;
    with AddChild('cac:Contact') do
    begin
      AddChild('cbc:Name').Text := _Invoice.AccountingCustomerParty.ContactName;
      AddChild('cbc:Telephone').Text := _Invoice.AccountingCustomerParty.ContactTelephone;
      AddChild('cbc:ElectronicMail').Text := _Invoice.AccountingCustomerParty.ContactElectronicMail;
    end;
  end;

  if (_Invoice.PaymentMeansCode <> ipmc_None) and (not _Invoice.PayeeFinancialAccount.IsEmpty) then
  with xRoot.AddChild('cac:PaymentMeans') do
  begin
    AddChild('cbc:PaymentMeansCode').Text := TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Invoice.PaymentMeansCode);
    if not _Invoice.PaymentID.IsEmpty then
      AddChild('cbc:PaymentID').Text := _Invoice.PaymentID;
    with AddChild('cac:PayeeFinancialAccount') do
    begin
      AddChild('cbc:ID').Text := _Invoice.PayeeFinancialAccount;
      if not _Invoice.PayeeFinancialAccountName.IsEmpty then
        AddChild('cbc:Name').Text := _Invoice.PayeeFinancialAccountName;
      if not _Invoice.PayeeFinancialInstitutionBranch.IsEmpty then
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
            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount1Base)+'#','');
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
            TXRechnungHelper.AmountToStr(_Invoice.PaymentTermCashDiscount2Base)+'#','');
      end;
    end;
  end;

  with xRoot.AddChild('cac:TaxTotal') do
  begin
    with AddChild('cbc:TaxAmount') do
    begin
      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
      Text := TXRechnungHelper.AmountToStr(_Invoice.TaxAmountTotal);
    end;
    for var taxSubtotal : TInvoiceTaxAmount in _Invoice.TaxAmountSubtotals do
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
        AddChild('cbc:ID').Text := 'S';
        AddChild('cbc:Percent').Text := TXRechnungHelper.PercentageToStr(taxSubtotal.TaxPercent);
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
      //      <cbc:AllowanceTotalAmount currencyID="EUR">0</cbc:AllowanceTotalAmount>
      //      <cbc:ChargeTotalAmount currencyID="EUR">0</cbc:ChargeTotalAmount>
      //      <cbc:PrepaidAmount currencyID="EUR">0</cbc:PrepaidAmount>
      //      <cbc:PayableRoundingAmount currencyID="EUR">0</cbc:PayableRoundingAmount>
      with AddChild('cbc:PayableAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.AmountToStr(_Invoice.PayableAmount);
      end;
  end;

  for var invoiceLine : TInvoiceLine in _Invoice.InvoiceLines do
  with xRoot.AddChild('cac:InvoiceLine') do
  begin
    AddChild('cbc:ID').Text := invoiceLine.ID;
    if not invoiceLine.Note.IsEmpty then
      AddChild('cbc:Note').Text := invoiceLine.Note;
    with AddChild('cbc:InvoicedQuantity') do
    begin
      Attributes['unitCode'] := TXRechnungHelper.InvoiceUnitCodeToStr(invoiceLine.UnitCode);
      Text := TXRechnungHelper.QuantityToStr(invoiceLine.Quantity);
    end;
    with AddChild('cbc:LineExtensionAmount') do
    begin
      Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
      Text := TXRechnungHelper.AmountToStr(invoiceLine.LineAmount);
    end;
    //  <cac:DocumentReference>
    //     <cbc:ID/>
    //     <cbc:DocumentType>916</cbc:DocumentType>
    //  </cac:DocumentReference>
    with AddChild('cac:Item') do
    begin
      AddChild('cbc:Description').Text := invoiceLine.Description;
      AddChild('cbc:Name').Text := invoiceLine.Name;
      //   <cac:BuyersItemIdentification>
      //      <cbc:ID/>
      //   </cac:BuyersItemIdentification>
      AddChild('cac:SellersItemIdentification').AddChild('cbc:ID').Text := invoiceLine.SellersItemIdentification;
      //<cac:StandardItemIdentification>
      //      <cbc:ID schemeID="0001"/>
      //   </cac:StandardItemIdentification>
      with AddChild('cac:ClassifiedTaxCategory') do
      begin
        AddChild('cbc:ID').Text := 'S';
        AddChild('cbc:Percent').Text := TXRechnungHelper.PercentageToStr(invoiceLine.TaxPercent);
        AddChild('cac:TaxScheme').AddChild('cbc:ID').Text := 'VAT';
      end;

    end;
    with AddChild('cac:Price') do
    begin
      with AddChild('cbc:PriceAmount') do
      begin
        Attributes['currencyID'] := _Invoice.TaxCurrencyCode;
        Text := TXRechnungHelper.UnitPriceAmountToStr(invoiceLine.PriceAmount);
      end;
      if (invoiceLine.BaseQuantity <> 0) and (invoiceLine.BaseQuantityUnitCode <> iuc_None) then
      with AddChild('cbc:BaseQuantity') do
      begin
        Attributes['unitCode'] := TXRechnungHelper.InvoiceUnitCodeToStr(invoiceLine.BaseQuantityUnitCode);
        Text := IntToStr(invoiceLine.BaseQuantity);
      end;
//         <cac:AllowanceCharge>
//            <cbc:ChargeIndicator>false</cbc:ChargeIndicator>
//            <cbc:Amount currencyID="EUR">4</cbc:Amount>
//            <cbc:BaseAmount currencyID="EUR">55</cbc:BaseAmount>
//         </cac:AllowanceCharge>
    end;

  end;
end;

{ TXRechnungXMLHelper }

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

{ TXRechnungHelper }

class function TXRechnungHelper.AmountToStr(
  _Val: Currency): String;
begin
  Result := System.StrUtils.ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TXRechnungHelper.UnitPriceAmountToStr(
  _Val: Currency): String;
begin
  Result := System.StrUtils.ReplaceText(Format('%.4f',[_Val]),',','.');
end;

//class function TXRechnungHelper.DateFromStr(
//  const _Val: String): TDateTime;
//begin
//  Result := 0;
//  if Length(_Val) <> 8 then
//    exit;
//  Result := EncodeDate(StrToIntDef(Copy(_Val,1,4),1999),StrToIntDef(Copy(_Val,5,2),1),StrToIntDef(Copy(_Val,7,2),1));
//end;

class function TXRechnungHelper.DateToStr(
  const _Val: TDateTime): String;
begin
  Result := FormatDateTime('yyyy-mm-dd',_Val);
end;

class function TXRechnungHelper.FloatToStr(
  _Val: double): String;
begin
  Result := System.StrUtils.ReplaceText(Format('%.2f',[_Val]),',','.');
end;

class function TXRechnungHelper.InvoicePaymentMeansCodeToStr(_Val: TInvoicePaymentMeansCode): String;
begin
  case _Val of
    ipmc_SEPACreditTransfer: Result := '58';
    else Result := '';
  end;
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

class function TXRechnungHelper.InvoiceUnitCodeToStr(_Val: TInvoiceUnitCode): String;
begin
  //mehr Konvertierungen in Res\intf.XRechnung.unusedUnits.pas
  case _Val of
    iuc_one : Result := 'C62';
    iuc_piece : Result := 'H87';
  end;
end;

class function TXRechnungHelper.PercentageToStr(_Val: double): String;
begin
  Result := System.StrUtils.ReplaceText(Format('%.2f',[_Val]),',','.');
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
  node : IXMLNode;
begin
  Result := XRechnungVersion_Unkown;
  if _XML = nil then
    exit;
  if (not (SameText(_XML.DocumentElement.NodeName,'Invoice') or SameText(_XML.DocumentElement.NodeName,'ubl:Invoice'))) then
    exit;
  if not TXRechnungXMLHelper.FindChild(_XML.DocumentElement,'cbc:CustomizationID',node) then
    exit;

  if node.Text.EndsWith('xrechnung_2.0',true) then
    Result := XRechnungVersion_200
  else
  if node.Text.EndsWith('xrechnung_1.2',true) then
    Result := XRechnungVersion_122;
end;

class function TXRechnungValidationHelper.GetXRechnungVersion(const _Filename: String): TXRechnungVersion;
var
  xml : IXMLDocument;
begin
  Result := XRechnungVersion_Unkown;
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

