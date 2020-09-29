unit intf.Invoice;

interface

uses
  System.SysUtils,System.Classes,System.Types,System.DateUtils,System.Rtti
  ,System.Variants,System.Generics.Collections
  ;

type
  TInvoiceTypeCode = (itc_None,
                      itc_PartialInvoice, //326
                      itc_CommercialInvoice, //380
                      itc_CorrectedInvoice, //384
                      itc_SelfbilledInvoice, //389
                      itc_CreditNote, //381
                      itc_PartialConstructionInvoice, //875
                      itc_PartialFinalConstructionInvoice, //876
                      itc_FinalConstructionInvoice); //877

  TInvoicePaymentMeansCode = (ipmc_None, //https://www.xrepository.de/details/urn:xoev-de:xrechnung:codeliste:untdid.4461_2
                      ipmc_SEPACreditTransfer); //58

  TInvoicePaymentTermsType = (iptt_None,
                      iptt_Net,
                      iptt_CashDiscount1,
                      iptt_CashDiscount2);

  {$region 'TInvoiceUnitCode'}
  TInvoiceUnitCode = (iuc_None //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:rec20_1
                      ,iuc_one
                      ,iuc_piece
                      );
  //mehr Einheiten in Res\intf.Invoice.unusedUnits.pas
  {$endregion}

  TInvoiceLine = class(TObject)
  public
    ID : String; //Positionsnummer
    Note : String; //Hinweis
    Name : String; //Kurztext
    Description : String; //Laengere Beschreibung
    Quantity : double; //Menge
    UnitCode : TInvoiceUnitCode; //Mengeneinheit
    SellersItemIdentification : String; //Artikelnummer
    TaxPercent : double; //MwSt
    PriceAmount : Currency; //Einzelpreis
    BaseQuantity : Integer; //Preiseinheit
    BaseQuantityUnitCode : TInvoiceUnitCode; //Preiseinheit Mengeneinheit
    LineAmount : Currency;
  end;

  TInvoiceLines = class(TObjectList<TInvoiceLine>)
  public
    function AddInvoiceLine : TInvoiceLine;
  end;

  TInvoiceTaxAmount = record
    TaxPercent : double;
    TaxableAmount : Currency;
    TaxAmount : Currency;
  end;

  TInvoiceAccountingParty = record
  public
    Name : String;
    RegistrationName : String;
    CompanyID : String;
    StreetName : String;
    //TODO <cbc:AdditionalStreetName>01129</cbc:AdditionalStreetName>
    City : String;
    PostalZone : String;
    //TODO <cbc:CountrySubentity>Sachsen</cbc:CountrySubentity>
    CountryCode : String;

    VATCompanyID : String;

    ContactName : String;
    ContactTelephone : String;
    ContactElectronicMail : String;
  end;

  TInvoice = class(TObject)
  public
    InvoiceNumber : String;  //Rechnungsnummer
    InvoiceIssueDate : TDate; //Rechnungsdatum
    InvoiceDueDate : TDate; //Fälligkeitsdatum
    InvoicePeriodStartDate : TDate; //Leistungszeitraum Beginn
    InvoicePeriodEndDate : TDate; //Leistungszeitraum Ende
    InvoiceTypeCode : TInvoiceTypeCode;
    InvoiceCurrencyCode : String;
    TaxCurrencyCode : String;
    BuyerReference : String; //Pflicht - Leitweg-ID - wird vom Rechnungsempfänger dem Rechnungsersteller zur Verfügung gestellt
    Note : String; //Hinweise zur Rechnung allgemein

    AccountingSupplierParty : TInvoiceAccountingParty;
    AccountingCustomerParty : TInvoiceAccountingParty;

    //TODO weitere Zahlungswege
    PaymentMeansCode : TInvoicePaymentMeansCode;
    PaymentID : String; //Verwendungszweck der Überweisung, optional
    PayeeFinancialAccount : String;
    PayeeFinancialAccountName : String;
    PayeeFinancialInstitutionBranch : String; //BIC

    //TODO Verzugszinsen
    PaymentTermsType : TInvoicePaymentTermsType;
    PaymentTermNetNote : String;
    PaymentTermCashDiscount1Days : Integer;
    PaymentTermCashDiscount1Percent : double;
    PaymentTermCashDiscount1Base : Currency; //Anderer Betrag als der Rechnungsbetrag
    PaymentTermCashDiscount2Days : Integer;
    PaymentTermCashDiscount2Percent : double;
    PaymentTermCashDiscount2Base : Currency; //Anderer Betrag als der Rechnungsbetrag

    InvoiceLines : TInvoiceLines;

    TaxAmountTotal : Currency;
    TaxAmountSubtotals : TArray<TInvoiceTaxAmount>;

    LineAmount : Currency;
    TaxExclusiveAmount : Currency;
    TaxInclusiveAmount : Currency;
    PayableAmount : Currency;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

implementation

{ TInvoice }

constructor TInvoice.Create;
begin
  InvoiceLines := TInvoiceLines.Create;
  Clear;
end;

destructor TInvoice.Destroy;
begin
  if Assigned(InvoiceLines) then begin InvoiceLines.Free; InvoiceLines := nil; end;
  inherited;
end;

procedure TInvoice.Clear;
begin
  InvoiceLines.Clear;
  SetLength(TaxAmountSubtotals,0);
end;

{ TInvoiceLines }

function TInvoiceLines.AddInvoiceLine: TInvoiceLine;
begin
  Result := TInvoiceLine.Create;
  Add(Result);
end;

end.

