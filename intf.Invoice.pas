// ***************************************************************************
//
// XRechnung for Delphi
//
// Copyright (c) 2020 Sven Harazim and Landrix Software
//
// https://github.com/LandrixSoftware/XRechnung-for-Delphi
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// *************************************************************************** }

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


  {$region 'TInvoiceAllowanceOrChargeIdentCode'}
  //cbc:ChargeIndicator = false dann sind folgende Code erlaubt 41 42 60 62 63 64 65 66 67 68 70 71 88 95 100 102 103 104
  TInvoiceAllowanceOrChargeIdentCode = (iacic_None, //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.5189_2
                        //iacic_HandlingCommission, //       Fee for the processing of documentary credit, collection and payment which are charged to the customer.                                                                                    '1';
                        //iacic_AmendmentCommission, //       Fee for amendments in documentary credit and collection business (not extensions and increases of documentary credits).                                                                   '2';
                        //iacic_AcceptanceCommission, //       Fee for the acceptance of draft in documentary credit and collection business which are drawn on us (also to be seen as a kind of 'guarantee commission').                               '3';
                        //iacic_CommissionForObtainingAcceptance, //       Fee for obtaining an acceptance under collections on the basis of 'documents against acceptance'.                                                                            '4';
                        //iacic_CommissionOnDelivery, //       Fee for delivery of documents without corresponding payment.                                                                                                                             '5';
                        //iacic_AdvisingCommission, //       Fee for advising documentary credits (charged also in case of confirmed credits).                                                                                                          '6';
                        //iacic_ConfirmationCommission, //       Fee for confirmation of credit.                                                                                                                                                        '7';
                        //iacic_DeferredPaymentCommission, //       Fee for the deferred payment period under documentary credits confirmed by bank. This fee are charges for the period from presentation of the document until due date of payment.   '8';
                        //iacic_CommissionForTakingUpDocuments, //       Fee charged to the foreign bank for the processing of documentary credit.                                                                                                      '9';
                        //iacic_OpeningCommission, //       Fee for opening revocable documentary credit.                                                                                                                                               '10';
                        //iacic_FeeForPaymentUnderReserve, //       Fee charged to the customer for discrepancies in credit documents in the case of which the bank have to stipulate payment under reserve.                                            '11';
                        //iacic_DiscrepancyFee, //       Fee charged to the foreign bank for discrepancies in credit documents.                                                                                                                         '12';
                        //iacic_DomicilationCommission, //       Fee for the domicilation of bills with the bank.                                                                                                                                       '13';
                        //iacic_CommissionForReleaseOfGoods, //       Commission for the release of goods sent to the bank.                                                                                                                             '14';
                        //iacic_CollectionCommission, //       Fee for settling collections on the basis of 'documents against payments'.                                                                                                               '15';
                        //iacic_NegotiationCommission, //       Fee for the purchase of documents under sight credit for the first ten days.                                                                                                            '16';
                        //iacic_ReturnCommission, //       Fee for cheques, bills and collections returned unpaid and/or recalled.                                                                                                                      '17';
                        //iacic_BLSplittingCharges, //       Fee for the splitting of bills of lading.                                                                                                                                                  '18';
                        //iacic_TrustCommission, //       Fee for the handling on a fiduciary basis of imported goods that have been warehoused.                                                                                                        '19';
                        //iacic_TransferCommission, //       Fee for the transfer of transferable documentary credits.                                                                                                                                  '20';
                        //iacic_CommissionForOpeningIrrevocableDocumentaryCredits, //       Fee for opening irrevocable documentary credits.                                                                                                            '21';
                        //iacic_PreadviceCommission, //       Fee for the pre-advice of a documentary credit.                                                                                                                                           '22';
                        //iacic_SupervisoryCommission, //       Fee for the supervising unconfirmed documentary credits with a deferred payment period.                                                                                                 '23';
                        //iacic_ModelCharges, //       Fee for decoding telex messages.                                                                                                                                                                 '24';
                        //iacic_RiskCommission, //       Commission in addition to the confirmation commission for documentary credits from sensitive countries.                                                                                        '25';
                        //iacic_GuaranteeCommission, //       Commission for drawing up guaranties.                                                                                                                                                     '26';
                        //iacic_ReimbursementCommission, //       Fee for reimbursement of, for example, documentary credits.                                                                                                                           '27';
                        //iacic_StampDuty, //       Tax payable on bills in accordance with national bill of exchange legislation.                                                                                                                      '28';
                        //iacic_Brokerage, //       Brokers commission arising, in trade with foreign currencies.                                                                                                                                       '29';
                        //iacic_BankCharges, //       Charges deducted/claimed by other banks involved in the transaction.                                                                                                                              '30';
                        //iacic_BankChargesInformation, //       Charges not included in the total charge amount i.e. the charges are for information only.                                                                                             '31';
                        //iacic_CourierFee, //       Fee for use of courier service.                                                                                                                                                                    '32';
                        //iacic_PhoneFee, //       Fee for use of phone.                                                                                                                                                                                '33';
                        //iacic_PostageFee, //       Fee for postage.                                                                                                                                                                                   '34';
                        //iacic_SWIFTFee, //       Fee for use of S.W.I.F.T.                                                                                                                                                                            '35';
                        //iacic_TelexFee, //       Fee for telex.                                                                                                                                                                                       '36';
                        //iacic_PenaltyForLateDeliveryOfDocuments, //       Penalty imposed when documents are delivered late.                                                                                                                          '37';
                        //iacic_PenaltyForLateDeliveryOfValuationOfWorks, //       Penalty imposed when valuation of works is delivered late.                                                                                                           '38';
                        //iacic_PenaltyForExecutionOfWorksBehindSchedule, //       Penalty imposed when the execution of works is behind schedule.                                                                                                      '39';
                        //iacic_OtherPenalties, //       Penalty imposed for other reasons.                                                                                                                                                             '40';
                        iacic_BonusForWorksAheadOfSchedule, //       Bonus for completing work ahead of schedule.                                                                                                                                     '41';
                        iacic_OtherBonus, //       Bonus earned for other reasons.                                                                                                                                                                    '42';
                        //iacic_ProjectManagementCost, //       Cost for project management.                                                                                                                                                            '44';
                        //iacic_ProRataRetention, //       Proportional retention charge.                                                                                                                                                               '45';
                        //iacic_ContractualRetention, //       Contractual retention charge.                                                                                                                                                            '46';
                        //iacic_OtherRetentions, //       Retention charge not otherwise specified.                                                                                                                                                     '47';
                        //iacic_InterestOnArrears, //       Interest for late payment.                                                                                                                                                                  '48';
                        //iacic_Interest, //       Cost of using money.                                                                                                                                                                                 '49';
                        //iacic_ChargePerCreditCover, //       Unit charge per credit cover established.                                                                                                                                                '50';
                        //iacic_ChargePerUnusedCreditCover, //       Unit charge per unused credit cover.                                                                                                                                               '51';
                        //iacic_MinimumCommission, //       Minimum commission charge.                                                                                                                                                                  '52';
                        //iacic_FactoringCommission, //       Commission charged for factoring services.                                                                                                                                                '53';
                        //iacic_ChamberOfCommerceCharge, //       Identifies the charges from the chamber of commerce.                                                                                                                                  '54';
                        //iacic_TransferCharges, //       Charges for transfer.                                                                                                                                                                         '55';
                        //iacic_RepatriationCharges, //       Charges for repatriation.                                                                                                                                                                 '56';
                        //iacic_MiscellaneousCharges, //       Not specifically defined charges.                                                                                                                                                        '57';
                        //iacic_ForeignExchangeCharges, //       Charges for foreign exchange.                                                                                                                                                          '58';
                        //iacic_AgreedDebitInterestCharge, //       Charge for agreed debit interest                                                                                                                                                    '59';
                        iacic_ManufacturersConsumerDiscount, //       A discount given by the manufacturer which should be passed on to the consumer.                                                                                                 '60';
                        //iacic_FaxAdviceCharge, //       Charge for fax advice.                                                                                                                                                                        '61';
                        iacic_DueToMilitaryStatus, //       Allowance granted because of the military status.                                                                                                                                         '62';
                        iacic_DueToWorkAccident, //       Allowance granted to a victim of a work accident.                                                                                                                                           '63';
                        iacic_SpecialAgreement, //       An allowance or charge as specified in a special agreement.                                                                                                                                  '64';
                        iacic_ProductionErrorDiscount, //       A discount given for the purchase of a product with a production error.                                                                                                               '65';
                        iacic_NewOutletDiscount, //       A discount given at the occasion of the opening of a new outlet.                                                                                                                            '66';
                        iacic_SampleDiscount, //       A discount given for the purchase of a sample of a product.                                                                                                                                    '67';
                        iacic_EndOfRangeDiscount, //       A discount given for the purchase of an end-of-range product.                                                                                                                              '68';
                        //iacic_ChargeForACustomerSpecificFinish, //       A charge for the addition of a customer specific finish to a product.                                                                                                        '69';
                        iacic_IncotermDiscount, //       A discount given for a specified Incoterm.                                                                                                                                                   '70';
                        iacic_PointOfSalesThresholdAllowance, //       Allowance for reaching or exceeding an agreed sales threshold at the point of sales.                                                                                           '71';
                        //iacic_TechnicalModificationCosts, //       Costs for technical modifications to a product.                                                                                                                                    '72';
                        //iacic_JoborderProductionCosts, //       Costs of job-order production.                                                                                                                                                        '73';
                        //iacic_OffpremisesCosts, //       Expenses for non-local activities.                                                                                                                                                           '74';
                        //iacic_AdditionalProcessingCosts, //       Costs of additional processing.                                                                                                                                                     '75';
                        //iacic_AttestingCharge, //       Costs of official attestation.                                                                                                                                                                '76';
                        //iacic_RushDeliverySurcharge, //       Charge for increased delivery speed.                                                                                                                                                    '77';
                        //iacic_SpecialConstructionCosts, //       Charge for costs incurred as result of special constructions.                                                                                                                        '78';
                        //iacic_FreightCharges, //       Amount to be paid for moving goods, by whatever means, from one place to another.                                                                                                              '79';
                        //iacic_PackingCharge, //       Charge for packing.                                                                                                                                                                             '80';
                        //iacic_RepairCharge, //       Charge for repair.                                                                                                                                                                               '81';
                        //iacic_LoadingCharge, //       Charge for loading.                                                                                                                                                                             '82';
                        //iacic_SetupCharge, //       Charge for setup.                                                                                                                                                                                 '83';
                        //iacic_TestingCharge, //       Charge for testing.                                                                                                                                                                             '84';
                        //iacic_WarehousingCharge, //       Charge for storage and handling.                                                                                                                                                            '85';
                        //iacic_GoldSurcharge, //       Difference between current price and basic value contained in product price in relation to gold content.                                                                                        '86';
                        //iacic_CopperSurcharge, //       Difference between current price and basic value contained in product price in relation to copper content.                                                                                    '87';
                        iacic_MaterialSurchargeDeduction, //       Surcharge/deduction, calculated for higher/ lower material's consumption.                                                                                                          '88';
                        //iacic_LeadSurcharge, //       Difference between current price and basic value contained in product price in relation to lead content.                                                                                        '89';
                        //iacic_PriceIndexSurcharge, //       Higher/lower price, resulting from change in costs between the times of making offer and delivery.                                                                                        '90';
                        //iacic_PlatinumSurcharge, //       Difference between current price and basic value contained in product price in relation to platinum content.                                                                                '91';
                        //iacic_SilverSurcharge, //       Difference between current price and basic value contained in product price in relation to silver content.                                                                                    '92';
                        //iacic_WolframSurcharge, //       Difference between current price and basic value contained in product price in relation to wolfram content.                                                                                  '93';
                        //iacic_AluminumSurcharge, //       Difference between current price and basic value contained in product price in relation to aluminum content.                                                                                '94';
                        iacic_Discount, //       A reduction from a usual or list price.                                                                                                                                                              '95';
                        //iacic_Insurance, //       Charge for insurance.                                                                                                                                                                               '96';
                        //iacic_MinimumOrderMinimumBillingCharge, //       Charge for minimum order or minimum billing.                                                                                                                                 '97';
                        //iacic_MaterialSurchargeSspecialMaterials, //       Surcharge for (special) materials.                                                                                                                                         '98';
                        //iacic_Surcharge, //       An additional amount added to the usual charge.                                                                                                                                                     '99';
                        iacic_SpecialRebate, //       A return of part of an amount paid for goods or services, serving as a reduction or discount.                                                                                                   '100';
                        //iacic_CarbonFootprintCharge, //       A monetary amount charged for carbon footprint related to a regulatory requirement.                                                                                                     '101';
                        iacic_FixedLongTerm, //       A fixed long term allowance or charge.                                                                                                                                                          '102';
                        iacic_Temporary, //       A temporary allowance or charge.                                                                                                                                                                    '103';
                        iacic_Standard //       The standard available allowance or charge.                                                                                                                                                          '104';
                        //iacic_YearlyTurnover, //       An allowance or charge based on yearly turnover.                                                                                                                                               '105';
                        //iacic_WithheldTaxesAndSocialSecurityContributions//       The amount of taxes and contributions for social security, that is subtracted from the payable amount as it is to be paid separately.                             '106';
                        );
  {$endregion}

  {$region 'TInvoiceSpecialServiceDescriptionCode'}
  //cbc:ChargeIndicator = true
  TInvoiceSpecialServiceDescriptionCode = (issdc_None, //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.7161_2
                        issdc_AAA_Telecommunication, //The service of providing telecommunication activities and/or faclities.
                        issdc_ABK_Miscellaneous,	//Miscellaneous services.
                        issdc_PC_Packing //The service of packing.
                        );
  {$endregion}

  {$region 'TInvoiceDutyTaxFeeCategoryCode'}
  //Nur ein Teil der Codes ist erlaubt
  TInvoiceDutyTaxFeeCategoryCode = (idtfcc_None, //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.5305_2
          //idtfcc_A_MixedTaxRate, //	Code specifying that the rate is based on mixed tax.
          //idtfcc_AA_LowerRate, //	Tax rate is lower than standard rate.
          //idtfcc_AB_ExemptForResale, //	A tax category code indicating the item is tax exempt when the item is bought for future resale.
          //idtfcc_AC_ValueAddedTaxVATNotNowDueForPayment, //	A code to indicate that the Value Added Tax (VAT) amount which is due on the current invoice is to be paid on receipt of a separate VAT payment request.
          //idtfcc_AD_ValueAddedTaxVATDueFromAPreviousInvoice, //	A code to indicate that the Value Added Tax (VAT) amount of a previous invoice is to be paid.
          idtfcc_AE_VATReverseCharge, //	Code specifying that the standard VAT rate is levied from the invoicee.
          //idtfcc_B_TransferredVAT, //	VAT not to be paid to the issuer of the invoice but directly to relevant tax authority.
          //idtfcc_C_DutyPaidBySupplier, //	Duty associated with shipment of goods is paid by the supplier; customer receives goods with duty paid.
          //idtfcc_D_ValueAddedTaxVATMmarginSchemeTravelAgents, //	Indication that the VAT margin scheme for travel agents is applied.
          idtfcc_E_ExemptFromTax, //	Code specifying that taxes are not applicable.
          //idtfcc_F_ValueAddedTaxVATMmarginSchemeSecondhandGoods, //	Indication that the VAT margin scheme for second-hand goods is applied.
          idtfcc_G_FreeExportItemTaxNotCharged, //	Code specifying that the item is free export and taxes are not charged.
          //idtfcc_H_HigherRate, //	Code specifying a higher rate of duty or tax or fee.
          //idtfcc_I_ValueAddedTaxVATMarginSchemeWorksOfArt, // Margin scheme — Works of art	Indication that the VAT margin scheme for works of art is applied.
          //idtfcc_J_ValueAddedTaxVATMarginSchemeCollectorsItemsAndAntiques, //	Indication that the VAT margin scheme for collector’s items and antiques is applied.
          idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices, //	A tax category code indicating the item is VAT exempt due to an intra-community supply in the European Economic Area.
          idtfcc_L_CanaryIslandsGeneralIndirectTax, //	Impuesto General Indirecto Canario (IGIC) is an indirect tax levied on goods and services supplied in the Canary Islands (Spain) by traders and professionals, as well as on import of goods.
          idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla, //	Impuesto sobre la Producción, los Servicios y la Importación (IPSI) is an indirect municipal tax, levied on the production, processing and import of all kinds of movable tangible property, the supply of services and the transfer of immovable property located in the cities of Ceuta and Melilla.
          idtfcc_O_ServicesOutsideScopeOfTax, //	Code specifying that taxes are not applicable to the services.
          idtfcc_S_StandardRate, //	Code specifying the standard rate.
          idtfcc_Z_ZeroRatedGoods);
  {$endregion}

  TInvoiceAllowanceCharge = class(TOBject)
  public
    ChargeIndicator : Boolean;
    ReasonCodeAllowance : TInvoiceAllowanceOrChargeIdentCode;
    ReasonCodeCharge : TInvoiceSpecialServiceDescriptionCode;
    Reason : String;
    BaseAmount : Currency;
    MultiplierFactorNumeric : double;
    Amount : Currency;
    TaxPercent : double;
    TaxCategory : TInvoiceDutyTaxFeeCategoryCode;
  end;

  TInvoiceAllowanceCharges = class(TObjectList<TInvoiceAllowanceCharge>)
  public
    function AddAllowanceCharge : TInvoiceAllowanceCharge;
  end;

  TInvoiceLines = class;

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
    TaxCategory : TInvoiceDutyTaxFeeCategoryCode; //MwSt-Einordnung
    PriceAmount : Currency; //Einzelpreis
    BaseQuantity : Integer; //Preiseinheit
    BaseQuantityUnitCode : TInvoiceUnitCode; //Preiseinheit Mengeneinheit
    LineAmount : Currency;
    AllowanceCharges : TInvoiceAllowanceCharges;
    SubInvoiceLines : TInvoiceLines;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TInvoiceLines = class(TObjectList<TInvoiceLine>)
  public
    function AddInvoiceLine : TInvoiceLine;
  end;

  TInvoiceTaxAmount = record
    TaxableAmount : Currency;
    TaxAmount : Currency;
    TaxPercent : double;
    TaxCategory : TInvoiceDutyTaxFeeCategoryCode;
    TaxExemptionReasonCode : String; //sollte gesetzt werden bei TaxCategory = AE,E,O,Z
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

    IdentifierSellerBuyer : String; //Kreditor-Nr AccountingSupplierParty / Debitor-Nr AccountingCustomerParty

    VATCompanyID : String;

    ContactName : String;
    ContactTelephone : String;
    ContactElectronicMail : String;
  end;

  TInvoicePrecedingInvoiceReference = class(TObject)
  public
    ID : String;
    IssueDate : TDate;
  end;

  TInvoicePrecedingInvoiceReferences = class(TObjectList<TInvoicePrecedingInvoiceReference>)
  public
    function AddPrecedingInvoiceReference : TInvoicePrecedingInvoiceReference;
  end;

  TInvoice = class(TObject)
  public
    InvoiceNumber : String;  //Rechnungsnummer
    InvoiceIssueDate : TDate; //Rechnungsdatum
    InvoiceDueDate : TDate; //Faelligkeitsdatum
    InvoicePeriodStartDate : TDate; //Leistungszeitraum Beginn
    InvoicePeriodEndDate : TDate; //Leistungszeitraum Ende
    InvoiceTypeCode : TInvoiceTypeCode;
    InvoiceCurrencyCode : String; //EUR
    TaxCurrencyCode : String;     //EUR
    BuyerReference : String; //Pflicht - Leitweg-ID - wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
    Note : String; //Hinweise zur Rechnung allgemein
    PurchaseOrderReference : String; //Bestellnummer oder Vertragsnummer des Kaeufers

    AccountingSupplierParty : TInvoiceAccountingParty;
    AccountingCustomerParty : TInvoiceAccountingParty;

    //TODO weitere Zahlungswege
    PaymentMeansCode : TInvoicePaymentMeansCode;
    PaymentID : String; //Verwendungszweck der Ueberweisung, optional
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

    AllowanceCharges : TInvoiceAllowanceCharges; //Nachlaesse, Zuschlaege
    PrecedingInvoiceReferences : TInvoicePrecedingInvoiceReferences;

    TaxAmountTotal : Currency;
    TaxAmountSubtotals : TArray<TInvoiceTaxAmount>;

    LineAmount : Currency;
    TaxExclusiveAmount : Currency;
    TaxInclusiveAmount : Currency;
    AllowanceTotalAmount : Currency;
    ChargeTotalAmount : Currency;
    PrepaidAmount : Currency;
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
  AllowanceCharges := TInvoiceAllowanceCharges.Create;
  PrecedingInvoiceReferences := TInvoicePrecedingInvoiceReferences.Create;
  Clear;
end;

destructor TInvoice.Destroy;
begin
  if Assigned(InvoiceLines) then begin InvoiceLines.Free; InvoiceLines := nil; end;
  if Assigned(AllowanceCharges) then begin AllowanceCharges.Free; AllowanceCharges := nil; end;
  if Assigned(PrecedingInvoiceReferences) then begin PrecedingInvoiceReferences.Free; PrecedingInvoiceReferences := nil; end;
  inherited;
end;

procedure TInvoice.Clear;
begin
  InvoiceLines.Clear;
  AllowanceCharges.Clear;
  PrecedingInvoiceReferences.Clear;
  SetLength(TaxAmountSubtotals,0);
  PaymentTermsType := iptt_None;
end;

{ TInvoiceLines }

function TInvoiceLines.AddInvoiceLine: TInvoiceLine;
begin
  Result := TInvoiceLine.Create;
  Add(Result);
end;

{ TInvoiceAllowanceCharges }

function TInvoiceAllowanceCharges.AddAllowanceCharge: TInvoiceAllowanceCharge;
begin
  Result := TInvoiceAllowanceCharge.Create;
  Add(Result);
end;

{ TInvoicePrecedingInvoiceReferences }

function TInvoicePrecedingInvoiceReferences.AddPrecedingInvoiceReference: TInvoicePrecedingInvoiceReference;
begin
  Result := TInvoicePrecedingInvoiceReference.Create;
  Add(Result);
end;

{ TInvoiceLine }

constructor TInvoiceLine.Create;
begin
  AllowanceCharges := TInvoiceAllowanceCharges.Create;
  SubInvoiceLines := TInvoiceLines.Create;
end;

destructor TInvoiceLine.Destroy;
begin
  if Assigned(AllowanceCharges) then begin AllowanceCharges.Free; AllowanceCharges := nil; end;
  if Assigned(SubInvoiceLines) then begin SubInvoiceLines.Free; SubInvoiceLines := nil; end;
  inherited;
end;

end.

