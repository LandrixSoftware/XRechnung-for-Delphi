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

unit intf.Invoice;

interface

uses
  System.SysUtils,System.Classes,System.Types
  ,System.Generics.Collections,System.NetEncoding
  ;

type
  TInvoiceEmptyLeitwegID = class
  public const
    NON_EXISTENT = 'non-existent';
  end;

  TInvoiceTypeCode = (
    itc_None,

    ///TypeCode: 84
    itc_DebitnoteRelatedToFinancialAdjustments, // 84 - nicht in XRechnung verwenden

    ///TypeCode: 261
    itc_SelfBilledCreditNote, // 261 - nicht in XRechnung verwenden

    ///TypeCode: 326 Teilrechnung (eigentlich: Teilschlussrechnung)
    ///Die Teilrechnung ist eine normale Rechnung mit der eine oder
    ///mehrere erbrachte Leistungsposten abgerechnet werden. Dies ist
    ///meisten der Fall bei laengeren Projekten oder z.B. gestaffelten
    ///Warenlieferungen. Im Gegensatz zu einer Abschlagsrechnung, die
    ///unabhaengig von der bereits erbrachten Leistung ist, werden mit
    ///einer Teilrechnung nur tatsaechlich erbrachte Leistungen abgerechnet
    ///mit allen damit verbundenen Gewaehrleistungspflichten.
    itc_PartialInvoice, //326

    ///TypeCode: 380 Rechnung
    ///Dieser Typ beschreibt den Normalfall einer Waren- oder
    ///Handelsrechnung und ist der gebraeuchlichste Typ fuer die meisten
    ///Rechnungen.
    itc_CommercialInvoice, //380

    itc_DebitNote, // 383 Belastungsanzeige - nicht in XRechnung verwenden

    ///TypeCode: 384 Rechnungskorrektur
    ///Eine Rechnungskorrektur (oder auch Stornorechnung) wird erstellt,
    ///wenn eine Rechnung falsch erstellt wurde oder die Leistung nicht
    ///vollstaendig oder mangelhaft erbracht wurde und damit der
    ///Rechnungsbetrag reduziert werden muss. Eine Rechnungskorrektur
    ///muss sich dabei immer auf eine bereits erstellte Rechnung beziehen.
    itc_CorrectedInvoice, //384

    itc_PrepaymentInvoice, // 386 Vorauszahlungsrechnung - nicht in XRechnung verwenden

    itc_Cancellation, // 457 Storno - nicht in XRechnung verwenden

    ///TypeCode: 389 Selbstfakturierte Rechnung
    ///Eine selbstfakturierte Rechnung wird vom Kunden ausgestellt.
    ///Der Lieferant erhaelt eine Rechnungskopie und die Zahlung von
    ///dem Kunde.
    ///Fuer die Abrechnung von Bauleistungen muessen gemaess Paragraph 14 und 16 VOB/B
    ///folgende Rechnungstypen verwendet werden.
    itc_SelfbilledInvoice,

    ///TypeCode: 381 Gutschrift
    ///Eine Gutschrift ist steuerrechtlich eine Rechnung, die -im Gegensatz
    ///zur Rechnung- vom Leistungsempfaenger ausgestellt wird. Nicht zu
    ///verwechseln ist die Gutschrift mit einer Rechnungskorrektur die
    ///landlaeufig auch als Gutschrift bezeichnet wird.
    ///Die Gutschrift weist immer einen positiven Betrag aus.
    itc_CreditNote,

    ///TypeCode: 875 Abschlagsrechnung (Bauleistung)
    ///Eine Abschlagsrechnung fuer Bauleistung. Eine Abschlagsrechnung
    ///hat nicht dieselbe Verbindlichkeit wie eine Teilschluss- oder
    ///Schlussrechnung.
    itc_PartialConstructionInvoice,

    ///Typecode: 876 Teilschlussrechnung (Bauleistung)
    ///Eine Teilschlussrechnung hat denselben Charakter wie eine
    ///Schlussrechnung. Mit einer Teilschlussrechnung werden bereits
    ///geleistet Arbeiten im Rahmen einer Teilabnahme abgerechnet.
    itc_PartialFinalConstructionInvoice,

    ///TypeCode: 877 Schlussrechnung (Bauleistung)
    ///Grundlage fuer die Schlussrechnung ist die Fertigstellung und die
    ///Abnahme der vereinbarten Leistungen zzgl. etwaiger Nachforderungen.
    itc_FinalConstructionInvoice
  );

  //TODO pruefen, ob es noch mehr gibt
  //https://www.xrepository.de/details/urn:xoev-de:xrechnung:codeliste:untdid.4461_2
  TInvoicePaymentMeansCode = (
    ipmc_NotImplemented,
    ipmc_InstrumentNotDefined, //1  Keine Angaben
    ipmc_InCash,               //10 Barzahlung
    ipmc_Cheque,               //20 Scheck
    ipmc_CreditTransfer,       //30 Ueberweisung - Ausland (nicht SEPA)
    ipmc_CreditCard,           //54 Kreditkarte
    ipmc_SEPACreditTransfer,   //58 Ueberweisung (SEPA)
    ipmc_SEPADirectDebit       //59 Lastschrift (SEPA)
  );

  TInvoicePaymentTermsType = (iptt_None,
                      iptt_Net,
                      iptt_CashDiscount1,
                      iptt_CashDiscount2);

  TInvoiceUnitCode = (iuc_None //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:rec20_1
                      ,iuc_one   //C62 A unit of count defining the number of pieces
                      ,iuc_piece //H87
                      ,iuc_number_of_articles
                      ,iuc_set
                      ,iuc_week
                      ,iuc_month
                      ,iuc_day
                      ,iuc_tonne_metric_ton
                      ,iuc_square_metre
                      ,iuc_cubic_metre
                      ,iuc_metre
                      ,iuc_square_millimetre
                      ,iuc_cubic_millimetre
                      ,iuc_millimetre
                      ,iuc_minute_unit_of_time
                      ,iuc_second_unit_of_time
                      ,iuc_litre
                      ,iuc_hour
                      ,iuc_kilogram
                      ,iuc_kilometre
                      ,iuc_kilowatt_hour
                      );
  //mehr Einheiten in Res\intf.Invoice.unusedUnits.pas

  TInvoiceAttachmentType = (iat_application_None,
                      iat_application_pdf,
                      iat_image_png,
                      iat_image_jpeg,
                      iat_text_csv,
                      iat_application_vnd_openxmlformats_officedocument_spreadsheetml_sheet,
                      iat_application_vnd_oasis_opendocument_spreadsheet,
                      iat_application_xml //ab XRechnung 2.0.0
                      );

  TInvoiceAttachmentTypeHelper = class(TObject)
  public
    class function GetTypeFromFilename(const _Filename : String): TInvoiceAttachmentType;
  end;

  //Entweder externe Referenz oder eingebettetes Objekt
  //Ob man die Daten als Base64 integriert oder separat mitliefert,
  //haengt wahrscheinlich vom Empfaenger ab
  TInvoiceAttachment = class(TObject)
  public
    ID : String;
    DocumentDescription : String;
    Filename : String;
    AttachmentType : TInvoiceAttachmentType;
    Data : TMemoryStream;         //https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice/cac-AdditionalDocumentReference/cac-Attachment/cbc-EmbeddedDocumentBinaryObject/
    ExternalReference : String;   //https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice/cac-AdditionalDocumentReference/cac-Attachment/cac-ExternalReference/
  public
    constructor Create(_AttachmentType : TInvoiceAttachmentType);
    destructor Destroy; override;
    procedure EmbedDataFromStream(_Stream : TStream);
    procedure EmbedDataFromFile(const _Filename : String);
    function GetDataAsBase64 : String;
    procedure SetDataFromBase64(const _Val : String);
    function ContainsBinaryObject : Boolean;
  end;

  TInvoiceAttachmentList = class(TObjectList<TInvoiceAttachment>)
  public
    function AddAttachment(_AttachmentType : TInvoiceAttachmentType) : TInvoiceAttachment;
    function TryAddAttachmentByExtension(const _Filename : String; out _Attachment : TInvoiceAttachment) : Boolean;
  end;

  TInvoiceUnitCodeHelper = class(TObject)
  public
    class function MapUnitOfMeasure(_UnitOfMeasure : String; out _Success : Boolean; _DefaultOnFailure : TInvoiceUnitCode = TInvoiceUnitCode.iuc_piece) : TInvoiceUnitCode;
  end;

  //cbc:ChargeIndicator = false dann sind folgende Code erlaubt 41 42 60 62 63 64 65 66 67 68 70 71 88 95 100 102 103 104 105
  //Keine anderen Codes in XRechnung moeglich !!!
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.5189_3
  TInvoiceAllowanceOrChargeIdentCode = (
    iacic_None,
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
    iacic_Standard, //       The standard available allowance or charge.                                                                                                                                                          '104';
    iacic_YearlyTurnover //       An allowance or charge based on yearly turnover.                                                                                                                                               '105';
    //iacic_WithheldTaxesAndSocialSecurityContributions//       The amount of taxes and contributions for social security, that is subtracted from the payable amount as it is to be paid separately.                             '106';
    );

  //cbc:ChargeIndicator = true
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.7161_3
  TInvoiceSpecialServiceDescriptionCode = (issdc_None,
                        issdc_AA_Advertising, //The service of providing advertising.
                        issdc_AAA_Telecommunication, //The service of providing telecommunication activities and/or faclities.
                        issdc_ABK_Miscellaneous,	//Miscellaneous services.
                        issdc_ABL_AdditionalPackaging, //The service of providing additional packaging.
                        issdc_ADR_OtherServices, //A code indicating that other non-specific services are in operation.
                        issdc_ADT_Pickup, //The service of picking up or collection of goods.
                        issdc_FC_FreightService, //The service of moving goods, by whatever means, from one place to another.
                        issdc_FI_Financing, //The service of providing financing.
                        issdc_LA_Labelling, //Labelling service.
                        issdc_PC_Packing //The service of packing.
                        );

  //Nur ein Teil der Codes ist erlaubt
  //Die Codes fuer die Umsatzsteuerkategorie sind Folgende:
  //- S = Umsatzsteuer faellt mit Normalsatz an
  //- Z = nach dem Nullsatz zu versteuernde Waren
  //- E = Steuerbefreit
  //- AE = Umkehrung der Steuerschuldnerschaft
  //- K = Kein Ausweis der Umsatzsteuer bei innergemeinschaftlichen Lieferungen
  //- G = Steuer nicht erhoben aufgrund von Export ausserhalb der EU
  //Bei gewerblichen Endkunden im Reverse-Charge ist hier "AE" anzugeben.
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
          //idtfcc_I_ValueAddedTaxVATMarginSchemeWorksOfArt, // Margin scheme - Works of art	Indication that the VAT margin scheme for works of art is applied.
          //idtfcc_J_ValueAddedTaxVATMarginSchemeCollectorsItemsAndAntiques, //	Indication that the VAT margin scheme for collector s items and antiques is applied.
          idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices, //	A tax category code indicating the item is VAT exempt due to an intra-community supply in the European Economic Area.
          idtfcc_L_CanaryIslandsGeneralIndirectTax, //	Impuesto General Indirecto Canario (IGIC) is an indirect tax levied on goods and services supplied in the Canary Islands (Spain) by traders and professionals, as well as on import of goods.
          idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla, //	Impuesto sobre la Produccion, los Servicios y la Importacion (IPSI) is an indirect municipal tax, levied on the production, processing and import of all kinds of movable tangible property, the supply of services and the transfer of immovable property located in the cities of Ceuta and Melilla.
          //idtfcc_O_ServicesOutsideScopeOfTax, //	Code specifying that taxes are not applicable to the services.
          idtfcc_S_StandardRate, //	Code specifying the standard rate.
          idtfcc_Z_ZeroRatedGoods);

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
    GlobalID_EAN_GTIN: String; //BT-157 GTIN/EAN
    Note : String; //Hinweis
    Name : String; //Kurztext
    Description : String; //Laengere Beschreibung
    Quantity : double; //Menge
    UnitCode : TInvoiceUnitCode; //Mengeneinheit
    SellersItemIdentification : String; //Artikelnummer
    TaxPercent : double; //MwSt
    TaxCategory : TInvoiceDutyTaxFeeCategoryCode; //MwSt-Einordnung
    GrossPriceAmount : Currency; //Brutto-Einzelpreis
    DiscountOnTheGrossPrice : Currency; //Rabatt auf den Bruttopreis ergibt Nettopreis, nur ein Rabatt moeglich wegen UBL, obwohl CII mehrere erlaubt
    NetPriceAmount : Currency; //Netto-Einzelpreis
    BaseQuantity : double; //Preiseinheit
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
    TaxExemptionReason : String; //sollte gesetzt werden bei TaxCategory = AE,E,O,Z
  end;

  TInvoiceTaxAmountArray = TArray<TInvoiceTaxAmount>;

  {$IF CompilerVersion >= 33.0}
  TInvoiceTaxAmountArrayHelper = record helper for TInvoiceTaxAmountArray
  public
    function AddTaxAmountIfTaxExists(_TaxPercent : double; _TaxableAmount,_TaxAmount : Currency) : Boolean;
    procedure SetCapacity(_Capacity : Integer);
  end;
  {$ENDIF}

  TInvoiceAddress = record
  public
    StreetName : String;
    AdditionalStreetName : String;
    City : String;
    PostalZone : String;
    CountrySubentity : String;
    AddressLine : String;
    CountryCode : String;
  end;

  TInvoiceAccountingParty = record
  public
    Name : String;
    RegistrationName : String;
    CompanyID : String;

    Address : TInvoiceAddress;

    IdentifierSellerBuyer : String; //Kreditor-Nr AccountingSupplierParty / Debitor-Nr AccountingCustomerParty
    BankAssignedCreditorIdentifier : String; //Glaeubiger-ID (BT-90)

    VATCompanyID : String;   //BT-31
    VATCompanyNumber: String;//BT-32

    ContactName : String;
    ContactTelephone : String;
    ContactElectronicMail : String;
    AdditionalLegalInformationSeller : String; //BT-33 Weitere rechtliche Informationen zum Verkaeufer
    ElectronicAddressSellerBuyer : String; //BT-34, BT-49 Pflicht
  end;

  TInvoiceDeliveryInformation = record
  public
    Name : String;
    //LocationIdentifier : String; //optional Ein Bezeichner fuer den Ort, an den die Waren geliefert oder an dem die Dienstleistungen erbracht werden.
    Address : TInvoiceAddress;
    ActualDeliveryDate : TDate; //Lieferdatum
  end;

  TInvoicePrecedingInvoiceReference = class(TObject)
  public
    ID : String;
    IssueDate : TDate;
  end;

  TInvoicePrecedingInvoiceReferences = class(TObjectList<TInvoicePrecedingInvoiceReference>)
  public
    function AddPrecedingInvoiceReference : TInvoicePrecedingInvoiceReference;
    function IndexOfPrecedingInvoiceReference(const _ID : String) : Integer;
  end;

  TInvoiceNote = class(Tobject)
  public
    Content : String;
  end;

  TInvoiceNotes = class(TObjectList<TInvoiceNote>)
  public
    function AddNote: TInvoiceNote;
    function NodeContentsAsText : String;
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
    BuyerReference : String; //Pflicht - Leitweg-ID - https://leitweg-id.de/home/ wird vom Rechnungsempfaenger dem Rechnungsersteller zur Verfuegung gestellt
    Notes : TInvoiceNotes; //Hinweise zur Rechnung allgemein
    SellerOrderReference : String; //!! in UBL entweder SellerOrderReference oder PurchaseOrderReference
    PurchaseOrderReference : String; //Bestellnummer oder Vertragsnummer des Kaeufers
    ProjectReference : String;
    ContractDocumentReference : String;
    DeliveryReceiptNumber : String; //Lieferscheinnummer (Lieferscheindatum fehlt und wuerde nur in ZUGFeRD unterstuetzt)

    AccountingSupplierParty : TInvoiceAccountingParty;
    AccountingCustomerParty : TInvoiceAccountingParty;
    DeliveryInformation : TInvoiceDeliveryInformation;

    //TODO weitere Zahlungswege, als Liste
    //TODO Auch 0 pruefen
    PaymentMeansCode : TInvoicePaymentMeansCode;
    PaymentID : String; //Verwendungszweck der Ueberweisung/Lastschrift
    PaymentFinancialAccount : String; //sowohl Payee (Ueberweisung 58) als auch Payer (Lastschrift 59)
    PaymentFinancialAccountName : String; //sowohl Payee (Ueberweisung 58) als auch Payer (Lastschrift 59)
    PaymentFinancialInstitutionBranch : String; //BIC sowohl Payee (Ueberweisung 58) als auch Payer (Lastschrift 59)
    PaymentMandateID : String; //Lastschrift (59) Mandatsreferenz BT-89

    //Infos unter
    //https://www.e-rechnung-bund.de/wp-content/uploads/2023/04/Angabe-Skonto-Upload.pdf
    PaymentTermsType : TInvoicePaymentTermsType;
    PaymentTermNetNote : String;
    PaymentTermCashDiscount1Days : Integer;
    PaymentTermCashDiscount1Percent : double;
    PaymentTermCashDiscount1Base : Currency; //Anderer Betrag als der Rechnungsbetrag
    PaymentTermCashDiscount2Days : Integer;
    PaymentTermCashDiscount2Percent : double;
    PaymentTermCashDiscount2Base : Currency; //Anderer Betrag als der Rechnungsbetrag

    InvoiceLines : TInvoiceLines;

    Attachments : TInvoiceAttachmentList;

    AllowanceCharges : TInvoiceAllowanceCharges; //Nachlaesse, Zuschlaege
    PrecedingInvoiceReferences : TInvoicePrecedingInvoiceReferences;

    TaxAmountTotal : Currency;
    TaxAmountSubtotals : TInvoiceTaxAmountArray;

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
  Attachments := TInvoiceAttachmentList.Create;
  AllowanceCharges := TInvoiceAllowanceCharges.Create;
  PrecedingInvoiceReferences := TInvoicePrecedingInvoiceReferences.Create;
  Notes := TInvoiceNotes.Create;
  PaymentMeansCode := ipmc_NotImplemented;
  PaymentTermsType := iptt_None;
  Clear;
end;

destructor TInvoice.Destroy;
begin
  if Assigned(InvoiceLines) then begin InvoiceLines.Free; InvoiceLines := nil; end;
  if Assigned(Attachments) then begin Attachments.Free; Attachments := nil; end;
  if Assigned(AllowanceCharges) then begin AllowanceCharges.Free; AllowanceCharges := nil; end;
  if Assigned(PrecedingInvoiceReferences) then begin PrecedingInvoiceReferences.Free; PrecedingInvoiceReferences := nil; end;
  if Assigned(Notes) then begin Notes.Free; Notes := nil; end;
  inherited;
end;

procedure TInvoice.Clear;
begin
  InvoiceLines.Clear;
  AllowanceCharges.Clear;
  PrecedingInvoiceReferences.Clear;
  Notes.Clear;
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

function TInvoicePrecedingInvoiceReferences.IndexOfPrecedingInvoiceReference(
  const _ID: String): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
  if SameText(_ID,Items[i].ID) then
  begin
    Result := i;
    break;
  end;
end;

{ TInvoiceNotes }

function TInvoiceNotes.AddNote: TInvoiceNote;
begin
  Result := TInvoiceNote.Create;
  Add(Result);
end;

function TInvoiceNotes.NodeContentsAsText: String;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Count-1 do
  begin
    Result := Result + Items[i].Content;
    if i < Count-1 then
      Result := Result + #13#10;
  end;
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

{ TInvoiceUnitCodeHelper }

class function TInvoiceUnitCodeHelper.MapUnitOfMeasure(_UnitOfMeasure: String; out _Success: Boolean;
  _DefaultOnFailure: TInvoiceUnitCode): TInvoiceUnitCode;
begin
  Result := _DefaultOnFailure;
  _Success := false;
  _UnitOfMeasure := Trim(_UnitOfMeasure);
  if _UnitOfMeasure = '' then
  begin
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'st') or
     SameText(_UnitOfMeasure,'stk.') or
     SameText(_UnitOfMeasure,'stk') or
     SameText(_UnitOfMeasure,'stck') or
     SameText(_UnitOfMeasure,'psch') then
  begin
    result := iuc_piece;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'mal') then
  begin
    result := iuc_one;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'std') or SameText(_UnitOfMeasure,'std.') then
  begin
    result := iuc_hour;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'tag') or SameText(_UnitOfMeasure,'tage') then
  begin
    result := iuc_day;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'monat') then
  begin
    result := iuc_month;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'woche') then
  begin
    result := iuc_week;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'kg') then
  begin
    result := iuc_kilogram;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'km') then
  begin
    result := iuc_kilometre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'kwh') then
  begin
    result := iuc_kilowatt_hour;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'t') or SameText(_UnitOfMeasure,'tonne') then
  begin
    result := iuc_tonne_metric_ton;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'qm') or
     SameText(_UnitOfMeasure,'m2') then
  begin
    result := iuc_square_metre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'qqm') then
  begin
    result := iuc_cubic_metre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'m') then
  begin
    result := iuc_metre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'mm') then
  begin
    result := iuc_millimetre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'min') then
  begin
    result := iuc_minute_unit_of_time;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'sek') then
  begin
    result := iuc_second_unit_of_time;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'l') then
  begin
    result := iuc_litre;
    _Success := true;
    exit;
  end;
end;

{ TInvoiceAttachment }

function TInvoiceAttachment.ContainsBinaryObject: Boolean;
begin
  Result := Data.Size > 0;
end;

constructor TInvoiceAttachment.Create(_AttachmentType: TInvoiceAttachmentType);
begin
  AttachmentType := _AttachmentType;
  Data := TMemoryStream.Create;
  ExternalReference := '';
end;

destructor TInvoiceAttachment.Destroy;
begin
  if Assigned(Data) then begin Data.Free; Data := nil; end;
  inherited;
end;

procedure TInvoiceAttachment.EmbedDataFromFile(const _Filename: String);
var
  str : TFileStream;
begin
  if not FileExists(_Filename) then
    exit;
  str := TFileStream.Create(_Filename,fmOpenRead);
  try
    EmbedDataFromStream(str);
  finally
    str.Free;
  end;
end;

procedure TInvoiceAttachment.EmbedDataFromStream(_Stream: TStream);
begin
  if _Stream = nil then
    exit;
  Data.Clear;
  Data.LoadFromStream(_Stream);
end;

function TInvoiceAttachment.GetDataAsBase64: String;
var
  str : TMemoryStream;
  base64 : System.NetEncoding.TBase64Encoding;
  internalResult : AnsiString;
begin
  Result := '';
  Data.Seek(0,soFromBeginning);
  if Data.Size = 0 then
    exit;
  str := TMemoryStream.Create;
  base64 := System.NetEncoding.TBase64Encoding.Create(0); // CharsPerLine = 0 means no line breaks
  try
    base64.Encode(Data,str);
    str.Seek(0,soFromBeginning);
    if str.Size = 0 then
      exit;
    SetLength(internalResult,str.Size);
    str.Read(internalResult[1],str.Size);
    Result := String(internalResult);
  finally
    base64.Free;
    str.Free;
  end;
end;

procedure TInvoiceAttachment.SetDataFromBase64(const _Val: String);
var
  str : TMemoryStream;
  internalValue : AnsiString;
begin
  Data.Clear;
  if _Val = '' then
    exit;
  internalValue := AnsiString(_Val);
  str := TMemoryStream.Create;
  try
    str.Write(internalValue[1],Length(internalValue));
    str.Seek(0,soFromBeginning);
    System.NetEncoding.TBase64Encoding.Base64.Decode(str,Data);
    Data.Seek(0,soFromBeginning);
  finally
    str.Free;
  end;
end;

{ TInvoiceAttachmentList }

function TInvoiceAttachmentList.AddAttachment(_AttachmentType: TInvoiceAttachmentType): TInvoiceAttachment;
begin
  Result := TInvoiceAttachment.Create(_AttachmentType);
  Add(Result);
end;

function TInvoiceAttachmentList.TryAddAttachmentByExtension(const _Filename: String;
  out _Attachment: TInvoiceAttachment): Boolean;
var
  fileType : TInvoiceAttachmentType;
begin
  Result := false;

  if not FileExists(_Filename) then
    exit;

  fileType := TInvoiceAttachmentTypeHelper.GetTypeFromFilename(_Filename);

  if fileType = iat_application_None then
    exit;

  _Attachment := AddAttachment(fileType);
  Result := true;
end;

{ TInvoiceAttachmentTypeHelper }

class function TInvoiceAttachmentTypeHelper.GetTypeFromFilename(
  const _Filename: String): TInvoiceAttachmentType;
var
  fileExt : String;
begin
  Result := iat_application_None;

  fileExt := ExtractFileExt(_Filename);
  if SameText(fileExt,'.xlsx') then
    Result := iat_application_vnd_openxmlformats_officedocument_spreadsheetml_sheet
  else
  if SameText(fileExt,'.ods') then
    Result := iat_application_vnd_oasis_opendocument_spreadsheet
  else
  if SameText(fileExt,'.csv') then
    Result := iat_text_csv
  else
  if SameText(fileExt,'.jpg') then
    Result := iat_image_jpeg
  else
  if SameText(fileExt,'.pdf') then
    Result := iat_application_pdf
  else
  if SameText(fileExt,'.png') then
    Result := iat_image_png
  else
  if SameText(fileExt,'.xml') then
    Result := iat_application_xml;
end;

{$IF CompilerVersion >= 33.0}

{ TInvoiceTaxAmountArrayHelper }

function TInvoiceTaxAmountArrayHelper.AddTaxAmountIfTaxExists(
  _TaxPercent: double; _TaxableAmount, _TaxAmount: Currency): Boolean;
var
  i : Integer;
begin
  Result := false;
  for i := 0 to Length(self)-1 do
  if self[i].TaxPercent = _TaxPercent then
  begin
    self[i].TaxableAmount := self[i].TaxableAmount + _TaxableAmount;
    self[i].TaxAmount := self[i].TaxAmount + _TaxAmount;
    Result := true;
    break;
  end;
end;

procedure TInvoiceTaxAmountArrayHelper.SetCapacity(_Capacity: Integer);
begin
  SetLength(self,_Capacity);
end;

{$ENDIF}

end.

