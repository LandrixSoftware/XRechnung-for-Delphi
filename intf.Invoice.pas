{
License XRechnung-for-Delphi

Copyright (C) 2024 Landrix Software GmbH & Co. KG
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
along with this program.  If not, see <https://www.gnu.org/licenses/>.
}

unit intf.Invoice;

interface

uses
  System.SysUtils,System.Classes,System.Types,System.Contnrs
  ,System.NetEncoding
  ;

type
  TInvoiceEmptyLeitwegID = class
  public const
    NON_EXISTENT = 'non-existent';
  end;

  {$IF CompilerVersion >= 36.0}
  TInvoiceListItemType = NativeInt;
  {$ELSE}
  TInvoiceListItemType = Integer;
  {$IFEND}
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
    ipmc_AutomatedClearingHouseCredit,                         //2  A credit transaction made through the automated clearing house system.
    ipmc_AutomatedClearingHouseDebit,                          //3  A debit transaction made through the automated clearing house system.
    ipmc_ACH_DemandDebitReversal,                              //4  A request to reverse an ACH debit transaction to a demand deposit account.
    ipmc_ACH_DemandCreditReversal,                             //5  A request to reverse a credit transaction to a demand deposit account.
    ipmc_ACH_Demand_Credit,                                    //6  A credit transaction made through the ACH system to a demand deposit account.
    ipmc_ACH_Demand_Debit,                                     //7  A debit transaction made through the ACH system to a demand deposit account.
    ipmc_Hold,                                                 //8  Indicates that the bank should hold the payment for collection by the beneficiary or other instructions.
    ipmc_NationalOrRegionalClearing,                           //9  Indicates that the payment should be made using the national or regional clearing.
    ipmc_InCash,               //10 Barzahlung
    ipmc_ACH_SavingsCreditReversal,                            //11 A request to reverse an ACH credit transaction to a savings account.
    ipmc_ACH_SavingsDebitReversal,                             //12 A request to reverse an ACH debit transaction to a savings account.
    ipmc_ACH_SavingsCredit,                                    //13 A credit transaction made through the ACH system to a savings account.
    ipmc_ACH_SavingsDebit,                                     //14 A debit transaction made through the ACH system to a savings account.
    ipmc_BookEntryCredit,                                      //15 A credit entry between two accounts at the same bank branch. Synonym: house credit.
    ipmc_BookEntryDebit,                                       //16 A debit entry between two accounts at the same bank branch. Synonym: house debit.
    ipmc_ACH_DemandCashConcentrationDisbursementCredit,        //17 A credit transaction made through the ACH system to a demand deposit account using the CCD payment format.
    ipmc_ACH_DemandCashConcentrationDisbursementDebit,         //18 A debit transaction made through the ACH system to a demand deposit account using the CCD payment format.
    ipmc_ACH_DemandCorporateTradePaymentCredit,                //19 A credit transaction made through the ACH system to a demand deposit account using the CTP payment format.
    ipmc_Cheque,               //20 Scheck
    ipmc_BankersDraft,                                         //21 Issue of a banker's draft in payment of the funds.
    ipmc_CertifiedBankerDraft,                                 //22 Cheque drawn by a bank on itself or its agent. A person who owes money to another buys the draft from a bank for cash and hands it to the Crdt who need have no fear that it might be dishonoured.
    ipmc_BankChequeIssuedByEstablishment,                      //23 Payment by a pre-printed form, which has been completed by a financial institution, on which instructions are given to an account holder (a bank or building society) to pay a stated sum to a named recipient.
    ipmc_BillOfExchangeAwaitingAcceptance,                     //24 Bill drawn by the Crdt on the debtor but not yet accepted by the debtor.
    ipmc_CertifiedCheque,                                      //25 Payment by a pre-printed form stamped with the paying bank's certification on which instructions are given to an account holder (a bank or building society) to pay a stated sum to a named recipient .
    ipmc_LocalCheque,                                          //26 Indicates that the cheque is given local to the recipient.
    ipmc_ACH_DemandCorporateTradePaymentDebit,                 //27 A debit transaction made through the ACH system to a demand deposit account using the CTP payment format.
    ipmc_ACH_DemandCorporateTradeExchangeCredit,               //28 A credit transaction made through the ACH system to a demand deposit account using the CTX payment format.
    ipmc_ACH_DemandCorporateTradeExchangeDebit,                //29 A debit transaction made through the ACH system to a demand account using the CTX payment format.
    ipmc_CreditTransfer,       //30 Ueberweisung nicht SEPA (nicht SEPA)
    ipmc_DebitTransfer,                                        //31 Payment by debit movement of funds from one account to another.
    ipmc_ACH_DemandCashConcentrationDisbursementPlusCredit,    //32 A credit transaction made through the ACH system to a demand deposit account using the CCD+ payment format.
    ipmc_ACH_DemandCashConcentrationDisbursementPlusDebit,     //33 A debit transaction made through the ACH system to a demand deposit account using the CCD+ payment format.
    ipmc_ACH_PrearrangedPaymentAndDeposit,                     //34 A consumer credit transaction made through the ACH system to a demand deposit or savings account.
    ipmc_ACH_SavingsCashConcentrationDisbursementCredit,       //35 A credit transaction made through the ACH system to a demand deposit or savings account.
    ipmc_ACH_SavingsCashConcentrationDisbursementDebit,        //36 A debit transaction made through the ACH system to a savings account using the CCD payment format.
    ipmc_ACH_SavingsCorporateTradePaymentCredit,               //37 A credit transaction made through the ACH system to a savings account using the CTP payment format.
    ipmc_ACH_SavingsCorporateTradePaymentDebit,                //38 A debit transaction made through the ACH system to a savings account using the CTP payment format.
    ipmc_ACH_SavingsCorporateTradeExchangeCredit,              //39 A credit transaction made through the ACH system to a savings account using the CTX payment format.
    ipmc_ACH_SavingsCorporateTradeExchangeDebit,               //40 A debit transaction made through the ACH system to a savings account using the CTX payment format.
    ipmc_ACH_SavingsCashConcentrationDisbursementPlusCredit,   //41 A credit transaction made through the ACH system to a savings account using the CCD+ payment format.
    ipmc_PaymentToBankAccount,                                 //42 Payment by an arrangement for settling debts that is operated by the Post Office.
    ipmc_ACH_SavingsCashConcentrationDisbursementPlusDebit,    //43 A debit transaction made through the ACH system to a savings account using the CCD+ payment format.
    ipmc_AcceptedBillOfExchange,                               //44 Bill drawn by the Crdt on the debtor and accepted by the debtor.
    ipmc_ReferencedHomeBankingCreditTransfer,                  //45 A referenced credit transfer initiated through home-banking.
    ipmc_InterbankDebitTransfer,                               //46 A debit transfer via interbank means.
    ipmc_HomeBankingDebitTransfer,                             //47 A debit transfer initiated through home-banking.
    ipmc_BankCard,                                             //48 Payment by means of a card issued by a bank or other financial institution.
    ipmc_DirectDebit,                                          //49 The amount is to be, or has been, directly debited to the customer's bank account.
    ipmc_PaymentByPostgiro,                                    //50 A method for the transmission of funds through the postal system rather than through the banking system.
    ipmc_FR_Norme_6_97,                                        //51 A French standard procedure that allows a debtor to pay an amount due to a Crdt. The Crdt will forward it to its bank, which will collect the money on the bank account of the debtor.
    ipmc_UrgentCommercialPayment,                              //52 Payment order which requires guaranteed processing by the most appropriate means to ensure it occurs on the requested execution date, provided that it is issued to the ordered bank before the agreed cut-off time.
    ipmc_UrgentTreasuryPayment,                                //53 Payment order or transfer which must be executed, by the most appropriate means, as urgently as possible and before urgent commercial payments.
    ipmc_CreditCard,           //54 Kreditkarte
    ipmc_DebitCard,                                            //55 Payment made by means of debit card.
    ipmc_Bankgiro,                                             //56 Payment will be, or has been, made by bankgiro.
    ipmc_StandingAgreement,                                    //57 The payment means have been previously agreed between seller and buyer and thus are not stated again.
    ipmc_SEPACreditTransfer,   //58 Ueberweisung (SEPA)
    ipmc_SEPADirectDebit,      //59 Lastschrift (SEPA)
    ipmc_PromissoryNote,                                       //60 Payment by an unconditional promise in writing made by one person to another, signed by the maker, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_PromissoryNoteSignedByDebtor,                         //61 Payment by an unconditional promise in writing made by the debtor to another person, signed by the debtor, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_PromissoryNoteSignedByDebtorEndorsedByBank,           //62 Payment by an unconditional promise in writing made by the debtor to another person, signed by the debtor and endorsed by a bank, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_PromissoryNoteSignedByDebtorEndorsedByThirdParty,     //63 Payment by an unconditional promise in writing made by the debtor to another person, signed by the debtor and endorsed by a third party, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_PromissoryNoteSignedByBank,                           //64 Payment by an unconditional promise in writing made by the bank to another person, signed by the bank, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_PromissoryNoteSignedByBankEndorsedByAnotherBank,      //65 Payment by an unconditional promise in writing made by the bank to another person, signed by the bank and endorsed by another bank, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_PromissoryNoteSignedByThirdParty,                     //66 Payment by an unconditional promise in writing made by a third party to another person, signed by the third party, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_PromissoryNoteSignedByThirdPartyEndorsedByBank,       //67 Payment by an unconditional promise in writing made by a third party to another person, signed by the third party and endorsed by a bank, engaging to pay on demand or at a fixed or determinable future time a sum certain in money, to order or to bearer.
    ipmc_OnlinePaymentService, //68 Online Payment Service PayPal, etc.
    ipmc_TransferAdvice,                                       //69 Transfer of an amount of money in the books of the account servicer. An advice should be sent back to the account owner.
    ipmc_BillDrawnByCrdtOnDebtor,                              //70 Bill drawn by the Crdt on the debtor.
    ipmc_BillDrawnByCrdtOnBank,                                //74 Bill drawn by the Crdt on a bank.
    ipmc_BillDrawnByCrdtEndorsedByAnotherBank,                 //75 Bill drawn by the Crdt, endorsed by another bank.
    ipmc_BillDrawnByCrdtOnBankEndorsedByThirdParty,            //76 Bill drawn by the Crdt on a bank and endorsed by a third party.
    ipmc_BillDrawnByCrdtOnThirdParty,                          //77 Bill drawn by the Crdt on a third party.
    ipmc_BillDrawnByCrdtOnThirdPartyAcceptedAndEndorsedByBank, //78 Bill drawn by Crdt on third party, accepted and endorsed by bank.
    ipmc_NotTransferableBankersDraft,                          //91 Issue a bankers draft not endorsable.
    ipmc_NotTransferableLocalCheque,                           //92 Issue a cheque not endorsable in payment of the funds.
    ipmc_ReferenceGiro,                                        //93 Ordering customer tells the bank to use the payment system 'Reference giro'. Used in the Finnish national banking system.
    ipmc_UrgentGiro,                                           //94 Ordering customer tells the bank to use the bank service 'Urgent Giro' when transferring the payment. Used in Finnish national banking system.
    ipmc_FreeFormatGiro,                                       //95 Ordering customer tells the ordering bank to use the bank service 'Free Format Giro' when transferring the payment. Used in Finnish national banking system.
    ipmc_RequestedMethodForPaymentWasNotUsed,                  //96 If the requested method for payment was or could not be used, this code indicates that.
    ipmc_ClearingBetweenPartners,                              //97 Amounts which two partners owe to each other to be compensated in order to avoid useless payments.
    ipmc_MutuallyDefined       //ZZZ Gegenseitig definiert (PayPal, etc.)
  );

  TInvoicePaymentTermsType = (iptt_None,
                      iptt_Net,
                      iptt_CashDiscount1,
                      iptt_CashDiscount2);

  TInvoiceUnitCode = (iuc_None //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:rec20_1
                      ,iuc_one   //C62 A unit of count defining the number of pieces
                      ,iuc_piece //H87
                      ,iuc_flaterate //Pauschale
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
                      ,iuc_gram
                      ,iuc_kilogram
                      ,iuc_kilometre
                      ,iuc_kilowatt_hour
                      ,iuc_percent
                      ,iuc_packaging //Verpackung
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

  //Der Code  916 "Referenzpapier" wird benutzt, um die Kennung der rechnungsbegr�ndenden Unterlage zu referenzieren. (BT-122)
  //Der Code 50 "Price/sales catalogue response" wird benutzt, um die Ausschreibung oder das Los zu referenzieren. (BT-17)
  //Der Code 130 "Rechnungsdatenblatt" wird benutzt, um eine vom Verk�ufer angegebene Kennung f�r ein Objekt zu referenzieren. (BT-18)
  //https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.1001_4#version
  TInvoiceAttachmentTypeCode = (iatc_None,
                      iatc_50,
                      iatc_130,
                      iatc_916);//Default

  //Entweder externe Referenz oder eingebettetes Objekt
  //Ob man die Daten als Base64 integriert oder separat mitliefert,
  //haengt wahrscheinlich vom Empfaenger ab
  //https://portal3.gefeg.com/projectdata/invoice/deliverables/installed/publishingproject/zugferd%202.1%20-%20facturx%201.0.05/en%2016931%20%E2%80%93%20facturx%201.0.05%20%E2%80%93%20zugferd%202.1%20-%20extended.scm/html/de/021.htm?https://portal3.gefeg.com/projectdata/invoice/deliverables/installed/publishingproject/zugferd%202.1%20-%20facturx%201.0.05/en%2016931%20%E2%80%93%20facturx%201.0.05%20%E2%80%93%20zugferd%202.1%20-%20extended.scm/html/de/02412.htm
  TInvoiceAttachment = class(TObject)
  public
    ID : String;
    DocumentDescription : String;
    Filename : String;
    TypeCode : TInvoiceAttachmentTypeCode;
    AttachmentType : TInvoiceAttachmentType;
    Data : TMemoryStream;         //https://docs.peppol.eu/poacc/billing/3.0/syntax/ubl-invoice/cac-AdditionalDocumentReference/cac-Attachment/cbc-EmbeddedDocumentBinaryObject/
    ExternalReference : String;   // Gemaesss BMF-Schreiben vom 15.10.2024 sollen Links auf rechnungsbegruendende Unterlagen nicht verwendet werden. Sie sollen stattdessen in die XML-Datei eingebettet werden. Dies gilt ab dem 01.01.2025.
  public
    constructor Create(_AttachmentType : TInvoiceAttachmentType);
    destructor Destroy; override;
    procedure EmbedDataFromStream(_Stream : TStream);
    procedure EmbedDataFromFile(const _Filename : String);
    function GetDataAsBase64 : String;
    procedure SetDataFromBase64(const _Val : String);
    function ContainsBinaryObject : Boolean;
  end;

  TInvoiceAttachmentList = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoiceAttachment;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoiceAttachment);
  public
	  function  Extract(Item: TObject): TInvoiceAttachment;
	  function  First: TInvoiceAttachment;
	  function  Last: TInvoiceAttachment;
	  property  Items[Index: TInvoiceListItemType]: TInvoiceAttachment read GetItem write SetItem; default;
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
          idtfcc_K_VATExemptForEEAIntracommunitySupplyOfGoodsAndServices, //	A tax category code indicating the item is VAT exempt due to an intra-community supply in the European Economic Area. Der Code �K� steht in der Hashtag#XRechnung f�r �VAT exempt for EEA intra-community supply of goods and services� � also f�r die Umsatzsteuerbefreiung bei grenz�berschreitenden Lieferungen und Dienstleistungen innerhalb des Europ�ischen Wirtschaftsraums (Hashtag#EWR).
          idtfcc_L_CanaryIslandsGeneralIndirectTax, //	Impuesto General Indirecto Canario (IGIC) is an indirect tax levied on goods and services supplied in the Canary Islands (Spain) by traders and professionals, as well as on import of goods.
          idtfcc_M_TaxForProductionServicesAndImportationInCeutaAndMelilla, //	Impuesto sobre la Produccion, los Servicios y la Importacion (IPSI) is an indirect municipal tax, levied on the production, processing and import of all kinds of movable tangible property, the supply of services and the transfer of immovable property located in the cities of Ceuta and Melilla.
          idtfcc_O_ServicesOutsideScopeOfTax, //	Code specifying that taxes are not applicable to the services.
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

  TInvoiceAllowanceCharges = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoiceAllowanceCharge;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoiceAllowanceCharge);
  public
	  function  Extract(Item: TObject): TInvoiceAllowanceCharge;
	  function  First: TInvoiceAllowanceCharge;
	  function  Last: TInvoiceAllowanceCharge;
	  property  Items[Index: TInvoiceListItemType]: TInvoiceAllowanceCharge read GetItem write SetItem; default;
  public
    function AddAllowanceCharge : TInvoiceAllowanceCharge;
  end;

  TInvoiceLines = class;

  TInvoiceLineItemAttribute = class(TObject)
  public
    Name : String; //BT-160
    Value : String; //BT-161
  end;

  TInvoiceLineItemAttributes = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoiceLineItemAttribute;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoiceLineItemAttribute);
  public
	  function  Extract(Item: TObject): TInvoiceLineItemAttribute;
	  function  First: TInvoiceLineItemAttribute;
	  function  Last: TInvoiceLineItemAttribute;
	  property  Items[Index: TInvoiceListItemType]: TInvoiceLineItemAttribute read GetItem write SetItem; default;
  public
    function  AddItemAttribute : TInvoiceLineItemAttribute;
  end;

  TInvoiceLine = class(TObject) // insgesamt BG-25
  public
    ID : String; //BT-126 Positionsnummer
    GlobalID_EAN_GTIN: String; //BG-31, BT-157 GTIN/EAN
    Note : String; //BT-127 Hinweis
    //BT-128 fehlt, "Objektkennung auf Ebene der Rechnungsposition", vom Verkaeufer vergeben
    Name : String; //BG-31, BT-153 Kurztext
    Description : String; //BG-31, BT-154 Laengere Beschreibung
    Quantity : double; //BT-129 Menge
    UnitCode : TInvoiceUnitCode; //BT-130 Mengeneinheit
    SellersItemIdentification : String; //BG-31, BT-155 Artikelnummer, vom Verkaeufer vergeben
    BuyersItemIdentification : String; //BG-31, BT-156 Artikelkennung, vom Kaeufer vergeben
    OrderLineReference : String; //BT-132 Referenz zur Bestellposition, vom Kaeufer vergeben
    BuyerAccountingReference : String; //BT-133 Buchungsreferenz des Kaeufers f�r die Rechnungsposition, vom Kaeufer vergeben
    TaxPercent : double; //BG-30, BT-152 MwSt
    TaxCategory : TInvoiceDutyTaxFeeCategoryCode; //BG-30, BT-151 MwSt-Einordnung
    // BG-29 Detailinformationen zum (Artikel-)-Preis
    GrossPriceAmount : Currency; //BG-29, BT-148 Brutto-Einzelpreis
    DiscountOnTheGrossPrice : Currency; //BG-29, BT-147 Rabatt auf den Bruttopreis ergibt Nettopreis, nur ein Rabatt moeglich wegen UBL, obwohl CII mehrere erlaubt
    NetPriceAmount : Currency; //BG-29, BT-146 Netto-Einzelpreis
    BaseQuantity : double; //BG-29, BT-149 Preiseinheit
    BaseQuantityUnitCode : TInvoiceUnitCode; //BG-29, BT-150 Preiseinheit Mengeneinheit
    LineAmount : Currency; //BT-131 Gesamtbetrag des Postens ohne MwSt
    AllowanceCharges : TInvoiceAllowanceCharges; // BG-27 (BT-136..BT-140) und BG-28 (BT-141..BT-145)
    InvoiceLinePeriodStartDate : TDate; //BG-26, BT-134 Leistungszeitraum Beginn
    InvoiceLinePeriodEndDate : TDate; //BG-26, BT-135 Leistungszeitraum Ende
    //BG-31, BT-158 fehlt , "Kennung der Artikelklassifizierung", (0..n)
    //BG-31, BT-159 fehlt, "Artikelherkunftsland"
    ItemAttributes : TInvoiceLineItemAttributes; //BG-31:BG-32 (BT-160..BT-161)

    // Extension XRechnung
    SubInvoiceLines : TInvoiceLines;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TInvoiceLines = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoiceLine;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoiceLine);
  public
	  function  Extract(Item: TObject): TInvoiceLine;
	  function  First: TInvoiceLine;
	  function  Last: TInvoiceLine;
	  property  Items[Index: TInvoiceListItemType]: TInvoiceLine read GetItem write SetItem; default;
  public
    function AddInvoiceLine : TInvoiceLine;
  end;

  TInvoiceTaxAmount = class(TObject)
  public
    TaxableAmount : Currency;
    TaxAmount : Currency;
    TaxPercent : double;
    TaxCategory : TInvoiceDutyTaxFeeCategoryCode;
    TaxExemptionReason : String; //sollte gesetzt werden bei TaxCategory = AE,E,O,Z
  public
    constructor Create;
  end;

  TInvoiceTaxAmounts = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoiceTaxAmount;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoiceTaxAmount);
  public
	  function  Extract(Item: TObject): TInvoiceTaxAmount;
	  function  First: TInvoiceTaxAmount;
	  function  Last: TInvoiceTaxAmount;
	  property  Items[Index: TInvoiceListItemType]: TInvoiceTaxAmount read GetItem write SetItem; default;
  public
    function AddTaxAmountIfTaxExists(_TaxPercent : double; _TaxableAmount,_TaxAmount : Currency) : Boolean;
    function AddTaxAmount : TInvoiceTaxAmount;
  end;

  TInvoiceAddress = class(TObject)
  public
    StreetName : String;
    AdditionalStreetName : String;
    City : String;
    PostalZone : String;
    CountrySubentity : String;
    AddressLine : String;
    CountryCode : String;
  end;

  TInvoiceAccountingParty = class(TObject)
  public
    Name : String;
    RegistrationName : String;
    CompanyID : String; //BT-30

    Address : TInvoiceAddress;

    IdentifierSellerBuyer : String; //BT-29 Kreditor-Nr AccountingSupplierParty / Debitor-Nr AccountingCustomerParty
    BankAssignedCreditorIdentifier : String; //Glaeubiger-ID (BT-90)

    VATCompanyID : String;   //BT-31 UStID
    VATCompanyNumber: String;//BT-32 Steuernummer

    ContactName : String;
    ContactTelephone : String;
    ContactElectronicMail : String;
    AdditionalLegalInformationSeller : String; //BT-33 Weitere rechtliche Informationen zum Verkaeufer
    ElectronicAddressSellerBuyer : String; //BT-34, BT-49 Pflicht
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TInvoiceDeliveryInformation = class(TObject)
  public
    Name : String;
    //LocationIdentifier : String; //optional Ein Bezeichner fuer den Ort, an den die Waren geliefert oder an dem die Dienstleistungen erbracht werden.
    Address : TInvoiceAddress;
    ActualDeliveryDate : TDate; //Lieferdatum
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TInvoicePrecedingInvoiceReference = class(TObject)
  public
    ID : String;
    IssueDate : TDate;
  end;

  TInvoicePrecedingInvoiceReferences = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoicePrecedingInvoiceReference;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoicePrecedingInvoiceReference);
  public
	  function  Extract(Item: TObject): TInvoicePrecedingInvoiceReference;
	  function  First: TInvoicePrecedingInvoiceReference;
	  function  Last: TInvoicePrecedingInvoiceReference;
	  property  Items[Index: TInvoiceListItemType]: TInvoicePrecedingInvoiceReference read GetItem write SetItem; default;
  public
    function AddPrecedingInvoiceReference : TInvoicePrecedingInvoiceReference;
    function IndexOfPrecedingInvoiceReference(const _ID : String) : Integer;
  end;

  //Auswahl aus https://www.xrepository.de/details/urn:xoev-de:kosit:codeliste:untdid.4451_4#version
  TInvoiceNoteSubjectCode = (
    insc_None, //Freitext
    insc_AAI,  //Allgemeine Informationen
    insc_AAJ,  //Zusaetzliche Konditionen zum Kauf - Der Verkaeufer bleibt Eigentuemer der Waren bis zur vollstaendigen Erfuellung der Kaufpreisforderung.
    insc_AAK,  //Preiskonditionen Informationen zu den erwarteten bzw. gegebenen Preiskonditionen. Es bestehen Rabatt- oder Bonusvereinbarungen.
    insc_SUR,  //Anmerkungen des Verkaeufers
    insc_REG,  //Regulatorische Informationen
    insc_ABL,  //Rechtliche Informationen
    insc_TXD,  //Informationen zur Steuer
    insc_CUS,  //Zollinformationen
    insc_PMT   //Payment Information B�rgschaften oder Sicherheitseinbehalte
    );

  TInvoiceNote = class(Tobject)
  public
    Content : String;
    SubjectCode : TInvoiceNoteSubjectCode;
  public
    constructor Create;
  end;

  TInvoiceNotes = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoiceNote;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoiceNote);
  public
	  function  Extract(Item: TObject): TInvoiceNote;
	  function  First: TInvoiceNote;
	  function  Last: TInvoiceNote;
	  property  Items[Index: TInvoiceListItemType]: TInvoiceNote read GetItem write SetItem; default;
  public
    function AddNote: TInvoiceNote;
    function NodeContentsAsText : String;
    procedure ReplaceContentWith(_Content : String);
  end;

  TInvoicePaymentType = class(TObject)
  public
    PaymentMeansCode : TInvoicePaymentMeansCode;
    PaymentMeansInformation : String;
    FinancialAccount : String; //sowohl Payee (Ueberweisung 58) als auch Payer (Lastschrift 59) oder CreditCard
    FinancialAccountName : String; //sowohl Payee (Ueberweisung 58) als auch Payer (Lastschrift 59) oder CreditCard Holder
    FinancialInstitutionBranch : String; //BIC sowohl Payee (Ueberweisung 58) als auch Payer (Lastschrift 59)
  public
    constructor Create;
  end;

  TInvoicePaymentTypeList = class(TObjectList)
  protected
    function GetItem(Index: TInvoiceListItemType): TInvoicePaymentType;
    procedure SetItem(Index: TInvoiceListItemType; AItem: TInvoicePaymentType);
  public
	  function  Extract(Item: TObject): TInvoicePaymentType;
	  function  First: TInvoicePaymentType;
	  function  Last: TInvoicePaymentType;
    function  AddPaymentType : TInvoicePaymentType;
	  property  Items[Index: TInvoiceListItemType]: TInvoicePaymentType read GetItem write SetItem; default;
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
    SellerOrderReference : String; //Auftragsnummer der Verkaeufers
    PurchaseOrderReference : String; //Bestellnummer oder Vertragsnummer des Kaeufers
    ProjectReference : String;
    ContractDocumentReference : String;
    DeliveryReceiptNumber : String; //Lieferscheinnummer (Lieferscheindatum fehlt und wuerde nur in ZUGFeRD unterstuetzt)

    AccountingSupplierParty : TInvoiceAccountingParty;
    AccountingCustomerParty : TInvoiceAccountingParty;
    DeliveryInformation : TInvoiceDeliveryInformation;

    PaymentID : String; //Verwendungszweck der Ueberweisung/Lastschrift
    PaymentTypes : TInvoicePaymentTypeList; //Zahlungswege
    PaymentMandateID : String; //Lastschrift (59) Mandatsreferenz BT-89 !!Nur eine pro Rechnung moeglich

    //Infos unter
    //https://www.e-rechnung-bund.de/wp-content/uploads/2023/04/Angabe-Skonto-Upload.pdf
    PaymentTermsType : TInvoicePaymentTermsType;
    PaymentTermNetNote : String;
    PaymentTermCashDiscount1Days : Integer;
    PaymentTermCashDiscount1Percent : double;
    PaymentTermCashDiscount1Base : Currency; //Anderer Betrag als der Rechnungsbetrag
    PaymentTermCashDiscount1ActualAmount : Currency; //Nur ZUGFeRD/Factur-X: Muss immer Basis * Prozent ergeben
    PaymentTermCashDiscount2Days : Integer;
    PaymentTermCashDiscount2Percent : double;
    PaymentTermCashDiscount2Base : Currency; //Anderer Betrag als der Rechnungsbetrag
    PaymentTermCashDiscount2ActualAmount : Currency; //Nur ZUGFeRD/Factur-X: Muss immer Basis * Prozent ergeben

    InvoiceLines : TInvoiceLines;

    Attachments : TInvoiceAttachmentList; //BG-24

    AllowanceCharges : TInvoiceAllowanceCharges; //Nachlaesse, Zuschlaege
    PrecedingInvoiceReferences : TInvoicePrecedingInvoiceReferences;

    TaxAmountTotal : Currency;
    TaxAmountSubtotals : TInvoiceTaxAmounts;

    LineAmount : Currency;
    TaxExclusiveAmount : Currency;
    TaxInclusiveAmount : Currency;
    AllowanceTotalAmount : Currency;
    ChargeTotalAmount : Currency;
    PrepaidAmount : Currency;
    PayableRoundingAmount : Currency; //BT-114
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
  PaymentTypes := TInvoicePaymentTypeList.Create;
  InvoiceLines := TInvoiceLines.Create;
  Attachments := TInvoiceAttachmentList.Create;
  AllowanceCharges := TInvoiceAllowanceCharges.Create;
  PrecedingInvoiceReferences := TInvoicePrecedingInvoiceReferences.Create;
  TaxAmountSubtotals := TInvoiceTaxAmounts.Create;
  Notes := TInvoiceNotes.Create;
  AccountingSupplierParty := TInvoiceAccountingParty.Create;
  AccountingCustomerParty := TInvoiceAccountingParty.Create;
  DeliveryInformation := TInvoiceDeliveryInformation.Create;
  PaymentTermsType := iptt_None;
  Clear;
end;

destructor TInvoice.Destroy;
begin
  if Assigned(PaymentTypes) then begin PaymentTypes.Free; PaymentTypes := nil; end;
  if Assigned(InvoiceLines) then begin InvoiceLines.Free; InvoiceLines := nil; end;
  if Assigned(Attachments) then begin Attachments.Free; Attachments := nil; end;
  if Assigned(AllowanceCharges) then begin AllowanceCharges.Free; AllowanceCharges := nil; end;
  if Assigned(PrecedingInvoiceReferences) then begin PrecedingInvoiceReferences.Free; PrecedingInvoiceReferences := nil; end;
  if Assigned(TaxAmountSubtotals) then begin TaxAmountSubtotals.Free; TaxAmountSubtotals := nil; end;
  if Assigned(Notes) then begin Notes.Free; Notes := nil; end;
  if Assigned(AccountingSupplierParty) then begin AccountingSupplierParty.Free; AccountingSupplierParty := nil; end;
  if Assigned(AccountingCustomerParty) then begin AccountingCustomerParty.Free; AccountingCustomerParty := nil; end;
  if Assigned(DeliveryInformation) then begin DeliveryInformation.Free; DeliveryInformation := nil; end;
  inherited;
end;

procedure TInvoice.Clear;
begin
  InvoiceLines.Clear;
  AllowanceCharges.Clear;
  PrecedingInvoiceReferences.Clear;
  Notes.Clear;
  TaxAmountSubtotals.Clear;
  PaymentTermsType := iptt_None;

  LineAmount := 0;
  TaxExclusiveAmount := 0;
  TaxInclusiveAmount := 0;
  AllowanceTotalAmount := 0;
  ChargeTotalAmount := 0;
  PrepaidAmount := 0;
  PayableRoundingAmount := 0;
  PayableAmount := 0;
end;

{ TInvoiceLines }

function TInvoiceLines.Extract(Item: TObject): TInvoiceLine;
begin Result := TInvoiceLine(inherited Extract(Item)); end;

function TInvoiceLines.First: TInvoiceLine;
begin if Count = 0 then Result := nil else Result := TInvoiceLine(inherited First); end;

function TInvoiceLines.GetItem(Index: TInvoiceListItemType): TInvoiceLine;
begin Result := TInvoiceLine(inherited Items[Index]); end;

function TInvoiceLines.Last: TInvoiceLine;
begin if Count = 0 then Result := nil else Result := TInvoiceLine(inherited Last); end;

procedure TInvoiceLines.SetItem(Index: TInvoiceListItemType; AItem: TInvoiceLine);
begin inherited Items[Index] := AItem; end;

function TInvoiceLines.AddInvoiceLine: TInvoiceLine;
begin
  Result := TInvoiceLine.Create;
  Add(Result);
end;

{ TInvoiceAllowanceCharges }

function TInvoiceAllowanceCharges.Extract(Item: TObject): TInvoiceAllowanceCharge;
begin Result := TInvoiceAllowanceCharge(inherited Extract(Item)); end;

function TInvoiceAllowanceCharges.First: TInvoiceAllowanceCharge;
begin if Count = 0 then Result := nil else Result := TInvoiceAllowanceCharge(inherited First); end;

function TInvoiceAllowanceCharges.GetItem(Index: TInvoiceListItemType): TInvoiceAllowanceCharge;
begin Result := TInvoiceAllowanceCharge(inherited Items[Index]); end;

function TInvoiceAllowanceCharges.Last: TInvoiceAllowanceCharge;
begin if Count = 0 then Result := nil else Result := TInvoiceAllowanceCharge(inherited Last); end;

procedure TInvoiceAllowanceCharges.SetItem(Index: TInvoiceListItemType; AItem: TInvoiceAllowanceCharge);
begin inherited Items[Index] := AItem; end;

function TInvoiceAllowanceCharges.AddAllowanceCharge: TInvoiceAllowanceCharge;
begin
  Result := TInvoiceAllowanceCharge.Create;
  Add(Result);
end;

{ TInvoicePrecedingInvoiceReferences }

function TInvoicePrecedingInvoiceReferences.Extract(Item: TObject): TInvoicePrecedingInvoiceReference;
begin Result := TInvoicePrecedingInvoiceReference(inherited Extract(Item)); end;

function TInvoicePrecedingInvoiceReferences.First: TInvoicePrecedingInvoiceReference;
begin if Count = 0 then Result := nil else Result := TInvoicePrecedingInvoiceReference(inherited First); end;

function TInvoicePrecedingInvoiceReferences.GetItem(Index: TInvoiceListItemType): TInvoicePrecedingInvoiceReference;
begin Result := TInvoicePrecedingInvoiceReference(inherited Items[Index]); end;

function TInvoicePrecedingInvoiceReferences.Last: TInvoicePrecedingInvoiceReference;
begin if Count = 0 then Result := nil else Result := TInvoicePrecedingInvoiceReference(inherited Last); end;

procedure TInvoicePrecedingInvoiceReferences.SetItem(Index: TInvoiceListItemType; AItem: TInvoicePrecedingInvoiceReference);
begin inherited Items[Index] := AItem; end;

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

{ TInvoiceNote }

constructor TInvoiceNote.Create;
begin
  Content := '';
  SubjectCode := insc_None;
end;

{ TInvoiceNotes }

function TInvoiceNotes.Extract(Item: TObject): TInvoiceNote;
begin Result := TInvoiceNote(inherited Extract(Item)); end;

function TInvoiceNotes.First: TInvoiceNote;
begin if Count = 0 then Result := nil else Result := TInvoiceNote(inherited First); end;

function TInvoiceNotes.GetItem(Index: TInvoiceListItemType): TInvoiceNote;
begin Result := TInvoiceNote(inherited Items[Index]); end;

function TInvoiceNotes.Last: TInvoiceNote;
begin if Count = 0 then Result := nil else Result := TInvoiceNote(inherited Last); end;

procedure TInvoiceNotes.SetItem(Index: TInvoiceListItemType; AItem: TInvoiceNote);
begin inherited Items[Index] := AItem; end;

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

procedure TInvoiceNotes.ReplaceContentWith(_Content: String);
begin
  Clear;
  if _Content <> '' then
    AddNote.Content := _Content;
end;

{ TInvoiceLine }

constructor TInvoiceLine.Create;
begin
  ID := '';
  GlobalID_EAN_GTIN := '';
  Note := '';
  Name := '';
  Description := '';
  Quantity := 0;
  UnitCode := iuc_None;
  SellersItemIdentification := '';
  TaxPercent := 0;
  TaxCategory := idtfcc_None;
  GrossPriceAmount := 0;
  DiscountOnTheGrossPrice := 0;
  NetPriceAmount := 0;
  BaseQuantity := 0;
  BaseQuantityUnitCode := iuc_None;
  LineAmount := 0;
  AllowanceCharges := TInvoiceAllowanceCharges.Create;
  SubInvoiceLines := TInvoiceLines.Create;
  ItemAttributes := TInvoiceLineItemAttributes.Create;
  InvoiceLinePeriodStartDate := 0;
  InvoiceLinePeriodEndDate := 0;
end;

destructor TInvoiceLine.Destroy;
begin
  if Assigned(AllowanceCharges) then begin AllowanceCharges.Free; AllowanceCharges := nil; end;
  if Assigned(SubInvoiceLines) then begin SubInvoiceLines.Free; SubInvoiceLines := nil; end;
  if Assigned(ItemAttributes) then begin ItemAttributes.Free; ItemAttributes := nil; end;
  inherited;
end;

{ TInvoiceUnitCodeHelper }

class function TInvoiceUnitCodeHelper.MapUnitOfMeasure(_UnitOfMeasure: String; out _Success: Boolean;
  _DefaultOnFailure: TInvoiceUnitCode): TInvoiceUnitCode;
begin
  //https://apps.datev.de/help-center/documents/1020477

  Result := _DefaultOnFailure;
  _Success := false;
  _UnitOfMeasure := Trim(_UnitOfMeasure);
  if _UnitOfMeasure = '' then
    exit;
  if SameText(_UnitOfMeasure,'st') or
     SameText(_UnitOfMeasure,'st.') or
     SameText(_UnitOfMeasure,'stk.') or
     SameText(_UnitOfMeasure,'stk') or
     SameText(_UnitOfMeasure,'C62') or
     SameText(_UnitOfMeasure,'stck') then
  begin
    Result := iuc_piece;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'Pauschale') or
     SameText(_UnitOfMeasure,'psch') or
     SameText(_UnitOfMeasure,'psch.') or
     SameText(_UnitOfMeasure,'pschl') or
     SameText(_UnitOfMeasure,'pschl.') then
  begin
    Result := iuc_flaterate;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'mal') then
  begin
    Result := iuc_one;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'std') or
     SameText(_UnitOfMeasure,'std.') or
     SameText(_UnitOfMeasure,'h') then
  begin
    Result := iuc_hour;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'tag') or SameText(_UnitOfMeasure,'tage') then
  begin
    Result := iuc_day;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'monat') then
  begin
    Result := iuc_month;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'%') then
  begin
    Result := iuc_percent;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'woche') then
  begin
    Result := iuc_week;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'g') or
     SameText(_UnitOfMeasure,'Gramm') then
  begin
    Result := iuc_gram;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'kg') then
  begin
    Result := iuc_kilogram;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'km') then
  begin
    Result := iuc_kilometre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'kwh') then
  begin
    Result := iuc_kilowatt_hour;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'t') or SameText(_UnitOfMeasure,'tonne') then
  begin
    Result := iuc_tonne_metric_ton;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'qm') or
     SameText(_UnitOfMeasure,'m2') or
     SameText(_UnitOfMeasure,'m'+#178)
      then
  begin
    Result := iuc_square_metre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'qqm')or
     SameText(_UnitOfMeasure,'cbm') or
     SameText(_UnitOfMeasure,'m3') or
     SameText(_UnitOfMeasure,'m'+#179)
   then
  begin
    Result := iuc_cubic_metre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'m') or
     SameText(_UnitOfMeasure,'lfm') or
     SameText(_UnitOfMeasure,'me') or
     SameText(_UnitOfMeasure,'mtr') then
  begin
    Result := iuc_metre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'mm') then
  begin
    Result := iuc_millimetre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'min') then
  begin
    Result := iuc_minute_unit_of_time;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'sek') then
  begin
    Result := iuc_second_unit_of_time;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'l') then
  begin
    Result := iuc_litre;
    _Success := true;
    exit;
  end;
  if SameText(_UnitOfMeasure,'Paket') or
     SameText(_UnitOfMeasure,'PCK') or
     SameText(_UnitOfMeasure,'Pack') then
  begin
    Result := iuc_packaging;
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
  TypeCode := iatc_916;
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

function TInvoiceAttachmentList.Extract(Item: TObject): TInvoiceAttachment;
begin Result := TInvoiceAttachment(inherited Extract(Item)); end;

function TInvoiceAttachmentList.First: TInvoiceAttachment;
begin if Count = 0 then Result := nil else Result := TInvoiceAttachment(inherited First); end;

function TInvoiceAttachmentList.GetItem(Index: TInvoiceListItemType): TInvoiceAttachment;
begin Result := TInvoiceAttachment(inherited Items[Index]); end;

function TInvoiceAttachmentList.Last: TInvoiceAttachment;
begin if Count = 0 then Result := nil else Result := TInvoiceAttachment(inherited Last); end;

procedure TInvoiceAttachmentList.SetItem(Index: TInvoiceListItemType; AItem: TInvoiceAttachment);
begin inherited Items[Index] := AItem; end;

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
  if SameText(fileExt,'.jpg') or SameText(fileExt,'.jpeg') then
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

{ TInvoiceTaxAmount }

constructor TInvoiceTaxAmount.Create;
begin
  TaxableAmount := 0;
  TaxAmount := 0;
  TaxPercent := 0;
  TaxCategory := idtfcc_None;
  TaxExemptionReason := '';
end;

{ TInvoiceTaxAmounts }

function TInvoiceTaxAmounts.Extract(Item: TObject): TInvoiceTaxAmount;
begin Result := TInvoiceTaxAmount(inherited Extract(Item)); end;

function TInvoiceTaxAmounts.First: TInvoiceTaxAmount;
begin if Count = 0 then Result := nil else Result := TInvoiceTaxAmount(inherited First); end;

function TInvoiceTaxAmounts.GetItem(Index: TInvoiceListItemType): TInvoiceTaxAmount;
begin Result := TInvoiceTaxAmount(inherited Items[Index]); end;

function TInvoiceTaxAmounts.Last: TInvoiceTaxAmount;
begin if Count = 0 then Result := nil else Result := TInvoiceTaxAmount(inherited Last); end;

procedure TInvoiceTaxAmounts.SetItem(Index: TInvoiceListItemType; AItem: TInvoiceTaxAmount);
begin inherited Items[Index] := AItem; end;

function TInvoiceTaxAmounts.AddTaxAmount: TInvoiceTaxAmount;
begin
  Result := TInvoiceTaxAmount.Create;
  Add(Result);
end;

function TInvoiceTaxAmounts.AddTaxAmountIfTaxExists(
  _TaxPercent: double; _TaxableAmount, _TaxAmount: Currency): Boolean;
var
  i : Integer;
begin
  Result := false;
  for i := 0 to Count-1 do
  if Items[i].TaxPercent = _TaxPercent then
  begin
    Items[i].TaxableAmount := Items[i].TaxableAmount + _TaxableAmount;
    Items[i].TaxAmount := Items[i].TaxAmount + _TaxAmount;
    Result := true;
    break;
  end;
end;

{ TInvoiceAccountingParty }

constructor TInvoiceAccountingParty.Create;
begin
  Address := TInvoiceAddress.Create;
end;

destructor TInvoiceAccountingParty.Destroy;
begin
  if Assigned(Address) then begin Address.Free; Address := nil; end;
  inherited;
end;

{ TInvoiceDeliveryInformation }

constructor TInvoiceDeliveryInformation.Create;
begin
  Address := TInvoiceAddress.Create;
end;

destructor TInvoiceDeliveryInformation.Destroy;
begin
  if Assigned(Address) then begin Address.Free; Address := nil; end;
  inherited;
end;

{ TInvoiceLineItemAttributes }

function TInvoiceLineItemAttributes.AddItemAttribute: TInvoiceLineItemAttribute;
begin
  Result := TInvoiceLineItemAttribute.Create;
  Add(Result);
end;

function TInvoiceLineItemAttributes.Extract(Item: TObject): TInvoiceLineItemAttribute;
begin Result := TInvoiceLineItemAttribute(inherited Extract(Item)); end;

function TInvoiceLineItemAttributes.First: TInvoiceLineItemAttribute;
begin if Count = 0 then Result := nil else Result := TInvoiceLineItemAttribute(inherited First); end;

function TInvoiceLineItemAttributes.GetItem(Index: TInvoiceListItemType): TInvoiceLineItemAttribute;
begin Result := TInvoiceLineItemAttribute(inherited Items[Index]); end;

function TInvoiceLineItemAttributes.Last: TInvoiceLineItemAttribute;
begin if Count = 0 then Result := nil else Result := TInvoiceLineItemAttribute(inherited Last); end;

procedure TInvoiceLineItemAttributes.SetItem(Index: TInvoiceListItemType; AItem: TInvoiceLineItemAttribute);
begin inherited Items[Index] := AItem; end;

{ TInvoicePaymentType }

constructor TInvoicePaymentType.Create;
begin
  PaymentMeansCode := ipmc_NotImplemented;
  PaymentMeansInformation := '';
  FinancialAccount := '';
  FinancialAccountName := '';
  FinancialInstitutionBranch := '';
end;

{ TInvoicePaymentTypeList }

function TInvoicePaymentTypeList.Extract(Item: TObject): TInvoicePaymentType;
begin Result := TInvoicePaymentType(inherited Extract(Item)); end;

function TInvoicePaymentTypeList.First: TInvoicePaymentType;
begin if Count = 0 then Result := nil else Result := TInvoicePaymentType(inherited First); end;

function TInvoicePaymentTypeList.GetItem(Index: TInvoiceListItemType): TInvoicePaymentType;
begin Result := TInvoicePaymentType(inherited Items[Index]); end;

function TInvoicePaymentTypeList.Last: TInvoicePaymentType;
begin if Count = 0 then Result := nil else Result := TInvoicePaymentType(inherited Last); end;

procedure TInvoicePaymentTypeList.SetItem(Index: TInvoiceListItemType; AItem: TInvoicePaymentType);
begin inherited Items[Index] := AItem; end;

function TInvoicePaymentTypeList.AddPaymentType : TInvoicePaymentType;
begin
  Result := TInvoicePaymentType.Create;
  Add(Result);
end;

end.

