# Kennungen der Parteien (BT-29 / BT-46)

Für die Kennung des Verkäufers (BT-29) und des Käufers (BT-46) stehen je Partei zwei Felder zur Verfügung:

```delphi
inv.AccountingSupplierParty.IdentifierSellerBuyer := 'KRED-10001';                //BT-29 Kreditor-Nr.
inv.AccountingSupplierParty.GlobalIdentifierSellerBuyer := '4123450000027';       //BT-29-0 z.B. GLN
inv.AccountingSupplierParty.GlobalIdentifierSellerBuyerSchemeID := '0088';        //BT-29-1 Schema, Default 0088

inv.AccountingCustomerParty.IdentifierSellerBuyer := '20003';                     //BT-46 Debitor-Nr.
inv.AccountingCustomerParty.GlobalIdentifierSellerBuyer := '4260000000004';       //BT-46-0
inv.AccountingCustomerParty.GlobalIdentifierSellerBuyerSchemeID := '0088';        //BT-46-1
```

Der Unterschied ist das Schema: `GlobalIdentifierSellerBuyer` ist für Kennungen aus der ICD-Codeliste gedacht (0088 GLN, 0060 DUNS, 0021 SWIFT, 0177 ODETTE), `IdentifierSellerBuyer` für eine eigene Kreditor- oder Debitor-Nummer ohne Schema.

**In UBL kommt je Partei nur eine Kennung an.** Sind beide Felder gefüllt, wird die GlobalID geschrieben und die Kennung verworfen. Ursache sind die Kardinalitätsregeln `UBL-SR-16` (Käufer) und das UBL-Syntaxbinding unter BG-4-0 (Verkäufer, `VD-Valitool-23`). In CII gilt dasselbe für den Käufer (`CII-SR-450`), lediglich unter ZUGFeRD EXTENDED werden `ram:ID` und `ram:GlobalID` gemeinsam ausgegeben.

Die Gläubiger-ID (BT-90) belegt in UBL ebenfalls eine `cac:PartyIdentification` beim Verkäufer, unterschieden über `schemeID="SEPA"`. Der KoSIT-Validator prüft beide Kennungsarten getrennt – je eine Regel für `[@schemeID = 'SEPA']` und für `[@schemeID != 'SEPA']` – und lässt die Kombination zu. [valitool.org](https://valitool.org/) zählt dagegen alle `cac:PartyIdentification` unter BG-4-0 zusammen und meldet `VD-Valitool-23`, sobald eine Verkäuferkennung und eine Gläubiger-ID zusammentreffen, also bei jeder Lastschrift mit GLN. Die Bibliothek schreibt trotzdem beide: BT-90 hat in UBL keine andere Abbildung, und eine der beiden Angaben zu unterdrücken würde gültige Daten verlieren.

Beim Einlesen greift dieselbe Aufteilung: Eine `cbc:ID` mit `schemeID` (außer SEPA) landet in `GlobalIdentifierSellerBuyer`, eine ohne Schema in `IdentifierSellerBuyer`.

## Hinweis für Bestandsanwendungen

Bis einschließlich Version 3.0.2.7 wurde `IdentifierSellerBuyer` in UBL immer mit `schemeID="0088"` ausgegeben, also als GLN deklariert. Wer dieses Feld tatsächlich für eine GLN genutzt hat, muss den Wert nach `GlobalIdentifierSellerBuyer` umstellen – andernfalls erreicht die GLN den Empfänger nicht mehr. Unter Peppol war die alte Ausgabe ohnehin fehlerhaft, dort wird `schemeID="0088"` gegen die GS1-Prüfziffer validiert (`PEPPOL-COMMON-R040`).
