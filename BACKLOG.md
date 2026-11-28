# Backlog

Bewusst aufgeschobene Punkte. Erledigtes wandert nach `CHANGELOG.md`.

## Delphi6-Sample: Testfaelle zu Issue #100 nicht nachgezogen

- **Datum:** 28.11.2026
- **Herkunft:** Issue #100 / Codex-Review
- **Worum es geht:** Datenmodell und CII-Adapter sind in `Delphi6/` gespiegelt. Die Testfaelle
  in `Delphi6/Samples/XRechnungUnit2TestCases.pas` (innergemeinschaftliche Lieferung mit
  Positionscode und Dokumentnachlass, Paragr13b mit Positionscode) aber nicht.
- **Warum aufgeschoben:** Das Delphi6-Sample weicht ohnehin vom Hauptstand ab, unter anderem
  fehlt dort schon `TaxExemptionReasonCode` in der Steueraufschluesselung.
- **Naechster Schritt:** Beim naechsten Abgleich des Delphi6-Samples mitnehmen.
