# Changelog

Erledigte Backlog-Eintraege, neueste zuerst.

## 28.11.2026 – ZUGFeRD-Lesepfad: VATEX-EU-135-1 fehlte im Enum der ZUGFeRD-Bibliothek

- **Herkunft:** Codex-Review zu Issue #100
- **Worum es ging:** Factur-X 1.09.2 erlaubt den Befreiungscode `VATEX-EU-135-1`. In
  `TZUGFeRDTaxExemptionReasonCodes` fehlte er als einziger Code der Liste; mit
  `{$DEFINE ZUGFeRD_Support}` kam er deshalb als Leerstring an.
- **Erledigt:** In ZUGFeRD-for-Delphi ergaenzt (Commit ad6054f, Enum-Wert, Mapping und Test
  `TestTaxExemptionReasonCodeMapping`). Ueber den Adapter geprueft: Code kommt an Position,
  Nachlass/Zuschlag und Steueraufschluesselung an.
