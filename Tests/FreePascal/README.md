# FreePascal-Portierung & Paritätstest

Dieses Verzeichnis enthält den FreePascal-(FPC-)Zweig der XRechnung-Bibliothek
und den Test, der die **Parität zur Delphi-Version** nachweist.

## Was wurde portiert

Portabel gemacht (per `{$IFDEF FPC}`, direkt neben dem Delphi-Code) wurden die
vier Kern-Units im Repo-Root:

| Unit | FPC-Status |
|---|---|
| `intf.Invoice.pas` | voll portiert (Datenmodell, Base64 über FPC-Unit `base64`) |
| `intf.XRechnung.pas` | Schreibpfad + Konverter portiert; MSXML-Lesepfad unter `{$IFNDEF FPC}` |
| `intf.XRechnung_3_0.pas` | Schreibpfad (UBL/UNCEFACT) portiert; MSXML-Lesepfad unter `{$IFNDEF FPC}` |
| `intf.XRechnungHelper.pas` | reiner MSXML/XPath-Helfer → unter FPC leere Unit |

Hinzugekommen:

- `intf.XRechnungXmlShim.pas` (Repo-Root) — bildet die im Schreibpfad genutzte
  Teilmenge der Delphi-Schnittstellen `IXMLNode`/`IXMLDocument`/`NewXMLDocument`
  auf **fcl-xml** (`DOM`/`XMLWrite`) ab. Wird nur unter `{$IFDEF FPC}` eingebunden.

### Umfang (Stand erste Stufe)

Portiert ist der **Schreibpfad** (XML erzeugen) — genau das, was für die
Erzeugung aller gültigen Beispiele nötig ist. Der **Lesepfad** (XML parsen)
basiert auf MSXML (Windows-COM) und ist unter FPC bewusst ausgeklammert
(`{$IFNDEF FPC}`); ein Ersatz über fcl-xml-XPath wäre der nächste Schritt.

## Der Test

`XRechnungParityTest.lpr`:

1. erzeugt mit der FPC-Bibliothek **alle gültigen XML-Beispiele**
   (`XRechnungGenerateExamples.pas` bildet `TForm1.Button1Click` aus
   `Samples\XRechnungUnit1.pas` 1:1 nach) nach `out\generated\`,
2. vergleicht jede Datei **kanonisch** (DOM-Baum; Whitespace-, Einrückungs- und
   Attribut-Reihenfolge-unabhängig) gegen die Delphi-Golden-Files unter
   `ValidXMLExamples\`,
3. gibt eine PASS/FAIL-Summary aus (ExitCode 0 = alle 102 Dateien identisch).

Geteilt mit dem Delphi-Sample wird `Samples\XRechnungUnit2TestCases.pas`
(34 Testfälle); dessen `uses`-Klausel ist per `{$IFDEF FPC}` von VCL/Windows
befreit, die Anhang-Pfade laufen über `TInvoiceTestCases.AttachmentBasePath`
(Delphi-Verhalten bei leerem Wert unverändert).

## Ausführen

```powershell
Tests\FreePascal\Scripts\run-xrechnung-paritytest-win.ps1            # Build + Test
Tests\FreePascal\Scripts\run-xrechnung-paritytest-win.ps1 -ShowAll   # voller Log
Tests\FreePascal\Scripts\run-xrechnung-paritytest-win.ps1 -Fpc <pfad\fpc.exe>
```

Voraussetzungen: FPC ≥ 3.2.2 (x86_64-win64) inkl. `fcl-xml`. Der Compiler wird
über `-Fpc`, `$env:FPC` oder bekannte fpcupdeluxe-/Lazarus-Pfade gefunden.

Build-Ausgaben landen unter `out\` (git-ignoriert): `XRechnungParityTest.exe`,
`*.buildlog`, `*.runlog` sowie die erzeugten XML unter `out\generated\`.

## Hinweise zur Portierung

- **Quelltext-Codepage:** FPC liest Quelldateien sonst als CP1252. Jede FPC-Unit
  mit Nicht-ASCII-Literalen trägt daher `{$codepage utf8}` (neben
  `{$MODE DELPHIUNICODE}`).
- **UTF-8 ohne Umweg:** fcl-xml schreibt UTF-8-Bytes; der Shim dekodiert diese
  explizit via `UTF8ToString` (nicht über `TStringStream.DataString`, das die
  Bytes als UTF-16 fehlinterpretiert). Dasselbe gilt für die Base64-Kodierung in
  `intf.Invoice.pas`.
- **Zahlenformat:** locale-unabhängig durch `Format('%.Nf',…)` + Ersetzen von
  `,` durch `.` (unverändert aus dem Delphi-Code übernommen).
