# FreePascal-Portierung & Tests

Dieses Verzeichnis enthält den FreePascal-(FPC-)Zweig der XRechnung-Bibliothek
und die Tests, die die **Parität zur Delphi-Version** nachweisen.

## Was wurde portiert

Portabel gemacht (per `{$IFDEF FPC}`, direkt neben dem Delphi-Code) wurden die
Kern-Units im Repo-Root:

| Unit | FPC-Status |
|---|---|
| `intf.Invoice.pas` | voll portiert (Datenmodell, Base64 über FPC-Unit `base64`) |
| `intf.XRechnung.pas` | voll portiert (Schreiben, Lesen, Versionserkennung, Konverter) |
| `intf.XRechnung_3_0.pas` | voll portiert (UBL/UNCEFACT schreiben und lesen) |
| `intf.XRechnungHelper.pas` | XPath-Lesehelfer; unter FPC über den Shim statt über MSXML |

Hinzugekommen:

- `intf.XRechnungXmlShim.pas` (Repo-Root) — bildet die genutzte Teilmenge der
  Delphi-Schnittstellen auf **fcl-xml** ab. Wird nur unter `{$IFDEF FPC}`
  eingebunden:
  - Schreiben: `IXMLNode`/`IXMLDocument`/`NewXMLDocument` → `DOM`/`XMLWrite`
  - Lesen: `IXMLDOMNode`/`IXMLDOMNodeList`/`IXMLDOMDocument2` → `DOM`/`XMLRead`/`XPath`

Der eigentliche Lese- und Schreibcode in `intf.XRechnung_3_0.pas` ist dadurch
**ungeteilt**: Es gibt dort keinen FPC-spezifischen Zweig, beide Compiler
übersetzen denselben Quelltext.

## Die Tests

| Test | Prüft |
|---|---|
| `XRechnungParityTest.lpr` | **Schreibpfad**: erzeugt alle Beispiele mit der FPC-Bibliothek und vergleicht sie kanonisch gegen die Delphi-Golden-Files |
| `XRechnungRoundtripTest.lpr` | **Lesepfad**: liest jedes Golden-File mit der FPC-Bibliothek ein, schreibt die `TInvoice` zurück und vergleicht kanonisch gegen das Original |

Beide nutzen `XRechnungXmlCompare.pas` für den kanonischen Vergleich (DOM-Baum;
Whitespace-, Einrückungs- und Attribut-Reihenfolge-unabhängig) und geben eine
PASS/FAIL-Summary aus (ExitCode 0 = alle 102 Dateien in Ordnung).

Der Roundtrip taugt als Lesetest, weil Lesen+Schreiben über alle 102
Golden-Files verlustfrei ist — unter Delphi ebenso wie unter FPC. Jede
Abweichung ist damit ein echter Lesefehler: ein fehlendes Kindelement heißt
„Feld nicht gelesen“, eine Text- oder Attributabweichung „falsch
interpretiert“. Eigene Referenzdateien braucht der Test deshalb nicht.

Für den Schreibtest wird `Samples\XRechnungUnit2TestCases.pas` mit dem
Delphi-Sample geteilt (34 Testfälle); dessen `uses`-Klausel ist per
`{$IFDEF FPC}` von VCL/Windows befreit, die Anhang-Pfade laufen über
`TInvoiceTestCases.AttachmentBasePath` (Delphi-Verhalten bei leerem Wert
unverändert). `XRechnungGenerateExamples.pas` bildet `TForm1.Button1Click` aus
`Samples\XRechnungUnit1.pas` 1:1 nach.

## Ausführen

```powershell
Tests\FreePascal\Scripts\run-xrechnung-paritytest-win.ps1              # Schreibpfad
Tests\FreePascal\Scripts\run-xrechnung-roundtriptest-win.ps1           # Lesepfad
Tests\FreePascal\Scripts\run-xrechnung-paritytest-win.ps1 -ShowAll     # voller Log
Tests\FreePascal\Scripts\run-xrechnung-paritytest-win.ps1 -Fpc <pfad\fpc.exe>
```

Voraussetzungen: FPC ≥ 3.2.2 inkl. `fcl-xml`. Der Compiler wird über `-Fpc`,
`$env:FPC` oder bekannte fpcupdeluxe-/Lazarus-Pfade gefunden.

**Linux** wird ebenfalls unterstützt (geprüft mit FPC 3.2.2 aarch64-linux,
beide Tests 102/102). Die Runner-Skripte sind PowerShell mit Windows-Pfaden,
daher dort direkt aufrufen:

```bash
REPO=/pfad/zum/repo
fpc -MDelphiUnicode -B "-Fu$REPO" "-Fu$REPO/Samples" "-Fu$REPO/Tests/FreePascal" \
    -FU/tmp/u -FE/tmp -o/tmp/ParityTest "$REPO/Tests/FreePascal/XRechnungParityTest.lpr"
/tmp/ParityTest "$REPO"
```

Beide Plattformen schreiben in dasselbe `out\` — nach einem Linux-Lauf die
Windows-Tests erneut ausführen, sonst stehen dort Fremdartefakte.

Build-Ausgaben landen unter `out\` (git-ignoriert): die EXEn, `*.buildlog`,
`*.runlog` sowie die erzeugten XML unter `out\generated\` (Schreibtest) und
`out\roundtrip\` (Lesetest).

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
- **fcl-xml-Parseroptionen (Lesepfad):** Beim Laden sind
  `Options.Namespaces := True` und `Options.PreserveWhitespace := False`
  zwingend. Ohne das erste bleiben `NamespaceURI`/`LocalName` leer und jede
  präfixbehaftete XPath-Abfrage läuft ins Leere; ohne das zweite landet die
  Einrückung eingerückter Fremdrechnungen in den Feldwerten. `ReadXMLFile`
  setzt beides **nicht** — deshalb nutzt der Shim `TDOMParser` direkt.
- **XPath-Namespaces:** FPCs XPath matcht gegen die Namespace-**URI**, nicht
  gegen das Präfix. Der Shim bindet `cbc/cac/ram/rsm/udt/qdt` fest an ihre URIs
  (`TXRechnungNSResolver`), analog zu MSXML
  `setProperty('SelectionNamespaces', …)`. Eingangsrechnungen dürfen ihre
  Namespaces damit beliebig benennen.
- **`TBase64DecodingStream`** kennt weder `Size` noch `Seek`; `CopyFrom(dec,0)`
  wirft `EStreamError`. `SetDataFromBase64` liest daher blockweise bis zum
  Stromende.
- **FPC-Bug im XPath-Scanner:** `TDomOwner.RewriteQuery` schreibt jedes Präfix
  auf einen zweistelligen Alias um (`ram:` → `ra:`). Das umgeht einen
  Off-by-one in `xpathkw.inc` (`hash <= MaxHash` bei `array[0..MaxHash-1]`),
  der bei Hash 55 hinter der Tabelle liest. Ohne den Workaround crashen fünf
  `ram:`-Elementnamen und damit alle CII-Dateien — unter aarch64-Linux
  reproduzierbar, unter x86_64-Windows unauffällig, aber ebenso undefiniert.
  Details in `CLAUDE.md`.
- **Locale:** Die Testfälle formatieren das Fälligkeitsdatum mit
  `FormatDateTime('dd.mm.yyyy',…)` statt mit `DateToStr`. Letzteres hängt am
  System-Locale und lieferte unter Linux `29-9-26` statt `29.09.2026`, was 37
  Golden-Files scheitern ließ. Die Bibliothek selbst war nie betroffen (feste
  Formate `yyyy-mm-dd` / `yyyymmdd`).
