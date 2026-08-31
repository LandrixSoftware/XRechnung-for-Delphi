# Test der pascalnativen PDF-Anhangsextraktion

Prüft `intf.XRechnungPdfExtract.pas` — die Extraktion der eingebetteten
Rechnungs-XML aus ZUGFeRD-/Factur-X-/XRechnung-PDFs ohne Mustang, PDFtk oder
sonstige externe Werkzeuge.

```powershell
Set-Location "d:\Projekte\src-XRechnung-for-Delphi\Tests\PdfExtract\Scripts"
& .\run-pdfextract-test-win.ps1                    # FreePascal
& .\run-pdfextract-test-win.ps1 -Compiler Delphi   # Delphi
```

## Nutzung in der Bibliothek

Die Fassade nutzt die Unit automatisch — `LoadFromFile` nimmt ein PDF genauso an
wie eine XML-Datei:

```pascal
if TXRechnungInvoiceAdapter.LoadFromFile(invoice, 'rechnung.pdf', err) then
  // gelesen, egal ob PDF oder XML
```

Ausdrücklich, wenn der Name des Anhangs interessiert:

```pascal
if TXRechnungInvoiceAdapter.LoadFromPdfFile(invoice, 'rechnung.pdf', err, attachmentName) then
  ShowMessage('Gelesen aus ' + attachmentName);   // z.B. factur-x.xml
```

Alle Anhänge eines PDFs, auch Beilagen nach BG-24:

```pascal
list := TXRechnungPdfAttachmentList.Create;
try
  if TXRechnungPdfExtractor.ExtractAllFromFile('rechnung.pdf', list, info) then
    for i := 0 to list.Count - 1 do
      Memo1.Lines.Add(Format('%s (%d Bytes, %s)',
        [list[i].FileName, list[i].Size, list[i].RootElement]));
finally
  list.Free;
end;
```

`info.UsedReconstruction` meldet, dass die xref-Kette unbrauchbar war und die
Objekttabelle per Scan rekonstruiert wurde — dann ist das Ergebnis nicht mehr
gegen überschriebene Vorversionen abgesichert. `info.Encrypted` meldet ein
verschlüsseltes PDF (PDF/A-3 verbietet das; es wird abgelehnt, nicht geraten).

## Was geprüft wird

**1. Lauf über einen PDF-Bestand** (Standard: `d:\Projekte\src-ZUGFeRD-for-Delphi`,
488 PDFs, davon 322 mit eingebetteter Rechnung). Gegenprobe ist ein bewusst
naiver Rohdaten-Scanner: alle `stream`-Bereiche durch zlib schicken und das
erste XML nehmen, dessen Wurzelelement nach einer Rechnung aussieht — also
genau der Ansatz, der ohne xref-Auflösung auskommt.

**2. Selbsttest ohne Fremddateien.** Baut ein minimales PDF mit eingebetteter
Rechnung und hängt ein inkrementelles Update an, das denselben Stream durch
eine zweite Fassung ersetzt. Nur die zweite ist laut xref-Kette gültig; die
erste bleibt vollständig in der Datei stehen. Geprüft wird, dass der Extraktor
die gültige Fassung liefert.

## Erwartetes Ergebnis

```
Rechnung gefunden - Parser      : 322
  beide, INHALTLICH ABWEICHEND  :   0
  xref rekonstruiert (Notpfad)  :   0
  Strukturfehler                :   0
PASS: Parser findet in allen Faellen mindestens so viel wie der naive Scanner.

--- Selbsttest: inkrementelles Update ---
  Parser liefert          : ZWEITE-GUELTIG  (richtig)
  Rohdaten-Scanner        : ERSTE-UEBERSCHRIEBEN  (tote Vorversion)
  PASS
```

Delphi und FPC liefern über alle 322 Dateien **byteidentische** Ergebnisse.

## Gegenprobe mit Mustang

Der maßgebliche externe Vergleich läuft über Mustang (PDFBox):

```bash
java -jar Distribution/mustangproject/Mustang-CLI.jar \
     --action extract --source <pdf> --out <xml> --disable-file-logging < leer.txt
```

Über alle 322 Dateien stimmt die Ausgabe dieser Unit **byteweise** mit der von
Mustang überein. Auch im konstruierten Fall mit inkrementellem Update liefern
Mustang und diese Unit dieselbe (neue) Fassung, während der Rohdaten-Scanner
die überschriebene Vorversion zurückgibt.

Zwei Fallstricke beim Batch-Aufruf:

- Mustang braucht **absolute** Pfade für `--out`; bei relativen schreibt es nichts.
- Eine mit Python erzeugte Dateiliste hat unter Windows CRLF. `while read -r`
  behält das `\r`, und Java meldet dann „File ... does not exists", obwohl die
  Datei existiert — die Liste vorher durch `tr -d '\r'` schicken.

## Warum der Umweg über die xref-Tabelle

Rund ein Fünftel der real vorkommenden eRechnungs-PDFs ist per inkrementellem
Update entstanden. Überschriebene Objektversionen bleiben dabei physisch in der
Datei; welche Fassung gilt, sagt ausschließlich die xref-Kette. Auf dem
vorhandenen Bestand liefert ein sorgfältig gebauter Rohdaten-Scanner zwar
dieselben Ergebnisse — dort ersetzt schlicht keine Datei ihre Rechnung. Sobald
das jemand tut, liegen beide Verfahren auseinander, und genau das zeigt der
Selbsttest.

Ein Scanner, der statt des Wurzelelements auf Textbausteine prüft, liegt
übrigens schon auf dem normalen Bestand daneben: das XMP-Metadatenpaket eines
Factur-X-PDFs enthält im Extension-Schema den Text `CrossIndustryDocument` und
wird dann in 186 von 322 Fällen für die Rechnung gehalten.

## Härtung gegen manipulierte Eingaben

Der Parser verarbeitet fremde PDFs, also nicht vertrauenswürdige Daten. Ein
externes Review (Codex) hat die folgenden Punkte aufgedeckt; sie sind behoben:

- **`/Length` wird über `Int64` geprüft.** Ein Wert nahe `MaxInt` ließ die
  Bereichsprüfung `StreamPos + rawLen > Length(FBuf)` überlaufen und führte
  danach zu einem negativen Pufferindex.
- **Filter schneiden nicht mehr erfolgreich ab.** Überschreitet ein Stream
  `XRechnungPdfMaxStreamSize`, schlägt der Filter fehl, statt ein Bruchstück
  als gültiges Ergebnis zu liefern. Dazu kommen ein Gesamtbudget über alle
  Anhänge (`XRechnungPdfMaxTotalSize`) und eine Obergrenze für deren Anzahl.
- **Objektnummern werden plausibilisiert.** Eine xref-Subsection wie
  `8388608 1` in einer 2-KB-Datei erzwang mehrere hundert MB an Tabellen;
  die Grenze richtet sich jetzt nach der Dateigröße.
- **`/DecodeParms` rechnet in `Int64`** und wird gegen die Streamlänge geprüft
  — `/Columns 200000000` forderte sonst zwei Zeilenpuffer à 200 MB an.
- **Der `/XRefStm`-Offset wird geprüft**, bevor er als Lexerposition dient.
- **Der Index eines Typ-2-xref-Eintrags wird ausgewertet.** Stimmt er nicht mit
  der Position im Object-Stream-Header überein, wird das Objekt verworfen.

Nicht übernommen wurde eine strikte Prüfung der Generationsnummer indirekter
Referenzen. Sie wäre formal korrekt (Objekt- und Generationsnummer bilden
zusammen die Identität), doch PDFBox ist hier tolerant — eine strikte Prüfung
könnte Dateien ablehnen, die jeder Viewer liest. Das bliebe zu untersuchen.

## Warum nicht FPCs fcl-pdf?

Seit FPC 3.3.1 gibt es in `fcl-pdf` einen echten PDF-Parser (`fppdfparser.pp`,
Michael Van Canneyt, 2022) mit xref-Tabellen, XRef-Streams, Object Streams und
denselben Stromfiltern. Er ist als Grundlage grundsätzlich brauchbar, kam hier
aber aus drei Gründen nicht in Frage:

- **Er kennt keine Anhänge.** Im gesamten Paket kommen `EmbeddedFile`,
  `Filespec` und `AFRelationship` kein einziges Mal vor. Er liefert den
  Objektgraphen; der Weg Katalog → `/Names/EmbeddedFiles` → Filespec → Stream,
  die Dateinamensregeln der ZUGFeRD-/Factur-X-Profile und die Prüfung des
  XML-Wurzelelements — also genau das, was diese Unit ausmacht — müsste man
  ohnehin selbst darauf bauen.
- **Er ist in FPC 3.2.2 nicht enthalten.** Dort liegt unter `fcl-pdf/src/` nur
  `fppdf.pp`, der Writer. Der Parser existiert erst im Entwicklungszweig.
- **Delphi hat ihn nicht.** Diese Bibliothek bedient beide Compiler aus einer
  Quelle; ein FPC-only-Unterbau hätte den Delphi-Zweig trotzdem nötig gemacht.

Inhaltlich bestätigt er den hier gewählten Weg: Auch `fppdfparser` läuft die
`/Prev`-Kette rückwärts und trägt einen xref-Eintrag nur ein, wenn die
Objektnummer noch unbekannt ist (`if FloadingXRef[aID] = Nil`) — die aktuelle
Fassung gewinnt, wie bei PDFBox und wie hier.

An zwei Stellen ist diese Unit robuster: `FindStreamLength` verlässt sich strikt
auf `/Length` und bricht sonst mit einem Fehler ab (hier: Plausibilitätsprüfung
und Rückfall auf die Suche nach `endstream`), und eine Rekonstruktion bei
defekter xref-Kette gibt es dort nicht. Seine `/Prev`-Schleife hat zudem keinen
Zyklenschutz, läuft bei zyklischem `/Prev` also endlos.

Übernehmenswert wäre umgekehrt sein Diagnosemodell: nummerierte Fehlercodes
plus `DoWarning`/`DoProgress`-Callbacks. Für den hiesigen Zweck — ein XML
herausholen — genügt der `TXRechnungPdfExtractInfo`-Record.

## Dateien

| Datei | Rolle |
|---|---|
| `XRechnungPdfExtractTestCore.pas` | Testkern: Bestandslauf, Kontrollgruppe, Selbsttest |
| `XRechnungPdfExtractTest.dpr` / `.lpr` | Programmrahmen für Delphi bzw. FPC |
| `Scripts/run-pdfextract-test-win.ps1` | Runner (baut und startet) |

Aufruf des Programms direkt:

```
XRechnungPdfExtractTest <Verzeichnis|Dateiliste.txt> [Dumpverzeichnis]
```

Im Dumpverzeichnis landet jedes extrahierte XML als `<Nummer>.xml` und der Fund
der Kontrollgruppe als `brute_<Nummer>.xml` — bei einer Dateiliste entspricht
die Nummer der Zeilennummer, sodass sich die Ergebnisse mit denen eines extern
abgearbeiteten Laufs derselben Liste paarweise vergleichen lassen.
