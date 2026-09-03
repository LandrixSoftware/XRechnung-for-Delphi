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

Die Fassade nutzt die Unit — `LoadFromFile` nimmt ein PDF genauso an wie eine
XML-Datei, sobald der letzte Parameter den PDF-Weg erlaubt:

```pascal
if TXRechnungInvoiceAdapter.LoadFromFile(invoice, 'rechnung.pdf', err, true) then
  // gelesen, egal ob PDF oder XML
```

Ohne diesen Schalter (Standard `false`) bleibt es bei der reinen XML-Sicht. Mit
aktivem `ZUGFeRD_Support` steht `_AdditionalContent` davor, der Aufruf lautet
dann `LoadFromFile(invoice, 'rechnung.pdf', err, nil, true)`.

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
verschlüsseltes PDF — auch dann, wenn es erfolgreich gelesen wurde
(siehe [Verschlüsselte PDFs](#verschlüsselte-pdfs)).

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

**3. Selbsttest hybride xref.** Eine hybride Datei nach ISO 32000-1, 7.5.8.4
führt in der klassischen Tabelle jedes Objekt, das in einem Object Stream
liegt, als **frei**; den echten Eintrag liefert erst der über `/XRefStm`
angehängte Querverweisstrom. Geprüft wird beides: dass dieser Eintrag den
freien Platzhalter ersetzen darf — und dass umgekehrt eine ältere Tabelle ein
Objekt, das ein neueres Update freigegeben hat, **nicht** wiederbelebt.

**4. Selbsttest Verschlüsselung.** Fünf PDFs um fest hinterlegte Testvektoren,
die ein unabhängig geschriebener Erzeuger (Python mit `hashlib`) produziert
hat: RC4 40 Bit (`/V 1 /R 2`), RC4 128 Bit (`/V 2 /R 3`, zusätzlich mit
`FlateDecode`) und RC4 über einen benannten Crypt-Filter (`/V 4 /R 4 /CFM /V2`)
müssen gelesen werden; ein echtes Benutzerpasswort und AES müssen abgelehnt
werden. Geprüft wird neben dem Inhalt auch der Anhangsname — der steht als
verschlüsselter String im PDF und belegt damit die String-Entschlüsselung.

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

--- Selbsttest: hybride xref (/XRefStm) ---
  /XRefStm ersetzt freien Platzhalter : HYBRID-XREFSTM  (richtig)
  Freigabe bleibt bestehen            : Anhang korrekt nicht gefunden
  PASS

--- Selbsttest: verschluesselte PDFs (RC4) ---
  RC4 40 Bit  (V1/R2)  : RC4-40-BIT  (richtig)
  RC4 128 Bit (V2/R3)  : RC4-128-BIT  (richtig)
  RC4 Cryptfilter (V4) : RC4-CRYPTFILTER  (richtig)
  Benutzerpasswort     : abgelehnt - PDF ist mit einem Benutzerpasswort geschuetzt
  AES (AESV2)          : abgelehnt - PDF ist AES-verschluesselt (AESV2)
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

## Verschlüsselte PDFs

PDF/A-3 verbietet Verschlüsselung — die Praxis hält sich nicht daran. In einem
Posteingang mit 80 Rechnungen war eine dabei, die mit RC4 128 Bit
(`/V 2 /R 3 /P -1036`) verschlüsselt ist: ein reines **Berechtigungspasswort**
bei leerem Benutzerpasswort, wie es Kanzlei- und Dokumentensysteme routinemäßig
setzen. Jeder Betrachter öffnet solche Dateien ohne Nachfrage, PDFBox liest sie
ebenfalls; nur unsere Unit lehnte sie ab.

Deshalb enthält `intf.XRechnungPdfExtract.pas` jetzt den Standard-Security-Handler
nach ISO 32000-1, 7.6.3 — mit eigener MD5- und RC4-Implementierung, damit Delphi
und FreePascal dieselbe Quelle benutzen:

| Fall | Verhalten |
|---|---|
| `/V 1`, `/V 2` (RC4 40–128 Bit), `/R 2`–`/R 4` | wird entschlüsselt gelesen |
| `/V 4` mit `/CFM /V2` (RC4 über Crypt-Filter) | wird entschlüsselt gelesen |
| `/CFM /Identity` bzw. `/None` | unverschlüsselter Teil, wird gelesen |
| `/CFM /AESV2`, `/AESV3` (AES-128/256) | wird gemeldet, nicht geraten |
| echtes Benutzerpasswort | wird gemeldet, nicht geraten |

Entschlüsselt werden Ströme (vor der Filterkette) und Strings — letztere, weil
sonst der Anhangsname unlesbar wäre. Ausgenommen sind die Stellen, die die
Spezifikation ausnimmt: XRef-Ströme, das `/Encrypt`-Wörterbuch selbst und die
Objekte innerhalb eines Object Streams, dessen Trägerstrom bereits als Ganzes
entschlüsselt wurde. Der Schlüssel steht deshalb vor dem ersten Katalogzugriff
fest — sonst ließe sich ein Object Stream nicht auspacken.

AES nachzurüsten wäre der nächste Schritt, wenn ein Bestand das verlangt: die
Schlüsselableitung ist dieselbe, es fehlt der AES-CBC-Kern (und für `/R 6` die
SHA-2-Ableitung).

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
- **Hybride Dateien werden vollständig aufgelöst.** Die Regel „der zuerst
  gesehene Abschnitt gewinnt" hatte einen blinden Fleck: die klassische
  Tabelle einer hybriden Datei führt Objekte aus Object Streams als *frei*,
  und dieser Platzhalter blockierte den echten Eintrag aus dem `/XRefStm`
  desselben Updates — solche Anhänge blieben unauffindbar. Die Ausnahme gilt
  eng: nur während genau dieses `/XRefStm` und nur von „frei" auf „belegt",
  damit ein älterer Abschnitt kein freigegebenes Objekt wiederbelebt.
  (Aufgefallen beim Quervergleich mit XelPDF, siehe unten.)
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

## Und warum nicht XelPDF?

[XelPDF](https://github.com/Xelitan/PDF-Viewer-exporter-in-pure-Free-Pascal-Lazarus-Delphi)
ist ein PDF-**Viewer** und -Exporter in Pascal: Renderer, Fonts (CFF/TrueType),
JPEG, CCITT, JBIG2, JPEG2000 — allein `PdfParser.pas` hat 209 KB. Anhänge kennt
er nicht: `EmbeddedFile`, `Filespec` und `AFRelationship` kommen im gesamten
Projekt kein einziges Mal vor. Für den hiesigen Zweck bliebe also derselbe
Aufbau ohnehin selbst zu schreiben.

Sein `PdfCrypt.pas` deckt dagegen genau den offenen Punkt ab — RC4, AESV2 und
AESV3 samt SHA-2-Ableitung für `/R 6`, in rund 1000 Zeilen inklusive der
SHA-Units. Übernehmen lässt sich das trotzdem nicht: XelPDF steht unter GPLv3
mit kommerzieller Zweitlizenz, und XRechnung-for-Delphi wird selbst dual
lizenziert (GPLv3 plus Landrix Software Commercial License). Fremder GPL-Code
würde die kommerzielle Schiene unmöglich machen. Als *Referenz* beim
Nachrechnen der Algorithmen ist die Unit dennoch brauchbar.

Ein Detail hat der Quervergleich gebracht: XelPDF lässt in einer hybriden Datei
den `/XRefStm` die freien Platzhalter der klassischen Tabelle ersetzen
(`FXRefOverrideFree`). Genau das fehlte hier — siehe den letzten Punkt unter
[Härtung](#härtung-gegen-manipulierte-eingaben). Bestätigt hat der Vergleich
umgekehrt die Grundregel: auch dort gewinnt die zuerst gesehene Definition,
und die `/Prev`-Kette hat einen Zyklenschutz (`FXRefVisited`) — anders als bei
`fppdfparser`.

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
