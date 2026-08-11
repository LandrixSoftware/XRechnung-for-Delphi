# FPC-Bug im XPath-Scanner — Reproduzierer

Status: fixed https://gitlab.com/freepascal.org/fpc/source/-/work_items/41835

Dieses Verzeichnis dokumentiert einen Fehler in **Free Pascal selbst**, nicht in
dieser Bibliothek. Er ist der Grund für `TDomOwner.RewriteQuery` in
`intf.XRechnungXmlShim.pas`.

| Datei | Zweck |
|---|---|
| `xpathkw_bug.lpr` | eigenständiger Reproduzierer, XML inline, ohne Fremddateien |
| `xpathkw-maxhash.patch` | der Ein-Zeichen-Fix für den FPC-Quellbaum |
| `BUGREPORT.md` | fertiger Bugreport-Text für das FPC-GitLab |

Quelltext und Bugreport sind **englisch** — Zielpublikum ist die
FPC-Gemeinschaft, nicht dieses Repo.

## Der Fehler

In `packages/fcl-xml/src/xpathkw.inc` (FPC 3.2.2 **und** aktueller `main`):

```pascal
MaxHash = 55;
KeywordIndex: array[0..MaxHash-1] of TXPathKeyword;   // gültig 0..54
if (hash >= 0) and (hash <= MaxHash) then             // lässt 55 zu
  p1 := XPathKeywords[KeywordIndex[hash]];            // liest hinter dem Array
```

`LookupXPathKeyword` bildet `hash` aus Tokenlänge sowie erstem und **drittem**
Zeichen. Trifft ein Token genau 55, wird hinter der Tabelle gelesen, das
Ergebnis als Enum gedeutet, damit ein Zeigerarray indiziert und dieser Zeiger
dereferenziert.

## Auswirkung auf diese Bibliothek

Betroffen waren fünf Elementnamen mit `ram:`-Präfix und 30 Zeichen Länge:

```
ram:ActualDeliverySupplyChainEvent    ram:DeliveryNoteReferencedDocument
ram:ApplicableHeaderTradeAgreement    ram:EmailURIUniversalCommunication
ram:AssociatedDocumentLineDocument
```

Beispiel: `ram:AssociatedDocumentLineDocument` = Länge 34 + `AssoValues['r']` 17
+ `AssoValues['m'+1]` 4 = 55. Damit war **jede CII-Rechnung** unter
aarch64-Linux unlesbar (60 von 102 Golden-Files), während UBL durchlief — dort
kommt kein `ram:` vor.

Unter x86_64-Windows fiel nichts auf: Dort liefert der Zugriff hinter dem Array
zufällig einen unschädlichen Index. Undefiniert ist er trotzdem.

## Der Workaround

`TDomOwner.RewriteQuery` schreibt vor jeder Abfrage jedes Präfix auf einen
**zweistelligen** Alias um (`.//ram:LineID` → `.//ra:LineID`); der Resolver löst
ihn zurück auf. Bei zweistelligem Präfix ist das dritte Zeichen stets `:`,
worauf die Hashfunktion sofort aussteigt — unabhängig von der Elementnamenlänge.

Der Workaround bleibt auch nach einem Upstream-Fix nötig, solange ältere
FPC-Versionen unterstützt werden.

## Reproduzieren

```bash
fpc -B xpathkw_bug.lpr && ./xpathkw_bug
```

Erwartet werden zwei Treffer; unter aarch64-Linux bricht die zweite Abfrage mit
`EAccessViolation` ab.

Gegenprobe mit gepatchtem FPC — den Quellbaum kopieren, ändern und per `-Fu`
vorziehen, damit `fcl-xml` neu übersetzt wird:

```bash
cp -r <fpc>/source/packages/fcl-xml/src xsrc
sed -i 's/hash <= MaxHash/hash < MaxHash/' xsrc/xpathkw.inc
mkdir -p units
fpc -B -Fuxsrc -FUunits xpathkw_bug.lpr && ./xpathkw_bug
```

Zwei Stolpersteine: `patch` scheitert an dieser Stelle mit
`Hunk FAILED (different line endings)`, weil die FPC-Quellen CRLF verwenden —
daher `sed`, das davon unabhängig ist. Und das `-FU`-Verzeichnis muss vorher
existieren, FPC legt es nicht an. Die Datei `xpathkw-maxhash.patch` ist für den
Bugreport gedacht, nicht zum lokalen Anwenden.

## Einen Fix einreichen

`BUGREPORT.md` enthält den fertigen Text. Einzureichen unter
<https://gitlab.com/freepascal.org/fpc/source/-/issues>, mit `xpathkw_bug.lpr`
und `xpathkw-maxhash.patch` im Anhang.
