# Bugreport-Entwurf (englisch, zum Einreichen im FPC-GitLab)

Einreichen unter <https://gitlab.com/freepascal.org/fpc/source/-/issues>.
Anhängen: `xpathkw_bug.lpr` und `xpathkw-maxhash.patch` aus diesem Verzeichnis.

Vor dem Absenden bitte die Zeile „Tested on“ an die eigene Umgebung anpassen und
kurz prüfen, ob es inzwischen ein Issue zum selben Thema gibt.

---

**Title:** fcl-xml: out-of-bounds read in LookupXPathKeyword (xpathkw.inc) — access violation on aarch64

**Summary**

`LookupXPathKeyword` in `packages/fcl-xml/src/xpathkw.inc` can read one element
past the end of `KeywordIndex`. The out-of-range value is then interpreted as a
`TXPathKeyword` and used to index the `XPathKeywords` pointer array, whose result
is dereferenced. On aarch64-linux this raises `EAccessViolation` for perfectly
valid XPath expressions; on x86_64-win64 it goes unnoticed because the adjacent
data happens to produce a harmless index.

**Cause**

```pascal
MaxHash = 55;
KeywordIndex: array[0..MaxHash-1] of TXPathKeyword = (...);   // valid 0..54

function LookupXPathKeyword(p: PWideChar; Len: Integer): TXPathKeyword;
...
  if (hash >= 0) and (hash <= MaxHash) then     // <-- admits hash = 55
  begin
    p1 := XPathKeywords[KeywordIndex[hash]];    // reads past the array
    if (ord(p1^) = Len) and                     // and dereferences the result
```

`hash` is `Len` plus `AssoValues` of the 1st and the 3rd character, so any
identifier token — including a QName such as `prefix:LocalName` — can reach 55.

**Steps to reproduce**

Attached `xpathkw_bug.lpr` is self-contained (XML inline, no external files):

```
fpc -B xpathkw_bug.lpr && ./xpathkw_bug
```

Expected:

```
FPC 3.2.2 / aarch64-Linux
expected: both queries return 1 hit
  ram:SpecifiedLineTradeAgreement  ->  1 hit(s)
  ram:AssociatedDocumentLineDocument  ->  1 hit(s)
```

Actual on aarch64-linux:

```
FPC 3.2.2 / aarch64-Linux
expected: both queries return 1 hit
  ram:SpecifiedLineTradeAgreement  ->  1 hit(s)
  ram:AssociatedDocumentLineDocument  ->  EAccessViolation: Access violation
```

The failing token is `ram:AssociatedDocumentLineDocument`:
`Len 34 + AssoValues['r'] 17 + AssoValues['m'+1] 4 = 55`.

Backtrace (compiled with `-gl -gw3 -O-`, fcl-xml rebuilt from source):

```
EAccessViolation: Access violation
  LookupXPathKeyword,  line 157 of xpathkw.inc
  ParsePathExpr,       line 2034 of xpath.pp
  ParseUnionExpr,      line 1989 of xpath.pp
  ...
  EvaluateXPathExpression, line 2898 of xpath.pp
```

**Proposed fix**

```diff
-  if (hash >= 0) and (hash <= MaxHash) then
+  if (hash >= 0) and (hash < MaxHash) then
```

All 48 real keywords in the table hash to 2..54, so tightening the bound loses
none of them; index 55 can only be produced by non-keyword tokens, for which
`xkNone` is the correct result. I verified this against a patched tree: the
crash is gone, both queries return 1 hit, and axes (`descendant::`) as well as
functions (`local-name()`, `position()`, `not()`) still resolve correctly.

Alternatively the table could be widened to `array[0..MaxHash]`, but that would
add an entry no keyword ever maps to.

**Tested on**

- FPC 3.2.2, aarch64-linux — crashes
- FPC 3.2.2, x86_64-win64 — silently unaffected, same out-of-bounds read
- Code path unchanged in current `main` (checked 2026-08-01)

**Impact**

Found while porting the XRechnung/ZUGFeRD invoice library
<https://github.com/LandrixSoftware/XRechnung-for-Delphi> to Free Pascal. Five
element names of the CII invoice format hit the boundary, among them
`ram:AssociatedDocumentLineDocument` and `ram:ApplicableHeaderTradeAgreement`,
which made every CII invoice unreadable on aarch64-linux. Since any XPath
expression can hit hash 55, this affects fcl-xml users generally — quietly on
x86_64, hard on ARM.
