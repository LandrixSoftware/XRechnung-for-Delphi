program xpathkw_bug;

{$MODE OBJFPC}{$H+}

{
  Standalone reproducer for an out-of-bounds read in LookupXPathKeyword
  (Free Pascal, packages/fcl-xml/src/xpathkw.inc). Present in FPC 3.2.2 and in
  current main.

  Build and run (no external files needed):
    fpc -B xpathkw_bug.lpr && ./xpathkw_bug

  Expected output (both queries find their element):
      ram:SpecifiedLineTradeAgreement  ->  1 hit(s)
      ram:AssociatedDocumentLineDocument  ->  1 hit(s)

  Actual output on aarch64-linux:
      ram:SpecifiedLineTradeAgreement  ->  1 hit(s)
      ram:AssociatedDocumentLineDocument  ->  EAccessViolation: Access violation

  On x86_64-win64 both queries succeed, but the same out-of-bounds read happens;
  the adjacent data merely yields a harmless index there.

  --- Cause ------------------------------------------------------------------

    MaxHash = 55;
    KeywordIndex: array[0..MaxHash-1] of TXPathKeyword;   // valid 0..54
    ...
    if (hash >= 0) and (hash <= MaxHash) then             // admits 55
    begin
      p1 := XPathKeywords[KeywordIndex[hash]];            // reads past the array
      if (ord(p1^) = Len) and ...                         // and dereferences it

  hash is built from the token length plus the 1st and 3rd character. A QName
  token hitting exactly 55 reads one element past KeywordIndex, interprets the
  garbage as a TXPathKeyword and uses it to index the XPathKeywords pointer
  array, then dereferences that pointer.

    'ram:AssociatedDocumentLineDocument'
       len 34 + AssoValues['r'] 17 + AssoValues['m'+1] 4 = 55

  --- Fix --------------------------------------------------------------------

    -  if (hash >= 0) and (hash <= MaxHash) then
    +  if (hash >= 0) and (hash < MaxHash) then

  All 48 real keywords hash to 2..54, so none of them is lost; index 55 can only
  be produced by non-keywords, for which xkNone is the correct result. Verified
  against a patched tree: crash gone, axes (descendant::) and functions
  (local-name(), position(), not()) unaffected.

  Found while porting https://github.com/LandrixSoftware/XRechnung-for-Delphi
  to Free Pascal: five element names of the CII/ZUGFeRD invoice format hit the
  boundary, e.g. ram:AssociatedDocumentLineDocument, which made every CII
  invoice unreadable on aarch64-linux.
}

uses
  SysUtils, Classes, DOM, XMLRead, XPath;

const
  NS = 'urn:example:ns';
  XML_SRC =
    '<?xml version="1.0" encoding="UTF-8"?>' +
    '<Root xmlns:ram="urn:example:ns">' +
    '<ram:AssociatedDocumentLineDocument>hit</ram:AssociatedDocumentLineDocument>' +
    '<ram:SpecifiedLineTradeAgreement>ok</ram:SpecifiedLineTradeAgreement>' +
    '</Root>';

type
  TRes = class(TXPathNSResolver)
  public
    function LookupNamespaceURI(const aPrefix: DOMString): DOMString; override;
  end;

function TRes.LookupNamespaceURI(const aPrefix: DOMString): DOMString;
begin
  if aPrefix = 'ram' then Result := NS else Result := '';
end;

procedure RunQuery(ADoc: TXMLDocument; ARes: TRes; const AExpr: DOMString);
var
  v: TXPathVariable;
  ns: TNodeSet;
begin
  Write('  ', string(AExpr), '  ->  ');
  try
    v := EvaluateXPathExpression(AExpr, ADoc.DocumentElement, ARes);
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Exit;
    end;
  end;
  try
    ns := v.AsNodeSet;
    if (ns = nil) or (ns.Count = 0) then
      WriteLn('0 hits')
    else
      WriteLn(ns.Count, ' hit(s)');
  finally
    v.Release;
  end;
end;

var
  doc: TXMLDocument;
  res: TRes;
  ms: TMemoryStream;
  src: TXMLInputSource;
  parser: TDOMParser;
  raw: RawByteString;
begin
  raw := XML_SRC;
  ms := TMemoryStream.Create;
  ms.Write(raw[1], Length(raw));
  ms.Position := 0;
  src := TXMLInputSource.Create(ms);
  parser := TDOMParser.Create;
  try
    parser.Options.Namespaces := True;
    parser.Parse(src, doc);
  finally
    parser.Free;
    src.Free;
    ms.Free;
  end;

  res := TRes.Create(doc);
  try
    WriteLn('FPC ', {$I %FPCVERSION%}, ' / ', {$I %FPCTARGETCPU%}, '-', {$I %FPCTARGETOS%});
    WriteLn('expected: both queries return 1 hit');
    RunQuery(doc, res, 'ram:SpecifiedLineTradeAgreement');    // hash 52 -> fine
    RunQuery(doc, res, 'ram:AssociatedDocumentLineDocument'); // hash 55 -> OOB read
  finally
    res.Free;
    doc.Free;
  end;
end.
