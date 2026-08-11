{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de

Kanonischer XML-Vergleich fuer die FreePascal-Tests: vergleicht zwei Dokumente
ueber den DOM-Baum statt ueber den Text, also unabhaengig von Einrueckung,
Zeilenumbruechen und Attributreihenfolge. Wird sowohl vom Schreib-Paritaetstest
(XRechnungParityTest) als auch vom Lese-Roundtriptest (XRechnungRoundtripTest)
verwendet.
}

unit XRechnungXmlCompare;

{$MODE DELPHIUNICODE}
{$H+}

interface

uses
  SysUtils, Classes, DOM, XMLRead;

// Vergleicht zwei XML-Dateien kanonisch. Bei Ungleichheit steht in _Diff die
// erste gefundene Abweichung mitsamt Elementpfad.
function CompareXmlFiles(const _FileExpected, _FileActual : String;
  out _Diff : String) : Boolean;

implementation

var
  GFirstDiff : String;

function Shorten(const _Val : String) : String;
begin
  if Length(_Val) > 120 then
    Result := Copy(_Val,1,120) + '...'
  else
    Result := _Val;
end;

// Sortierte Attributliste "name=value" (inkl. xmlns:*), Attribute durch " | "
// getrennt - die Reihenfolge im Dokument ist nicht signifikant.
function AttribList(_El : TDOMNode) : String;
var
  attrs : TDOMNamedNodeMap;
  sl : TStringList;
  i : Integer;
begin
  Result := '';
  if not (_El is TDOMElement) then
    exit;
  attrs := _El.Attributes;
  if attrs = nil then
    exit;
  sl := TStringList.Create;
  try
    for i := 0 to attrs.Length-1 do
      sl.Add(String(attrs[i].NodeName) + '=' + String(attrs[i].NodeValue));
    sl.Sort;
    Result := StringReplace(sl.Text, #10, ' | ', [rfReplaceAll]);
  finally
    sl.Free;
  end;
end;

// Direkter Textinhalt eines Elements (nur unmittelbare Textknoten), getrimmt.
// Container-Elemente liefern '' (nur Einrueckungs-Whitespace), Blaetter den Wert.
function DirectText(_Node : TDOMNode) : String;
var
  ch : TDOMNode;
begin
  Result := '';
  ch := _Node.FirstChild;
  while ch <> nil do
  begin
    if ch.NodeType in [TEXT_NODE, CDATA_SECTION_NODE] then
      Result := Result + String(ch.NodeValue);
    ch := ch.NextSibling;
  end;
  Result := Trim(Result);
end;

// Liste der Kind-ELEMENTE (Whitespace/Kommentare uebersprungen).
procedure ChildElements(_Node : TDOMNode; _List : TList);
var
  ch : TDOMNode;
begin
  ch := _Node.FirstChild;
  while ch <> nil do
  begin
    if ch.NodeType = ELEMENT_NODE then
      _List.Add(ch);
    ch := ch.NextSibling;
  end;
end;

function CompareNodes(_Expected, _Actual : TDOMNode; const _Path : String) : Boolean;
var
  listE, listA : TList;
  i : Integer;
  p, missing : String;
begin
  Result := False;

  if _Expected.NodeName <> _Actual.NodeName then
  begin
    GFirstDiff := Format('%s: Elementname "%s" <> "%s"',
      [_Path, String(_Expected.NodeName), String(_Actual.NodeName)]);
    exit;
  end;

  p := _Path + '/' + String(_Expected.NodeName);

  if AttribList(_Expected) <> AttribList(_Actual) then
  begin
    GFirstDiff := Format('%s: Attribute  erwartet[%s]  erhalten[%s]',
      [p, Shorten(AttribList(_Expected)), Shorten(AttribList(_Actual))]);
    exit;
  end;

  if DirectText(_Expected) <> DirectText(_Actual) then
  begin
    GFirstDiff := Format('%s: Text  erwartet["%s"]  erhalten["%s"]',
      [p, Shorten(DirectText(_Expected)), Shorten(DirectText(_Actual))]);
    exit;
  end;

  listE := TList.Create;
  listA := TList.Create;
  try
    ChildElements(_Expected, listE);
    ChildElements(_Actual, listA);
    if listE.Count <> listA.Count then
    begin
      missing := '';
      if listE.Count > listA.Count then
        for i := listA.Count to listE.Count-1 do
          missing := missing + String(TDOMNode(listE[i]).NodeName) + ' ';
      GFirstDiff := Format('%s: Kindelement-Anzahl erwartet=%d erhalten=%d',
        [p, listE.Count, listA.Count]);
      if missing <> '' then
        GFirstDiff := GFirstDiff + ' (fehlt: ' + Trim(missing) + ')';
      exit;
    end;
    for i := 0 to listE.Count-1 do
      if not CompareNodes(TDOMNode(listE[i]), TDOMNode(listA[i]), p) then
        exit;
    Result := True;
  finally
    listE.Free;
    listA.Free;
  end;
end;

function CompareXmlFiles(const _FileExpected, _FileActual : String;
  out _Diff : String) : Boolean;
var
  docE, docA : TXMLDocument;
begin
  Result := False;
  _Diff := '';
  docE := nil;
  docA := nil;
  try
    try
      ReadXMLFile(docE, _FileExpected);
    except
      on E:Exception do begin _Diff := 'erwartete Datei nicht lesbar: '+E.Message; exit; end;
    end;
    try
      ReadXMLFile(docA, _FileActual);
    except
      on E:Exception do begin _Diff := 'erhaltene Datei nicht lesbar: '+E.Message; exit; end;
    end;
    GFirstDiff := '';
    Result := CompareNodes(docE.DocumentElement, docA.DocumentElement, '');
    if not Result then
      _Diff := GFirstDiff;
  finally
    docE.Free;
    docA.Free;
  end;
end;

end.
