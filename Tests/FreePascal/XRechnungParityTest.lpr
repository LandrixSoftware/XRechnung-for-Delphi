{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de

FreePascal-Paritaetstest:
  1. Erzeugt alle gueltigen XML-Beispiele mit der FPC-portierten Bibliothek in
     ein separates Ausgabeverzeichnis (Tests\FreePascal\out\generated\).
  2. Vergleicht jede erzeugte Datei KANONISCH (DOM-Baum, Whitespace- und
     Attribut-Reihenfolge-unabhaengig) gegen die von der Delphi-Version
     erzeugten Golden-Files unter ValidXMLExamples\.
  3. Gibt eine kompakte PASS/FAIL-Summary aus; ExitCode 0 = alle gleich.

Aufruf: XRechnungParityTest.exe [RepoRoot]
  RepoRoot optional; ohne Angabe wird es relativ zur EXE bestimmt
  (Tests\FreePascal\out\ -> 3x hoch).
}

program XRechnungParityTest;

{$MODE DELPHIUNICODE}
{$H+}
{$codepage utf8}

uses
  SysUtils, Classes, DOM, XMLRead,
  XRechnungGenerateExamples;

var
  GFailFirstDiff : String;

function NormName(_Node : TDOMNode) : String;
begin
  Result := String(_Node.NodeName);
end;

// Sortierte Attributliste "name=value" (inkl. xmlns:*), je Zeile ein Attribut.
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
    Result := sl.Text;
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

function CompareNodes(_A, _B : TDOMNode; const _Path : String) : Boolean;
var
  listA, listB : TList;
  i : Integer;
  p : String;
begin
  Result := False;

  if NormName(_A) <> NormName(_B) then
  begin
    GFailFirstDiff := Format('%s: Elementname "%s" <> "%s"',
      [_Path, NormName(_A), NormName(_B)]);
    exit;
  end;

  p := _Path + '/' + NormName(_A);

  if AttribList(_A) <> AttribList(_B) then
  begin
    GFailFirstDiff := Format('%s: Attribute unterschiedlich'#10'  golden: %s'#10'  fpc:    %s',
      [p, StringReplace(AttribList(_A), #10, ' | ', [rfReplaceAll]),
          StringReplace(AttribList(_B), #10, ' | ', [rfReplaceAll])]);
    exit;
  end;

  if DirectText(_A) <> DirectText(_B) then
  begin
    GFailFirstDiff := Format('%s: Text "%s" <> "%s"', [p, DirectText(_A), DirectText(_B)]);
    exit;
  end;

  listA := TList.Create;
  listB := TList.Create;
  try
    ChildElements(_A, listA);
    ChildElements(_B, listB);
    if listA.Count <> listB.Count then
    begin
      GFailFirstDiff := Format('%s: Kindelement-Anzahl %d <> %d', [p, listA.Count, listB.Count]);
      exit;
    end;
    for i := 0 to listA.Count-1 do
      if not CompareNodes(TDOMNode(listA[i]), TDOMNode(listB[i]), p) then
        exit;
    Result := True;
  finally
    listA.Free;
    listB.Free;
  end;
end;

function CompareXmlFiles(const _GoldenFile, _FpcFile : String; out _Diff : String) : Boolean;
var
  docA, docB : TXMLDocument;
begin
  Result := False;
  _Diff := '';
  docA := nil;
  docB := nil;
  try
    try
      ReadXMLFile(docA, _GoldenFile);
    except
      on E:Exception do begin _Diff := 'Golden nicht lesbar: '+E.Message; exit; end;
    end;
    try
      ReadXMLFile(docB, _FpcFile);
    except
      on E:Exception do begin _Diff := 'FPC-XML nicht lesbar: '+E.Message; exit; end;
    end;
    GFailFirstDiff := '';
    Result := CompareNodes(docA.DocumentElement, docB.DocumentElement, '');
    if not Result then
      _Diff := GFailFirstDiff;
  finally
    docA.Free;
    docB.Free;
  end;
end;

var
  RepoRoot, GoldenDir, OutDir, AttachDir : String;
  sr : TSearchRec;
  total, passed, failed, missing : Integer;
  goldenFile, fpcFile, diff : String;
  failNames : TStringList;
begin
  if ParamCount >= 1 then
    RepoRoot := ParamStr(1)
  else
    RepoRoot := ExtractFileDir(ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))));
  RepoRoot := IncludeTrailingPathDelimiter(RepoRoot);

  GoldenDir := RepoRoot + 'ValidXMLExamples' + PathDelim;
  OutDir    := RepoRoot + 'Tests' + PathDelim + 'FreePascal' + PathDelim + 'out' + PathDelim + 'generated' + PathDelim;
  AttachDir := RepoRoot + 'Samples' + PathDelim;

  if not DirectoryExists(GoldenDir) then
  begin
    Writeln('FEHLER: ValidXMLExamples nicht gefunden: ', GoldenDir);
    Halt(2);
  end;

  ForceDirectories(OutDir);
  // altes Generat entfernen
  if FindFirst(OutDir + '*.xml', faAnyFile, sr) = 0 then
  begin
    repeat
      DeleteFile(OutDir + sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  Writeln('>>> Generiere XML mit FPC-Bibliothek nach: ', OutDir);
  try
    GenerateAllExamples(OutDir, AttachDir);
  except
    on E:Exception do
    begin
      Writeln('FEHLER bei der Generierung: ', E.ClassName, ' ', E.Message);
      Halt(3);
    end;
  end;

  total := 0; passed := 0; failed := 0; missing := 0;
  failNames := TStringList.Create;
  try
    if FindFirst(GoldenDir + '*.xml', faAnyFile, sr) = 0 then
    begin
      repeat
        Inc(total);
        goldenFile := GoldenDir + sr.Name;
        fpcFile := OutDir + sr.Name;
        if not FileExists(fpcFile) then
        begin
          Inc(missing);
          failNames.Add('[FEHLT] ' + sr.Name + ' (von FPC nicht erzeugt)');
          Continue;
        end;
        if CompareXmlFiles(goldenFile, fpcFile, diff) then
          Inc(passed)
        else
        begin
          Inc(failed);
          failNames.Add('[DIFF]  ' + sr.Name + ' -> ' + diff);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

    Writeln('');
    if (failed = 0) and (missing = 0) then
    begin
      Writeln(Format('PASS: %d/%d Golden-Files identisch (kanonisch).', [passed, total]));
      Halt(0);
    end
    else
    begin
      Writeln(Format('FAIL: %d von %d identisch, %d abweichend, %d fehlend.',
        [passed, total, failed, missing]));
      Writeln('--- Abweichungen ---');
      Writeln(failNames.Text);
      Halt(1);
    end;
  finally
    failNames.Free;
  end;
end.
