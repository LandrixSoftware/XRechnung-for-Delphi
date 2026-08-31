{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de

Testkern fuer die pascalnative PDF-Anhangsextraktion
(intf.XRechnungPdfExtract). Laeuft ueber einen Verzeichnisbaum voller PDFs und
vergleicht das Ergebnis des Parsers mit einer bewusst naiven Kontrollgruppe:
"alle Streams durch zlib schicken und das erste XML nehmen, das nach einer
Rechnung aussieht".

Die Kontrollgruppe ist genau der Ansatz, der ohne xref-Aufloesung auskommt.
Sie ist hier nicht als Alternative gedacht, sondern um sichtbar zu machen, wo
beide Verfahren auseinanderlaufen - jede Abweichung ist ein Fall, in dem der
Rohdaten-Scanner etwas anderes liefert als der Objektgraph des Dokuments.
}

unit XRechnungPdfExtractTestCore;

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
  {$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  SysUtils, Classes, Math, zstream
  {$ELSE}
  System.SysUtils, System.Classes, System.Math, System.ZLib
  {$ENDIF}
  , intf.XRechnungPdfExtract;

// Durchlaeuft _Root rekursiv. Rueckgabe: True, wenn keine Abweichung zwischen
// Parser und Kontrollgruppe aufgetreten ist, die auf einen Parserfehler
// hindeutet.
function RunPdfExtractTest(const _Root : String) : Boolean;

// Wie RunPdfExtractTest, arbeitet aber eine Textdatei mit einem PDF-Pfad je
// Zeile ab. Die laufende Nummer entspricht der Zeilennummer - damit lassen
// sich die Ergebnisse mit denen eines externen Werkzeugs paarweise
// vergleichen, das dieselbe Liste abgearbeitet hat.
function RunPdfExtractTestFromList(const _ListFile : String) : Boolean;

// Wenn gesetzt, wird jedes extrahierte Rechnungs-XML als <Nummer>.xml dort
// abgelegt (zur Gegenpruefung mit einem unabhaengigen Werkzeug).
procedure SetDumpDir(const _Dir : String);

// Selbsttest ohne Fremddateien: erzeugt ein minimales PDF, haengt ein
// inkrementelles Update an, das den Rechnungsanhang ersetzt, und prueft, dass
// der Extraktor die GUELTIGE (neue) Fassung liefert und nicht die
// ueberschriebene Vorversion, die physisch in der Datei stehen bleibt.
function RunIncrementalUpdateSelfTest : Boolean;

// Erkennungstest: XML mit %PDF- im Fliesstext darf nicht als PDF gelten.
function RunPdfDetectionSelfTest : Boolean;

implementation

{$IFNDEF FPC}
type
  PtrInt = NativeInt;
{$ENDIF}

type
  TStats = record
    Files : Integer;
    WithAttachments : Integer;
    ParserFound : Integer;
    BruteFound : Integer;
    Identical : Integer;
    IdenticalTrimmed : Integer;
    OnlyParser : Integer;
    OnlyBrute : Integer;
    Differing : Integer;
    Reconstructed : Integer;
    Encrypted : Integer;
    Incremental : Integer;
    MultiAttachment : Integer;
    Errors : Integer;
  end;

var
  GStats : TStats;
  GDiffList : TStringList;
  GOnlyBruteList : TStringList;
  GOnlyParserList : TStringList;
  GErrorList : TStringList;
  GNameCount : TStringList;

// Wenn gesetzt, schreibt der Test jedes extrahierte Rechnungs-XML dorthin -
// zur Gegenpruefung mit einem unabhaengigen Werkzeug.
var
  GDumpDir : String = '';
  GDumpIndex : Integer = 0;

procedure SetDumpDir(const _Dir : String);
begin
  GDumpDir := _Dir;
end;

procedure DumpBytes(const _Xml : TBytes; const _Prefix : String);
var
  fs : TFileStream;
  fn : String;
begin
  if GDumpDir = '' then exit;
  fn := IncludeTrailingPathDelimiter(GDumpDir) +
        Format('%s%.3d.xml', [_Prefix, GDumpIndex]);
  try
    fs := TFileStream.Create(fn, fmCreate);
    try
      if Length(_Xml) > 0 then
        fs.WriteBuffer(_Xml[0], Length(_Xml));
    finally
      fs.Free;
    end;
  except
  end;
end;

//------------------------------------------------------------------------------
// Kontrollgruppe: naiver Rohdaten-Scanner ohne xref
//------------------------------------------------------------------------------

function TryInflate(const _Src : TBytes; _Raw : Boolean; out _Dst : TBytes) : Boolean;
var
  srcStm, dstStm : TMemoryStream;
  zs : TStream;
  buf : array[0..32767] of Byte;
  got : Integer;
begin
  Result := False;
  SetLength(_Dst, 0);
  if Length(_Src) = 0 then exit;
  srcStm := TMemoryStream.Create;
  dstStm := TMemoryStream.Create;
  try
    srcStm.WriteBuffer(_Src[0], Length(_Src));
    srcStm.Position := 0;
    zs := nil;
    try
      {$IFDEF FPC}
      zs := Tdecompressionstream.create(srcStm, _Raw);
      {$ELSE}
      if _Raw then
        zs := TZDecompressionStream.Create(srcStm, -15)
      else
        zs := TZDecompressionStream.Create(srcStm, 15);
      {$ENDIF}
      repeat
        got := zs.Read(buf, SizeOf(buf));
        if got > 0 then dstStm.WriteBuffer(buf, got);
        if dstStm.Size > 64 * 1024 * 1024 then break;
      until got <= 0;
    except
    end;
    if zs <> nil then
      try
        zs.Free;
      except
      end;
    if dstStm.Size > 0 then
    begin
      SetLength(_Dst, dstStm.Size);
      Move(dstStm.Memory^, _Dst[0], dstStm.Size);
      Result := True;
    end;
  finally
    dstStm.Free;
    srcStm.Free;
  end;
end;

// Prueft das XML-Wurzelelement statt auf Textbausteine zu suchen. Das ist
// wichtig fuer die Fairness der Kontrollgruppe: das XMP-Metadatenpaket eines
// Factur-X-PDFs enthaelt im Extension-Schema den Text "CrossIndustryDocument"
// und wuerde einer Substring-Suche als Rechnung durchgehen.
function LooksLikeInvoiceXml(const _B : TBytes) : Boolean;
var
  i, n, start : Integer;
  name : AnsiString;
  p : Integer;

  function At(_Pos : Integer; const _S : AnsiString) : Boolean;
  var
    k : Integer;
  begin
    Result := False;
    if _Pos + Length(_S) > n then exit;
    for k := 1 to Length(_S) do
      if _B[_Pos + k - 1] <> Byte(_S[k]) then exit;
    Result := True;
  end;

  function Find(const _S : AnsiString; _From : Integer) : Integer;
  var
    k : Integer;
  begin
    Result := -1;
    for k := _From to n - Length(_S) do
      if At(k, _S) then
      begin
        Result := k;
        exit;
      end;
  end;

begin
  Result := False;
  n := Length(_B);
  if n < 32 then exit;
  i := 0;
  if (n >= 3) and (_B[0] = $EF) and (_B[1] = $BB) and (_B[2] = $BF) then i := 3;

  while i < n do
  begin
    while (i < n) and ((_B[i] = 32) or (_B[i] = 9) or (_B[i] = 10) or (_B[i] = 13)) do
      Inc(i);
    if (i >= n) or (_B[i] <> Byte('<')) then exit;

    if At(i, '<?') then
    begin
      p := Find('?>', i);
      if p < 0 then exit;
      i := p + 2;
      Continue;
    end;
    if At(i, '<!--') then
    begin
      p := Find('-->', i);
      if p < 0 then exit;
      i := p + 3;
      Continue;
    end;
    if At(i, '<!') then
    begin
      p := Find('>', i);
      if p < 0 then exit;
      i := p + 1;
      Continue;
    end;

    Inc(i);
    start := i;
    while (i < n) and (_B[i] <> 32) and (_B[i] <> 9) and (_B[i] <> 10) and
          (_B[i] <> 13) and (_B[i] <> Byte('>')) and (_B[i] <> Byte('/')) do
      Inc(i);
    name := '';
    for p := start to i - 1 do
      name := name + AnsiChar(_B[p]);
    p := System.Pos(AnsiString(':'), name);
    if p > 0 then
      name := Copy(name, p + 1, Length(name) - p);
    Result := (name = 'CrossIndustryInvoice') or (name = 'CrossIndustryDocument') or
              (name = 'Invoice') or (name = 'CreditNote');
    exit;
  end;
end;

// Naiver Scanner: sucht alle stream/endstream-Bereiche, versucht sie zu
// entpacken und liefert das erste gefundene Rechnungs-XML.
function BruteForceExtract(const _FileName : String; out _Xml : TBytes) : Boolean;
var
  fs : TFileStream;
  buf : TBytes;
  n, i, sPos, ePos, rawLen : Integer;
  raw, dec : TBytes;

  function MatchAtLocal(_Pos : Integer; const _S : AnsiString) : Boolean;
  var
    k : Integer;
  begin
    Result := False;
    if _Pos + Length(_S) > Length(buf) then exit;
    for k := 1 to Length(_S) do
      if buf[_Pos + k - 1] <> Byte(_S[k]) then exit;
    Result := True;
  end;

begin
  Result := False;
  SetLength(_Xml, 0);
  try
    fs := TFileStream.Create(_FileName, fmOpenRead or fmShareDenyWrite);
  except
    exit;
  end;
  try
    n := fs.Size;
    if n <= 0 then exit;
    SetLength(buf, n);
    fs.ReadBuffer(buf[0], n);
  finally
    fs.Free;
  end;

  i := 0;
  while i < n - 6 do
  begin
    if (buf[i] = Byte('s')) and MatchAtLocal(i, 'stream') and
       ((i = 0) or not (((buf[i - 1] >= 65) and (buf[i - 1] <= 90)) or
                        ((buf[i - 1] >= 97) and (buf[i - 1] <= 122)))) then
    begin
      sPos := i + 6;
      if (sPos < n) and (buf[sPos] = 13) then Inc(sPos);
      if (sPos < n) and (buf[sPos] = 10) then Inc(sPos);
      ePos := sPos;
      while ePos < n - 9 do
      begin
        if (buf[ePos] = Byte('e')) and MatchAtLocal(ePos, 'endstream') then break;
        Inc(ePos);
      end;
      if ePos >= n - 9 then break;
      rawLen := ePos - sPos;
      if rawLen > 0 then
      begin
        SetLength(raw, rawLen);
        Move(buf[sPos], raw[0], rawLen);
        // Unkomprimiert eingebettet? Zuerst pruefen - ein Inflate-Versuch auf
        // Klartext kann Teilmuell liefern und wuerde den Fund verdecken.
        if LooksLikeInvoiceXml(raw) then
        begin
          _Xml := raw;
          Result := True;
          exit;
        end;
        if TryInflate(raw, False, dec) and LooksLikeInvoiceXml(dec) then
        begin
          _Xml := dec;
          Result := True;
          exit;
        end;
        if TryInflate(raw, True, dec) and LooksLikeInvoiceXml(dec) then
        begin
          _Xml := dec;
          Result := True;
          exit;
        end;
      end;
      i := ePos + 9;
      continue;
    end;
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

function SameBytes(const _A, _B : TBytes) : Boolean;
begin
  Result := (Length(_A) = Length(_B));
  if Result and (Length(_A) > 0) then
    Result := CompareMem(@_A[0], @_B[0], Length(_A));
end;

// Vergleich ohne fuehrenden/abschliessenden Weissraum. Der naive Scanner kann
// die Stromgrenze nur ueber "endstream" schaetzen und nimmt dabei regelmaessig
// das Trenn-EOL mit; das ist ein Artefakt der Kontrollgruppe und keine
// inhaltliche Abweichung.
function SameBytesTrimmed(const _A, _B : TBytes) : Boolean;
var
  a0, a1, b0, b1, i : Integer;

  procedure Bounds(const _X : TBytes; out _Lo, _Hi : Integer);
  begin
    _Lo := 0;
    _Hi := Length(_X) - 1;
    while (_Lo <= _Hi) and ((_X[_Lo] = 32) or (_X[_Lo] = 9) or (_X[_Lo] = 10) or
                            (_X[_Lo] = 13) or (_X[_Lo] = 0)) do Inc(_Lo);
    while (_Hi >= _Lo) and ((_X[_Hi] = 32) or (_X[_Hi] = 9) or (_X[_Hi] = 10) or
                            (_X[_Hi] = 13) or (_X[_Hi] = 0)) do Dec(_Hi);
  end;

begin
  Bounds(_A, a0, a1);
  Bounds(_B, b0, b1);
  Result := (a1 - a0) = (b1 - b0);
  if not Result then exit;
  for i := 0 to a1 - a0 do
    if _A[a0 + i] <> _B[b0 + i] then
    begin
      Result := False;
      exit;
    end;
end;

procedure NoteName(const _Name : String);
var
  i : Integer;
begin
  i := GNameCount.IndexOf(_Name);
  if i < 0 then
    GNameCount.AddObject(_Name, TObject(PtrInt(1)))
  else
    GNameCount.Objects[i] := TObject(PtrInt(PtrInt(GNameCount.Objects[i]) + 1));
end;

procedure TestOneFile(const _FileName, _Display : String);
var
  list : TXRechnungPdfAttachmentList;
  info : TXRechnungPdfExtractInfo;
  idx : Integer;
  parserXml, bruteXml : TBytes;
  parserOk, bruteOk : Boolean;
  parserName : String;
begin
  Inc(GStats.Files);

  list := TXRechnungPdfAttachmentList.Create;
  try
    parserOk := False;
    parserName := '';
    SetLength(parserXml, 0);

    if not TXRechnungPdfExtractor.ExtractAllFromFile(_FileName, list, info) then
    begin
      // Nur echte Strukturfehler zaehlen, nicht "enthaelt keine Anhaenge"
      Inc(GStats.Errors);
      GErrorList.Add(_Display + ' -> ' + info.Error);
    end
    else
    begin
      if info.UsedReconstruction then Inc(GStats.Reconstructed);
      if info.Encrypted then Inc(GStats.Encrypted);
      if info.XrefSections > 1 then Inc(GStats.Incremental);
      if list.Count > 0 then Inc(GStats.WithAttachments);
      if list.Count > 1 then Inc(GStats.MultiAttachment);

      idx := TXRechnungPdfExtractor.FindInvoiceIndex(list);
      if idx >= 0 then
      begin
        parserOk := True;
        parserXml := list[idx].Data;
        parserName := list[idx].FileName;
        NoteName(parserName);
        Inc(GStats.ParserFound);
        DumpBytes(parserXml, '');
      end;
    end;

    bruteOk := BruteForceExtract(_FileName, bruteXml);
    if bruteOk then
    begin
      Inc(GStats.BruteFound);
      DumpBytes(bruteXml, 'brute_');
    end;

    if parserOk and bruteOk then
    begin
      if SameBytes(parserXml, bruteXml) then
        Inc(GStats.Identical)
      else if SameBytesTrimmed(parserXml, bruteXml) then
        Inc(GStats.IdenticalTrimmed)
      else
      begin
        Inc(GStats.Differing);
        GDiffList.Add(Format('%s  [Parser: %s, %d Bytes | BruteForce: %d Bytes]',
          [_Display, parserName, Length(parserXml), Length(bruteXml)]));
      end;
    end
    else if parserOk and not bruteOk then
    begin
      Inc(GStats.OnlyParser);
      GOnlyParserList.Add(Format('%s  [%s, %d Bytes]',
        [_Display, parserName, Length(parserXml)]));
    end
    else if bruteOk and not parserOk then
    begin
      Inc(GStats.OnlyBrute);
      GOnlyBruteList.Add(Format('%s  [%d Bytes, %d Anhang/Anhaenge erkannt]',
        [_Display, Length(bruteXml), list.Count]));
    end;
  finally
    list.Free;
  end;
end;

procedure ScanDir(const _Dir, _RootPrefix : String);
var
  sr : TSearchRec;
  full : String;
begin
  if FindFirst(IncludeTrailingPathDelimiter(_Dir) + '*', faAnyFile, sr) = 0 then
  try
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then Continue;
      full := IncludeTrailingPathDelimiter(_Dir) + sr.Name;
      if (sr.Attr and faDirectory) <> 0 then
        ScanDir(full, _RootPrefix)
      else if LowerCase(ExtractFileExt(sr.Name)) = '.pdf' then
      begin
        Inc(GDumpIndex);
        TestOneFile(full, Copy(full, Length(_RootPrefix) + 1, MaxInt));
      end;
    until FindNext(sr) <> 0;
  finally
    FindClose(sr);
  end;
end;

procedure DumpList(const _Caption : String; _L : TStringList; _Max : Integer);
var
  i : Integer;
begin
  if _L.Count = 0 then exit;
  Writeln;
  Writeln(_Caption, ' (', _L.Count, ')');
  for i := 0 to Min(_L.Count, _Max) - 1 do
    Writeln('  ', _L[i]);
  if _L.Count > _Max then
    Writeln('  ... und ', _L.Count - _Max, ' weitere');
end;

//------------------------------------------------------------------------------
// Selbsttest: inkrementelles Update
//------------------------------------------------------------------------------

// Baut ein minimales, gueltiges PDF mit eingebetteter Rechnung und haengt ein
// inkrementelles Update an, das denselben Stream durch eine zweite Fassung
// ersetzt. Nur die zweite Fassung ist laut xref-Kette gueltig - die erste
// bleibt vollstaendig in der Datei stehen.
function BuildIncrementalUpdatePdf(const _Xml1, _Xml2 : AnsiString) : TBytes;
var
  stm : TMemoryStream;
  ofs : array[1..5] of Integer;
  xref1, xref2, ofs5neu : Integer;

  procedure Put(const _S : AnsiString);
  begin
    if _S <> '' then
      stm.WriteBuffer(_S[1], Length(_S));
  end;

  function Pos10(_V : Integer) : AnsiString;
  begin
    Result := AnsiString(Format('%.10d', [_V]));
  end;

begin
  stm := TMemoryStream.Create;
  try
    Put('%PDF-1.7'#10'%'#$E2#$E3#$CF#$D3#10);

    ofs[1] := stm.Position;
    Put('1 0 obj'#10'<< /Type /Catalog /Pages 2 0 R /Names << /EmbeddedFiles '
      + '<< /Names [(factur-x.xml) 4 0 R] >> >> /AF [4 0 R] >>'#10'endobj'#10);

    ofs[2] := stm.Position;
    Put('2 0 obj'#10'<< /Type /Pages /Kids [3 0 R] /Count 1 >>'#10'endobj'#10);

    ofs[3] := stm.Position;
    Put('3 0 obj'#10'<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] >>'#10'endobj'#10);

    ofs[4] := stm.Position;
    Put('4 0 obj'#10'<< /Type /Filespec /F (factur-x.xml) /UF (factur-x.xml) '
      + '/EF << /F 5 0 R >> /AFRelationship /Alternative >>'#10'endobj'#10);

    ofs[5] := stm.Position;
    Put(AnsiString(Format('5 0 obj'#10'<< /Type /EmbeddedFile /Subtype /text#2Fxml /Length %d >>'#10'stream'#10,
      [Length(_Xml1)])));
    Put(_Xml1);
    Put(#10'endstream'#10'endobj'#10);

    // erste xref-Tabelle
    xref1 := stm.Position;
    Put('xref'#10'0 6'#10);
    Put('0000000000 65535 f '#10);
    Put(Pos10(ofs[1]) + ' 00000 n '#10);
    Put(Pos10(ofs[2]) + ' 00000 n '#10);
    Put(Pos10(ofs[3]) + ' 00000 n '#10);
    Put(Pos10(ofs[4]) + ' 00000 n '#10);
    Put(Pos10(ofs[5]) + ' 00000 n '#10);
    Put(AnsiString(Format('trailer'#10'<< /Size 6 /Root 1 0 R >>'#10'startxref'#10'%d'#10'%%%%EOF'#10,
      [xref1])));

    // --- inkrementelles Update: Objekt 5 wird ersetzt ---
    ofs5neu := stm.Position;
    Put(AnsiString(Format('5 0 obj'#10'<< /Type /EmbeddedFile /Subtype /text#2Fxml /Length %d >>'#10'stream'#10,
      [Length(_Xml2)])));
    Put(_Xml2);
    Put(#10'endstream'#10'endobj'#10);

    xref2 := stm.Position;
    Put('xref'#10'5 1'#10);
    Put(Pos10(ofs5neu) + ' 00000 n '#10);
    Put(AnsiString(Format('trailer'#10'<< /Size 6 /Root 1 0 R /Prev %d >>'#10'startxref'#10'%d'#10'%%%%EOF'#10,
      [xref1, xref2])));

    SetLength(Result, stm.Size);
    if stm.Size > 0 then
      Move(stm.Memory^, Result[0], stm.Size);
  finally
    stm.Free;
  end;
end;

// Erkennungstest: eine XML-Rechnung, die die Zeichenfolge %PDF- im Fliesstext
// fuehrt, darf nicht als PDF durchgehen - sonst wuerde die Fassade sie an den
// PDF-Pfad schicken und das Lesen schluege fehl.
function RunPdfDetectionSelfTest : Boolean;
var
  ms : TMemoryStream;
  xml : AnsiString;
  pdf : AnsiString;
begin
  Writeln;
  Writeln('--- Selbsttest: PDF-Erkennung ---');
  xml := '<?xml version="1.0" encoding="UTF-8"?>'#10 +
         '<Invoice><Note>Dokument basiert nicht auf %PDF-1.7</Note></Invoice>';
  pdf := '%PDF-1.7'#10'1 0 obj'#10'<< >>'#10'endobj'#10;

  Result := True;
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(xml[1], Length(xml));
    ms.Position := 0;
    if TXRechnungPdfExtractor.IsPdfStream(ms) then
    begin
      Writeln('  FEHLER: XML mit %PDF- im Text wurde als PDF erkannt');
      Result := False;
    end
    else
      Writeln('  XML mit %PDF- im Text  : korrekt NICHT als PDF erkannt');
    // Die Position muss unveraendert bleiben
    if ms.Position <> 0 then
    begin
      Writeln('  FEHLER: IsPdfStream hat die Stromposition veraendert (',
              ms.Position, ')');
      Result := False;
    end;

    ms.Clear;
    ms.WriteBuffer(pdf[1], Length(pdf));
    ms.Position := 0;
    if not TXRechnungPdfExtractor.IsPdfStream(ms) then
    begin
      Writeln('  FEHLER: echtes PDF wurde nicht erkannt');
      Result := False;
    end
    else
      Writeln('  echtes PDF             : korrekt erkannt');
  finally
    ms.Free;
  end;

  if Result then
    Writeln('  PASS')
  else
    Writeln('  FAIL');
end;


function RunIncrementalUpdateSelfTest : Boolean;
const
  XML1 : AnsiString =
    '<?xml version="1.0" encoding="UTF-8"?>'#10 +
    '<rsm:CrossIndustryInvoice xmlns:rsm="urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100">' +
    '<Fassung>ERSTE-UEBERSCHRIEBEN</Fassung></rsm:CrossIndustryInvoice>';
  XML2 : AnsiString =
    '<?xml version="1.0" encoding="UTF-8"?>'#10 +
    '<rsm:CrossIndustryInvoice xmlns:rsm="urn:un:unece:uncefact:data:standard:CrossIndustryInvoice:100">' +
    '<Fassung>ZWEITE-GUELTIG</Fassung></rsm:CrossIndustryInvoice>';
var
  pdf : TBytes;
  fn : String;
  fs : TFileStream;
  list : TXRechnungPdfAttachmentList;
  info : TXRechnungPdfExtractInfo;
  idx : Integer;
  got : AnsiString;
  i : Integer;
  bruteXml : TBytes;
  bruteGot : AnsiString;
begin
  Result := False;
  Writeln;
  Writeln('--- Selbsttest: inkrementelles Update ---');

  pdf := BuildIncrementalUpdatePdf(XML1, XML2);
  fn := GetEnvironmentVariable('TEMP');
  if fn = '' then
    fn := GetEnvironmentVariable('TMP');
  if fn = '' then
    fn := GetCurrentDir;
  fn := IncludeTrailingPathDelimiter(fn) + 'xr_incremental_selftest.pdf';
  try
    fs := TFileStream.Create(fn, fmCreate);
    try
      fs.WriteBuffer(pdf[0], Length(pdf));
    finally
      fs.Free;
    end;
  except
    Writeln('  FEHLER: Testdatei nicht schreibbar (', fn, ')');
    exit;
  end;

  list := TXRechnungPdfAttachmentList.Create;
  try
    if not TXRechnungPdfExtractor.ExtractAllFromFile(fn, list, info) then
    begin
      Writeln('  FEHLER: ', info.Error);
      exit;
    end;
    idx := TXRechnungPdfExtractor.FindInvoiceIndex(list);
    if idx < 0 then
    begin
      Writeln('  FEHLER: keine Rechnung gefunden');
      exit;
    end;

    got := '';
    for i := 0 to Length(list[idx].Data) - 1 do
      got := got + AnsiChar(list[idx].Data[i]);

    Writeln('  xref-Abschnitte erkannt : ', info.XrefSections);
    Writeln('  Anhang                  : ', list[idx].FileName,
            ' (', list[idx].Size, ' Bytes)');

    // Ohne diese beiden Zusicherungen beweist der Test nichts ueber die
    // xref-Aufloesung: der Rekonstruktionspfad nimmt ebenfalls die physisch
    // letzte Definition und bestuende denselben Vergleich.
    if info.UsedReconstruction then
    begin
      Writeln('  FEHLER: xref wurde rekonstruiert - der Test wuerde die');
      Writeln('          xref-Aufloesung dann gar nicht pruefen.');
      exit;
    end;
    if info.XrefSections <> 2 then
    begin
      Writeln('  FEHLER: erwartet wurden 2 xref-Abschnitte, erkannt: ',
              info.XrefSections);
      exit;
    end;

    if System.Pos(AnsiString('ZWEITE-GUELTIG'), got) > 0 then
    begin
      Writeln('  Parser liefert          : ZWEITE-GUELTIG  (richtig)');
      Result := True;
    end
    else if System.Pos(AnsiString('ERSTE-UEBERSCHRIEBEN'), got) > 0 then
      Writeln('  Parser liefert          : ERSTE-UEBERSCHRIEBEN  (FALSCH - tote Vorversion!)')
    else
      Writeln('  Parser liefert          : unerwarteten Inhalt');

    // Zum Vergleich: was der naive Rohdaten-Scanner an derselben Datei tut
    if BruteForceExtract(fn, bruteXml) then
    begin
      bruteGot := '';
      for i := 0 to Length(bruteXml) - 1 do
        bruteGot := bruteGot + AnsiChar(bruteXml[i]);
      if System.Pos(AnsiString('ERSTE-UEBERSCHRIEBEN'), bruteGot) > 0 then
        Writeln('  Rohdaten-Scanner        : ERSTE-UEBERSCHRIEBEN  (tote Vorversion)')
      else if System.Pos(AnsiString('ZWEITE-GUELTIG'), bruteGot) > 0 then
        Writeln('  Rohdaten-Scanner        : ZWEITE-GUELTIG')
      else
        Writeln('  Rohdaten-Scanner        : unerwarteter Inhalt');
    end
    else
      Writeln('  Rohdaten-Scanner        : nichts gefunden');
  finally
    list.Free;
    DeleteFile(fn);
  end;

  if Result then
    Writeln('  PASS')
  else
    Writeln('  FAIL');
end;

procedure ResetTest;
begin
  FillChar(GStats, SizeOf(GStats), 0);
  GDumpIndex := 0;
  GDiffList := TStringList.Create;
  GOnlyBruteList := TStringList.Create;
  GOnlyParserList := TStringList.Create;
  GErrorList := TStringList.Create;
  GNameCount := TStringList.Create;
end;

function ReportTest : Boolean;
var
  i : Integer;
begin
  Writeln('=========================================================');
  Writeln('PDF-Dateien gesamt              : ', GStats.Files);
  Writeln('  mit eingebetteten Dateien     : ', GStats.WithAttachments);
  Writeln('  davon mehr als ein Anhang     : ', GStats.MultiAttachment);
  Writeln('  inkrementelle Updates         : ', GStats.Incremental);
  Writeln('  xref rekonstruiert (Notpfad)  : ', GStats.Reconstructed);
  Writeln('  verschluesselt                : ', GStats.Encrypted);
  Writeln('  Strukturfehler                : ', GStats.Errors);
  Writeln('---------------------------------------------------------');
  Writeln('Rechnung gefunden - Parser      : ', GStats.ParserFound);
  Writeln('Rechnung gefunden - BruteForce  : ', GStats.BruteFound);
  Writeln('  beide, Bytes identisch        : ', GStats.Identical);
  Writeln('  beide, gleich bis auf Rand-WS : ', GStats.IdenticalTrimmed);
  Writeln('  beide, INHALTLICH ABWEICHEND  : ', GStats.Differing);
  Writeln('  nur Parser                    : ', GStats.OnlyParser);
  Writeln('  nur BruteForce                : ', GStats.OnlyBrute);
  Writeln('=========================================================');

  if GNameCount.Count > 0 then
  begin
    Writeln;
    Writeln('Gefundene Anhangsnamen:');
    for i := 0 to GNameCount.Count - 1 do
      Writeln(Format('  %-34s %d', [GNameCount[i], PtrInt(GNameCount.Objects[i])]));
  end;

  DumpList('INHALTLICHE ABWEICHUNGEN Parser <> BruteForce:', GDiffList, 25);
  DumpList('Nur der Parser fand eine Rechnung:', GOnlyParserList, 25);
  DumpList('Nur BruteForce fand eine Rechnung:', GOnlyBruteList, 25);
  DumpList('Strukturfehler:', GErrorList, 25);

  Writeln;
  // Der Parser darf nie schlechter sein als der naive Scanner - und wo beide
  // etwas finden, muss es dasselbe sein. Ohne die Differing-Bedingung meldete
  // der Test auch dann PASS, wenn saemtliche Ergebnisse voneinander abwichen.
  Result := (GStats.OnlyBrute = 0) and (GStats.Errors = 0) and
            (GStats.Differing = 0);
  if Result then
    Writeln('PASS: Parser findet in allen Faellen mindestens so viel wie der naive Scanner.')
  else
    Writeln('FAIL: siehe Auflistung oben.');

  GNameCount.Free;
  GErrorList.Free;
  GOnlyParserList.Free;
  GOnlyBruteList.Free;
  GDiffList.Free;
end;

function RunPdfExtractTest(const _Root : String) : Boolean;
var
  root : String;
begin
  ResetTest;
  root := IncludeTrailingPathDelimiter(_Root);
  Writeln('Durchsuche: ', root);
  Writeln;
  ScanDir(ExcludeTrailingPathDelimiter(root), root);
  Result := ReportTest;
end;

function RunPdfExtractTestFromList(const _ListFile : String) : Boolean;
var
  lst : TStringList;
  i : Integer;
  fn : String;
begin
  ResetTest;
  Writeln('Arbeite Liste ab: ', _ListFile);
  Writeln;
  lst := TStringList.Create;
  try
    lst.LoadFromFile(_ListFile);
    for i := 0 to lst.Count - 1 do
    begin
      fn := Trim(lst[i]);
      if fn = '' then Continue;
      // Die laufende Nummer folgt der Zeilennummer, damit die Dumps zu denen
      // eines extern abgearbeiteten Laufs derselben Liste passen.
      GDumpIndex := i + 1;
      if FileExists(fn) then
        TestOneFile(fn, ExtractFileName(fn))
      else
      begin
        Inc(GStats.Errors);
        GErrorList.Add('Nicht gefunden: ' + fn);
      end;
    end;
  finally
    lst.Free;
  end;
  Result := ReportTest;
end;

end.
