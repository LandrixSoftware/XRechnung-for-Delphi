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

// Selbsttest ohne Fremddateien fuer den Standard-Security-Handler. Baut fuenf
// PDFs um fest hinterlegte, mit einem unabhaengigen Werkzeug erzeugte
// Testvektoren: RC4 40 Bit, RC4 128 Bit, RC4 ueber Crypt-Filter (die drei
// muessen gelesen werden), dazu ein echtes Benutzerpasswort und AES (die beiden
// muessen abgelehnt werden).
function RunEncryptionSelfTest : Boolean;

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

//==============================================================================
// Selbsttest: Standard-Security-Handler (RC4)
//
// Die Testvektoren unten stammen aus einem unabhaengig geschriebenen Erzeuger
// (Python mit hashlib) und sind hier fest hinterlegt: das /Encrypt-Woerterbuch
// mit /O und /U, die verschluesselten Dateinamen (in Objekt 1 und 4) und die
// verschluesselte Nutzlast (Objekt 5). Das PDF drumherum baut der Test selbst.
//
// Damit prueft der Test genau das, was der Extraktor nachrechnen muss:
// Schluesselableitung aus dem leeren Benutzerpasswort (Algorithmus 2), die
// Pruefung gegen /U (Algorithmus 4 bzw. 5), den Objektschluessel
// (Algorithmus 1) und die Reihenfolge "erst entschluesseln, dann Filterkette".
//==============================================================================

// Wandelt eine Hexfolge in Bytes. Ungerade Laenge oder Fremdzeichen liefern
// ein leeres Ergebnis - im Test faellt das sofort als FAIL auf.
function HexToBytes(const _Hex : AnsiString) : TBytes;
var
  i, n, hi, lo : Integer;

  function Nibble(_C : AnsiChar) : Integer;
  begin
    case _C of
      '0'..'9' : Result := Ord(_C) - Ord('0');
      'A'..'F' : Result := Ord(_C) - Ord('A') + 10;
      'a'..'f' : Result := Ord(_C) - Ord('a') + 10;
    else
      Result := -1;
    end;
  end;

begin
  SetLength(Result, 0);
  n := Length(_Hex);
  if (n = 0) or ((n mod 2) <> 0) then exit;
  SetLength(Result, n div 2);
  for i := 0 to (n div 2) - 1 do
  begin
    hi := Nibble(_Hex[i * 2 + 1]);
    lo := Nibble(_Hex[i * 2 + 2]);
    if (hi < 0) or (lo < 0) then
    begin
      SetLength(Result, 0);
      exit;
    end;
    Result[i] := Byte(hi shl 4) or Byte(lo);
  end;
end;

// Baut ein verschluesseltes Mini-PDF um die vorgegebenen Bausteine. Die
// Objektnummern sind fest, weil sie in den Objektschluessel eingehen: der
// Namensbaum steht in 1, der Filespec in 4, der Anhangsstrom in 5, das
// /Encrypt-Woerterbuch in 6.
function BuildEncryptedPdf(const _EncDict, _NameHex1, _NameHex4,
  _DataHex : AnsiString; _Flate : Boolean) : TBytes;
var
  stm : TMemoryStream;
  ofs : array[1..6] of Integer;
  xref, i : Integer;
  data : TBytes;
  filt : AnsiString;

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
  data := HexToBytes(_DataHex);
  if _Flate then
    filt := ' /Filter /FlateDecode'
  else
    filt := '';

  stm := TMemoryStream.Create;
  try
    Put('%PDF-1.6'#10'%'#$E2#$E3#$CF#$D3#10);

    ofs[1] := stm.Position;
    Put('1 0 obj'#10'<< /Type /Catalog /Pages 2 0 R /Names << /EmbeddedFiles '
      + '<< /Names [ <' + _NameHex1 + '> 4 0 R ] >> >> /AF [ 4 0 R ] >>'#10'endobj'#10);

    ofs[2] := stm.Position;
    Put('2 0 obj'#10'<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>'#10'endobj'#10);

    ofs[3] := stm.Position;
    Put('3 0 obj'#10'<< /Type /Page /Parent 2 0 R /MediaBox [ 0 0 595 842 ] >>'#10'endobj'#10);

    ofs[4] := stm.Position;
    Put('4 0 obj'#10'<< /Type /Filespec /F <' + _NameHex4 + '> /UF <' + _NameHex4
      + '> /EF << /F 5 0 R >> /AFRelationship /Alternative >>'#10'endobj'#10);

    ofs[5] := stm.Position;
    Put(AnsiString(Format('5 0 obj'#10'<< /Type /EmbeddedFile /Subtype /text#2Fxml%s'
      + ' /Length %d >>'#10'stream'#10, [filt, Length(data)])));
    if Length(data) > 0 then
      stm.WriteBuffer(data[0], Length(data));
    Put(#10'endstream'#10'endobj'#10);

    ofs[6] := stm.Position;
    Put('6 0 obj'#10 + _EncDict + #10'endobj'#10);

    xref := stm.Position;
    Put('xref'#10'0 7'#10);
    Put('0000000000 65535 f '#10);
    for i := 1 to 6 do
      Put(Pos10(ofs[i]) + ' 00000 n '#10);
    // /ID[0] geht in die Schluesselableitung ein und muss zu den Testvektoren
    // passen: die Bytes 00 01 .. 0F.
    Put(AnsiString(Format('trailer'#10'<< /Size 7 /Root 1 0 R /Encrypt 6 0 R '
      + '/ID [ <000102030405060708090A0B0C0D0E0F> <000102030405060708090A0B0C0D0E0F> ] >>'#10
      + 'startxref'#10'%d'#10'%%%%EOF'#10, [xref])));

    SetLength(Result, stm.Size);
    if stm.Size > 0 then
      Move(stm.Memory^, Result[0], stm.Size);
  finally
    stm.Free;
  end;
end;

function RunEncryptionSelfTest : Boolean;
const
  // RC4 40 Bit, /V 1 /R 2, unkomprimiert
  ENC_A_DICT : AnsiString =
    '<< /Filter /Standard /V 1 /R 2 /Length 40 /O <4B5950AB07FD'
    + '5763B547C26FD9BF1A066D734250F0A7090069E275C57074F240> /U <'
    + 'FAA956718B8B2683A2F2786FD4535A2E7179E5A3AC3BFDA7BC389558DA'
    + '0AD1DA> /P -1036 >>';
  ENC_A_N1 : AnsiString = '7B61F7B8A2F5AEE1D496D15E';
  ENC_A_N4 : AnsiString = '72712126DC3A2C4111351618';
  ENC_A_DATA : AnsiString =
    '87F03695BE45BF7FB5A6D317AD6D6D25ED4600028785421DBB6752BC5E'
    + 'D3930102EA44C42DBEA794F46A8C8F4082D7FD8EBE9485559D9E45C671'
    + 'B302DF6AAB06CAACAC03B7A25D5474E9DE52835AC31DBE3F7165E8F97B'
    + '685B3C6B006B76D1E4EA796D57D9E6AF278998B2B6EE8876235DA407BC'
    + 'CFAB30C79ED5BA6C70D6E01549A249666C80469B88391F285FDC12475C'
    + '2A4E20E7547870676DC919BBD8C0F8D0D720378F721B3B0AE8E15B7952'
    + 'F55A9D9ECE8E75E11A54CECAD6EF2742800EC57D';

  // RC4 128 Bit, /V 2 /R 3, zusaetzlich FlateDecode
  ENC_B_DICT : AnsiString =
    '<< /Filter /Standard /V 2 /R 3 /Length 128 /O <40BCFC88425'
    + '729C0579B7774CD18E51203FD6E4CD875E4231633E376CF3BC3C1> /U '
    + '<20C59A604A9A936AE0D7CA31F0FC132D0000000000000000000000000'
    + '0000000> /P -1036 >>';
  ENC_B_N1 : AnsiString = '1D2CF7E53C9C488CFA03C09F';
  ENC_B_N4 : AnsiString = 'DA61693108C4CC52511DF04C';
  ENC_B_DATA : AnsiString =
    'DE6BA18D37778CD425CED7CF631D6040C3F685CC87AB4E237645BE35EE'
    + '5CF7AB7BE718847A2B1D0136CEC20AD3A9AA8BCDB19CE519B976698F9D'
    + '3B5EA72063667D620D8DC7CD62DE5E464F07DDE667D0E25754A6731E13'
    + 'AADB94082800284C579C7E1BB0F63A85E9012D9959D841594762AB2C57'
    + 'C43C07B2FA8CFDAAB9BF3381A28AE1093AC61BE4A2875AD1287E8D15D0';

  // RC4 ueber benannten Crypt-Filter, /V 4 /R 4 /CFM /V2, mit FlateDecode
  ENC_C_DICT : AnsiString =
    '<< /Filter /Standard /V 4 /R 4 /Length 128 /CF << /StdCF <'
    + '< /CFM /V2 /Length 16 >> >> /StmF /StdCF /StrF /StdCF /O <'
    + '40BCFC88425729C0579B7774CD18E51203FD6E4CD875E4231633E376CF'
    + '3BC3C1> /U <20C59A604A9A936AE0D7CA31F0FC132D00000000000000'
    + '000000000000000000> /P -1036 >>';
  ENC_C_N1 : AnsiString = '1D2CF7E53C9C488CFA03C09F';
  ENC_C_N4 : AnsiString = 'DA61693108C4CC52511DF04C';
  ENC_C_DATA : AnsiString =
    'DE6BA18D37778CD425CED7CF631D98BF659F149D0408EC3CEF3829299B'
    + '1121C9CA22C73F288062D929D94DFB946B50087982227571C0742CF01F'
    + '98733DA9B884F56662F480A3317162AEEC37F5DE0FF89271087EF9AB8E'
    + '276D9FC23871A1DD9EB2880B774A55FEEC9A1800DF3BF1FF703612DA8C'
    + '5E37428C856DB2BD71E13727D64B71E3C714957B575E1E349ACC130D64'
    + '4AB8AE344D';

  // Echtes Benutzerpasswort ("geheim") - muss abgelehnt werden
  ENC_D_DICT : AnsiString =
    '<< /Filter /Standard /V 2 /R 3 /Length 128 /O <0F66DAB3654'
    + 'F8B3E7DC57757B8A3801A638591000915F48D39899AE095E885B7> /U '
    + '<473ECB70C3593EF2E217BEF3F18BD7620000000000000000000000000'
    + '0000000> /P -1036 >>';
  ENC_D_N1 : AnsiString = '8669F6F85AADFE700070761F';
  ENC_D_N4 : AnsiString = '0C43DCC1892BF8ECAC89530C';
  ENC_D_DATA : AnsiString =
    '15B254921E7D6EAA71B9A712F9886F1DD24FD5D4FAC1B2794D14A95FA3'
    + 'BA326144755370A5969B2E141AE1861F189EED690DFA85FC27754F86B7'
    + '40315A53CD233E86588750F4B4979E8F4A602429200EECF7902A722447'
    + '23AE58B03C32BE42460ADBC5D6873EE34EFA1B507C0829EBD70350670D'
    + '022B93E2A0D5D47B3768EAEB050BB335F44F419C4CE5C19980F4A357CB'
    + 'AD0102F972FE95F0E5A0D889E92C2B786D726F5E33BCF5F4AA2A11B0D3'
    + '30E117BB65973B96BDEB70BC7104D63FA1F528FD9A14';

  // AES (/CFM /AESV2) - muss als nicht unterstuetzt gemeldet werden
  ENC_E_DICT : AnsiString =
    '<< /Filter /Standard /V 4 /R 4 /Length 128 /CF << /StdCF <'
    + '< /CFM /AESV2 /Length 16 >> >> /StmF /StdCF /StrF /StdCF /'
    + 'O <40BCFC88425729C0579B7774CD18E51203FD6E4CD875E4231633E37'
    + '6CF3BC3C1> /U <20C59A604A9A936AE0D7CA31F0FC132D00000000000'
    + '000000000000000000000> /P -1036 >>';
  ENC_E_N1 : AnsiString = '1D2CF7E53C9C488CFA03C09F';
  ENC_E_N4 : AnsiString = 'DA61693108C4CC52511DF04C';
  ENC_E_DATA : AnsiString =
    '9AC8AC6EEA5D798143BD49DE88440DA43F87B9A566CFAF27A333C0C087'
    + '7A51FDD2EF4259FB61FFEB86F0982C3721E6966BE67A56E890B6FCB080'
    + '54753A622E5961FC31970B897FE9314AD2E1943526767E0C8F4D4B6CDA'
    + 'EC8F292657D066A964D64068712250F07147346C10C985D550D8464E4C'
    + '2C3371E3D5190B5379F90A2E93D9FBF766A254712A467B0F7E139E25A2'
    + '1AC7AF37D3CD998D75CD7AEB08EF68CB12D3456CC8D93252F49157E81A'
    + '663E825FAF87D1C14430C5EA2D986B81D0DD475F6904';

var
  ok : Boolean;

  // Baut den Fall, laesst ihn extrahieren und prueft das Ergebnis.
  // _ExpectMarker <> '' : muss gelesen werden und diesen Text enthalten.
  // _ExpectMarker = ''  : muss scheitern, und die Meldung muss _ExpectErr
  //                       enthalten.
  procedure Check(const _Title : String;
    const _Dict, _N1, _N4, _Data : AnsiString; _Flate : Boolean;
    const _ExpectMarker : AnsiString; const _ExpectErr : String);
  var
    pdf, xml : TBytes;
    ms : TMemoryStream;
    info : TXRechnungPdfExtractInfo;
    attName : String;
    got : AnsiString;
    i : Integer;
    good : Boolean;
  begin
    pdf := BuildEncryptedPdf(_Dict, _N1, _N4, _Data, _Flate);
    ms := TMemoryStream.Create;
    try
      if Length(pdf) > 0 then
        ms.WriteBuffer(pdf[0], Length(pdf));
      ms.Position := 0;
      SetLength(xml, 0);
      attName := '';
      good := TXRechnungPdfExtractor.ExtractInvoiceFromStream(ms, xml, attName, info);
    finally
      ms.Free;
    end;

    got := '';
    for i := 0 to Length(xml) - 1 do
      got := got + AnsiChar(xml[i]);

    if _ExpectMarker <> '' then
    begin
      if not good then
      begin
        Writeln('  ', _Title, ': FEHLER - nicht gelesen (', info.Error, ')');
        ok := False;
      end
      else if System.Pos(_ExpectMarker, got) = 0 then
      begin
        Writeln('  ', _Title, ': FEHLER - falscher Inhalt');
        ok := False;
      end
      else if attName <> 'factur-x.xml' then
      begin
        Writeln('  ', _Title, ': FEHLER - Anhangsname "', attName,
                '" statt factur-x.xml (String nicht entschluesselt?)');
        ok := False;
      end
      else if not info.Encrypted then
      begin
        Writeln('  ', _Title, ': FEHLER - Datei nicht als verschluesselt gemeldet');
        ok := False;
      end
      else
        Writeln('  ', _Title, ': ', String(_ExpectMarker), '  (richtig)');
    end
    else
    begin
      if good then
      begin
        Writeln('  ', _Title, ': FEHLER - haette abgelehnt werden muessen');
        ok := False;
      end
      else if System.Pos(_ExpectErr, info.Error) = 0 then
      begin
        Writeln('  ', _Title, ': FEHLER - unerwartete Meldung: ', info.Error);
        ok := False;
      end
      else
        Writeln('  ', _Title, ': abgelehnt - ', info.Error);
    end;
  end;

begin
  Writeln;
  Writeln('--- Selbsttest: verschluesselte PDFs (RC4) ---');
  ok := True;

  Check('RC4 40 Bit  (V1/R2)  ', ENC_A_DICT, ENC_A_N1, ENC_A_N4, ENC_A_DATA,
        False, 'RC4-40-BIT', '');
  Check('RC4 128 Bit (V2/R3)  ', ENC_B_DICT, ENC_B_N1, ENC_B_N4, ENC_B_DATA,
        True, 'RC4-128-BIT', '');
  Check('RC4 Cryptfilter (V4) ', ENC_C_DICT, ENC_C_N1, ENC_C_N4, ENC_C_DATA,
        True, 'RC4-CRYPTFILTER', '');
  Check('Benutzerpasswort     ', ENC_D_DICT, ENC_D_N1, ENC_D_N4, ENC_D_DATA,
        False, '', 'Benutzerpasswort');
  Check('AES (AESV2)          ', ENC_E_DICT, ENC_E_N1, ENC_E_N4, ENC_E_DATA,
        False, '', 'AES');

  Result := ok;
  if Result then
    Writeln('  PASS')
  else
    Writeln('  FAIL');
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
