{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
}

{ Pascalnative Extraktion der eingebetteten Rechnungs-XML aus einem
  ZUGFeRD-/Factur-X-/XRechnung-PDF (PDF/A-3). Ohne externe Werkzeuge
  (Mustang, PDFtk) und ohne Fremdbibliotheken - laeuft unter Delphi
  wie unter FreePascal.

  Das ist bewusst KEIN vollstaendiger PDF-Reader: es gibt kein Rendering,
  keine Fonts, keine Grafik. Gebraucht wird nur der Weg
     Trailer -> /Root -> /Names/EmbeddedFiles bzw. /AF -> Filespec -> Stream.

  Warum der Umweg ueber die xref-Tabelle und nicht einfach "alle Flate-Streams
  auspacken und das erste XML nehmen":

  Rund ein Fuenftel der real vorkommenden eRechnungs-PDFs ist per inkrementellem
  Update entstanden. Dabei bleiben ueberschriebene Vorversionen von Objekten
  physisch in der Datei stehen; welche Fassung gilt, sagt ausschliesslich die
  xref-Kette. Ein Rohdaten-Scanner findet also unter Umstaenden eine ersetzte
  Rechnung, waehrend jeder normale Viewer die aktuelle anzeigt - eine
  Manipulation, die eine formal voellig valide PDF-Datei hinterlaesst.
  Deshalb wird hier der Objektgraph aufgeloest.

  Faellt die xref-Kette aus (defekte Datei), rekonstruiert die Unit die
  Objekttabelle durch einen Scan - dann gewinnt die LETZTE Definition einer
  Objektnummer. Dieser Fall wird im Ergebnis ueber UsedReconstruction
  gemeldet, weil er die oben beschriebene Sicherheit nicht mehr bietet.

  Beruecksichtigte Stromfilter: FlateDecode (mit /Predictor), LZWDecode
  (mit /Predictor), ASCIIHexDecode, ASCII85Decode, RunLengthDecode.
  Verschluesselte Dateien werden erkannt und abgelehnt, nicht geraten -
  PDF/A-3 verbietet Verschluesselung ohnehin.

  Anhaenge auf Seitenebene (/AF am Seitenobjekt) werden nicht gesucht;
  ZUGFeRD und Factur-X betten die Rechnung immer am Katalog ein.
}

unit intf.XRechnungPdfExtract;

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
  ;

type
  // Einordnung eines gefundenen Anhangs. pakInvoice heisst: der Name entspricht
  // einem der spezifizierten Rechnungsdateinamen ODER das Wurzelelement ist
  // eine CII-/UBL-Rechnung.
  TXRechnungPdfAttachmentKind = (pakUnspecified, pakInvoice, pakOtherXml, pakOther);

  TXRechnungPdfAttachment = class(TObject)
  private
    FFileName : String;
    FRelationship : String;
    FMimeType : String;
    FDescription : String;
    FData : TBytes;
    FKind : TXRechnungPdfAttachmentKind;
    FRootElement : String;
    function GetSize : Integer;
  public
    // Dateiname laut /UF (bevorzugt) bzw. /F des Filespec.
    property FileName : String read FFileName write FFileName;
    // /AFRelationship - in der Praxis unzuverlaessig, daher NICHT zum Filtern
    // verwenden. Real kommen Alternative, Data, Unspecified und Source vor.
    property Relationship : String read FRelationship write FRelationship;
    property MimeType : String read FMimeType write FMimeType;
    property Description : String read FDescription write FDescription;
    property Data : TBytes read FData write FData;
    property Size : Integer read GetSize;
    property Kind : TXRechnungPdfAttachmentKind read FKind write FKind;
    // Lokalname des XML-Wurzelelements, leer wenn kein XML.
    property RootElement : String read FRootElement write FRootElement;
  end;

  TXRechnungPdfAttachmentList = class(TObject)
  private
    FList : TList;
    function GetCount : Integer;
    function GetItem(_Index : Integer) : TXRechnungPdfAttachment;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(_Item : TXRechnungPdfAttachment) : Integer;
    property Count : Integer read GetCount;
    property Items[_Index : Integer] : TXRechnungPdfAttachment read GetItem; default;
  end;

  TXRechnungPdfExtractInfo = record
    // True, wenn die xref-Kette unbrauchbar war und die Objekttabelle durch
    // einen Scan rekonstruiert wurde. Das Ergebnis ist dann nicht mehr gegen
    // ueberschriebene Vorversionen abgesichert.
    UsedReconstruction : Boolean;
    // Anzahl der xref-Abschnitte (1 = ohne inkrementelles Update).
    XrefSections : Integer;
    Encrypted : Boolean;
    PdfVersion : String;
    Error : String;
  end;

  TXRechnungPdfExtractor = class(TObject)
  public
    // Bequemer Einstieg: liefert die eingebettete Rechnung als Bytes (UTF-8).
    class function ExtractInvoiceFromFile(const _PdfFilename : String;
      out _Xml : TBytes; out _AttachmentName : String;
      out _Info : TXRechnungPdfExtractInfo) : Boolean;
    class function ExtractInvoiceFromStream(_Stream : TStream;
      out _Xml : TBytes; out _AttachmentName : String;
      out _Info : TXRechnungPdfExtractInfo) : Boolean;

    // Vollstaendig: alle eingebetteten Dateien in Reihenfolge des Namensbaums.
    class function ExtractAllFromFile(const _PdfFilename : String;
      _List : TXRechnungPdfAttachmentList;
      out _Info : TXRechnungPdfExtractInfo) : Boolean;
    class function ExtractAllFromStream(_Stream : TStream;
      _List : TXRechnungPdfAttachmentList;
      out _Info : TXRechnungPdfExtractInfo) : Boolean;

    // Waehlt aus einer gefuellten Liste die Rechnung aus: erst nach
    // spezifiziertem Dateinamen, dann nach XML-Wurzelelement. Liefert -1,
    // wenn nichts passt.
    class function FindInvoiceIndex(_List : TXRechnungPdfAttachmentList) : Integer;

    // True, wenn die Datei mit %PDF- beginnt.
    class function IsPdfFile(const _PdfFilename : String) : Boolean;
    // Wie IsPdfFile, aber auf einem Datenstrom. Die Position des Stroms bleibt
    // unveraendert, sodass der Aufrufer danach normal weiterlesen kann.
    // Geprueft wird - wie bei IsPdfFile - ausschliesslich der Dateianfang:
    // eine XML-Rechnung, die die Zeichenfolge %PDF- irgendwo im Text fuehrt,
    // darf nicht als PDF durchgehen.
    class function IsPdfStream(_Stream : TStream) : Boolean;
  end;

const
  // Obergrenze fuer einen einzelnen dekomprimierten Stream (Schutz gegen
  // Dekompressionsbomben).
  XRechnungPdfMaxStreamSize : Int64 = 64 * 1024 * 1024;
  // Maximale Verschachtelungstiefe im Objektgraph (Schutz gegen Zyklen).
  XRechnungPdfMaxDepth = 64;
  // Gesamtbudget ueber ALLE Anhaenge eines Dokuments. Die Grenze pro Stream
  // allein genuegt nicht: viele Anhaenge summieren sich sonst auf ein
  // Vielfaches davon.
  XRechnungPdfMaxTotalSize : Int64 = 192 * 1024 * 1024;
  // Mehr Anhaenge als das hat keine Rechnung.
  XRechnungPdfMaxAttachments = 512;

implementation

//==============================================================================
// Byte-Helfer
//==============================================================================

function IsPdfWhite(_B : Byte) : Boolean;
begin
  Result := (_B = 0) or (_B = 9) or (_B = 10) or (_B = 12) or (_B = 13) or (_B = 32);
end;

function IsPdfDelim(_B : Byte) : Boolean;
begin
  case _B of
    40,41,60,62,91,93,123,125,47,37 : Result := True;  // ( ) < > [ ] { } / %
  else
    Result := False;
  end;
end;

function IsPdfRegular(_B : Byte) : Boolean;
begin
  Result := not IsPdfWhite(_B) and not IsPdfDelim(_B);
end;

// Vergleicht ab _Pos mit einem ASCII-Literal.
function MatchAt(const _Buf : TBytes; _Pos : Integer; const _S : AnsiString) : Boolean;
var
  i : Integer;
begin
  Result := False;
  if _Pos < 0 then exit;
  if _Pos + Length(_S) > Length(_Buf) then exit;
  for i := 1 to Length(_S) do
    if _Buf[_Pos + i - 1] <> Byte(_S[i]) then exit;
  Result := True;
end;

// Sucht _S ab _From vorwaerts, -1 wenn nicht gefunden.
function IndexOfBytes(const _Buf : TBytes; const _S : AnsiString; _From : Integer) : Integer;
var
  i, n : Integer;
  first : Byte;
begin
  Result := -1;
  if _S = '' then exit;
  n := Length(_Buf) - Length(_S);
  if _From < 0 then _From := 0;
  first := Byte(_S[1]);
  for i := _From to n do
    if (_Buf[i] = first) and MatchAt(_Buf, i, _S) then
    begin
      Result := i;
      exit;
    end;
end;

// Sucht _S rueckwaerts ab _From.
function LastIndexOfBytes(const _Buf : TBytes; const _S : AnsiString; _From : Integer) : Integer;
var
  i : Integer;
begin
  Result := -1;
  if _S = '' then exit;
  if _From > Length(_Buf) - Length(_S) then _From := Length(_Buf) - Length(_S);
  for i := _From downto 0 do
    if MatchAt(_Buf, i, _S) then
    begin
      Result := i;
      exit;
    end;
end;

function BytesToAnsiStr(const _Buf : TBytes; _Pos, _Len : Integer) : AnsiString;
var
  i : Integer;
begin
  Result := '';
  if _Pos < 0 then _Pos := 0;
  if _Pos + _Len > Length(_Buf) then _Len := Length(_Buf) - _Pos;
  if _Len <= 0 then exit;
  SetLength(Result, _Len);
  for i := 0 to _Len - 1 do
    Result[i + 1] := AnsiChar(_Buf[_Pos + i]);
end;

//==============================================================================
// Stromfilter
//==============================================================================

// zlib-Inflate ueber die jeweilige Plattformbibliothek. Liefert auch dann
// True, wenn der Strom vorzeitig abbricht, sofern etwas dekomprimiert wurde -
// abgeschnittene Streams kommen in freier Wildbahn vor und der bis dahin
// gelesene Teil ist gueltig.
function InflateBytes(const _Src : TBytes; _Raw : Boolean; out _Dst : TBytes) : Boolean;
var
  srcStm, dstStm : TMemoryStream;
  zs : TStream;
  buf : array[0..65535] of Byte;
  got : Integer;
  tooBig : Boolean;
begin
  Result := False;
  SetLength(_Dst, 0);
  if Length(_Src) = 0 then exit;

  tooBig := False;
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
        if got > 0 then
        begin
          dstStm.WriteBuffer(buf, got);
          if dstStm.Size > XRechnungPdfMaxStreamSize then
          begin
            // Dekompressionsbombe. Abgeschnittene Daten duerfen NICHT als
            // Erfolg durchgehen - sonst landet ein Bruchstueck als Anhang in
            // der Ergebnisliste und der Speicher fuellt sich ueber viele
            // solcher Streams trotzdem.
            tooBig := True;
            break;
          end;
        end;
      until got <= 0;
    except
      // Teilergebnis behalten
    end;
    if zs <> nil then
      try
        zs.Free;
      except
      end;

    if (dstStm.Size > 0) and not tooBig then
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

// FlateDecode mit Toleranz: manche Erzeuger schreiben Muell vor den
// zlib-Header oder liefern rohes Deflate ohne Header.
function FilterFlate(const _Src : TBytes; out _Dst : TBytes) : Boolean;
var
  shifted : TBytes;
  i, n : Integer;
begin
  Result := InflateBytes(_Src, False, _Dst);
  if Result then exit;
  Result := InflateBytes(_Src, True, _Dst);
  if Result then exit;
  // Fuehrende Whitespaces ueberspringen und erneut versuchen
  i := 0;
  while (i < Length(_Src)) and IsPdfWhite(_Src[i]) do
    Inc(i);
  if (i > 0) and (i < Length(_Src)) then
  begin
    n := Length(_Src) - i;
    SetLength(shifted, n);
    Move(_Src[i], shifted[0], n);
    Result := InflateBytes(shifted, False, _Dst);
    if not Result then
      Result := InflateBytes(shifted, True, _Dst);
  end;
end;

function HexVal(_B : Byte) : Integer;
begin
  case _B of
    48..57 : Result := _B - 48;
    65..70 : Result := _B - 55;
    97..102: Result := _B - 87;
  else
    Result := -1;
  end;
end;

function FilterASCIIHex(const _Src : TBytes; out _Dst : TBytes) : Boolean;
var
  i, n, hi, v : Integer;
begin
  SetLength(_Dst, (Length(_Src) div 2) + 1);
  n := 0;
  hi := -1;
  for i := 0 to Length(_Src) - 1 do
  begin
    if _Src[i] = Byte('>') then break;
    v := HexVal(_Src[i]);
    if v < 0 then continue;
    if hi < 0 then
      hi := v
    else
    begin
      _Dst[n] := Byte((hi shl 4) or v);
      Inc(n);
      hi := -1;
    end;
  end;
  if hi >= 0 then
  begin
    _Dst[n] := Byte(hi shl 4);
    Inc(n);
  end;
  SetLength(_Dst, n);
  Result := True;
end;

function FilterASCII85(const _Src : TBytes; out _Dst : TBytes) : Boolean;
var
  i, n, cnt, k : Integer;
  tuple : Cardinal;
  b : Byte;
  outStm : TMemoryStream;
  quad : array[0..3] of Byte;
begin
  outStm := TMemoryStream.Create;
  try
    i := 0;
    // optionale Einleitung <~
    if (Length(_Src) >= 2) and (_Src[0] = Byte('<')) and (_Src[1] = Byte('~')) then
      i := 2;
    tuple := 0;
    cnt := 0;
    while i < Length(_Src) do
    begin
      b := _Src[i];
      Inc(i);
      if IsPdfWhite(b) then continue;
      if b = Byte('~') then break;
      if b = Byte('z') then
      begin
        if cnt = 0 then
        begin
          FillChar(quad, SizeOf(quad), 0);
          outStm.WriteBuffer(quad, 4);
          continue;
        end;
        Result := False;
        exit;
      end;
      if (b < Byte('!')) or (b > Byte('u')) then continue;
      tuple := tuple * 85 + Cardinal(b - Byte('!'));
      Inc(cnt);
      if cnt = 5 then
      begin
        quad[0] := Byte(tuple shr 24);
        quad[1] := Byte((tuple shr 16) and $FF);
        quad[2] := Byte((tuple shr 8) and $FF);
        quad[3] := Byte(tuple and $FF);
        outStm.WriteBuffer(quad, 4);
        tuple := 0;
        cnt := 0;
      end;
    end;
    if cnt > 1 then
    begin
      for k := cnt to 4 do
        tuple := tuple * 85 + 84;
      quad[0] := Byte(tuple shr 24);
      quad[1] := Byte((tuple shr 16) and $FF);
      quad[2] := Byte((tuple shr 8) and $FF);
      quad[3] := Byte(tuple and $FF);
      outStm.WriteBuffer(quad, cnt - 1);
    end;
    n := outStm.Size;
    SetLength(_Dst, n);
    if n > 0 then
      Move(outStm.Memory^, _Dst[0], n);
    Result := True;
  finally
    outStm.Free;
  end;
end;

function FilterRunLength(const _Src : TBytes; out _Dst : TBytes) : Boolean;
var
  i, j, len : Integer;
  outStm : TMemoryStream;
  b : Byte;
begin
  Result := False;
  SetLength(_Dst, 0);
  outStm := TMemoryStream.Create;
  try
    i := 0;
    while i < Length(_Src) do
    begin
      len := _Src[i];
      Inc(i);
      if len = 128 then break;
      if len < 128 then
      begin
        if i + len + 1 > Length(_Src) then
          len := Length(_Src) - i - 1;
        if len < 0 then break;
        outStm.WriteBuffer(_Src[i], len + 1);
        Inc(i, len + 1);
      end
      else
      begin
        if i >= Length(_Src) then break;
        b := _Src[i];
        Inc(i);
        for j := 0 to 256 - len do
          outStm.WriteBuffer(b, 1);
      end;
      if outStm.Size > XRechnungPdfMaxStreamSize then
        exit;   // Bombe: kein Teilergebnis zurueckgeben
    end;
    SetLength(_Dst, outStm.Size);
    if outStm.Size > 0 then
      Move(outStm.Memory^, _Dst[0], outStm.Size);
    Result := True;
  finally
    outStm.Free;
  end;
end;

// LZW nach PDF-Spezifikation (frueher Wechsel der Codebreite, EarlyChange=1
// ist der Normalfall).
function FilterLZW(const _Src : TBytes; _EarlyChange : Integer; out _Dst : TBytes) : Boolean;
type
  TDictEntry = record
    Prev : Integer;
    Ch   : Byte;
    Len  : Integer;
  end;
var
  dict : array[0..4095] of TDictEntry;
  next, codeLen, prevCode, code, i, k : Integer;
  bitBuf : Cardinal;
  bitCnt : Integer;
  outStm : TMemoryStream;
  seq : TBytes;
  srcPos : Integer;

  procedure ResetDict;
  var
    d : Integer;
  begin
    for d := 0 to 255 do
    begin
      dict[d].Prev := -1;
      dict[d].Ch := Byte(d);
      dict[d].Len := 1;
    end;
    next := 258;
    codeLen := 9;
    prevCode := -1;
  end;

  // Baut die Bytefolge zu einem Code rueckwaerts auf.
  function Expand(_Code : Integer; out _Seq : TBytes) : Boolean;
  var
    n, p : Integer;
  begin
    Result := False;
    if (_Code < 0) or (_Code >= next) or (dict[_Code].Len <= 0) then exit;
    n := dict[_Code].Len;
    SetLength(_Seq, n);
    p := _Code;
    while (n > 0) and (p >= 0) do
    begin
      Dec(n);
      _Seq[n] := dict[p].Ch;
      p := dict[p].Prev;
    end;
    Result := True;
  end;

begin
  Result := False;
  SetLength(_Dst, 0);
  outStm := TMemoryStream.Create;
  try
    ResetDict;
    bitBuf := 0;
    bitCnt := 0;
    srcPos := 0;
    while True do
    begin
      while (bitCnt < codeLen) and (srcPos < Length(_Src)) do
      begin
        bitBuf := (bitBuf shl 8) or _Src[srcPos];
        Inc(srcPos);
        Inc(bitCnt, 8);
      end;
      if bitCnt < codeLen then break;
      code := Integer((bitBuf shr (bitCnt - codeLen)) and ((Cardinal(1) shl codeLen) - 1));
      Dec(bitCnt, codeLen);
      if code = 256 then
      begin
        ResetDict;
        continue;
      end;
      if code = 257 then break;
      if prevCode < 0 then
      begin
        if not Expand(code, seq) then break;
        outStm.WriteBuffer(seq[0], Length(seq));
        prevCode := code;
      end
      else
      begin
        if code < next then
        begin
          if not Expand(code, seq) then break;
          if next < 4096 then
          begin
            dict[next].Prev := prevCode;
            dict[next].Ch := seq[0];
            dict[next].Len := dict[prevCode].Len + 1;
            Inc(next);
          end;
        end
        else
        begin
          // KwKwK-Fall
          if not Expand(prevCode, seq) then break;
          k := Length(seq);
          SetLength(seq, k + 1);
          seq[k] := seq[0];
          if next < 4096 then
          begin
            dict[next].Prev := prevCode;
            dict[next].Ch := seq[0];
            dict[next].Len := dict[prevCode].Len + 1;
            Inc(next);
          end;
        end;
        outStm.WriteBuffer(seq[0], Length(seq));
        prevCode := code;
      end;
      if outStm.Size > XRechnungPdfMaxStreamSize then
        exit;   // Bombe: kein Teilergebnis zurueckgeben
      if (next + _EarlyChange >= 512) and (codeLen = 9) then codeLen := 10
      else if (next + _EarlyChange >= 1024) and (codeLen = 10) then codeLen := 11
      else if (next + _EarlyChange >= 2048) and (codeLen = 11) then codeLen := 12;
    end;
    i := outStm.Size;
    SetLength(_Dst, i);
    if i > 0 then
    begin
      Move(outStm.Memory^, _Dst[0], i);
      Result := True;
    end;
  finally
    outStm.Free;
  end;
end;

// PNG- und TIFF-Praediktoren nach /DecodeParms.
function ApplyPredictor(const _Src : TBytes; _Predictor, _Colors, _Bpc, _Columns : Integer;
  out _Dst : TBytes) : Boolean;
var
  bpp, rowLen, rows, r, i, ft : Integer;
  rowLen64 : Int64;
  prior, cur : TBytes;
  outStm : TMemoryStream;
  a, b, c, p, pa, pb, pc : Integer;
begin
  Result := False;
  SetLength(_Dst, 0);
  if _Predictor < 2 then
  begin
    _Dst := _Src;
    Result := True;
    exit;
  end;
  if (_Colors <= 0) or (_Bpc <= 0) or (_Columns <= 0) then exit;

  // Kenngroessen aus /DecodeParms stammen aus der Datei. In Integer-Arithmetik
  // koennte "/Columns 200000000" ueberlaufen bzw. Zeilenpuffer im
  // dreistelligen MB-Bereich anfordern - deshalb in Int64 rechnen und gegen
  // die tatsaechliche Streamlaenge pruefen.
  if (_Colors > 32) or (_Bpc > 32) or (_Columns > XRechnungPdfMaxStreamSize) then exit;
  rowLen64 := ((Int64(_Colors) * _Bpc * _Columns) + 7) div 8;
  // Eine Zeile, die nicht einmal in die vorhandenen Daten passt, ist unecht.
  if (rowLen64 <= 0) or (rowLen64 > Length(_Src)) then exit;
  bpp := Max(1, (_Colors * _Bpc) div 8);
  rowLen := Integer(rowLen64);

  if _Predictor = 2 then
  begin
    // TIFF-Praediktor, hier nur der gebraeuchliche 8-Bit-Fall
    if _Bpc <> 8 then
    begin
      _Dst := _Src;
      Result := True;
      exit;
    end;
    _Dst := Copy(_Src, 0, Length(_Src));
    rows := Length(_Dst) div rowLen;
    for r := 0 to rows - 1 do
      for i := bpp to rowLen - 1 do
        _Dst[r * rowLen + i] := Byte((_Dst[r * rowLen + i] + _Dst[r * rowLen + i - bpp]) and $FF);
    Result := True;
    exit;
  end;

  // PNG-Praediktoren: jede Zeile beginnt mit einem Filtertyp-Byte
  SetLength(prior, rowLen);
  SetLength(cur, rowLen);
  FillChar(prior[0], rowLen, 0);
  outStm := TMemoryStream.Create;
  try
    i := 0;
    while i + 1 <= Length(_Src) - 1 do
    begin
      ft := _Src[i];
      Inc(i);
      if i + rowLen > Length(_Src) then
        break;
      Move(_Src[i], cur[0], rowLen);
      Inc(i, rowLen);
      case ft of
        0 : ;
        1 : for r := bpp to rowLen - 1 do
              cur[r] := Byte((cur[r] + cur[r - bpp]) and $FF);
        2 : for r := 0 to rowLen - 1 do
              cur[r] := Byte((cur[r] + prior[r]) and $FF);
        3 : for r := 0 to rowLen - 1 do
            begin
              if r >= bpp then a := cur[r - bpp] else a := 0;
              cur[r] := Byte((cur[r] + ((a + prior[r]) div 2)) and $FF);
            end;
        4 : for r := 0 to rowLen - 1 do
            begin
              if r >= bpp then a := cur[r - bpp] else a := 0;
              b := prior[r];
              if r >= bpp then c := prior[r - bpp] else c := 0;
              p := a + b - c;
              pa := Abs(p - a);
              pb := Abs(p - b);
              pc := Abs(p - c);
              if (pa <= pb) and (pa <= pc) then
                cur[r] := Byte((cur[r] + a) and $FF)
              else if pb <= pc then
                cur[r] := Byte((cur[r] + b) and $FF)
              else
                cur[r] := Byte((cur[r] + c) and $FF);
            end;
      else
        break;
      end;
      outStm.WriteBuffer(cur[0], rowLen);
      Move(cur[0], prior[0], rowLen);
    end;
    SetLength(_Dst, outStm.Size);
    if outStm.Size > 0 then
      Move(outStm.Memory^, _Dst[0], outStm.Size);
    Result := True;
  finally
    outStm.Free;
  end;
end;

//==============================================================================
// Objektmodell
//==============================================================================

type
  TPdfObjKind = (pokNull, pokBool, pokNum, pokStr, pokName, pokArr, pokDict,
                 pokStream, pokRef);

  // Alle Instanzen gehoeren dem TPdfDocument (Owned-Liste). Destroy gibt daher
  // nur die eigenen Container frei, niemals die Kindobjekte - die haengen als
  // geteilte Referenzen an mehreren Stellen im Graph.
  TPdfObj = class(TObject)
  public
    Kind : TPdfObjKind;
    BoolVal : Boolean;
    NumVal : Double;
    StrVal : TBytes;
    NameVal : String;
    Items : TList;
    DKeys : TStringList;
    DVals : TList;
    StreamPos : Integer;
    StreamLen : Integer;
    RefNum : Integer;
    RefGen : Integer;
    constructor Create(_Kind : TPdfObjKind);
    destructor Destroy; override;
    function AsInt : Int64;
    function IsDictLike : Boolean;
    // Rohzugriff ohne Aufloesung indirekter Referenzen.
    function RawGet(const _Key : String) : TPdfObj;
    function ArrCount : Integer;
    function ArrItem(_Index : Integer) : TPdfObj;
  end;

constructor TPdfObj.Create(_Kind : TPdfObjKind);
begin
  inherited Create;
  Kind := _Kind;
  StreamPos := -1;
  StreamLen := -1;
  RefNum := -1;
  RefGen := 0;
  if _Kind = pokArr then
    Items := TList.Create;
  if (_Kind = pokDict) or (_Kind = pokStream) then
  begin
    DKeys := TStringList.Create;
    DVals := TList.Create;
  end;
end;

destructor TPdfObj.Destroy;
begin
  Items.Free;
  DKeys.Free;
  DVals.Free;
  inherited;
end;

function TPdfObj.AsInt : Int64;
begin
  if Kind = pokNum then
    Result := Trunc(NumVal)
  else
    Result := 0;
end;

function TPdfObj.IsDictLike : Boolean;
begin
  Result := (Kind = pokDict) or (Kind = pokStream);
end;

function TPdfObj.RawGet(const _Key : String) : TPdfObj;
var
  i : Integer;
begin
  Result := nil;
  if not IsDictLike then exit;
  i := DKeys.IndexOf(_Key);
  if i >= 0 then
    Result := TPdfObj(DVals[i]);
end;

function TPdfObj.ArrCount : Integer;
begin
  if Kind = pokArr then
    Result := Items.Count
  else
    Result := 0;
end;

function TPdfObj.ArrItem(_Index : Integer) : TPdfObj;
begin
  if (Kind = pokArr) and (_Index >= 0) and (_Index < Items.Count) then
    Result := TPdfObj(Items[_Index])
  else
    Result := nil;
end;

//==============================================================================
// Lexer / Objektparser
//==============================================================================

type
  TPdfLexer = class(TObject)
  private
    FBuf : TBytes;
    FPos : Integer;
    FOwned : TList;
    function NewObj(_Kind : TPdfObjKind) : TPdfObj;
    function ParseLiteralString : TPdfObj;
    function ParseHexString : TPdfObj;
    function ParseName : TPdfObj;
    function ParseArray(_Depth : Integer) : TPdfObj;
    function ParseDict(_Depth : Integer) : TPdfObj;
  public
    constructor Create(const _Buf : TBytes; _Owned : TList);
    procedure SkipWhite;
    function AtEnd : Boolean;
    // Liest ein regulaeres Token (Zahl, Schluesselwort) ohne es zu deuten.
    function ReadToken : AnsiString;
    function ParseObject(_Depth : Integer) : TPdfObj;
    property Pos : Integer read FPos write FPos;
  end;

constructor TPdfLexer.Create(const _Buf : TBytes; _Owned : TList);
begin
  inherited Create;
  FBuf := _Buf;
  FOwned := _Owned;
  FPos := 0;
end;

function TPdfLexer.NewObj(_Kind : TPdfObjKind) : TPdfObj;
begin
  Result := TPdfObj.Create(_Kind);
  FOwned.Add(Result);
end;

function TPdfLexer.AtEnd : Boolean;
begin
  Result := FPos >= Length(FBuf);
end;

procedure TPdfLexer.SkipWhite;
begin
  while FPos < Length(FBuf) do
  begin
    if IsPdfWhite(FBuf[FPos]) then
      Inc(FPos)
    else if FBuf[FPos] = Byte('%') then
    begin
      // Kommentar bis Zeilenende
      while (FPos < Length(FBuf)) and (FBuf[FPos] <> 10) and (FBuf[FPos] <> 13) do
        Inc(FPos);
    end
    else
      break;
  end;
end;

function TPdfLexer.ReadToken : AnsiString;
var
  start : Integer;
begin
  SkipWhite;
  start := FPos;
  while (FPos < Length(FBuf)) and IsPdfRegular(FBuf[FPos]) do
    Inc(FPos);
  if FPos = start then
  begin
    // Einzelnes Trennzeichen als Token zurueckgeben
    if FPos < Length(FBuf) then
    begin
      Result := AnsiChar(FBuf[FPos]);
      Inc(FPos);
    end
    else
      Result := '';
    exit;
  end;
  Result := BytesToAnsiStr(FBuf, start, FPos - start);
end;

function TPdfLexer.ParseLiteralString : TPdfObj;
var
  depth, n, oct, k : Integer;
  outStm : TMemoryStream;
  b : Byte;
begin
  Result := NewObj(pokStr);
  outStm := TMemoryStream.Create;
  try
    Inc(FPos);   // (
    depth := 1;
    while FPos < Length(FBuf) do
    begin
      b := FBuf[FPos];
      Inc(FPos);
      if b = Byte('\') then
      begin
        if FPos >= Length(FBuf) then break;
        b := FBuf[FPos];
        Inc(FPos);
        case b of
          Byte('n') : b := 10;
          Byte('r') : b := 13;
          Byte('t') : b := 9;
          Byte('b') : b := 8;
          Byte('f') : b := 12;
          10 : continue;                       // Zeilenfortsetzung
          13 : begin
                 if (FPos < Length(FBuf)) and (FBuf[FPos] = 10) then Inc(FPos);
                 continue;
               end;
          48..55 : begin                       // Oktal \ddd
                     oct := b - 48;
                     k := 1;
                     while (k < 3) and (FPos < Length(FBuf)) and
                           (FBuf[FPos] >= 48) and (FBuf[FPos] <= 55) do
                     begin
                       oct := oct * 8 + (FBuf[FPos] - 48);
                       Inc(FPos);
                       Inc(k);
                     end;
                     b := Byte(oct and $FF);
                   end;
        end;
        outStm.WriteBuffer(b, 1);
        continue;
      end;
      if b = Byte('(') then Inc(depth)
      else if b = Byte(')') then
      begin
        Dec(depth);
        if depth = 0 then break;
      end;
      outStm.WriteBuffer(b, 1);
    end;
    n := outStm.Size;
    SetLength(Result.StrVal, n);
    if n > 0 then
      Move(outStm.Memory^, Result.StrVal[0], n);
  finally
    outStm.Free;
  end;
end;

function TPdfLexer.ParseHexString : TPdfObj;
var
  hi, v, n : Integer;
  outStm : TMemoryStream;
  b : Byte;
begin
  Result := NewObj(pokStr);
  outStm := TMemoryStream.Create;
  try
    Inc(FPos);   // <
    hi := -1;
    while FPos < Length(FBuf) do
    begin
      if FBuf[FPos] = Byte('>') then
      begin
        Inc(FPos);
        break;
      end;
      v := HexVal(FBuf[FPos]);
      Inc(FPos);
      if v < 0 then continue;
      if hi < 0 then
        hi := v
      else
      begin
        b := Byte((hi shl 4) or v);
        outStm.WriteBuffer(b, 1);
        hi := -1;
      end;
    end;
    if hi >= 0 then
    begin
      b := Byte(hi shl 4);
      outStm.WriteBuffer(b, 1);
    end;
    n := outStm.Size;
    SetLength(Result.StrVal, n);
    if n > 0 then
      Move(outStm.Memory^, Result.StrVal[0], n);
  finally
    outStm.Free;
  end;
end;

function TPdfLexer.ParseName : TPdfObj;
var
  s : AnsiString;
  i, v1, v2 : Integer;
  res : AnsiString;
begin
  Result := NewObj(pokName);
  Inc(FPos);   // /
  s := '';
  while (FPos < Length(FBuf)) and IsPdfRegular(FBuf[FPos]) do
  begin
    s := s + AnsiChar(FBuf[FPos]);
    Inc(FPos);
  end;
  // #xx-Ersetzungen aufloesen
  res := '';
  i := 1;
  while i <= Length(s) do
  begin
    if (s[i] = '#') and (i + 2 <= Length(s)) then
    begin
      v1 := HexVal(Byte(s[i + 1]));
      v2 := HexVal(Byte(s[i + 2]));
      if (v1 >= 0) and (v2 >= 0) then
      begin
        res := res + AnsiChar((v1 shl 4) or v2);
        Inc(i, 3);
        continue;
      end;
    end;
    res := res + s[i];
    Inc(i);
  end;
  Result.NameVal := String(res);
end;

function TPdfLexer.ParseArray(_Depth : Integer) : TPdfObj;
var
  o : TPdfObj;
begin
  Result := NewObj(pokArr);
  Inc(FPos);   // [
  while True do
  begin
    SkipWhite;
    if FPos >= Length(FBuf) then break;
    if FBuf[FPos] = Byte(']') then
    begin
      Inc(FPos);
      break;
    end;
    o := ParseObject(_Depth + 1);
    if o = nil then break;
    Result.Items.Add(o);
    if Result.Items.Count > 200000 then break;
  end;
end;

function TPdfLexer.ParseDict(_Depth : Integer) : TPdfObj;
var
  keyObj, valObj : TPdfObj;
  save : Integer;
begin
  Result := NewObj(pokDict);
  Inc(FPos, 2);   // <<
  while True do
  begin
    SkipWhite;
    if FPos >= Length(FBuf) then break;
    if MatchAt(FBuf, FPos, '>>') then
    begin
      Inc(FPos, 2);
      break;
    end;
    if FBuf[FPos] <> Byte('/') then
    begin
      // Unerwartetes Token - ueberspringen, um nicht haengenzubleiben
      save := FPos;
      ReadToken;
      if FPos = save then Inc(FPos);
      continue;
    end;
    keyObj := ParseName;
    valObj := ParseObject(_Depth + 1);
    if valObj = nil then break;
    Result.DKeys.Add(keyObj.NameVal);
    Result.DVals.Add(valObj);
  end;

  // Folgt ein Stream, wird aus dem Dict ein Streamobjekt.
  save := FPos;
  SkipWhite;
  if MatchAt(FBuf, FPos, 'stream') then
  begin
    Inc(FPos, 6);
    if (FPos < Length(FBuf)) and (FBuf[FPos] = 13) then Inc(FPos);
    if (FPos < Length(FBuf)) and (FBuf[FPos] = 10) then Inc(FPos);
    Result.Kind := pokStream;
    Result.StreamPos := FPos;
    Result.StreamLen := -1;   // wird vom Dokument bestimmt (/Length indirekt)
    // Grobe Fortsetzung fuer den Aufrufer
    save := IndexOfBytes(FBuf, 'endstream', FPos);
    if save >= 0 then
      FPos := save + 9
    else
      FPos := Length(FBuf);
  end
  else
    FPos := save;
end;

function TPdfLexer.ParseObject(_Depth : Integer) : TPdfObj;
var
  b : Byte;
  tok : AnsiString;
  save, savePos : Integer;
  num1, num2 : Int64;
  code : Integer;
  d : Double;
begin
  Result := nil;
  if _Depth > XRechnungPdfMaxDepth then exit;
  SkipWhite;
  if FPos >= Length(FBuf) then exit;

  b := FBuf[FPos];
  case b of
    Byte('/') : begin Result := ParseName; exit; end;
    Byte('(') : begin Result := ParseLiteralString; exit; end;
    Byte('[') : begin Result := ParseArray(_Depth); exit; end;
    Byte(']') : exit;
    Byte('<') : begin
                  if MatchAt(FBuf, FPos, '<<') then
                    Result := ParseDict(_Depth)
                  else
                    Result := ParseHexString;
                  exit;
                end;
    Byte('>') : exit;
    Byte(')') : exit;
    Byte('{') : begin Inc(FPos); exit; end;
    Byte('}') : begin Inc(FPos); exit; end;
  end;

  // Position vor dem Token merken: ein nicht deutbares Token (endobj, stream)
  // muss unverbraucht zurueckgegeben werden, damit der Aufrufer es sieht.
  savePos := FPos;
  tok := ReadToken;
  if tok = '' then exit;

  if tok = 'true' then
  begin
    Result := NewObj(pokBool);
    Result.BoolVal := True;
    exit;
  end;
  if tok = 'false' then
  begin
    Result := NewObj(pokBool);
    Result.BoolVal := False;
    exit;
  end;
  if tok = 'null' then
  begin
    Result := NewObj(pokNull);
    exit;
  end;

  // Zahl? Dann pruefen, ob eine indirekte Referenz "n g R" folgt.
  Val(String(tok), d, code);
  if code <> 0 then
  begin
    // Kein deutbares Token (z.B. endobj, stream) - als Null melden.
    FPos := savePos;
    exit;
  end;

  Result := NewObj(pokNum);
  Result.NumVal := d;

  if (Frac(d) = 0) and (d >= 0) and (d < 2147483647) then
  begin
    num1 := Trunc(d);
    save := FPos;
    SkipWhite;
    savePos := FPos;
    tok := ReadToken;
    Val(String(tok), d, code);
    if (code = 0) and (Frac(d) = 0) and (d >= 0) and (d < 65536) then
    begin
      num2 := Trunc(d);
      SkipWhite;
      if (FPos < Length(FBuf)) and (FBuf[FPos] = Byte('R')) and
         ((FPos + 1 >= Length(FBuf)) or not IsPdfRegular(FBuf[FPos + 1])) then
      begin
        Inc(FPos);
        Result.Kind := pokRef;
        Result.RefNum := Integer(num1);
        Result.RefGen := Integer(num2);
        exit;
      end;
    end;
    FPos := save;
  end;
end;

//==============================================================================
// Dokument: xref-Aufloesung, Objektzugriff, Streamdaten
//==============================================================================

type
  TPdfXrefKind = (xkFree, xkNormal, xkCompressed);

  TPdfXrefEntry = record
    Known : Boolean;
    Kind : TPdfXrefKind;
    Offset : Int64;
    Gen : Integer;
    StmNum : Integer;
    StmIdx : Integer;
  end;

  TPdfDocument = class(TObject)
  private
    FBuf : TBytes;
    FXref : array of TPdfXrefEntry;
    FCache : array of TPdfObj;
    FLoading : array of Boolean;
    FObjStmDone : array of Boolean;
    FOwned : TList;
    FRootRef : TPdfObj;
    FEncryptRef : TPdfObj;
    FXrefSections : Integer;
    FReconstructed : Boolean;
    function MaxPlausibleObjNum : Integer;
    procedure EnsureSize(_Num : Integer);
    procedure SetEntry(_Num : Integer; _Kind : TPdfXrefKind; _Ofs : Int64;
      _Gen, _StmNum, _StmIdx : Integer);
    procedure NoteTrailer(_Dict : TPdfObj);
    function ParseXrefChain(_StartOfs : Int64) : Boolean;
    function ParseClassicXref(_Lex : TPdfLexer; out _Trailer : TPdfObj) : Boolean;
    function ParseXrefStream(_Obj : TPdfObj) : Boolean;
    procedure Reconstruct;
    procedure LoadObjStm(_StmNum : Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Load(_Stream : TStream; out _Err : String) : Boolean;
    function GetObject(_Num : Integer) : TPdfObj;
    function Resolve(_O : TPdfObj) : TPdfObj;
    // Woerterbuchzugriff inklusive Aufloesung indirekter Referenzen.
    function DGet(_Dict : TPdfObj; const _Key : String) : TPdfObj;
    function GetStreamData(_O : TPdfObj; out _Data : TBytes) : Boolean;
    function Catalog : TPdfObj;
    property XrefSections : Integer read FXrefSections;
    property Reconstructed : Boolean read FReconstructed;
    property EncryptRef : TPdfObj read FEncryptRef;
    property Buf : TBytes read FBuf;
  end;

constructor TPdfDocument.Create;
begin
  inherited Create;
  FOwned := TList.Create;
  FXrefSections := 0;
  FReconstructed := False;
end;

destructor TPdfDocument.Destroy;
var
  i : Integer;
begin
  if FOwned <> nil then
  begin
    for i := 0 to FOwned.Count - 1 do
      TPdfObj(FOwned[i]).Free;
    FOwned.Free;
  end;
  inherited;
end;

procedure TPdfDocument.EnsureSize(_Num : Integer);
var
  old, i : Integer;
begin
  if _Num < Length(FXref) then exit;
  old := Length(FXref);
  SetLength(FXref, _Num + 1);
  SetLength(FCache, _Num + 1);
  SetLength(FLoading, _Num + 1);
  SetLength(FObjStmDone, _Num + 1);
  for i := old to _Num do
  begin
    FXref[i].Known := False;
    FCache[i] := nil;
    FLoading[i] := False;
    FObjStmDone[i] := False;
  end;
end;

// Traegt einen xref-Eintrag ein - aber nur, wenn die Objektnummer noch nicht
// bekannt ist. Da die /Prev-Kette vom neuesten zum aeltesten Abschnitt
// durchlaufen wird, gewinnt damit genau die aktuelle Fassung eines Objekts.
// Obergrenze fuer Objektnummern. Jedes Objekt braucht in der Datei Platz -
// eine xref-Subsection wie "8388608 1" in einer 2-KB-Datei ist daher unecht
// und wuerde hier nur mehrere hundert MB an Tabellen erzwingen.
function TPdfDocument.MaxPlausibleObjNum : Integer;
var
  lim : Int64;
begin
  lim := (Int64(Length(FBuf)) div 2) + 1024;
  if lim > 8388608 then
    lim := 8388608;
  Result := Integer(lim);
end;

procedure TPdfDocument.SetEntry(_Num : Integer; _Kind : TPdfXrefKind;
  _Ofs : Int64; _Gen, _StmNum, _StmIdx : Integer);
begin
  if (_Num < 0) or (_Num > MaxPlausibleObjNum) then exit;
  EnsureSize(_Num);
  if FXref[_Num].Known then exit;
  FXref[_Num].Known := True;
  FXref[_Num].Kind := _Kind;
  FXref[_Num].Offset := _Ofs;
  FXref[_Num].Gen := _Gen;
  FXref[_Num].StmNum := _StmNum;
  FXref[_Num].StmIdx := _StmIdx;
end;

procedure TPdfDocument.NoteTrailer(_Dict : TPdfObj);
var
  o : TPdfObj;
begin
  if (_Dict = nil) or not _Dict.IsDictLike then exit;
  if FRootRef = nil then
  begin
    o := _Dict.RawGet('Root');
    if o <> nil then FRootRef := o;
  end;
  if FEncryptRef = nil then
  begin
    o := _Dict.RawGet('Encrypt');
    if o <> nil then FEncryptRef := o;
  end;
end;

function TPdfDocument.ParseClassicXref(_Lex : TPdfLexer; out _Trailer : TPdfObj) : Boolean;
var
  tok : AnsiString;
  startNum, cnt, i, code, gen : Integer;
  ofs : Int64;
  d : Double;
  save : Integer;
begin
  Result := False;
  _Trailer := nil;
  // _Lex steht hinter dem Schluesselwort "xref"
  while True do
  begin
    _Lex.SkipWhite;
    save := _Lex.Pos;
    tok := _Lex.ReadToken;
    if tok = '' then exit;
    if tok = 'trailer' then
    begin
      _Trailer := _Lex.ParseObject(0);
      Result := True;
      exit;
    end;
    // Subsection-Kopf: start count
    Val(String(tok), d, code);
    if code <> 0 then
    begin
      _Lex.Pos := save;
      Result := True;   // Tabelle zu Ende, aber kein trailer gefunden
      exit;
    end;
    startNum := Trunc(d);
    tok := _Lex.ReadToken;
    Val(String(tok), d, code);
    if code <> 0 then exit;
    cnt := Trunc(d);
    if (cnt < 0) or (cnt > 8388608) then exit;
    for i := 0 to cnt - 1 do
    begin
      tok := _Lex.ReadToken;
      Val(String(tok), d, code);
      if code <> 0 then exit;
      ofs := Trunc(d);
      tok := _Lex.ReadToken;
      Val(String(tok), d, code);
      if code <> 0 then exit;
      gen := Trunc(d);
      tok := _Lex.ReadToken;
      if tok = 'n' then
        SetEntry(startNum + i, xkNormal, ofs, gen, -1, -1)
      else if tok = 'f' then
        SetEntry(startNum + i, xkFree, 0, gen, -1, -1)
      else
        exit;
    end;
  end;
end;

function TPdfDocument.ParseXrefStream(_Obj : TPdfObj) : Boolean;
var
  data : TBytes;
  wObj, idxObj, sizeObj, o : TPdfObj;
  w : array[0..2] of Integer;
  i, j, k, rowLen, pos, entries, startNum, cnt, sizeVal : Integer;
  f : array[0..2] of Int64;
  idxPairs : array of Integer;
begin
  Result := False;
  if (_Obj = nil) or (_Obj.Kind <> pokStream) then exit;
  if not GetStreamData(_Obj, data) then exit;

  wObj := DGet(_Obj, 'W');
  if (wObj = nil) or (wObj.Kind <> pokArr) or (wObj.ArrCount < 3) then exit;
  for i := 0 to 2 do
  begin
    o := Resolve(wObj.ArrItem(i));
    if (o = nil) or (o.Kind <> pokNum) then exit;
    w[i] := Integer(o.AsInt);
    if (w[i] < 0) or (w[i] > 8) then exit;
  end;
  rowLen := w[0] + w[1] + w[2];
  if rowLen <= 0 then exit;

  sizeObj := DGet(_Obj, 'Size');
  if (sizeObj <> nil) and (sizeObj.Kind = pokNum) then
    sizeVal := Integer(sizeObj.AsInt)
  else
    sizeVal := Length(data) div rowLen;

  idxObj := DGet(_Obj, 'Index');
  if (idxObj <> nil) and (idxObj.Kind = pokArr) and (idxObj.ArrCount >= 2) then
  begin
    SetLength(idxPairs, idxObj.ArrCount);
    for i := 0 to idxObj.ArrCount - 1 do
    begin
      o := Resolve(idxObj.ArrItem(i));
      if (o = nil) or (o.Kind <> pokNum) then exit;
      idxPairs[i] := Integer(o.AsInt);
    end;
  end
  else
  begin
    SetLength(idxPairs, 2);
    idxPairs[0] := 0;
    idxPairs[1] := sizeVal;
  end;

  pos := 0;
  k := 0;
  while k + 1 < Length(idxPairs) do
  begin
    startNum := idxPairs[k];
    cnt := idxPairs[k + 1];
    Inc(k, 2);
    if (cnt < 0) or (cnt > 8388608) then exit;
    for entries := 0 to cnt - 1 do
    begin
      if pos + rowLen > Length(data) then
      begin
        Result := True;   // vorzeitiges Ende tolerieren
        exit;
      end;
      for i := 0 to 2 do
      begin
        f[i] := 0;
        for j := 0 to w[i] - 1 do
        begin
          f[i] := (f[i] shl 8) or data[pos];
          Inc(pos);
        end;
      end;
      // Feld 1 fehlt (w[0]=0) -> Typ 1 laut Spezifikation
      if w[0] = 0 then f[0] := 1;
      case f[0] of
        0 : SetEntry(startNum + entries, xkFree, 0, Integer(f[2]), -1, -1);
        1 : SetEntry(startNum + entries, xkNormal, f[1], Integer(f[2]), -1, -1);
        2 : SetEntry(startNum + entries, xkCompressed, 0, 0, Integer(f[1]), Integer(f[2]));
      end;
    end;
  end;
  Result := True;
end;

function TPdfDocument.ParseXrefChain(_StartOfs : Int64) : Boolean;
var
  ofs : Int64;
  visited : array of Int64;
  i : Integer;
  seen : Boolean;
  lex : TPdfLexer;
  trailer, obj, prevObj, stmObj : TPdfObj;
  tok : AnsiString;
begin
  Result := False;
  ofs := _StartOfs;
  SetLength(visited, 0);

  while (ofs > 0) and (ofs < Length(FBuf)) do
  begin
    seen := False;
    for i := 0 to Length(visited) - 1 do
      if visited[i] = ofs then
      begin
        seen := True;
        break;
      end;
    if seen then break;                       // Zyklus in der /Prev-Kette
    if Length(visited) > 1024 then break;     // Notbremse
    SetLength(visited, Length(visited) + 1);
    visited[Length(visited) - 1] := ofs;

    lex := TPdfLexer.Create(FBuf, FOwned);
    try
      lex.Pos := Integer(ofs);
      lex.SkipWhite;
      trailer := nil;

      if MatchAt(FBuf, lex.Pos, 'xref') then
      begin
        lex.Pos := lex.Pos + 4;
        if not ParseClassicXref(lex, trailer) then break;
        Inc(FXrefSections);
        Result := True;
        NoteTrailer(trailer);

        // Hybrid-Datei: zusaetzlicher XRef-Stream mit denselben Objekten
        if (trailer <> nil) and trailer.IsDictLike then
        begin
          obj := trailer.RawGet('XRefStm');
          // Offset gegen die Dateigrenzen pruefen - ein manipulierter Wert
          // (etwa -1) wuerde sonst als Lexerposition landen.
          if (obj <> nil) and (obj.Kind = pokNum) and
             (obj.AsInt > 0) and (obj.AsInt < Length(FBuf)) then
          begin
            stmObj := nil;
            try
              lex.Pos := Integer(obj.AsInt);
              lex.SkipWhite;
              lex.ReadToken;   // Objektnummer
              lex.ReadToken;   // Generation
              tok := lex.ReadToken;
              if tok = 'obj' then
                stmObj := lex.ParseObject(0);
            except
              stmObj := nil;
            end;
            if (stmObj <> nil) and (stmObj.Kind = pokStream) then
              ParseXrefStream(stmObj);
          end;
        end;
      end
      else
      begin
        // XRef-Stream: "N G obj << ... >> stream"
        lex.ReadToken;
        lex.ReadToken;
        tok := lex.ReadToken;
        if tok <> 'obj' then break;
        obj := lex.ParseObject(0);
        if (obj = nil) or (obj.Kind <> pokStream) then break;
        if not ParseXrefStream(obj) then break;
        Inc(FXrefSections);
        Result := True;
        NoteTrailer(obj);
        trailer := obj;
      end;

      // Naechster Abschnitt
      ofs := 0;
      if (trailer <> nil) and trailer.IsDictLike then
      begin
        prevObj := trailer.RawGet('Prev');
        if (prevObj <> nil) and (prevObj.Kind = pokNum) then
          ofs := Trunc(prevObj.NumVal);
      end;
    finally
      lex.Free;
    end;
  end;
end;

// Notfallpfad: Objekttabelle durch einen Scan ueber "N G obj" rekonstruieren.
// Hier gewinnt die LETZTE Definition - was der Reihenfolge in der Datei
// entspricht, aber keine Garantie gegen ueberschriebene Vorversionen bietet.
procedure TPdfDocument.Reconstruct;
var
  i, j, k, numStart, num, gen, code : Integer;
  d : Double;
  s : AnsiString;
  lex : TPdfLexer;
  trailerPos : Integer;
  trailerObj : TPdfObj;
begin
  FReconstructed := True;
  i := 0;
  while i < Length(FBuf) - 3 do
  begin
    if (FBuf[i] = Byte('o')) and MatchAt(FBuf, i, 'obj') and
       ((i + 3 >= Length(FBuf)) or not IsPdfRegular(FBuf[i + 3])) then
    begin
      // Rueckwaerts: Generation und Objektnummer
      j := i - 1;
      while (j >= 0) and IsPdfWhite(FBuf[j]) do Dec(j);
      k := j;
      while (k >= 0) and (FBuf[k] >= 48) and (FBuf[k] <= 57) do Dec(k);
      if k = j then
      begin
        Inc(i);
        continue;
      end;
      s := BytesToAnsiStr(FBuf, k + 1, j - k);
      Val(String(s), d, code);
      if code <> 0 then
      begin
        Inc(i);
        continue;
      end;
      gen := Trunc(d);
      j := k;
      while (j >= 0) and IsPdfWhite(FBuf[j]) do Dec(j);
      k := j;
      while (k >= 0) and (FBuf[k] >= 48) and (FBuf[k] <= 57) do Dec(k);
      if k = j then
      begin
        Inc(i);
        continue;
      end;
      s := BytesToAnsiStr(FBuf, k + 1, j - k);
      Val(String(s), d, code);
      if code <> 0 then
      begin
        Inc(i);
        continue;
      end;
      num := Trunc(d);
      numStart := k + 1;
      if (num >= 0) and (num <= MaxPlausibleObjNum) then
      begin
        EnsureSize(num);
        // Spaetere Definition ueberschreibt die fruehere
        FXref[num].Known := True;
        FXref[num].Kind := xkNormal;
        FXref[num].Offset := numStart;
        FXref[num].Gen := gen;
        FCache[num] := nil;
      end;
      Inc(i, 3);
      continue;
    end;
    Inc(i);
  end;

  // Trailer bzw. Katalog suchen, falls /Root noch fehlt
  if FRootRef = nil then
  begin
    trailerPos := LastIndexOfBytes(FBuf, 'trailer', Length(FBuf));
    while trailerPos >= 0 do
    begin
      lex := TPdfLexer.Create(FBuf, FOwned);
      try
        lex.Pos := trailerPos + 7;
        trailerObj := lex.ParseObject(0);
        NoteTrailer(trailerObj);
      finally
        lex.Free;
      end;
      if FRootRef <> nil then break;
      if trailerPos = 0 then break;
      trailerPos := LastIndexOfBytes(FBuf, 'trailer', trailerPos - 1);
    end;
  end;
end;

procedure TPdfDocument.LoadObjStm(_StmNum : Integer);
var
  stm : TPdfObj;
  data : TBytes;
  nObj, firstObj : TPdfObj;
  n, first, i, num, ofs, code : Integer;
  d : Double;
  hdrLex, objLex : TPdfLexer;
  tok : AnsiString;
  nums, offs : array of Integer;
  parsed : TPdfObj;
begin
  if (_StmNum < 0) or (_StmNum >= Length(FObjStmDone)) then exit;
  if FObjStmDone[_StmNum] then exit;
  FObjStmDone[_StmNum] := True;

  stm := GetObject(_StmNum);
  if (stm = nil) or (stm.Kind <> pokStream) then exit;
  if not GetStreamData(stm, data) then exit;

  nObj := DGet(stm, 'N');
  firstObj := DGet(stm, 'First');
  if (nObj = nil) or (firstObj = nil) then exit;
  n := Integer(nObj.AsInt);
  first := Integer(firstObj.AsInt);
  if (n <= 0) or (n > 100000) or (first < 0) or (first > Length(data)) then exit;

  SetLength(nums, n);
  SetLength(offs, n);
  hdrLex := TPdfLexer.Create(data, FOwned);
  try
    for i := 0 to n - 1 do
    begin
      tok := hdrLex.ReadToken;
      Val(String(tok), d, code);
      if code <> 0 then exit;
      nums[i] := Trunc(d);
      tok := hdrLex.ReadToken;
      Val(String(tok), d, code);
      if code <> 0 then exit;
      offs[i] := Trunc(d);
    end;
  finally
    hdrLex.Free;
  end;

  objLex := TPdfLexer.Create(data, FOwned);
  try
    for i := 0 to n - 1 do
    begin
      num := nums[i];
      ofs := first + offs[i];
      if (num < 0) or (num > 8388608) then continue;
      if (ofs < 0) or (ofs >= Length(data)) then continue;
      EnsureSize(num);
      // Nur eintragen, wenn der xref dieses Objekt auch wirklich hier erwartet
      if not FXref[num].Known then continue;
      if FXref[num].Kind <> xkCompressed then continue;
      if FXref[num].StmNum <> _StmNum then continue;
      // Das dritte Feld eines Typ-2-xref-Eintrags benennt den Index im Object
      // Stream. Stimmt er nicht mit der Position im Header ueberein, ist der
      // Stream manipuliert - dann nicht uebernehmen.
      if FXref[num].StmIdx <> i then continue;
      if FCache[num] <> nil then continue;
      objLex.Pos := ofs;
      parsed := objLex.ParseObject(0);
      if parsed <> nil then
        FCache[num] := parsed;
    end;
  finally
    objLex.Free;
  end;
end;

function TPdfDocument.GetObject(_Num : Integer) : TPdfObj;
var
  lex : TPdfLexer;
  tok : AnsiString;
  d : Double;
  code, gotNum : Integer;
begin
  Result := nil;
  if (_Num < 0) or (_Num >= Length(FXref)) then exit;
  if not FXref[_Num].Known then exit;
  if FCache[_Num] <> nil then
  begin
    Result := FCache[_Num];
    exit;
  end;
  if FLoading[_Num] then exit;   // Zyklus
  FLoading[_Num] := True;
  try
    case FXref[_Num].Kind of
      xkFree : exit;
      xkCompressed :
        begin
          LoadObjStm(FXref[_Num].StmNum);
          Result := FCache[_Num];
          exit;
        end;
      xkNormal :
        begin
          if (FXref[_Num].Offset < 0) or (FXref[_Num].Offset >= Length(FBuf)) then exit;
          lex := TPdfLexer.Create(FBuf, FOwned);
          try
            lex.Pos := Integer(FXref[_Num].Offset);
            tok := lex.ReadToken;
            Val(String(tok), d, code);
            if code <> 0 then exit;
            gotNum := Trunc(d);
            // Objektnummer muss passen - sonst zeigt der xref ins Leere
            if gotNum <> _Num then exit;
            lex.ReadToken;             // Generation
            tok := lex.ReadToken;
            if tok <> 'obj' then exit;
            Result := lex.ParseObject(0);
            FCache[_Num] := Result;
          finally
            lex.Free;
          end;
        end;
    end;
  finally
    FLoading[_Num] := False;
  end;
end;

function TPdfDocument.Resolve(_O : TPdfObj) : TPdfObj;
var
  depth : Integer;
begin
  Result := _O;
  depth := 0;
  while (Result <> nil) and (Result.Kind = pokRef) do
  begin
    Inc(depth);
    if depth > 32 then
    begin
      Result := nil;
      exit;
    end;
    Result := GetObject(Result.RefNum);
  end;
end;

function TPdfDocument.DGet(_Dict : TPdfObj; const _Key : String) : TPdfObj;
begin
  Result := nil;
  if _Dict = nil then exit;
  Result := Resolve(_Dict.RawGet(_Key));
end;

function TPdfDocument.Catalog : TPdfObj;
begin
  Result := Resolve(FRootRef);
  if (Result <> nil) and not Result.IsDictLike then
    Result := nil;
end;

function TPdfDocument.GetStreamData(_O : TPdfObj; out _Data : TBytes) : Boolean;
var
  lenObj, filterObj, parmObj, f, p, o : TPdfObj;
  rawLen, endPos, i, nFilters : Integer;
  rawLen64 : Int64;
  raw, decoded : TBytes;
  fname : String;
  pred, colors, bpc, cols, early : Integer;
  ok : Boolean;
begin
  Result := False;
  SetLength(_Data, 0);
  if (_O = nil) or (_O.Kind <> pokStream) then exit;
  if (_O.StreamPos < 0) or (_O.StreamPos > Length(FBuf)) then exit;

  // Laenge aus /Length, notfalls ueber die Suche nach "endstream".
  // Die Pruefung laeuft ueber Int64: ein manipuliertes /Length nahe MaxInt
  // wuerde bei Integer-Arithmetik ueberlaufen, die Bereichspruefung
  // unterlaufen und danach zu einem negativen Pufferindex fuehren.
  rawLen64 := -1;
  lenObj := DGet(_O, 'Length');
  if (lenObj <> nil) and (lenObj.Kind = pokNum) then
    rawLen64 := lenObj.AsInt;

  // Subtraktion statt Addition - sie kann nicht ueberlaufen, weil
  // StreamPos bereits als <= Length(FBuf) geprueft ist.
  if (rawLen64 < 0) or (rawLen64 > Int64(Length(FBuf)) - _O.StreamPos) then
    rawLen := -1
  else
  begin
    rawLen := Integer(rawLen64);
    // Plausibilitaet: hinter den Daten muss (nach Whitespace) endstream stehen
    i := _O.StreamPos + rawLen;
    while (i < Length(FBuf)) and IsPdfWhite(FBuf[i]) do Inc(i);
    if not MatchAt(FBuf, i, 'endstream') then
      rawLen := -1;
  end;

  if rawLen < 0 then
  begin
    endPos := IndexOfBytes(FBuf, 'endstream', _O.StreamPos);
    if endPos < 0 then exit;
    rawLen := endPos - _O.StreamPos;
    // abschliessendes EOL gehoert nicht zu den Daten
    if (rawLen > 0) and (FBuf[_O.StreamPos + rawLen - 1] = 10) then Dec(rawLen);
    if (rawLen > 0) and (FBuf[_O.StreamPos + rawLen - 1] = 13) then Dec(rawLen);
  end;
  if rawLen < 0 then exit;

  SetLength(raw, rawLen);
  if rawLen > 0 then
    Move(FBuf[_O.StreamPos], raw[0], rawLen);

  // Filterkette anwenden
  filterObj := DGet(_O, 'Filter');
  parmObj := DGet(_O, 'DecodeParms');
  if parmObj = nil then
    parmObj := DGet(_O, 'DP');

  if filterObj = nil then
  begin
    _Data := raw;
    Result := True;
    exit;
  end;

  if filterObj.Kind = pokName then
    nFilters := 1
  else if filterObj.Kind = pokArr then
    nFilters := filterObj.ArrCount
  else
    exit;

  for i := 0 to nFilters - 1 do
  begin
    if filterObj.Kind = pokName then
      f := filterObj
    else
      f := Resolve(filterObj.ArrItem(i));
    if (f = nil) or (f.Kind <> pokName) then exit;
    fname := f.NameVal;

    if (parmObj <> nil) and (parmObj.Kind = pokArr) then
      p := Resolve(parmObj.ArrItem(i))
    else if (parmObj <> nil) and parmObj.IsDictLike and (i = 0) then
      p := parmObj
    else
      p := nil;

    ok := False;
    if (fname = 'FlateDecode') or (fname = 'Fl') then
      ok := FilterFlate(raw, decoded)
    else if (fname = 'LZWDecode') or (fname = 'LZW') then
    begin
      early := 1;
      if (p <> nil) and p.IsDictLike then
      begin
        o := DGet(p, 'EarlyChange');
        if (o <> nil) and (o.Kind = pokNum) then early := Integer(o.AsInt);
      end;
      ok := FilterLZW(raw, early, decoded);
    end
    else if (fname = 'ASCIIHexDecode') or (fname = 'AHx') then
      ok := FilterASCIIHex(raw, decoded)
    else if (fname = 'ASCII85Decode') or (fname = 'A85') then
      ok := FilterASCII85(raw, decoded)
    else if (fname = 'RunLengthDecode') or (fname = 'RL') then
      ok := FilterRunLength(raw, decoded)
    else if (fname = 'Crypt') then
    begin
      // Identity-Crypt ist ein Durchreicher, alles andere koennen wir nicht.
      decoded := raw;
      ok := True;
    end
    else
      exit;   // DCTDecode/JPXDecode etc. - kein Rechnungs-XML

    if not ok then exit;
    raw := decoded;

    // Praediktor nach diesem Filter
    if (p <> nil) and p.IsDictLike then
    begin
      pred := 1; colors := 1; bpc := 8; cols := 1;
      o := DGet(p, 'Predictor');
      if (o <> nil) and (o.Kind = pokNum) then pred := Integer(o.AsInt);
      if pred > 1 then
      begin
        o := DGet(p, 'Colors');
        if (o <> nil) and (o.Kind = pokNum) then colors := Integer(o.AsInt);
        o := DGet(p, 'BitsPerComponent');
        if (o <> nil) and (o.Kind = pokNum) then bpc := Integer(o.AsInt);
        o := DGet(p, 'Columns');
        if (o <> nil) and (o.Kind = pokNum) then cols := Integer(o.AsInt);
        if not ApplyPredictor(raw, pred, colors, bpc, cols, decoded) then exit;
        raw := decoded;
      end;
    end;
  end;

  _Data := raw;
  Result := True;
end;

function TPdfDocument.Load(_Stream : TStream; out _Err : String) : Boolean;
var
  n, sxPos, i, code : Integer;
  tok : AnsiString;
  d : Double;
  startOfs : Int64;
  lex : TPdfLexer;
  obj, typeObj : TPdfObj;
  headerOk : Boolean;
begin
  Result := False;
  _Err := '';
  n := _Stream.Size - _Stream.Position;
  if n <= 0 then
  begin
    _Err := 'Leerer Datenstrom';
    exit;
  end;
  if n > 512 * 1024 * 1024 then
  begin
    _Err := 'Datei zu gross';
    exit;
  end;
  SetLength(FBuf, n);
  _Stream.ReadBuffer(FBuf[0], n);

  // %PDF- darf laut Spezifikation innerhalb der ersten 1024 Bytes stehen
  headerOk := False;
  for i := 0 to Min(1024, Length(FBuf) - 5) do
    if MatchAt(FBuf, i, '%PDF-') then
    begin
      headerOk := True;
      break;
    end;
  if not headerOk then
  begin
    _Err := 'Keine PDF-Datei (Kennung %PDF- fehlt)';
    exit;
  end;

  // Letztes startxref suchen und der /Prev-Kette folgen
  startOfs := 0;
  sxPos := LastIndexOfBytes(FBuf, 'startxref', Length(FBuf));
  if sxPos >= 0 then
  begin
    lex := TPdfLexer.Create(FBuf, FOwned);
    try
      lex.Pos := sxPos + 9;
      tok := lex.ReadToken;
      Val(String(tok), d, code);
      if code = 0 then
        startOfs := Trunc(d);
    finally
      lex.Free;
    end;
  end;

  if startOfs > 0 then
  begin
    try
      ParseXrefChain(startOfs);
    except
      on E : Exception do
        FRootRef := nil;
    end;
  end;

  // Kein brauchbarer Katalog -> Objekttabelle rekonstruieren
  if Catalog = nil then
  begin
    try
      Reconstruct;
    except
      on E : Exception do
      begin
        _Err := 'PDF-Struktur unlesbar: ' + E.Message;
        exit;
      end;
    end;
    if Catalog = nil then
    begin
      // Letzter Versuch: irgendein Objekt mit /Type /Catalog
      for i := 0 to Length(FXref) - 1 do
      begin
        if not FXref[i].Known then
          Continue;
        try
          obj := GetObject(i);
          if (obj = nil) or not obj.IsDictLike then
            Continue;
          typeObj := DGet(obj, 'Type');
          if (typeObj <> nil) and (typeObj.Kind = pokName) and
             (typeObj.NameVal = 'Catalog') then
          begin
            FRootRef := obj;
            break;
          end;
        except
          // defektes Objekt ueberspringen
        end;
      end;
    end;
    if Catalog = nil then
    begin
      _Err := 'Kein PDF-Katalog gefunden - vermutlich keine PDF-Datei';
      exit;
    end;
  end;

  Result := True;
end;

//==============================================================================
// Textstrings, XML-Erkennung
//==============================================================================

// PDF-Textstring nach Delphi/FPC-String. /UF ist UTF-16BE mit BOM, /F liegt in
// PDFDocEncoding vor - fuer Dateinamen wie "factur-x.xml" ist das ASCII.
function PdfTextToString(const _B : TBytes) : String;
var
  i, n : Integer;
  w : Word;
  res : String;
begin
  res := '';
  n := Length(_B);
  if (n >= 2) and (_B[0] = $FE) and (_B[1] = $FF) then
  begin
    i := 2;
    while i + 1 < n do
    begin
      w := (Word(_B[i]) shl 8) or _B[i + 1];
      Inc(i, 2);
      if w = 0 then break;
      res := res + Char(w);
    end;
  end
  else if (n >= 2) and (_B[0] = $FF) and (_B[1] = $FE) then
  begin
    i := 2;
    while i + 1 < n do
    begin
      w := (Word(_B[i + 1]) shl 8) or _B[i];
      Inc(i, 2);
      if w = 0 then break;
      res := res + Char(w);
    end;
  end
  else
  begin
    for i := 0 to n - 1 do
    begin
      if _B[i] = 0 then continue;
      res := res + Char(_B[i]);
    end;
  end;
  Result := res;
end;

// Liefert den Lokalnamen des XML-Wurzelelements oder '' wenn es kein XML ist.
// Arbeitet direkt auf den Bytes, damit kein XML-Parser noetig ist (und damit
// insbesondere keine externen Entitaeten aufgeloest werden koennen).
function DetectXmlRoot(const _Data : TBytes) : String;
var
  i, limit, start : Integer;
  s : AnsiString;
  p : Integer;
begin
  Result := '';
  limit := Min(Length(_Data), 65536);
  i := 0;
  // BOM
  if (limit >= 3) and (_Data[0] = $EF) and (_Data[1] = $BB) and (_Data[2] = $BF) then
    i := 3;

  while i < limit do
  begin
    while (i < limit) and IsPdfWhite(_Data[i]) do Inc(i);
    if i >= limit then exit;
    if _Data[i] <> Byte('<') then exit;

    if MatchAt(_Data, i, '<?') then
    begin
      p := IndexOfBytes(_Data, '?>', i);
      if (p < 0) or (p >= limit) then exit;
      i := p + 2;
      continue;
    end;
    if MatchAt(_Data, i, '<!--') then
    begin
      p := IndexOfBytes(_Data, '-->', i);
      if p < 0 then exit;
      i := p + 3;
      // Kommentare koennen laenger als das Limit sein
      if i > limit then limit := Min(Length(_Data), i + 4096);
      continue;
    end;
    if MatchAt(_Data, i, '<!') then
    begin
      // DOCTYPE - bis zum schliessenden > ausserhalb einer internen Teilmenge
      Inc(i, 2);
      while i < Length(_Data) do
      begin
        if _Data[i] = Byte('[') then
        begin
          p := IndexOfBytes(_Data, ']', i);
          if p < 0 then exit;
          i := p + 1;
          continue;
        end;
        if _Data[i] = Byte('>') then
        begin
          Inc(i);
          break;
        end;
        Inc(i);
      end;
      if i > limit then limit := Min(Length(_Data), i + 4096);
      continue;
    end;

    // Wurzelelement
    Inc(i);
    start := i;
    while (i < Length(_Data)) and (_Data[i] <> Byte(' ')) and (_Data[i] <> 9) and
          (_Data[i] <> 10) and (_Data[i] <> 13) and (_Data[i] <> Byte('>')) and
          (_Data[i] <> Byte('/')) do
      Inc(i);
    s := BytesToAnsiStr(_Data, start, i - start);
    // Namensraumpraefix abschneiden
    p := System.Pos(AnsiString(':'), s);
    if p > 0 then
      s := Copy(s, p + 1, Length(s) - p);
    Result := String(s);
    exit;
  end;
end;

// Die von ZUGFeRD, Factur-X, XRechnung und Order-X spezifizierten Dateinamen.
// Der Vergleich ist bewusst ohne Ruecksicht auf Gross-/Kleinschreibung: real
// kommen sowohl zugferd-invoice.xml als auch ZUGFeRD-invoice.xml vor.
function IsSpecifiedInvoiceName(const _Name : String) : Boolean;
var
  n : String;
begin
  n := LowerCase(Trim(_Name));
  Result := (n = 'factur-x.xml') or
            (n = 'zugferd-invoice.xml') or
            (n = 'xrechnung.xml') or
            (n = 'order-x.xml') or
            (n = 'cii.xml');
end;

// Wurzelelemente einer CII- oder UBL-Rechnung.
function IsInvoiceRootElement(const _Root : String) : Boolean;
begin
  Result := (_Root = 'CrossIndustryInvoice') or
            (_Root = 'CrossIndustryDocument') or
            (_Root = 'Invoice') or
            (_Root = 'CreditNote');
end;

//==============================================================================
// TXRechnungPdfAttachment / -List
//==============================================================================

function TXRechnungPdfAttachment.GetSize : Integer;
begin
  Result := Length(FData);
end;

constructor TXRechnungPdfAttachmentList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TXRechnungPdfAttachmentList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TXRechnungPdfAttachmentList.Clear;
var
  i : Integer;
begin
  for i := 0 to FList.Count - 1 do
    TXRechnungPdfAttachment(FList[i]).Free;
  FList.Clear;
end;

function TXRechnungPdfAttachmentList.Add(_Item : TXRechnungPdfAttachment) : Integer;
begin
  Result := FList.Add(_Item);
end;

function TXRechnungPdfAttachmentList.GetCount : Integer;
begin
  Result := FList.Count;
end;

function TXRechnungPdfAttachmentList.GetItem(_Index : Integer) : TXRechnungPdfAttachment;
begin
  if (_Index >= 0) and (_Index < FList.Count) then
    Result := TXRechnungPdfAttachment(FList[_Index])
  else
    Result := nil;
end;

//==============================================================================
// Einsammeln der Anhaenge
//==============================================================================

type
  TPdfAttachmentCollector = class(TObject)
  private
    FDoc : TPdfDocument;
    FList : TXRechnungPdfAttachmentList;
    FSeen : TList;          // bereits verarbeitete Stream-Objekte (TPdfObj)
    FNodeCount : Integer;
    FTotalBytes : Int64;    // Summe aller bisher uebernommenen Anhaenge
    function AlreadySeen(_O : TPdfObj) : Boolean;
    procedure AddFilespec(_Spec : TPdfObj);
    procedure WalkNameTree(_Node : TPdfObj; _Depth : Integer);
  public
    constructor Create(_Doc : TPdfDocument; _List : TXRechnungPdfAttachmentList);
    destructor Destroy; override;
    procedure Collect;
  end;

constructor TPdfAttachmentCollector.Create(_Doc : TPdfDocument;
  _List : TXRechnungPdfAttachmentList);
begin
  inherited Create;
  FDoc := _Doc;
  FList := _List;
  FSeen := TList.Create;
  FNodeCount := 0;
  FTotalBytes := 0;
end;

destructor TPdfAttachmentCollector.Destroy;
begin
  FSeen.Free;
  inherited;
end;

function TPdfAttachmentCollector.AlreadySeen(_O : TPdfObj) : Boolean;
begin
  Result := FSeen.IndexOf(_O) >= 0;
  if not Result then
    FSeen.Add(_O);
end;

procedure TPdfAttachmentCollector.AddFilespec(_Spec : TPdfObj);
var
  ef, stm, o : TPdfObj;
  att : TXRechnungPdfAttachment;
  data : TBytes;
  nameF, nameUF : String;
begin
  if (_Spec = nil) or not _Spec.IsDictLike then exit;

  ef := FDoc.DGet(_Spec, 'EF');
  if (ef = nil) or not ef.IsDictLike then exit;

  // /F ist der Regelfall, /UF die Unicode-Variante desselben Streams
  stm := FDoc.DGet(ef, 'F');
  if stm = nil then
    stm := FDoc.DGet(ef, 'UF');
  if stm = nil then
    stm := FDoc.DGet(ef, 'DOS');
  if (stm = nil) or (stm.Kind <> pokStream) then exit;

  if AlreadySeen(stm) then exit;
  if FList.Count >= XRechnungPdfMaxAttachments then exit;
  if not FDoc.GetStreamData(stm, data) then exit;
  // Gesamtbudget ueber alle Anhaenge einhalten
  if FTotalBytes + Length(data) > XRechnungPdfMaxTotalSize then exit;
  FTotalBytes := FTotalBytes + Length(data);

  att := TXRechnungPdfAttachment.Create;
  try
    // Dateiname: /UF hat Vorrang vor /F
    nameF := '';
    nameUF := '';
    o := FDoc.DGet(_Spec, 'F');
    if (o <> nil) and (o.Kind = pokStr) then nameF := PdfTextToString(o.StrVal);
    o := FDoc.DGet(_Spec, 'UF');
    if (o <> nil) and (o.Kind = pokStr) then nameUF := PdfTextToString(o.StrVal);
    if nameUF <> '' then
      att.FileName := nameUF
    else
      att.FileName := nameF;

    o := FDoc.DGet(_Spec, 'AFRelationship');
    if (o <> nil) and (o.Kind = pokName) then
      att.Relationship := o.NameVal;

    o := FDoc.DGet(_Spec, 'Desc');
    if (o <> nil) and (o.Kind = pokStr) then
      att.Description := PdfTextToString(o.StrVal);

    o := FDoc.DGet(stm, 'Subtype');
    if (o <> nil) and (o.Kind = pokName) then
      att.MimeType := o.NameVal;

    att.Data := data;
    att.RootElement := DetectXmlRoot(data);
    if IsInvoiceRootElement(att.RootElement) then
      att.Kind := pakInvoice
    else if att.RootElement <> '' then
      att.Kind := pakOtherXml
    else
      att.Kind := pakOther;

    FList.Add(att);
    att := nil;
  finally
    att.Free;
  end;
end;

procedure TPdfAttachmentCollector.WalkNameTree(_Node : TPdfObj; _Depth : Integer);
var
  namesArr, kidsArr, entry : TPdfObj;
  i : Integer;
begin
  if (_Node = nil) or not _Node.IsDictLike then exit;
  if _Depth > XRechnungPdfMaxDepth then exit;
  Inc(FNodeCount);
  if FNodeCount > 100000 then exit;

  // Blatt: /Names [ (name) spec (name) spec ... ]
  namesArr := FDoc.DGet(_Node, 'Names');
  if (namesArr <> nil) and (namesArr.Kind = pokArr) then
  begin
    i := 0;
    while i + 1 < namesArr.ArrCount do
    begin
      entry := FDoc.Resolve(namesArr.ArrItem(i + 1));
      AddFilespec(entry);
      Inc(i, 2);
    end;
  end;

  // Zwischenknoten: /Kids [ node node ... ]
  kidsArr := FDoc.DGet(_Node, 'Kids');
  if (kidsArr <> nil) and (kidsArr.Kind = pokArr) then
    for i := 0 to kidsArr.ArrCount - 1 do
      WalkNameTree(FDoc.Resolve(kidsArr.ArrItem(i)), _Depth + 1);
end;

procedure TPdfAttachmentCollector.Collect;
var
  cat, names, ef, af : TPdfObj;
  i : Integer;
begin
  cat := FDoc.Catalog;
  if cat = nil then exit;

  // Regelweg: /Names /EmbeddedFiles (Namensbaum)
  names := FDoc.DGet(cat, 'Names');
  if (names <> nil) and names.IsDictLike then
  begin
    ef := FDoc.DGet(names, 'EmbeddedFiles');
    if (ef <> nil) and ef.IsDictLike then
      WalkNameTree(ef, 0);
  end;

  // Ergaenzend: /AF am Katalog (PDF/A-3). Dubletten filtert AlreadySeen.
  af := FDoc.DGet(cat, 'AF');
  if (af <> nil) and (af.Kind = pokArr) then
    for i := 0 to af.ArrCount - 1 do
      AddFilespec(FDoc.Resolve(af.ArrItem(i)));
end;

//==============================================================================
// TXRechnungPdfExtractor
//==============================================================================

class function TXRechnungPdfExtractor.IsPdfFile(const _PdfFilename : String) : Boolean;
var
  fs : TFileStream;
  hdr : array[0..4] of Byte;
  got : Integer;
begin
  Result := False;
  if not FileExists(_PdfFilename) then exit;
  try
    fs := TFileStream.Create(_PdfFilename, fmOpenRead or fmShareDenyWrite);
    try
      got := fs.Read(hdr, 5);
      Result := (got = 5) and (hdr[0] = Byte('%')) and (hdr[1] = Byte('P')) and
                (hdr[2] = Byte('D')) and (hdr[3] = Byte('F')) and (hdr[4] = Byte('-'));
    finally
      fs.Free;
    end;
  except
    Result := False;
  end;
end;

class function TXRechnungPdfExtractor.IsPdfStream(_Stream : TStream) : Boolean;
var
  save : Int64;
  hdr : array[0..4] of Byte;
  got : Integer;
begin
  Result := False;
  if _Stream = nil then exit;
  save := _Stream.Position;
  try
    got := _Stream.Read(hdr, 5);
    Result := (got = 5) and (hdr[0] = Byte('%')) and (hdr[1] = Byte('P')) and
              (hdr[2] = Byte('D')) and (hdr[3] = Byte('F')) and (hdr[4] = Byte('-'));
  finally
    _Stream.Position := save;
  end;
end;

class function TXRechnungPdfExtractor.ExtractAllFromStream(_Stream : TStream;
  _List : TXRechnungPdfAttachmentList; out _Info : TXRechnungPdfExtractInfo) : Boolean;
var
  doc : TPdfDocument;
  coll : TPdfAttachmentCollector;
  err : String;
begin
  Result := False;
  _Info.UsedReconstruction := False;
  _Info.XrefSections := 0;
  _Info.Encrypted := False;
  _Info.PdfVersion := '';
  _Info.Error := '';

  if _List = nil then
  begin
    _Info.Error := 'Keine Ergebnisliste uebergeben';
    exit;
  end;
  if _Stream = nil then
  begin
    _Info.Error := 'Kein Datenstrom uebergeben';
    exit;
  end;

  doc := TPdfDocument.Create;
  try
    try
      if not doc.Load(_Stream, err) then
      begin
        _Info.Error := err;
        exit;
      end;
    except
      on E : Exception do
      begin
        _Info.Error := 'PDF nicht lesbar: ' + E.Message;
        exit;
      end;
    end;

    _Info.UsedReconstruction := doc.Reconstructed;
    _Info.XrefSections := doc.XrefSections;

    // PDF-Version aus dem Header
    if Length(doc.Buf) >= 8 then
      _Info.PdfVersion := String(BytesToAnsiStr(doc.Buf, 5, 3));

    // Verschluesselung: PDF/A-3 verbietet sie. Wir raten nicht, sondern melden.
    if doc.EncryptRef <> nil then
    begin
      _Info.Encrypted := True;
      _Info.Error := 'PDF ist verschluesselt - Anhaenge koennen nicht gelesen werden';
      exit;
    end;

    coll := TPdfAttachmentCollector.Create(doc, _List);
    try
      try
        coll.Collect;
      except
        on E : Exception do
        begin
          _Info.Error := 'Fehler beim Einsammeln der Anhaenge: ' + E.Message;
          exit;
        end;
      end;
    finally
      coll.Free;
    end;

    Result := True;
  finally
    doc.Free;
  end;
end;

class function TXRechnungPdfExtractor.ExtractAllFromFile(const _PdfFilename : String;
  _List : TXRechnungPdfAttachmentList; out _Info : TXRechnungPdfExtractInfo) : Boolean;
var
  fs : TFileStream;
begin
  Result := False;
  _Info.UsedReconstruction := False;
  _Info.XrefSections := 0;
  _Info.Encrypted := False;
  _Info.PdfVersion := '';
  _Info.Error := '';
  if not FileExists(_PdfFilename) then
  begin
    _Info.Error := 'Datei nicht gefunden: ' + _PdfFilename;
    exit;
  end;
  try
    fs := TFileStream.Create(_PdfFilename, fmOpenRead or fmShareDenyWrite);
  except
    on E : Exception do
    begin
      _Info.Error := 'Datei nicht lesbar: ' + E.Message;
      exit;
    end;
  end;
  try
    Result := ExtractAllFromStream(fs, _List, _Info);
  finally
    fs.Free;
  end;
end;

class function TXRechnungPdfExtractor.FindInvoiceIndex(
  _List : TXRechnungPdfAttachmentList) : Integer;
var
  i, fallback : Integer;
  att : TXRechnungPdfAttachment;
begin
  Result := -1;
  fallback := -1;
  if _List = nil then exit;

  // 1. Spezifizierter Dateiname UND passendes Wurzelelement - der Normalfall.
  for i := 0 to _List.Count - 1 do
  begin
    att := _List[i];
    if (att.Kind = pakInvoice) and IsSpecifiedInvoiceName(att.FileName) then
    begin
      Result := i;
      exit;
    end;
    if (fallback < 0) and (att.Kind = pakInvoice) then
      fallback := i;
  end;

  // 2. Sonst der erste Anhang, dessen Wurzelelement eine Rechnung ist. Der
  //    Dateiname allein reicht bewusst nicht - er ist nur eine Heuristik.
  Result := fallback;
end;

class function TXRechnungPdfExtractor.ExtractInvoiceFromStream(_Stream : TStream;
  out _Xml : TBytes; out _AttachmentName : String;
  out _Info : TXRechnungPdfExtractInfo) : Boolean;
var
  list : TXRechnungPdfAttachmentList;
  idx : Integer;
begin
  Result := False;
  SetLength(_Xml, 0);
  _AttachmentName := '';
  list := TXRechnungPdfAttachmentList.Create;
  try
    if not ExtractAllFromStream(_Stream, list, _Info) then exit;
    idx := FindInvoiceIndex(list);
    if idx < 0 then
    begin
      if list.Count = 0 then
        _Info.Error := 'Das PDF enthaelt keine eingebetteten Dateien'
      else
        _Info.Error := 'Keine eingebettete Rechnung gefunden (' +
                       IntToStr(list.Count) + ' Anhang/Anhaenge vorhanden)';
      exit;
    end;
    _Xml := list[idx].Data;
    _AttachmentName := list[idx].FileName;
    Result := True;
  finally
    list.Free;
  end;
end;

class function TXRechnungPdfExtractor.ExtractInvoiceFromFile(const _PdfFilename : String;
  out _Xml : TBytes; out _AttachmentName : String;
  out _Info : TXRechnungPdfExtractInfo) : Boolean;
var
  fs : TFileStream;
begin
  Result := False;
  SetLength(_Xml, 0);
  _AttachmentName := '';
  _Info.UsedReconstruction := False;
  _Info.XrefSections := 0;
  _Info.Encrypted := False;
  _Info.PdfVersion := '';
  _Info.Error := '';
  if not FileExists(_PdfFilename) then
  begin
    _Info.Error := 'Datei nicht gefunden: ' + _PdfFilename;
    exit;
  end;
  try
    fs := TFileStream.Create(_PdfFilename, fmOpenRead or fmShareDenyWrite);
  except
    on E : Exception do
    begin
      _Info.Error := 'Datei nicht lesbar: ' + E.Message;
      exit;
    end;
  end;
  try
    Result := ExtractInvoiceFromStream(fs, _Xml, _AttachmentName, _Info);
  finally
    fs.Free;
  end;
end;

end.
