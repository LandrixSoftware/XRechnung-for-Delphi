{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de

FreePascal-Test fuer frei gewaehlte Namespace-Praefixe (Issue #92):
  1. Schreibt jedes Golden-File aus ValidXMLExamples\ in zwei Varianten um
     (Tests\FreePascal\out\prefix\), die URIs bleiben unveraendert:
       *-praefix.xml  fremde Praefixe (ubl->n0, cbc->n1, cac->n2,
                      rsm->a, ram->b, udt->c, qdt->d)
       *-default.xml  Wurzel-Namespace als Default-Namespace ohne Praefix
  2. Erwartet fuer jede Variante dieselbe Version wie beim Original, liest sie
     ein, schreibt sie zurueck und vergleicht KANONISCH gegen das Golden-File.
  3. Negativfaelle: SelfBilledInvoice und InvoiceResponse als Wurzelelement
     duerfen nicht als Rechnung erkannt oder geladen werden.

Aufruf: XRechnungPrefixTest.exe [RepoRoot]
  RepoRoot optional; ohne Angabe wird es relativ zur EXE bestimmt
  (Tests\FreePascal\out\ -> 3x hoch).
}

program XRechnungPrefixTest;

{$MODE DELPHIUNICODE}
{$H+}
{$codepage utf8}

uses
  SysUtils, Classes,
  intf.Invoice, intf.XRechnung,
  XRechnungXmlCompare;

const
  PrefixMap : array[0..6, 0..1] of String = (
    ('ubl','n0'), ('cbc','n1'), ('cac','n2'),
    ('rsm','a'), ('ram','b'), ('udt','c'), ('qdt','d'));

var
  RepoRoot, GoldenDir, OutDir : String;
  total, passed, failed : Integer;
  failNames : TStringList;

function ReadText(const _Filename : String) : String;
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename, TEncoding.UTF8);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure WriteText(const _Filename, _Text : String);
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := _Text;
    sl.WriteBOM := false;
    sl.SaveToFile(_Filename, TEncoding.UTF8);
  finally
    sl.Free;
  end;
end;

//Element- und Deklarationspraefix umbenennen; ein leerer neuer Praefix macht
//daraus den Default-Namespace
function RenamePrefix(const _Xml, _Old, _New : String) : String;
begin
  if _New = '' then
  begin
    Result := StringReplace(_Xml, '<' + _Old + ':', '<', [rfReplaceAll]);
    Result := StringReplace(Result, '</' + _Old + ':', '</', [rfReplaceAll]);
    Result := StringReplace(Result, 'xmlns:' + _Old + '=', 'xmlns=', [rfReplaceAll]);
  end else
  begin
    Result := StringReplace(_Xml, '<' + _Old + ':', '<' + _New + ':', [rfReplaceAll]);
    Result := StringReplace(Result, '</' + _Old + ':', '</' + _New + ':', [rfReplaceAll]);
    Result := StringReplace(Result, 'xmlns:' + _Old + '=', 'xmlns:' + _New + '=', [rfReplaceAll]);
  end;
end;

procedure CheckVariant(const _GoldenFile, _VariantFile : String; _Expected : TXRechnungVersion);
var
  inv : TInvoice;
  err, diff, outFile : String;
  ver : TXRechnungVersion;
begin
  Inc(total);
  ver := TXRechnungValidationHelper.GetXRechnungVersion(_VariantFile);
  if ver <> _Expected then
  begin
    Inc(failed);
    failNames.Add(Format('[VERSION] %s -> %d statt %d', [ExtractFileName(_VariantFile), Ord(ver), Ord(_Expected)]));
    exit;
  end;

  outFile := ChangeFileExt(_VariantFile, '.out.xml');
  inv := TInvoice.Create;
  try
    err := '';
    if not TXRechnungInvoiceAdapter.LoadFromFile(inv, _VariantFile, err) then
    begin
      Inc(failed);
      failNames.Add('[LOAD]    ' + ExtractFileName(_VariantFile) + ' -> ' + err);
      exit;
    end;
    try
      TXRechnungInvoiceAdapter.SaveToFile(inv, ver, outFile);
    except
      on E:Exception do
      begin
        Inc(failed);
        failNames.Add('[SAVE]    ' + ExtractFileName(_VariantFile) + ' -> ' + E.ClassName + ' ' + E.Message);
        exit;
      end;
    end;
  finally
    inv.Free;
  end;

  if CompareXmlFiles(_GoldenFile, outFile, diff) then
    Inc(passed)
  else
  begin
    Inc(failed);
    failNames.Add('[DIFF]    ' + ExtractFileName(_VariantFile) + ' -> ' + diff);
  end;
end;

procedure CheckRejected(const _VariantFile : String);
var
  inv : TInvoice;
  err : String;
  ver : TXRechnungVersion;
begin
  Inc(total);
  ver := TXRechnungValidationHelper.GetXRechnungVersion(_VariantFile);
  inv := TInvoice.Create;
  try
    err := '';
    if (ver = XRechnungVersion_Unknown) and
       (not TXRechnungInvoiceAdapter.LoadFromFile(inv, _VariantFile, err)) then
      Inc(passed)
    else
    begin
      Inc(failed);
      failNames.Add('[REJECT]  ' + ExtractFileName(_VariantFile) + ' -> faelschlich als Rechnung erkannt');
    end;
  finally
    inv.Free;
  end;
end;

var
  sr : TSearchRec;
  goldenFile, baseName, xml, variant, rootPrefix, firstUbl : String;
  ver : TXRechnungVersion;
  i : Integer;
begin
  if ParamCount >= 1 then
    RepoRoot := ParamStr(1)
  else
    RepoRoot := ExtractFileDir(ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))));
  RepoRoot := IncludeTrailingPathDelimiter(RepoRoot);

  GoldenDir := RepoRoot + 'ValidXMLExamples' + PathDelim;
  OutDir    := RepoRoot + 'Tests' + PathDelim + 'FreePascal' + PathDelim + 'out' + PathDelim + 'prefix' + PathDelim;

  if not DirectoryExists(GoldenDir) then
  begin
    Writeln('FEHLER: ValidXMLExamples nicht gefunden: ', GoldenDir);
    Halt(2);
  end;

  ForceDirectories(OutDir);
  if FindFirst(OutDir + '*.xml', faAnyFile, sr) = 0 then
  begin
    repeat
      DeleteFile(OutDir + sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  Writeln('>>> Lese Golden-Files mit fremden Namespace-Praefixen, Ausgabe nach: ', OutDir);

  total := 0; passed := 0; failed := 0;
  firstUbl := '';
  failNames := TStringList.Create;
  try
    if FindFirst(GoldenDir + '*.xml', faAnyFile, sr) = 0 then
    begin
      repeat
        goldenFile := GoldenDir + sr.Name;
        baseName := OutDir + ChangeFileExt(sr.Name, '');
        ver := TXRechnungValidationHelper.GetXRechnungVersion(goldenFile);
        xml := ReadText(goldenFile);
        if Pos('<ubl:', xml) > 0 then
          rootPrefix := 'ubl'
        else
          rootPrefix := 'rsm';
        if (firstUbl = '') and (Pos('<ubl:Invoice', xml) > 0) then
          firstUbl := xml;

        variant := xml;
        for i := Low(PrefixMap) to High(PrefixMap) do
          variant := RenamePrefix(variant, PrefixMap[i, 0], PrefixMap[i, 1]);
        WriteText(baseName + '-praefix.xml', variant);
        CheckVariant(goldenFile, baseName + '-praefix.xml', ver);

        WriteText(baseName + '-default.xml', RenamePrefix(xml, rootPrefix, ''));
        CheckVariant(goldenFile, baseName + '-default.xml', ver);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

    if firstUbl = '' then
    begin
      Writeln('FEHLER: kein UBL-Golden-File fuer die Negativfaelle gefunden');
      Halt(2);
    end;
    WriteText(OutDir + 'neg-SelfBilledInvoice.xml',
      StringReplace(firstUbl, 'ubl:Invoice', 'ubl:SelfBilledInvoice', [rfReplaceAll]));
    CheckRejected(OutDir + 'neg-SelfBilledInvoice.xml');
    WriteText(OutDir + 'neg-InvoiceResponse.xml',
      StringReplace(firstUbl, 'ubl:Invoice', 'ubl:InvoiceResponse', [rfReplaceAll]));
    CheckRejected(OutDir + 'neg-InvoiceResponse.xml');

    Writeln('');
    if failed = 0 then
    begin
      Writeln(Format('PASS: %d/%d Varianten mit fremden Praefixen korrekt erkannt und gelesen.',
        [passed, total]));
      Halt(0);
    end
    else
    begin
      Writeln(Format('FAIL: %d von %d korrekt, %d abweichend.',
        [passed, total, failed]));
      Writeln('--- Abweichungen ---');
      Writeln(failNames.Text);
      Halt(1);
    end;
  finally
    failNames.Free;
  end;
end.
