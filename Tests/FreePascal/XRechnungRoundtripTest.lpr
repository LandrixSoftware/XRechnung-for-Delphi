{
License XRechnung-for-Delphi

Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de

FreePascal-Roundtriptest fuer den LESEPFAD:
  1. Liest jedes Golden-File aus ValidXMLExamples\ mit der FPC-portierten
     Bibliothek ein (TXRechnungInvoiceAdapter.LoadFromFile ueber fcl-xml/XPath).
  2. Schreibt die eingelesene TInvoice mit der erkannten Version wieder heraus
     (Tests\FreePascal\out\roundtrip\).
  3. Vergleicht das Ergebnis KANONISCH gegen das Golden-File.

  Lesen und Schreiben sind fuer alle Golden-Files verlustfrei - unter Delphi
  ebenso wie unter FreePascal. Jede Abweichung ist daher ein echter Fehler im
  Lesepfad: entweder wurde ein Feld nicht gelesen (Element fehlt) oder falsch
  interpretiert (Text-/Attributabweichung).

  Ergaenzt den Schreib-Paritaetstest (XRechnungParityTest), der die erzeugten
  Dateien gegen die Delphi-Ausgabe prueft.

Aufruf: XRechnungRoundtripTest.exe [RepoRoot]
  RepoRoot optional; ohne Angabe wird es relativ zur EXE bestimmt
  (Tests\FreePascal\out\ -> 3x hoch).
}

program XRechnungRoundtripTest;

{$MODE DELPHIUNICODE}
{$H+}
{$codepage utf8}

uses
  SysUtils, Classes,
  intf.Invoice, intf.XRechnung,
  XRechnungXmlCompare;

var
  RepoRoot, GoldenDir, OutDir : String;
  sr : TSearchRec;
  inv : TInvoice;
  err, diff, goldenFile, outFile : String;
  ver : TXRechnungVersion;
  total, passed, failed : Integer;
  failNames : TStringList;
begin
  if ParamCount >= 1 then
    RepoRoot := ParamStr(1)
  else
    RepoRoot := ExtractFileDir(ExtractFileDir(ExtractFileDir(ExtractFileDir(ParamStr(0)))));
  RepoRoot := IncludeTrailingPathDelimiter(RepoRoot);

  GoldenDir := RepoRoot + 'ValidXMLExamples' + PathDelim;
  OutDir    := RepoRoot + 'Tests' + PathDelim + 'FreePascal' + PathDelim + 'out' + PathDelim + 'roundtrip' + PathDelim;

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

  Writeln('>>> Lese Golden-Files mit FPC-Bibliothek und schreibe zurueck nach: ', OutDir);

  total := 0; passed := 0; failed := 0;
  failNames := TStringList.Create;
  try
    if FindFirst(GoldenDir + '*.xml', faAnyFile, sr) = 0 then
    begin
      repeat
        Inc(total);
        goldenFile := GoldenDir + sr.Name;
        outFile    := OutDir + sr.Name;

        ver := TXRechnungValidationHelper.GetXRechnungVersion(goldenFile);
        if ver = XRechnungVersion_Unknown then
        begin
          Inc(failed);
          failNames.Add('[VERSION] ' + sr.Name + ' -> Version nicht erkannt');
          Continue;
        end;

        inv := TInvoice.Create;
        try
          err := '';
          if not TXRechnungInvoiceAdapter.LoadFromFile(inv, goldenFile, err) then
          begin
            Inc(failed);
            failNames.Add('[LOAD]    ' + sr.Name + ' -> ' + err);
            Continue;
          end;
          try
            TXRechnungInvoiceAdapter.SaveToFile(inv, ver, outFile);
          except
            on E:Exception do
            begin
              Inc(failed);
              failNames.Add('[SAVE]    ' + sr.Name + ' -> ' + E.ClassName + ' ' + E.Message);
              Continue;
            end;
          end;
        finally
          inv.Free;
        end;

        if CompareXmlFiles(goldenFile, outFile, diff) then
          Inc(passed)
        else
        begin
          Inc(failed);
          failNames.Add('[DIFF]    ' + sr.Name + ' -> ' + diff);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

    Writeln('');
    if failed = 0 then
    begin
      Writeln(Format('PASS: %d/%d Golden-Files verlustfrei gelesen und zurueckgeschrieben.',
        [passed, total]));
      Halt(0);
    end
    else
    begin
      Writeln(Format('FAIL: %d von %d verlustfrei, %d abweichend.',
        [passed, total, failed]));
      Writeln('--- Abweichungen ---');
      Writeln(failNames.Text);
      Halt(1);
    end;
  finally
    failNames.Free;
  end;
end.
