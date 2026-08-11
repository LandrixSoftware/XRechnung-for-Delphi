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
  SysUtils, Classes,
  XRechnungGenerateExamples,
  XRechnungXmlCompare;

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
