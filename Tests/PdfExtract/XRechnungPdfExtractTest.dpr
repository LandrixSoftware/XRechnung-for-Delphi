program XRechnungPdfExtractTest;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  intf.XRechnungPdfExtract in '..\..\intf.XRechnungPdfExtract.pas',
  XRechnungPdfExtractTestCore in 'XRechnungPdfExtractTestCore.pas';

// Aufruf:  XRechnungPdfExtractTest <Verzeichnis|Dateiliste.txt> [Dumpverzeichnis]
var
  target : String;
  ok : Boolean;
begin
  ok := False;
  try
    if ParamCount >= 1 then
      target := ParamStr(1)
    else
      target := GetCurrentDir;
    if ParamCount >= 2 then
      SetDumpDir(ParamStr(2));
    if FileExists(target) then
      ok := RunPdfExtractTestFromList(target)
    else if DirectoryExists(target) then
      ok := RunPdfExtractTest(target)
    else
    begin
      Writeln('Weder Verzeichnis noch Dateiliste: ', target);
      Halt(2);
    end;
    // Die Selbsttests brauchen keine Fremddateien und laufen immer mit.
    if not RunIncrementalUpdateSelfTest then
      ok := False;
    if not RunPdfDetectionSelfTest then
      ok := False;
    if ok then
      Halt(0)
    else
      Halt(1);
  except
    on E : Exception do
    begin
      Writeln('Ausnahme: ', E.ClassName, ': ', E.Message);
      Halt(3);
    end;
  end;
end.
