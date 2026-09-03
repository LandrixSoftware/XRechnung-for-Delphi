{
Copyright (C) 2026 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

License
This file is not official part of the package XRechnung-for-Delphi.

This is provided as is, expressly without a warranty of any kind.
You use it at your own risc.
}

{
Offene Punkte (aus dem Review zu PR #83/#84, bewusst nicht mit umgesetzt):

1. QuoteIfContainsSpace ist kein korrektes Windows-Argument-Quoting.
   Es setzt nur Anfuehrungszeichen aussen herum. Endet ein Wert auf "\", entsteht
   z.B. "C:\Mein Ordner\" - die CRT des Kindprozesses liest das abschliessende \"
   als maskiertes Anfuehrungszeichen, das Argument laeuft weiter und alle folgenden
   Parameter verrutschen. Betroffen ist praktisch nur ValitoolValidateDirectory, weil
   dort ein Verzeichnis von aussen hereingereicht wird; alle intern gebauten Pfade
   enden auf einen Dateinamen. Tabulatoren werden ebenfalls nicht als
   quotierungsbeduerftig erkannt. Richtig waere echtes Quoting, das Backslashes vor
   einem " und vor dem schliessenden " verdoppelt. Der Fehler ist aelter als PR #83 -
   ueber die Batchdatei ging dieselbe Zeichenkette unveraendert an das Programm.

2. CmdOutput ist geteilter Instanzzustand statt Rueckgabewert.
   ExecAndWait leert das Feld zu Beginn und der Aufrufer liest es danach aus. Ruft der
   ValidationErrorHandler - der beim Timeout aus ExecAndWait heraus aufgerufen wird -
   synchron eine weitere Operation desselben Objekts auf, leert deren ExecAndWait das
   Feld, und der aeussere Aufrufer bekommt die Ausgabe des inneren Laufs. Parallele
   Nutzung eines Objekts ist aus demselben Grund nicht moeglich (TStringList ist nicht
   threadsicher). Sauber waere, die Prozessausgabe als out-Parameter zurueckzugeben;
   das aendert die Signatur und saemtliche Aufrufstellen.

3. Die Leseschleife endet am Ende des direkten Kindprozesses, nicht am Pipe-EOF.
   Vor PR #84 wurde bis EOF gelesen, also implizit gewartet, bis auch Enkelprozesse
   ihr Schreib-Handle geschlossen hatten. Startet ein Werkzeug nach Launcher-Muster
   einen Enkel und beendet sich vor ihm, geht dessen Ausgabe verloren. Bei gesetztem
   Timeout ist das gewollt (das Jobobjekt beendet den Baum), ohne Timeout wird kein
   Jobobjekt mehr angelegt. java.exe ist nicht betroffen, valitool.exe nur, falls es
   nicht auf seine JVM wartet.

4. hstrl.LoadFromFile(...,TEncoding.UTF8) kann EEncodingError werfen.
   In Validate, Visualize und VisualizeFile werden Werkzeugausgaben so eingelesen.
   TEncoding.UTF8 hat MB_ERR_INVALID_CHARS gesetzt und wirft bei einer mitten in einer
   Mehrbyte-Sequenz abgeschnittenen Datei - denkbar nach einem Timeout. Die beiden
   Mustang-Stellen nutzen dafuer bereits ReadTextFileUtf8; die drei uebrigen liessen
   sich genauso umstellen.

5. BuildEnvironmentBlock: TStringList.Values[Name] := '' entfernt den Eintrag, statt
   ihn leer zu setzen. Ein Override der Form "NAME=" loescht die Variable also aus dem
   Block. Die heutigen Aufrufer uebergeben nie leere Werte.

6. Delphi6/intfXRechnungValidationHelperJava.pas ist nicht nachgezogen und arbeitet
   weiter mit .bat-Dateien, ohne Timeout und ohne Aufraeumen der Temp-Dateien. Eine
   woertliche Portierung scheitert an fehlenden APIs (TEncoding, TBytesStream); das
   Temp-Datei-Leck aus GetTempFileName besteht dort aber unveraendert.
}

unit intf.XRechnungValidationHelperJava;

interface

uses
  Winapi.Windows, Winapi.Messages
  ,System.IOUtils, System.SysUtils, System.Variants, System.Classes
  ,System.Types, System.Win.COMObj,System.UITypes, System.StrUtils
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema
  ;

type
  TValidationErrorHandler = procedure (const ErrMessage: string) of object;
  //TValidationErrorHandler = reference to procedure (const _ErrMessage : String); Bei neueren Delphi-Versionen ist auch das moeglich

  IXRechnungValidationHelperJava = interface
    ['{6DCEC6AF-1B1B-4C65-B004-B335397CF10D}']
    function SetTempPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetJavaRuntimeEnvironmentPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorConfigurationPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetVisualizationLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetPeppolVisualizationLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetSaxonLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetFopLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetMustangprojectLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValitoolLicense(const _License : String) : IXRechnungValidationHelperJava;
    function SetValitoolPath(const _Path : String) : IXRechnungValidationHelperJava;
    function Validate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function ValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function Visualize(const _InvoiceXMLData : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeAsPdf(const _InvoiceXMLData : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function VisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function PeppolVisualize(const _InvoiceXMLData : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function PeppolVisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function MustangValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML : String) : Boolean;
    function MustangVisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function MustangVisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function MustangCombinePdfAndXML(const _InvoicePDFFilename, _InvoiceXMLFilename : String; _Extended : Boolean; out _CmdOutput : String; out _CombinedPdf : TMemoryStream) : Boolean;
    function MustangUpgradeToPDFA3Only(const _InvoicePDFFilename : String; out _CmdOutput : String; out _PdfA3 : TMemoryStream) : Boolean;
    function ValitoolValidate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function ValitoolValidateDirectory(const _Directory : String; out _CmdOutput : String) : Boolean;
    function SetValidationErrorHandler(const _Value : TValidationErrorHandler) : IXRechnungValidationHelperJava;
    function SetExecTimeout(const _Seconds : Integer) : IXRechnungValidationHelperJava;
  end;

  function GetXRechnungValidationHelperJava : IXRechnungValidationHelperJava;

implementation

type
  TXRechnungValidationHelperJava = class(TInterfacedObject,IXRechnungValidationHelperJava)
  private
    TempPath : String;
    JavaRuntimeEnvironmentPath : String;
    ValidatorLibPath : String;
    ValidatorConfigurationPath : TStringList;
    VisualizationLibPath : String;
    PeppolVisualizationLibPath : String;
    SaxonLibPath : String;
    FopLibPath : String;
    MustangprojectPath : String;
    ValitoolPath : String;
    ValitoolLicense : String;
    CmdOutput : TStringList;
    ExecTimeoutSeconds : Integer; //0 = kein Timeout (Standard, Verhalten wie bisher)
    // true, wenn der letzte ExecAndWait-Aufruf den Prozess wirklich bis zu dessen Ende
    // laufen liess. false bei Startfehler oder Timeout - dann sagt ein evtl. vorhandenes
    // Ausgabefile *nichts* ueber den Erfolg aus (die Datei wird vor CreateProcess angelegt).
    LastRunCompleted : Boolean;
    FValidationErrorHandler : TValidationErrorHandler;
    procedure HandleValidationError(const _ErrMessage : String);
    function RequireFile(const _Filename : String) : Boolean;
    function RequireDir(const _Dirname : String) : Boolean;
    function ExecAndWait(const _Filename, _Params : String; const _WorkDir : String = '';
      const _StdOutFilename : String = ''; const _EnvOverrides : TStrings = nil) : Boolean;
    function BuildEnvironmentBlock(const _Overrides : TStrings) : String;
    function DecodeOutput(const _Bytes : TBytes) : String;
    function JavaExe : String;
    function JavaUtf8Params : String;
    function SaxonClassPath : String;
    function SaxonXslForVersion(_Version : Integer) : String;
    function SaxonTransform(const _Source,_Xsl,_Out,_WorkDir : String) : Boolean;
    function FopTransform(const _FoFilename,_PdfFilename,_WorkDir : String) : Boolean;
    function MustangCliParams(const _Params : String) : String;
    function QuoteIfContainsSpace(const _Value : String) : String;
    function GetVersionFromStr(const _Xml : String) : Integer;
    function GetVersionFromFile(const _Filename : String) : Integer;
    function GetNewTempFileName(const _TempPath : String): string;
    procedure DeleteTempFiles(const _TmpFilename : String);
    function ReadTextFileUtf8(const _Filename : String) : String;
    function GetNewTempPath: string;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function SetTempPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetJavaRuntimeEnvironmentPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorConfigurationPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetVisualizationLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetPeppolVisualizationLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetSaxonLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetFopLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetMustangprojectLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValitoolLicense(const _License : String) : IXRechnungValidationHelperJava;
    function SetValitoolPath(const _Path : String) : IXRechnungValidationHelperJava;
    function Validate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function ValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function Visualize(const _InvoiceXMLData : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeAsPdf(const _InvoiceXMLData : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function VisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function PeppolVisualize(const _InvoiceXMLData : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function PeppolVisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function MustangValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML : String) : Boolean;
    function MustangVisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function MustangVisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function MustangCombinePdfAndXML(const _InvoicePDFFilename, _InvoiceXMLFilename : String; _Extended : Boolean; out _CmdOutput : String; out _CombinedPdf : TMemoryStream) : Boolean;
    function MustangUpgradeToPDFA3Only(const _InvoicePDFFilename : String; out _CmdOutput : String; out _PdfA3 : TMemoryStream) : Boolean;
    function ValitoolValidate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function ValitoolValidateDirectory(const _Directory : String; out _CmdOutput : String) : Boolean;
    function SetValidationErrorHandler(const _Value : TValidationErrorHandler) : IXRechnungValidationHelperJava;
    function SetExecTimeout(const _Seconds : Integer) : IXRechnungValidationHelperJava;
  end;

function GetXRechnungValidationHelperJava : IXRechnungValidationHelperJava;
begin
  Result := TXRechnungValidationHelperJava.Create;
end;

{ TXRechnungValidationHelperJava }

constructor TXRechnungValidationHelperJava.Create;
begin
  CmdOutput := TStringList.Create;
  ValidatorConfigurationPath := TStringList.Create;
  TempPath := GetNewTempPath;
end;

destructor TXRechnungValidationHelperJava.Destroy;
begin
  if Assigned(CmdOutput) then begin CmdOutput.Free; CmdOutput := nil; end;
  if Assigned(ValidatorConfigurationPath) then begin ValidatorConfigurationPath.Free; ValidatorConfigurationPath := nil; end;
  inherited;
end;

procedure TXRechnungValidationHelperJava.HandleValidationError(const _ErrMessage: String);
begin
  if Assigned(FValidationErrorHandler) then
    FValidationErrorHandler(_ErrMessage);
end;

function TXRechnungValidationHelperJava.RequireFile(const _Filename: String): Boolean;
begin
  Result := FileExists(_Filename);
  if not Result then
    HandleValidationError(Format('Datei nicht gefunden: %s',[_Filename]));
end;

function TXRechnungValidationHelperJava.RequireDir(const _Dirname: String): Boolean;
begin
  Result := DirectoryExists(_Dirname);
  if not Result then
    HandleValidationError(Format('Verzeichnis nicht gefunden: %s',[_Dirname]));
end;

// Startet das Programm direkt via CreateProcessW (Unicode, damit Umlaute/Nicht-ANSI-Zeichen
// und Leerzeichen im Pfad funktionieren, Issue #39). Der frueher noetige Umweg ueber cmd.exe
// entfaellt, weil keine .bat-Datei mehr erzeugt wird.
// _WorkDir ersetzt das "pushd" der Batchdatei, _StdOutFilename das ">datei",
// _EnvOverrides das "SET name=wert".
function TXRechnungValidationHelperJava.ExecAndWait(const _Filename, _Params : String;
  const _WorkDir : String = ''; const _StdOutFilename : String = '';
  const _EnvOverrides : TStrings = nil) : Boolean;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite, StdOutFile, StdInRead, StdInWrite, Job: THandle;
  JobLimits : TJobObjectExtendedLimitInformation;
  Buffer: array[0..4095] of Byte;
  BytesRead, BytesAvail: Cardinal;
  StartTick : UInt64;
  TimedOut : Boolean;
  ProcessExitCode : DWORD;
  Output : TBytesStream;
  CmdLine, WorkDir, EnvBlock : String;
  EnvPtr : Pointer;
  Flags : DWORD;
begin
  Result := false;
  LastRunCompleted := false;
  CmdOutput.Clear;

  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  StdOutFile := INVALID_HANDLE_VALUE;
  StdInRead := INVALID_HANDLE_VALUE;
  Job := 0;
  if not CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0) then
    exit;
  try
    if _StdOutFilename <> '' then
    begin
      StdOutFile := CreateFile(PChar(_StdOutFilename),GENERIC_WRITE,FILE_SHARE_READ,
                               @SA,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,0);
      if StdOutFile = INVALID_HANDLE_VALUE then
        exit;
    end;

    // Leeres stdin statt des geerbten Handles: eine VCL-Anwendung hat keine Konsole,
    // GetStdHandle liefert dort 0. Der KoSIT-Validator ruft System.in.available() auf
    // (isPiped) und quittiert ein ungueltiges Handle mit
    // "java.io.IOException: Unzulaessige Funktion" statt zu validieren.
    // Eine Pipe mit sofort geschlossenem Schreibende liefert available()=0 und beim
    // Lesen EOF. Das NUL-Geraet reicht hier nicht - darauf schlaegt available() mit
    // derselben IOException fehl (geprueft gegen Validator 1.6.2).
    if CreatePipe(StdInRead, StdInWrite, @SA, 0) then
      CloseHandle(StdInWrite)
    else
      StdInRead := INVALID_HANDLE_VALUE;

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    if StdInRead <> INVALID_HANDLE_VALUE then
      SI.hStdInput := StdInRead
    else
      SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    if StdOutFile <> INVALID_HANDLE_VALUE then
      SI.hStdOutput := StdOutFile
    else
      SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    CmdLine := QuoteIfContainsSpace(_Filename)+' '+_Params;
    UniqueString(CmdLine); // CreateProcessW darf den Kommandozeilen-Puffer veraendern

    WorkDir := _WorkDir;
    if WorkDir = '' then
      WorkDir := ExtractFileDir(ParamStr(0));

    Flags := CREATE_NO_WINDOW;
    EnvPtr := nil;
    if (_EnvOverrides <> nil) and (_EnvOverrides.Count > 0) then
    begin
      EnvBlock := BuildEnvironmentBlock(_EnvOverrides);
      EnvPtr := PChar(EnvBlock);
      Flags := Flags or CREATE_UNICODE_ENVIRONMENT;
    end;

    // Jobobjekt, damit beim Timeout der ganze Prozessbaum stirbt: TerminateProcess
    // beendet nur das direkte Kind. valitool.exe startet seinerseits eine JVM, die
    // sonst weiterlaeuft und Arbeitsverzeichnis und Ausgabedatei gesperrt haelt.
    // Nur bei gesetztem Timeout, denn KILL_ON_JOB_CLOSE beendet beim Schliessen des
    // Handles auch Enkel, die im Normalbetrieb absichtlich laenger laufen duerfen.
    // Schlaegt das Anlegen fehl (aeltere Windows-Versionen ohne verschachtelte Jobs),
    // faellt der Code auf TerminateProcess zurueck.
    if ExecTimeoutSeconds > 0 then
    begin
      Job := CreateJobObject(nil,nil);
      if Job <> 0 then
      begin
        FillChar(JobLimits,SizeOf(JobLimits),0);
        JobLimits.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        if not SetInformationJobObject(Job,JobObjectExtendedLimitInformation,
                                       @JobLimits,SizeOf(JobLimits)) then
        begin
          CloseHandle(Job);
          Job := 0;
        end;
      end;
    end;
    if Job <> 0 then
      Flags := Flags or CREATE_SUSPENDED;

    if not CreateProcess(PChar(_Filename), PChar(CmdLine),
                         nil, nil, True, Flags, EnvPtr,
                         PChar(WorkDir), SI, PI) then
      exit;

    if Job <> 0 then
    begin
      if not AssignProcessToJobObject(Job,PI.hProcess) then
      begin
        CloseHandle(Job); // ohne Job bleibt es beim TerminateProcess-Verhalten
        Job := 0;
      end;
      ResumeThread(PI.hThread);
    end;

    CloseHandle(StdOutPipeWrite);
    StdOutPipeWrite := 0;
    try
      Output := TBytesStream.Create;
      try
        //Timeout statt endlosem Warten, wenn Java/Valitool haengt.
        //Gelesen wird nur nach PeekNamedPipe-Abfrage, weil ReadFile auf einer anonymen
        //Pipe sonst genauso endlos blockiert wie WaitForSingleObject(...,INFINITE).
        //Erst lesen, dann auf Prozessende pruefen - sonst Deadlock, wenn der volle
        //Pipe-Puffer den Kindprozess blockiert.
        StartTick := GetTickCount64;
        TimedOut := false;
        repeat
          while PeekNamedPipe(StdOutPipeRead,nil,0,nil,@BytesAvail,nil) and (BytesAvail > 0) do
          begin
            if not ReadFile(StdOutPipeRead, Buffer, SizeOf(Buffer), BytesRead, nil) or (BytesRead = 0) then
              Break;
            Output.Write(Buffer,BytesRead);
            // Deadline auch hier pruefen: ein Prozess, der die Pipe dauerhaft gefuellt
            // haelt, wuerde die Schleife sonst nie verlassen und den Timeout aushebeln.
            if (ExecTimeoutSeconds > 0) and
               (GetTickCount64 - StartTick >= UInt64(ExecTimeoutSeconds) * 1000) then
              Break;
          end;
          if WaitForSingleObject(PI.hProcess, 100) = WAIT_OBJECT_0 then
            Break;
          if (ExecTimeoutSeconds > 0) and
             (GetTickCount64 - StartTick >= UInt64(ExecTimeoutSeconds) * 1000) then
          begin
            TimedOut := true;
            if Job <> 0 then
              TerminateJobObject(Job,1) // beendet auch Enkelprozesse
            else
              TerminateProcess(PI.hProcess, 1);
            WaitForSingleObject(PI.hProcess, 5000); // sonst sind Tempdateien beim Loeschen evtl. noch gesperrt
            Break;
          end;
        until false;
        while PeekNamedPipe(StdOutPipeRead,nil,0,nil,@BytesAvail,nil) and (BytesAvail > 0) do
        begin
          if not ReadFile(StdOutPipeRead, Buffer, SizeOf(Buffer), BytesRead, nil) or (BytesRead = 0) then
            Break;
          Output.Write(Buffer,BytesRead);
        end;
        CmdOutput.Text := DecodeOutput(Copy(Output.Bytes,0,Integer(Output.Size)));
      finally
        Output.Free;
      end;
      if TimedOut then
      begin
        CmdOutput.Add(Format('Abbruch durch Zeitueberschreitung nach %d Sekunden',[ExecTimeoutSeconds]));
        HandleValidationError(Format('Zeitueberschreitung: %s wurde nach %d Sekunden abgebrochen',
          [_Filename,ExecTimeoutSeconds]));
      end
      else
      begin
        LastRunCompleted := true;
        Result := GetExitCodeProcess(PI.hProcess, ProcessExitCode) and (ProcessExitCode = 0);
      end;
    finally
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
  finally
    if StdOutPipeWrite <> 0 then
      CloseHandle(StdOutPipeWrite);
    CloseHandle(StdOutPipeRead);
    if StdOutFile <> INVALID_HANDLE_VALUE then
      CloseHandle(StdOutFile);
    if StdInRead <> INVALID_HANDLE_VALUE then
      CloseHandle(StdInRead);
    if Job <> 0 then
      CloseHandle(Job); // KILL_ON_JOB_CLOSE raeumt hier evtl. noch laufende Enkel ab
  end;
end;

// Ohne Batchdatei gibt es kein "chcp 65001" mehr, das die Ausgabecodepage festlegt.
// Java schreibt je nach Version und Aufrufparametern UTF-8 oder ANSI.
// TEncoding.UTF8 ist mit MB_ERR_INVALID_CHARS angelegt und wirft bei ungueltigem
// UTF-8 eine EEncodingError, statt den Fallback unten zu erreichen - deshalb ein
// eigenes UTF-8-Encoding ohne dieses Flag. Betroffen war jede lokalisierte Meldung
// von Java/Valitool, z.B. "Unzulaessige Funktion" des KoSIT-Validators.
function TXRechnungValidationHelperJava.DecodeOutput(const _Bytes : TBytes) : String;
var
  lRoundTrip : TBytes;
  lUtf8 : TEncoding;
begin
  Result := '';
  if Length(_Bytes) = 0 then
    exit;
  lUtf8 := TUTF8Encoding.Create(CP_UTF8,0,0);
  try
    Result := lUtf8.GetString(_Bytes);
    lRoundTrip := lUtf8.GetBytes(Result);
  finally
    lUtf8.Free;
  end;
  if (Length(lRoundTrip) <> Length(_Bytes)) or
     not CompareMem(@lRoundTrip[0],@_Bytes[0],Length(_Bytes)) then
    Result := TEncoding.ANSI.GetString(_Bytes);
end;

// Vergleicht zwei "NAME=WERT"-Eintraege nach dem Namen, so wie CreateProcess es erwartet.
function CompareEnvNames(_List : TStringList; _Index1,_Index2 : Integer) : Integer;
var
  lName1, lName2 : String;
  p : Integer;
begin
  lName1 := _List[_Index1];
  p := Pos('=',lName1);
  if p > 0 then
    lName1 := Copy(lName1,1,p-1);
  lName2 := _List[_Index2];
  p := Pos('=',lName2);
  if p > 0 then
    lName2 := Copy(lName2,1,p-1);
  Result := CompareText(lName1,lName2);
end;

// Liest eine von den Werkzeugen geschriebene Textdatei. Bewusst nicht ueber
// TFile.ReadAllText(...,TEncoding.UTF8): dessen UTF-8-Encoding hat MB_ERR_INVALID_CHARS
// gesetzt und wirft EEncodingError, sobald die Datei mitten in einer Mehrbyte-Sequenz
// abgeschnitten ist - genau der Fall nach einem Timeout. Leere oder fehlende Datei
// liefert ''. Die Dekodierlogik ist dieselbe wie fuer die Konsolenausgabe.
function TXRechnungValidationHelperJava.ReadTextFileUtf8(const _Filename : String) : String;
var
  lBytes : TBytes;
begin
  Result := '';
  if not FileExists(_Filename) then
    exit;
  try
    lBytes := TFile.ReadAllBytes(_Filename);
  except
    exit;
  end;
  // BOM entfernen, das TFile.ReadAllText sonst geschluckt haette
  if (Length(lBytes) >= 3) and (lBytes[0] = $EF) and (lBytes[1] = $BB) and (lBytes[2] = $BF) then
    lBytes := Copy(lBytes,3,Length(lBytes)-3);
  Result := DecodeOutput(lBytes);
end;

function TXRechnungValidationHelperJava.BuildEnvironmentBlock(const _Overrides : TStrings) : String;
var
  lEnv : TStringList;
  lBlock : TStringBuilder;
  lEntry, lStart : PChar;
  i : Integer;
begin
  lEnv := TStringList.Create;
  try
    lEnv.CaseSensitive := false;
    lStart := Winapi.Windows.GetEnvironmentStrings;
    try
      lEntry := lStart;
      while (lEntry <> nil) and (lEntry^ <> #0) do
      begin
        if lEntry^ <> '=' then // "=C:=..." der Laufwerks-Arbeitsverzeichnisse ueberspringen
          lEnv.Add(lEntry);
        Inc(lEntry,StrLen(lEntry)+1);
      end;
    finally
      Winapi.Windows.FreeEnvironmentStrings(lStart);
    end;

    for i := 0 to _Overrides.Count-1 do
      lEnv.Values[_Overrides.Names[i]] := _Overrides.ValueFromIndex[i];

    // CreateProcess verlangt einen nach Variablennamen sortierten Block; GetEnvironmentStrings
    // liefert zwar sortiert, neu hinzugekommene Variablen haengen aber hinten.
    // MSDN: case-insensitive, Unicode-Reihenfolge, ohne Beruecksichtigung der Locale -
    // deshalb CompareText und nicht das locale-abhaengige TStringList.Sort.
    lEnv.CustomSort(CompareEnvNames);

    lBlock := TStringBuilder.Create;
    try
      for i := 0 to lEnv.Count-1 do
        lBlock.Append(lEnv[i]+#0);
      lBlock.Append(#0);
      Result := lBlock.ToString;
    finally
      lBlock.Free;
    end;
  finally
    lEnv.Free;
  end;
end;

function TXRechnungValidationHelperJava.JavaExe : String;
begin
  Result := JavaRuntimeEnvironmentPath+'bin\java.exe';
end;

// Fuer jeden Java-Aufruf: Bis Java 17 schreibt die JVM bei Pipe-Umleitung in der
// ANSI-Codepage, erst ab Java 18 in UTF-8. -Dfile.encoding legt das Encoding bis
// Java 17 fest, stdout/stderr.encoding sind die ab JDK 19 offiziellen Properties.
// Damit kommt die Prozessausgabe unabhaengig von der Java-Version als UTF-8 an und
// DecodeOutput muss nicht auf den ANSI-Fallback ausweichen - wichtig, sobald die
// Ausgabe Nicht-ASCII-Zeichen enthaelt (z.B. Umlaute im Temp-Pfad, wenn der
// Windows-Benutzername welche traegt).
function TXRechnungValidationHelperJava.JavaUtf8Params : String;
begin
  Result := '-Dfile.encoding=UTF-8 -Dstdout.encoding=UTF-8 -Dstderr.encoding=UTF-8';
end;

function TXRechnungValidationHelperJava.SaxonClassPath : String;
begin
  Result := QuoteIfContainsSpace(SaxonLibPath+'saxon-he-12.9.jar;'+SaxonLibPath+'lib\xmlresolver-5.3.3.jar');
end;

function TXRechnungValidationHelperJava.SaxonXslForVersion(_Version : Integer) : String;
begin
  case _Version of
    1 : Result := VisualizationLibPath+'xsl\ubl-invoice-xr.xsl';
    2 : Result := VisualizationLibPath+'xsl\ubl-creditnote-xr.xsl';
    3 : Result := VisualizationLibPath+'xsl\cii-xr.xsl';
    else Result := '';
  end;
end;

function TXRechnungValidationHelperJava.SaxonTransform(const _Source,_Xsl,_Out,_WorkDir : String) : Boolean;
begin
  Result := ExecAndWait(JavaExe,
    JavaUtf8Params+' -cp '+SaxonClassPath+' net.sf.saxon.Transform'+
    ' -s:'+QuoteIfContainsSpace(_Source)+
    ' -xsl:'+QuoteIfContainsSpace(_Xsl)+
    ' -o:'+QuoteIfContainsSpace(_Out),_WorkDir);
end;

// Klassenpfad aus apache-fop\fop\fop.bat ausgelesen mit
// echo "%JAVACMD%" %JAVAOPTS% %LOGCHOICE% %LOGLEVEL% -cp "%LOCALCLASSPATH%" %FOP_OPTS% org.apache.fop.cli.Main %FOP_CMD_LINE_ARGS%
function TXRechnungValidationHelperJava.FopTransform(const _FoFilename,_PdfFilename,_WorkDir : String) : Boolean;
begin
  Result := ExecAndWait(JavaExe,
    JavaUtf8Params+
    ' -cp '+QuoteIfContainsSpace(FopLibPath+'fop\build\fop.jar;'+FopLibPath+'fop\lib\batik-all-1.16.jar;' +
                                FopLibPath+'fop\lib\commons-io-2.11.0.jar;'+FopLibPath+'fop\lib\commons-logging-1.0.4.jar;' +
                                FopLibPath+'fop\lib\fontbox-2.0.24.jar;'+FopLibPath+'fop\lib\serializer-2.7.2.jar;' +
                                FopLibPath+'fop\lib\xml-apis-1.4.01.jar;'+FopLibPath+'fop\lib\xml-apis-ext-1.3.04.jar;' +
                                FopLibPath+'fop\lib\xmlgraphics-commons-2.8.jar;')+
    ' org.apache.fop.cli.Main '+
    QuoteIfContainsSpace(_FoFilename)+' '+
    QuoteIfContainsSpace(_PdfFilename),_WorkDir);
end;

//https://github.com/ZUGFeRD/mustangproject/blob/f9905d6fca18733b468541415b9750654045cc09/Mustang-CLI/src/main/java/org/mustangproject/commandline/Main.java#L45
function TXRechnungValidationHelperJava.MustangCliParams(const _Params : String) : String;
begin
  Result := '-Xmx1G '+JavaUtf8Params+' -jar '+
            QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI.jar')+' '+_Params;
end;

// Liefert '' wenn keine Temp-Datei angelegt werden konnte - die Aufrufer brechen dann ab.
// Der Ausgabepuffer muss laut MSDN mindestens MAX_PATH Zeichen fassen (vorher 256 - zu
// klein) und wird initialisiert, weil er bei einem Fehlschlag sonst Stackmuell enthaelt,
// der als Dateiname weiterverwendet wuerde.
function TXRechnungValidationHelperJava.GetNewTempFileName(
  const _TempPath: String): string;
var
  lTempPath: array[0..MAX_PATH] of Char;
  lTempFileName: array[0..MAX_PATH] of Char;
begin
  Result := '';
  FillChar(lTempPath,SizeOf(lTempPath),0);
  FillChar(lTempFileName,SizeOf(lTempFileName),0);
  StrPLCopy(lTempPath, _TempPath, Length(lTempPath) - 1);
  if GetTempFileName(lTempPath, 'TMP', 0, lTempFileName) = 0 then
  begin
    HandleValidationError(Format('Temporaere Datei in %s konnte nicht angelegt werden: %s',
      [_TempPath,SysErrorMessage(GetLastError)]));
    exit;
  end;
  Result := lTempFileName;
end;

// Raeumt alles weg, was zu einem Temp-Namen aus GetNewTempFileName gehoert.
// Wichtig ist vor allem der Stamm selbst: GetTempFileName mit uUnique=0 *legt die
// Datei an*, und geloescht wurden bisher nur die abgeleiteten Dateien - pro Aufruf
// blieb also eine 0-Byte-Leiche im Temp-Verzeichnis liegen.
// Zwei Namensschemata: Mustang und Valitool *haengen* an ("TMP1.tmp.pdf",
// "TMP1.tmp.report.de.xml"), Saxon/FOP/KOSIT *ersetzen* die Endung ("TMP1-xr.xml").
// Die angehaengten deckt das Suchmuster ab - eine feste Endungsliste kennt die
// Valitool-Namen nicht. Fremde Dateien koennen nicht getroffen werden, weil der Stamm
// von GetTempFileName eindeutig ist.
procedure TXRechnungValidationHelperJava.DeleteTempFiles(const _TmpFilename : String);
const
  cErsetzt : array[0..5] of String =
    ('-xr.xml','-.fo','-.pdf','-.html','-report.xml','-report.html');
var
  i : Integer;
  lDir : String;
  lTreffer : TStringDynArray;
begin
  if _TmpFilename = '' then
    exit;

  lDir := ExtractFilePath(_TmpFilename);
  if DirectoryExists(lDir) then
  try
    lTreffer := TDirectory.GetFiles(lDir,ExtractFileName(_TmpFilename)+'.*');
    for i := 0 to Length(lTreffer)-1 do
      DeleteFile(lTreffer[i]);
  except
    //Aufraeumen darf den Aufrufer nie mit einer Ausnahme behelligen
  end;

  if FileExists(_TmpFilename) then
    DeleteFile(_TmpFilename);
  for i := Low(cErsetzt) to High(cErsetzt) do
    if FileExists(ChangeFileExt(_TmpFilename,cErsetzt[i])) then
      DeleteFile(ChangeFileExt(_TmpFilename,cErsetzt[i]));
end;

function TXRechnungValidationHelperJava.GetNewTempPath: string;
var
  lTempPath: array[0..255] of Char;
begin
  GetTempPath(255, lTempPath);
  Result := lTempPath;
end;

function TXRechnungValidationHelperJava.GetVersionFromFile(
  const _Filename: String): Integer;
var
  hstrl : TStringList;
begin
  Result := 0;
  if not RequireFile(_Filename) then
    exit;
  hstrl := TStringList.Create;
  try
    hstrl.LoadFromFile(_Filename,TEncoding.UTF8);
    Result := GetVersionFromStr(hstrl.Text);
  finally
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.GetVersionFromStr(
  const _Xml: String): Integer;
begin
  Result := 0;
  if (Pos('<Invoice',_Xml) > 0) or
     (Pos('<ubl:Invoice',_Xml) > 0) or
     (Pos('<ns0:Invoice',_Xml) > 0) then
    Result := 1
  else
  if (Pos('<CreditNote',_Xml) > 0) or
     (Pos('<ubl:CreditNote',_Xml) > 0) or
     (Pos('<ns0:CreditNote',_Xml) > 0) then
    Result := 2
  else
  if (Pos('<CrossIndustryInvoice',_Xml) > 0) or
     (Pos('<rsm:CrossIndustryInvoice',_Xml) > 0) then
    Result := 3;
end;

function TXRechnungValidationHelperJava.MustangCombinePdfAndXML(
  const _InvoicePDFFilename, _InvoiceXMLFilename: String; _Extended : Boolean;
  out _CmdOutput: String; out _CombinedPdf: TMemoryStream): Boolean;
var
  tmpFilename : String;
begin
  Result := false;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(_InvoicePDFFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(MustangprojectPath+'Mustang-CLI.jar') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  try
    Result := ExecAndWait(JavaExe,MustangCliParams(
                '--action combine' +
                ' --source '+ QuoteIfContainsSpace(_InvoicePDFFilename)+
                ' --source-xml '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
                ' --out '+QuoteIfContainsSpace(tmpFilename+'.pdf')+
                ' --format zf'+
                ' --version 2'+
                ' --profile '+IfThen(_Extended,'T','E')+
                ' --no-additional-attachments'),ExtractFilePath(tmpFilename));

    if Result and FileExists(tmpFilename+'.pdf') then
    begin
      _CombinedPdf := TMemoryStream.Create;
      _CombinedPdf.LoadFromFile(tmpFilename+'.pdf');
      _CombinedPdf.Position := 0;
    end else
      _CombinedPdf := nil;

    _CmdOutput := CmdOutput.Text;
  finally
    DeleteTempFiles(tmpFilename);
  end;
end;

function TXRechnungValidationHelperJava.MustangUpgradeToPDFA3Only(const _InvoicePDFFilename: String;
  out _CmdOutput: String; out _PdfA3: TMemoryStream): Boolean;
var
  tmpFilename : String;
begin
  Result := false;
  if not RequireFile(_InvoicePDFFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(MustangprojectPath+'Mustang-CLI.jar') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  try
    Result := ExecAndWait(JavaExe,MustangCliParams(
                '--action a3only' +
                ' --source '+ QuoteIfContainsSpace(_InvoicePDFFilename)+
                ' --out '+QuoteIfContainsSpace(tmpFilename+'.pdf')),ExtractFilePath(tmpFilename));

    if Result and FileExists(tmpFilename+'.pdf') then
    begin
      _PdfA3 := TMemoryStream.Create;
      _PdfA3.LoadFromFile(tmpFilename+'.pdf');
      _PdfA3.Position := 0;
    end else
      _PdfA3 := nil;

    _CmdOutput := CmdOutput.Text;
  finally
    DeleteTempFiles(tmpFilename);
  end;
end;

function TXRechnungValidationHelperJava.MustangValidateFile(
  const _InvoiceXMLFilename: String; out _CmdOutput,
  _ValidationResultAsXML: String): Boolean;
var
  tmpFilename : String;
begin
  Result := false;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(MustangprojectPath+'Mustang-CLI.jar') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  try
    Result := ExecAndWait(JavaExe,MustangCliParams(
                '--action validate' +
                ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)),
                ExtractFilePath(tmpFilename),tmpFilename+'.xml');

    // Mustang liefert bei einer fachlich ungueltigen Rechnung einen Exitcode <> 0,
    // schreibt aber trotzdem einen Report - der zaehlt als Erfolg. Ein Startfehler oder
    // Timeout darf das dagegen nicht: die Ausgabedatei wird von ExecAndWait schon vor
    // CreateProcess angelegt und existiert deshalb auch dann, leer oder abgeschnitten.
    if LastRunCompleted then
    begin
      _ValidationResultAsXML := ReadTextFileUtf8(tmpFilename+'.xml');
      Result := _ValidationResultAsXML <> '';
    end;

    _CmdOutput := CmdOutput.Text;
  finally
    DeleteTempFiles(tmpFilename);
  end;
end;

function TXRechnungValidationHelperJava.MustangVisualizeFile(
  const _InvoiceXMLFilename: String; out _CmdOutput,
  _VisualizationAsHTML: String): Boolean;
var
  tmpFilename : String;
begin
  Result := false;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(MustangprojectPath+'Mustang-CLI.jar') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  try
    Result := ExecAndWait(JavaExe,MustangCliParams(
                '--action visualize' +
                ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
                ' --out '+QuoteIfContainsSpace(tmpFilename+'.html')+
                ' --language de'),ExtractFilePath(tmpFilename));

    if Result and FileExists(tmpFilename+'.html') then
    begin
      _VisualizationAsHTML := ReadTextFileUtf8(tmpFilename+'.html');
    end;

    _CmdOutput := CmdOutput.Text;
  finally
    DeleteTempFiles(tmpFilename);
    // Mustang legt diese beiden neben die HTML-Datei, unabhaengig vom Temp-Namen
    if FileExists(ExtractFilePath(tmpFilename)+'xrechnung-viewer.css') then
      DeleteFile(ExtractFilePath(tmpFilename)+'xrechnung-viewer.css');
    if FileExists(ExtractFilePath(tmpFilename)+'xrechnung-viewer.js') then
      DeleteFile(ExtractFilePath(tmpFilename)+'xrechnung-viewer.js');
  end;
end;

function TXRechnungValidationHelperJava.MustangVisualizeFileAsPdf(
  const _InvoiceXMLFilename: String; out _CmdOutput: String;
  out _VisualizationAsPdf: TMemoryStream): Boolean;
var
  tmpFilename : String;
begin
  Result := false;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(MustangprojectPath+'Mustang-CLI.jar') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  try
    Result := ExecAndWait(JavaExe,MustangCliParams(
                '--action pdf' +
                ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
                ' --out '+QuoteIfContainsSpace(tmpFilename+'.pdf')+
                ' --language de'),ExtractFilePath(tmpFilename));

    if Result and FileExists(tmpFilename+'.pdf') then
    begin
      _VisualizationAsPdf := TMemoryStream.Create;
      _VisualizationAsPdf.LoadFromFile(tmpFilename+'.pdf');
      _VisualizationAsPdf.Position := 0;
    end else
      _VisualizationAsPdf := nil;

    _CmdOutput := CmdOutput.Text;
  finally
    DeleteTempFiles(tmpFilename);
  end;
end;

function TXRechnungValidationHelperJava.SetSaxonLibPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  SaxonLibPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetFopLibPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  FopLibPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  JavaRuntimeEnvironmentPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetMustangprojectLibPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  MustangprojectPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetTempPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  TempPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetValidatorConfigurationPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  ValidatorConfigurationPath.Add(IncludeTrailingPathDelimiter(_Path));
  Result := self;
end;

function TXRechnungValidationHelperJava.SetValidatorLibPath(const _Path: String): IXRechnungValidationHelperJava;
begin
  ValidatorLibPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetValitoolLicense(
  const _License: String): IXRechnungValidationHelperJava;
begin
  ValitoolLicense := _License;
  Result := self;
end;

function TXRechnungValidationHelperJava.SetValitoolPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  ValitoolPath := _Path;
  Result := self;
end;

function TXRechnungValidationHelperJava.SetValidationErrorHandler(
  const _Value: TValidationErrorHandler): IXRechnungValidationHelperJava;
begin
  FValidationErrorHandler := _Value;
  Result := self;
end;

function TXRechnungValidationHelperJava.SetExecTimeout(
  const _Seconds: Integer): IXRechnungValidationHelperJava;
begin
  ExecTimeoutSeconds := _Seconds;
  Result := self;
end;

function TXRechnungValidationHelperJava.SetVisualizationLibPath(const _Path: String): IXRechnungValidationHelperJava;
begin
  VisualizationLibPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetPeppolVisualizationLibPath(const _Path: String): IXRechnungValidationHelperJava;
begin
  PeppolVisualizationLibPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.Validate(const _InvoiceXMLData: String; out _CmdOutput,
  _ValidationResultAsXML, _ValidationResultAsHTML: String): Boolean;
var
  hstrl: TStringList;
  tmpFilename,params : String;
  i : Integer;
begin
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(ValidatorLibPath+'validator-1.6.2-standalone.jar') then
    exit;
  if ValidatorConfigurationPath.Count=0 then
    exit;
  for i := 0 to ValidatorConfigurationPath.Count-1 do
  if not RequireFile(ValidatorConfigurationPath[i]+'scenarios.xml') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  hstrl := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    params := '-Xmx1024m '+JavaUtf8Params+' -classpath '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs')+' -jar '+
             QuoteIfContainsSpace(ValidatorLibPath+'validator-1.6.2-standalone.jar');
    for i := 0 to ValidatorConfigurationPath.Count-1 do
    begin
      params := params +
         ' -s '+QuoteIfContainsSpace(ValidatorConfigurationPath[i]+'scenarios.xml')+
         ' -r '+QuoteIfContainsSpace(ExcludeTrailingPathDelimiter(ValidatorConfigurationPath[i]))
    end;
    params := params + ' -h '+QuoteIfContainsSpace(tmpFilename);

    // KOSIT legt den Report im Arbeitsverzeichnis ab
    Result := ExecAndWait(JavaExe,params,ExtractFilePath(tmpFilename));

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename);

    if FileExists(ChangeFileExt(tmpFilename,'-report.xml')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(tmpFilename,'-report.xml'),TEncoding.UTF8);
      _ValidationResultAsXML := hstrl.Text;
      DeleteFile(ChangeFileExt(tmpFilename,'-report.xml'));
    end;

    if FileExists(ChangeFileExt(tmpFilename,'-report.html')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(tmpFilename,'-report.html'),TEncoding.UTF8);
      _ValidationResultAsHTML := hstrl.Text;
      DeleteFile(ChangeFileExt(tmpFilename,'-report.html'));
    end;

  finally
    DeleteTempFiles(tmpFilename);
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.ValidateFile(
  const _InvoiceXMLFilename: String; out _CmdOutput,
  _ValidationResultAsXML, _ValidationResultAsHTML: String): Boolean;
var
  hstrl: TStringList;
  lInvoiceXMLFilename, cmd, params: String;
  i : Integer;
begin
  Result := false;
  if _InvoiceXMLFilename = '' then
    exit;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(ValidatorLibPath+'validator-1.6.2-standalone.jar') then
    exit;
  if ValidatorConfigurationPath.Count=0 then
    exit;
  for i := 0 to ValidatorConfigurationPath.Count-1 do
  if not RequireFile(ValidatorConfigurationPath[i]+'scenarios.xml') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  hstrl := TStringList.Create;
  try
    cmd := JavaExe;
    params:= '-Xmx1024m '+JavaUtf8Params+' -classpath '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs')+' -jar '+
             QuoteIfContainsSpace(ValidatorLibPath+'validator-1.6.2-standalone.jar');
    for i := 0 to ValidatorConfigurationPath.Count-1 do
    begin
      params := params +
         ' -s '+QuoteIfContainsSpace(ValidatorConfigurationPath[i]+'scenarios.xml')+
         ' -r '+QuoteIfContainsSpace(ExcludeTrailingPathDelimiter(ValidatorConfigurationPath[i]))
    end;
    params := params
             + ' -o ' + QuoteIfContainsSpace(ExcludeTrailingPathDelimiter(TempPath)) // \" am Ende wird von KOSIT fehlinterpretiert !!!
             + ' -h '
             + QuoteIfContainsSpace(_InvoiceXMLFilename);

    Result := ExecAndWait(cmd, params);

    _CmdOutput := CmdOutput.Text;

    lInvoiceXMLFilename := ExtractFileName(_InvoiceXMLFilename);
    lInvoiceXMLFilename := StringReplace(lInvoiceXMLFilename,' ','%20',[rfReplaceAll]);
    lInvoiceXMLFilename := IncludeTrailingPathDelimiter(TempPath)+lInvoiceXMLFilename;

    if FileExists(ChangeFileExt(lInvoiceXMLFilename,'-report.xml')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(lInvoiceXMLFilename,'-report.xml'),TEncoding.UTF8);
      _ValidationResultAsXML := hstrl.Text;
      DeleteFile(ChangeFileExt(lInvoiceXMLFilename,'-report.xml'));
    end;

    if FileExists(ChangeFileExt(lInvoiceXMLFilename,'-report.html')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(lInvoiceXMLFilename,'-report.html'),TEncoding.UTF8);
      _ValidationResultAsHTML := hstrl.Text;
      DeleteFile(ChangeFileExt(lInvoiceXMLFilename,'-report.html'));
    end;
  finally
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.ValitoolValidate(
  const _InvoiceXMLData : String;
  out _CmdOutput, _ValidationResultAsXML: String;
  out _VisualizationAsPdf : TMemoryStream): Boolean;
var
  hstrl,lEnv: TStringList;
  tmpFilename : String;
  lResults : TStringDynArray;
  i : Integer;
begin
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if ValitoolLicense = '' then
    exit;
  if not RequireFile(ValitoolPath+'valitool.exe') then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  hstrl := TStringList.Create;
  lEnv := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.WriteBOM := false;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    lEnv.Add('JAVA_HOME='+ExcludeTrailingPathDelimiter(JavaRuntimeEnvironmentPath));
    lEnv.Add('PATH='+JavaRuntimeEnvironmentPath+'bin;'+System.SysUtils.GetEnvironmentVariable('PATH'));

    Result := ExecAndWait(ValitoolPath+'valitool.exe',
             '--license '+ValitoolLicense+
             ' --lang de'+
             ' --file '+QuoteIfContainsSpace(tmpFilename)+
             ' --mode validate'+
             ' --pdfReport',ExtractFilePath(tmpFilename),'',lEnv);

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename);

    _VisualizationAsPdf := nil;

    lResults := TDirectory.GetFiles(ExtractFilePath(tmpFilename),ExtractFileName(tmpFilename)+'.*');
    for i := 0 to Length(lResults)-1 do
    if lResults[i].EndsWith('report.de.xml',true) then
    begin
      hstrl.LoadFromFile(lResults[i],TEncoding.UTF8);
      _ValidationResultAsXML := hstrl.Text;
      DeleteFile(lResults[i]);
    end else
    if lResults[i].EndsWith('report.de.pdf',true) then
    if _VisualizationAsPdf = nil then
    begin
      _VisualizationAsPdf := TMemoryStream.Create;
      _VisualizationAsPdf.LoadFromFile(lResults[i]);
      _VisualizationAsPdf.Position := 0;
      DeleteFile(lResults[i]);
    end;

  finally
    DeleteTempFiles(tmpFilename);
    lEnv.Free;
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.ValitoolValidateDirectory(
  const _Directory: String; out _CmdOutput : String): Boolean;
var
  lEnv: TStringList;
begin
  Result := false;
  if _Directory = '' then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if ValitoolLicense = '' then
    exit;
  if not RequireDir(_Directory) then
    exit;
  if not RequireFile(ValitoolPath+'valitool.exe') then
    exit;

  lEnv := TStringList.Create;
  try
    lEnv.Add('JAVA_HOME='+ExcludeTrailingPathDelimiter(JavaRuntimeEnvironmentPath));
    lEnv.Add('PATH='+JavaRuntimeEnvironmentPath+'bin;'+System.SysUtils.GetEnvironmentVariable('PATH'));

    Result := ExecAndWait(ValitoolPath+'valitool.exe',
           '--license '+ValitoolLicense+
           ' --lang de'+
           ' --dir '+QuoteIfContainsSpace(_Directory)+
           ' --mode validate'+
           ' --pdfReport'+
           ' --noXMLReport',TempPath,'',lEnv);

    _CmdOutput := CmdOutput.Text;
  finally
    lEnv.Free;
  end;
end;

function TXRechnungValidationHelperJava.Visualize(const _InvoiceXMLData: String;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  hstrl: TStringList;
  tmpFilename : String;
  version : Integer;
begin
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(SaxonLibPath+'saxon-he-12.9.jar') then
    exit;
  if not RequireFile(SaxonLibPath+'lib\xmlresolver-5.3.3.jar') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-creditnote-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;
  version := GetVersionFromStr(_InvoiceXMLData);
  if version = 0 then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  hstrl := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    Result := SaxonTransform(tmpFilename,SaxonXslForVersion(version),
                ChangeFileExt(tmpFilename,'-xr.xml'),ExtractFilePath(tmpFilename));
    _CmdOutput := CmdOutput.Text;

    if Result then
    begin
      Result := SaxonTransform(ChangeFileExt(tmpFilename,'-xr.xml'),
                  VisualizationLibPath+'xsl\xrechnung-html.xsl',
                  ChangeFileExt(tmpFilename,'-.html'),ExtractFilePath(tmpFilename));
      _CmdOutput := _CmdOutput+CmdOutput.Text;
    end;

    DeleteFile(tmpFilename);
    DeleteFile(ChangeFileExt(tmpFilename,'-xr.xml'));

    if FileExists(ChangeFileExt(tmpFilename,'-.html')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(tmpFilename,'-.html'),TEncoding.UTF8);
      _VisualizationAsHTML := hstrl.Text;
      DeleteFile(ChangeFileExt(tmpFilename,'-.html'));
    end;

  finally
    DeleteTempFiles(tmpFilename);
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.VisualizeAsPdf(
  const _InvoiceXMLData: String;
  out _CmdOutput: String; out _VisualizationAsPdf: TMemoryStream): Boolean;
var
  tmpFilename : String;
  hstrl: TStringList;
  version : Integer;
begin
  //Experimental - it does not work
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(SaxonLibPath+'saxon-he-12.9.jar') then
    exit;
  if not RequireFile(SaxonLibPath+'lib\xmlresolver-5.3.3.jar') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-creditnote-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;
  if not RequireFile(FopLibPath+'fop\build\fop.jar') then
    exit;
  version := GetVersionFromStr(_InvoiceXMLData);
  if version = 0 then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  hstrl := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    Result := SaxonTransform(tmpFilename,SaxonXslForVersion(version),
                ChangeFileExt(tmpFilename,'-xr.xml'),ExtractFilePath(tmpFilename));
    _CmdOutput := CmdOutput.Text;

    if Result then
    begin
      Result := SaxonTransform(ChangeFileExt(tmpFilename,'-xr.xml'),
                  VisualizationLibPath+'xsl\xr-pdf.xsl',
                  ChangeFileExt(tmpFilename,'-.fo'),ExtractFilePath(tmpFilename)); // geaendert von pdf auf fo
      _CmdOutput := _CmdOutput+CmdOutput.Text;
    end;

    if not Result then
      exit;

    ////////////////////////////////////////////////////////////////////////////
    // Fopper aufrufen. Datei ist eine fo Datei. Saxon HE gibt eine fo-Datei zurueck!
    if FileExists(ChangeFileExt(tmpFilename,'-.fo')) then
    begin
      Result := FopTransform(ChangeFileExt(tmpFilename,'-.fo'),
                  ChangeFileExt(tmpFilename,'-.pdf'),ExtractFilePath(tmpFilename));

     _CmdOutput := _CmdOutput + #13#10 + CmdOutput.Text;

     DeleteFile(ChangeFileExt(tmpFilename,'-.fo'));
    end else
      Result := false;

    DeleteFile(ChangeFileExt(tmpFilename,'-xr.xml'));
    ////////////////////////////////////////////////////////////////////////////
    if FileExists(ChangeFileExt(tmpFilename,'-.pdf')) then
    begin
      _VisualizationAsPdf := TMemoryStream.Create;
      _VisualizationAsPdf.LoadFromFile(ChangeFileExt(tmpFilename,'-.pdf'));
      _VisualizationAsPdf.Position := 0;
      DeleteFile(ChangeFileExt(tmpFilename,'-.pdf'));
    end else
      Result := false;
  finally
    DeleteTempFiles(tmpFilename); // deckt auch den frueheren "exit"-Pfad ab
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.VisualizeFile(
  const _InvoiceXMLFilename: String;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  hstrl: TStringList;
  tmpFilename : String;
  version : Integer;
begin
  Result := false;
  if _InvoiceXMLFilename = '' then
    exit;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(SaxonLibPath+'saxon-he-12.9.jar') then
    exit;
  if not RequireFile(SaxonLibPath+'lib\xmlresolver-5.3.3.jar') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-creditnote-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;
  version := GetVersionFromFile(_InvoiceXMLFilename);
  if version = 0 then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  hstrl := TStringList.Create;
  try
    Result := SaxonTransform(_InvoiceXMLFilename,SaxonXslForVersion(version),
                ChangeFileExt(tmpFilename,'-xr.xml'),ExtractFilePath(tmpFilename));
    _CmdOutput := CmdOutput.Text;

    if Result then
    begin
      Result := SaxonTransform(ChangeFileExt(tmpFilename,'-xr.xml'),
                  VisualizationLibPath+'xsl\xrechnung-html.xsl',
                  ChangeFileExt(tmpFilename,'-.html'),ExtractFilePath(tmpFilename));
      _CmdOutput := _CmdOutput+CmdOutput.Text;
    end;

    DeleteFile(ChangeFileExt(tmpFilename,'-xr.xml'));

    if FileExists(ChangeFileExt(tmpFilename,'-.html')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(tmpFilename,'-.html'),TEncoding.UTF8);
      _VisualizationAsHTML := hstrl.Text;
      DeleteFile(ChangeFileExt(tmpFilename,'-.html'));
    end else
      Result := false;

  finally
    DeleteTempFiles(tmpFilename);
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.VisualizeFileAsPdf(
  const _InvoiceXMLFilename: String;
  out _CmdOutput: String; out _VisualizationAsPdf: TMemoryStream): Boolean;
var
  tmpFilename : String;
  version : Integer;
begin
  //Experimental - it does not work
  Result := false;
  if _InvoiceXMLFilename = '' then
    exit;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(SaxonLibPath+'saxon-he-12.9.jar') then
    exit;
  if not RequireFile(SaxonLibPath+'lib\xmlresolver-5.3.3.jar') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\ubl-creditnote-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;
  if not RequireFile(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;
  if not RequireFile(FopLibPath+'fop\build\fop.jar') then
    exit;
  version := GetVersionFromFile(_InvoiceXMLFilename);
  if version = 0 then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  try
    Result := SaxonTransform(_InvoiceXMLFilename,SaxonXslForVersion(version),
                ChangeFileExt(tmpFilename,'-xr.xml'),ExtractFilePath(tmpFilename));
    _CmdOutput := CmdOutput.Text;

    if Result then
    begin
      Result := SaxonTransform(ChangeFileExt(tmpFilename,'-xr.xml'),
                  VisualizationLibPath+'xsl\xr-pdf.xsl',
                  ChangeFileExt(tmpFilename,'-.fo'),ExtractFilePath(tmpFilename)); // geaendert von pdf auf fo
      _CmdOutput := _CmdOutput+CmdOutput.Text;
    end;

    if not Result then
      exit;

    ////////////////////////////////////////////////////////////////////////////
    // Fopper aufrufen. Datei ist eine fo Datei. Saxon HE gibt eine fo-Datei zurueck!
    if FileExists(ChangeFileExt(tmpFilename,'-.fo')) then
    begin
      Result := FopTransform(ChangeFileExt(tmpFilename,'-.fo'),
                  ChangeFileExt(tmpFilename,'-.pdf'),ExtractFilePath(tmpFilename));

      _CmdOutput := _CmdOutput + #13#10 + CmdOutput.Text;

      DeleteFile(ChangeFileExt(tmpFilename,'-.fo'));
    end else
      Result := false;

    DeleteFile(ChangeFileExt(tmpFilename,'-xr.xml'));
    ////////////////////////////////////////////////////////////////////////////
    if FileExists(ChangeFileExt(tmpFilename,'-.pdf')) then
    begin
      _VisualizationAsPdf := TMemoryStream.Create;
      _VisualizationAsPdf.LoadFromFile(ChangeFileExt(tmpFilename,'-.pdf'));
      _VisualizationAsPdf.Position := 0;
      DeleteFile(ChangeFileExt(tmpFilename,'-.pdf'));
    end else
      Result := false;
  finally
    DeleteTempFiles(tmpFilename); // deckt auch den "exit"-Pfad oben ab
  end;
end;

// Visualisierung mit dem offiziellen OpenPEPPOL-Stylesheet (Peppol BIS Billing 3.0).
// Anders als die KoSIT-Visualisierung ist das ein einstufiger Weg: das Stylesheet erzeugt
// direkt HTML, es gibt kein XR-Zwischenformat und damit auch keinen FO-/PDF-Zweig.
// Es verarbeitet UBL Invoice und UBL CreditNote; CII kann es nicht - dafuer bleibt die
// KoSIT-Visualisierung (Visualize/VisualizeFile) zustaendig.
// Die Beschriftungen sind englisch: das Stylesheet kennt zwar einen Parameter "language",
// seine Uebersetzungstabelle enthaelt aber nur Eintraege mit id="en".
function TXRechnungValidationHelperJava.PeppolVisualize(const _InvoiceXMLData: String;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  hstrl: TStringList;
  tmpFilename : String;
begin
  Result := false;
  _CmdOutput := '';
  _VisualizationAsHTML := '';
  if _InvoiceXMLData = '' then
    exit;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  hstrl := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    Result := PeppolVisualizeFile(tmpFilename,_CmdOutput,_VisualizationAsHTML);
  finally
    DeleteTempFiles(tmpFilename);
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.PeppolVisualizeFile(
  const _InvoiceXMLFilename: String;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  tmpFilename : String;
  version : Integer;
begin
  Result := false;
  _CmdOutput := '';
  _VisualizationAsHTML := '';
  if _InvoiceXMLFilename = '' then
    exit;
  if not RequireFile(_InvoiceXMLFilename) then
    exit;
  if not RequireFile(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not RequireFile(SaxonLibPath+'saxon-he-12.9.jar') then
    exit;
  if not RequireFile(SaxonLibPath+'lib\xmlresolver-5.3.3.jar') then
    exit;
  if not RequireFile(PeppolVisualizationLibPath+'xsl\stylesheet-ubl.xslt') then
    exit;
  version := GetVersionFromFile(_InvoiceXMLFilename);
  if not (version in [1,2]) then //nur UBL Invoice und UBL CreditNote
  begin
    HandleValidationError('Das Peppol-Stylesheet verarbeitet nur UBL Invoice und UBL CreditNote.');
    exit;
  end;
  if not RequireDir(TempPath) then
    exit;

  tmpFilename := GetNewTempFileName(TempPath);
  if tmpFilename = '' then
    exit;

  try
    Result := SaxonTransform(_InvoiceXMLFilename,
                PeppolVisualizationLibPath+'xsl\stylesheet-ubl.xslt',
                ChangeFileExt(tmpFilename,'-.html'),ExtractFilePath(tmpFilename));
    _CmdOutput := CmdOutput.Text;

    if FileExists(ChangeFileExt(tmpFilename,'-.html')) then
      _VisualizationAsHTML := ReadTextFileUtf8(ChangeFileExt(tmpFilename,'-.html'));

    if _VisualizationAsHTML = '' then
      Result := false;
  finally
    DeleteTempFiles(tmpFilename);
  end;
end;

function TXRechnungValidationHelperJava.QuoteIfContainsSpace(const _Value: String): String;
begin
  if Pos(' ',_Value)>0 then
    Result := '"'+_Value+'"'
  else
    Result := _Value;
end;



end.
