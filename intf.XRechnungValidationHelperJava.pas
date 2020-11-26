{
Copyright (C) 2020 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 1.2.0

License
This file is not official part of the package XRechnung-for-Delphi.

This is provided as is, expressly without a warranty of any kind.
You use it at your own risc.
}

unit intf.XRechnungValidationHelperJava;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes
  ,System.IOUtils,System.Win.COMObj,System.UITypes
  ,Xml.xmldom,Xml.XMLDoc,Xml.XMLIntf,Xml.XMLSchema
  ;

type
  IXRechnungValidationHelperJava = interface
    ['{6DCEC6AF-1B1B-4C65-B004-B335397CF10D}']
    function SetJavaRuntimeEnvironmentPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorConfigurationPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetVisualizationLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function Validate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function Visualize(const _InvoiceXMLData : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
  end;

  function GetXRechnungValidationHelperJava : IXRechnungValidationHelperJava;

implementation

type
  TXRechnungValidationHelperJava = class(TInterfacedObject,IXRechnungValidationHelperJava)
  private
    JavaRuntimeEnvironmentPath : String;
    ValidatorLibPath : String;
    ValidatorConfigurationPath : String;
    VisualizationLibPath : String;
    CmdOutput : TStringList;
    function ExecAndWait(_Filename, _Params: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function SetJavaRuntimeEnvironmentPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorConfigurationPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetVisualizationLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function Validate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function Visualize(const _InvoiceXMLData : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
  end;

function GetXRechnungValidationHelperJava : IXRechnungValidationHelperJava;
begin
  Result := TXRechnungValidationHelperJava.Create;
end;

{ TXRechnungValidationHelperJava }

constructor TXRechnungValidationHelperJava.Create;
begin
  CmdOutput := TStringList.Create;
end;

destructor TXRechnungValidationHelperJava.Destroy;
begin
  if Assigned(CmdOutput) then begin CmdOutput.Free; CmdOutput := nil; end;
  inherited;
end;

function TXRechnungValidationHelperJava.ExecAndWait(_Filename, _Params: string): Boolean;
var
  SA: TSecurityAttributes;
  SI: TStartupInfoA;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  Handle:Boolean;
  ProcessExitCode : DWORD;
  ReadLine : AnsiString;
begin
  Result := false;
  CmdOutput.Clear;

  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;

  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try

    FillChar(SI, SizeOf(SI), 0);
    SI.cb := SizeOf(SI);
    SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    SI.wShowWindow := SW_HIDE;
    SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
    SI.hStdOutput := StdOutPipeWrite;
    SI.hStdError := StdOutPipeWrite;

    Handle := CreateProcessA(nil, PAnsiChar(AnsiString(_Filename+ ' ' + _Params)),
                            nil, nil, True, 0, nil,
                            PAnsiChar(AnsiString(ExtractFileDir(ParamStr(0)))), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            ReadLine := Copy(Buffer,0,BytesRead);
            CmdOutput.add(Trim(String(ReadLine)));
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
        Result := GetExitCodeProcess(pi.hProcess, ProcessExitCode);
        if Result then
          Result := ProcessExitCode = 0;
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

function TXRechnungValidationHelperJava.SetJavaRuntimeEnvironmentPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  JavaRuntimeEnvironmentPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetValidatorConfigurationPath(
  const _Path: String): IXRechnungValidationHelperJava;
begin
  ValidatorConfigurationPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetValidatorLibPath(const _Path: String): IXRechnungValidationHelperJava;
begin
  ValidatorLibPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.SetVisualizationLibPath(const _Path: String): IXRechnungValidationHelperJava;
begin
  VisualizationLibPath := IncludeTrailingPathDelimiter(_Path);
  Result := self;
end;

function TXRechnungValidationHelperJava.Validate(const _InvoiceXMLData: String; out _CmdOutput,
  _ValidationResultAsXML, _ValidationResultAsHTML: String): Boolean;
var
  hstrl,cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(ValidatorLibPath+'validationtool-1.4.0-java8-standalone.jar') then
    exit;
  if not FileExists(ValidatorConfigurationPath+'scenarios.xml') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  hstrl := TStringList.Create;
  cmd := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    cmd.Add('pushd '+ExtractFilePath(tmpFilename));
    cmd.Add(JavaRuntimeEnvironmentPath+'bin\java -jar '+
             ValidatorLibPath+'validationtool-1.4.0-java8-standalone.jar -s '+
             ValidatorConfigurationPath+'scenarios.xml -h '+
             tmpFilename);
    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
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
    hstrl.Free;
  end;
end;

function TXRechnungValidationHelperJava.Visualize(const _InvoiceXMLData: String;
  _TrueIfUBL_FalseIfCII : Boolean;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  hstrl,cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(ValidatorLibPath+'libs\Saxon-HE-9.9.1-7.jar') then
    exit;
  if _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;

  if not FileExists(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  hstrl := TStringList.Create;
  cmd := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    cmd.Add('pushd '+ExtractFilePath(tmpFilename));
    if _TrueIfUBL_FalseIfCII then
      cmd.Add(JavaRuntimeEnvironmentPath+'bin\java -jar '+
             ValidatorLibPath+'libs\Saxon-HE-9.9.1-7.jar -s:'+tmpFilename+
             ' -xsl:'+VisualizationLibPath+'xsl\ubl-invoice-xr.xsl'+
             ' -o:'+ChangeFileExt(tmpFilename,'-xr.xml'))
    else
      cmd.Add(JavaRuntimeEnvironmentPath+'bin\java -jar '+
             ValidatorLibPath+'libs\Saxon-HE-9.9.1-7.jar -s:'+tmpFilename+
             ' -xsl:'+VisualizationLibPath+'xsl\cii-xr.xsl'+
             ' -o:'+ChangeFileExt(tmpFilename,'-xr.xml'));
    cmd.Add(JavaRuntimeEnvironmentPath+'bin\java -jar '+
             ValidatorLibPath+'libs\Saxon-HE-9.9.1-7.jar -s:'+ChangeFileExt(tmpFilename,'-xr.xml')+
             ' -xsl:'+VisualizationLibPath+'xsl\xrechnung-html.xsl'+
             ' -o:'+ChangeFileExt(tmpFilename,'-.html'));

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    DeleteFile(tmpFilename);
    DeleteFile(ChangeFileExt(tmpFilename,'-xr.xml'));

    if FileExists(ChangeFileExt(tmpFilename,'-.html')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(tmpFilename,'-.html'),TEncoding.UTF8);
      _VisualizationAsHTML := hstrl.Text;
      DeleteFile(ChangeFileExt(tmpFilename,'-.html'));
    end;

  finally
    hstrl.Free;
  end;
end;

end.
