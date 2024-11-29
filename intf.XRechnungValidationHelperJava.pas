{
Copyright (C) 2024 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

License
This file is not official part of the package XRechnung-for-Delphi.

This is provided as is, expressly without a warranty of any kind.
You use it at your own risc.
}

unit intf.XRechnungValidationHelperJava;

interface

uses
  Winapi.Windows, Winapi.Messages
  ,System.SysUtils, System.Variants, System.Classes
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
    function SetFopLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetMustangprojectLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function Validate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function ValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function Visualize(const _InvoiceXMLData : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeAsPdf(const _InvoiceXMLData : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function VisualizeFile(const _InvoiceXMLFilename : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFileAsPdf(const _InvoiceXMLFilename : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function MustangValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML : String) : Boolean;
    function MustangVisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function MustangVisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function MustangCombinePdfAndXML(const _InvoicePDFFilename, _InvoiceXMLFilename : String; out _CmdOutput : String; out _CombinedPdf : TMemoryStream) : Boolean;
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
    FopLibPath : String;
    MustangprojectPath : String;
    CmdOutput : TStringList;
    function ExecAndWait(_Filename, _Params: string): Boolean;
    function QuoteIfContainsSpace(const _Value : String) : String;
  public
    constructor Create;
    destructor Destroy; override;
    function SetJavaRuntimeEnvironmentPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetValidatorConfigurationPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetVisualizationLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetFopLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function SetMustangprojectLibPath(const _Path : String) : IXRechnungValidationHelperJava;
    function Validate(const _InvoiceXMLData : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function ValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML,_ValidationResultAsHTML : String) : Boolean;
    function Visualize(const _InvoiceXMLData : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeAsPdf(const _InvoiceXMLData : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function VisualizeFile(const _InvoiceXMLFilename : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function VisualizeFileAsPdf(const _InvoiceXMLFilename : String; _TrueIfUBL_FalseIfCII : Boolean; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function MustangValidateFile(const _InvoiceXMLFilename : String; out _CmdOutput,_ValidationResultAsXML : String) : Boolean;
    function MustangVisualizeFile(const _InvoiceXMLFilename : String; out _CmdOutput,_VisualizationAsHTML : String) : Boolean;
    function MustangVisualizeFileAsPdf(const _InvoiceXMLFilename : String; out _CmdOutput : String; out _VisualizationAsPdf : TMemoryStream) : Boolean;
    function MustangCombinePdfAndXML(const _InvoicePDFFilename, _InvoiceXMLFilename : String; out _CmdOutput : String; out _CombinedPdf : TMemoryStream) : Boolean;
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

  _Filename := QuoteIfContainsSpace(_Filename);

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

function TXRechnungValidationHelperJava.MustangCombinePdfAndXML(
  const _InvoicePDFFilename, _InvoiceXMLFilename: String;
  out _CmdOutput: String; out _CombinedPdf: TMemoryStream): Boolean;
var
  cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(_InvoicePDFFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI.jar')+
            ' --action combine' +
            ' --source '+ QuoteIfContainsSpace(_InvoicePDFFilename)+
            ' --source-xml '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
            ' --out '+tmpFilename+'.pdf'+
            ' --format zf'+
            ' --version 2'+
            ' --profile T'+
            ' --no-additional-attachments');

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    if Result and FileExists(tmpFilename+'.pdf') then
    begin
      _CombinedPdf := TMemoryStream.Create;
      _CombinedPdf.LoadFromFile(tmpFilename+'.pdf');
      _CombinedPdf.Position := 0;
    end else
      _CombinedPdf := nil;

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    if FileExists(tmpFilename+'.pdf') then
      DeleteFile(tmpFilename+'.pdf');
  finally
    cmd.Free;
  end;
end;

function TXRechnungValidationHelperJava.MustangValidateFile(
  const _InvoiceXMLFilename: String; out _CmdOutput,
  _ValidationResultAsXML: String): Boolean;
var
  cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI.jar')+
            ' --action validate' +
            ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
            ' >'+tmpFilename+'.xml');

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    if FileExists(tmpFilename+'.xml') then
    begin
      _ValidationResultAsXML := TFile.ReadAllText(tmpFilename+'.xml',TEncoding.UTF8);
      Result := true;
    end;

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    if FileExists(tmpFilename+'.xml') then
      DeleteFile(tmpFilename+'.xml');
  finally
    cmd.Free;
  end;
end;

function TXRechnungValidationHelperJava.MustangVisualizeFile(
  const _InvoiceXMLFilename: String; out _CmdOutput,
  _VisualizationAsHTML: String): Boolean;
var
  cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI.jar')+
            ' --action visualize' +
            ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
            ' --out '+tmpFilename+'.html'+
            ' --language de');

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    if Result and FileExists(tmpFilename+'.html') then
    begin
      _VisualizationAsHTML := TFile.ReadAllText(tmpFilename+'.html',TEncoding.UTF8);
    end;

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    if FileExists(tmpFilename+'.html') then
      DeleteFile(tmpFilename+'.html');
    if FileExists(ExtractFilePath(tmpFilename)+'xrechnung-viewer.css') then
      DeleteFile(ExtractFilePath(tmpFilename)+'xrechnung-viewer.css');
    if FileExists(ExtractFilePath(tmpFilename)+'xrechnung-viewer.js') then
      DeleteFile(ExtractFilePath(tmpFilename)+'xrechnung-viewer.js');
  finally
    cmd.Free;
  end;
end;

function TXRechnungValidationHelperJava.MustangVisualizeFileAsPdf(
  const _InvoiceXMLFilename: String; out _CmdOutput: String;
  out _VisualizationAsPdf: TMemoryStream): Boolean;
var
  cmd: TStringList;
  tmpFilename : String;
begin
  Result := false;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(MustangprojectPath+'Mustang-CLI.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));

    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -Xmx1G '+
            '-Dfile.encoding=UTF-8 -jar '+QuoteIfContainsSpace(MustangprojectPath+'Mustang-CLI.jar')+
            ' --action pdf' +
            ' --source '+ QuoteIfContainsSpace(_InvoiceXMLFilename)+
            ' --out '+tmpFilename+'.pdf'+
            ' --language de');

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(tmpFilename+'.bat','');

    if Result and FileExists(tmpFilename+'.pdf') then
    begin
      _VisualizationAsPdf := TMemoryStream.Create;
      _VisualizationAsPdf.LoadFromFile(tmpFilename+'.pdf');
      _VisualizationAsPdf.Position := 0;
    end else
      _VisualizationAsPdf := nil;

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');
    if FileExists(tmpFilename+'.pdf') then
      DeleteFile(tmpFilename+'.pdf');
  finally
    cmd.Free;
  end;
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
  if not FileExists(ValidatorLibPath+'validationtool-1.5.0-java8-standalone.jar') then
    exit;
  if not FileExists(ValidatorConfigurationPath+'scenarios.xml') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  hstrl := TStringList.Create;
  cmd := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));
    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -classpath '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs')+' -jar '+
             QuoteIfContainsSpace(ValidatorLibPath+'validationtool-1.5.0-standalone.jar')+' -s '+
             QuoteIfContainsSpace(ValidatorConfigurationPath+'scenarios.xml')+' -r '+
             QuoteIfContainsSpace(ExcludeTrailingPathDelimiter(ValidatorConfigurationPath))+' -h '+
             QuoteIfContainsSpace(tmpFilename));
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
    cmd.Free;
  end;
end;

function TXRechnungValidationHelperJava.ValidateFile(
  const _InvoiceXMLFilename: String; out _CmdOutput,
  _ValidationResultAsXML, _ValidationResultAsHTML: String): Boolean;
var
  hstrl,cmd: TStringList;
  lInvoiceXMLFilename: String;
begin
  Result := false;
  if _InvoiceXMLFilename = '' then
    exit;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(ValidatorLibPath+'validationtool-1.5.0-java8-standalone.jar') then
    exit;
  if not FileExists(ValidatorConfigurationPath+'scenarios.xml') then
    exit;

  hstrl := TStringList.Create;
  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(_InvoiceXMLFilename)));
    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -classpath '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs')+' -jar '+
             QuoteIfContainsSpace(ValidatorLibPath+'validationtool-1.5.0-standalone.jar')+' -s '+
             QuoteIfContainsSpace(ValidatorConfigurationPath+'scenarios.xml')+' -r '+
             QuoteIfContainsSpace(ExcludeTrailingPathDelimiter(ValidatorConfigurationPath))+' -h '+
             QuoteIfContainsSpace(_InvoiceXMLFilename));
    cmd.SaveToFile(_InvoiceXMLFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(_InvoiceXMLFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    DeleteFile(_InvoiceXMLFilename+'.bat');

    lInvoiceXMLFilename := ExtractFileName(_InvoiceXMLFilename);
    lInvoiceXMLFilename := StringReplace(lInvoiceXMLFilename,' ','%20',[rfReplaceAll]);
    lInvoiceXMLFilename := ExtractFilePath(_InvoiceXMLFilename)+lInvoiceXMLFilename;

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
    cmd.Free;
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
  if not FileExists(ValidatorLibPath+'libs\Saxon-HE-11.4.jar') then
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

    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));
    if _TrueIfUBL_FalseIfCII then
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(tmpFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-xr.xml')))
    else
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(tmpFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\cii-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-xr.xml')));
    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-xr.xml'))+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\xrechnung-html.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-.html')));

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
    cmd.Free;
  end;
end;

function TXRechnungValidationHelperJava.VisualizeAsPdf(
  const _InvoiceXMLData: String; _TrueIfUBL_FalseIfCII: Boolean;
  out _CmdOutput: String; out _VisualizationAsPdf: TMemoryStream): Boolean;
var
  tmpFilename : String;
  hstrl,cmd: TStringList;
begin
  //Experimental - it does not work
  Result := false;
  if _InvoiceXMLData = '' then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(ValidatorLibPath+'libs\Saxon-HE-11.4.jar') then
    exit;
  if _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;
  if not FileExists(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;
  if not FileExists(FopLibPath+'fop\build\fop.jar') then
    exit;

  tmpFilename := TPath.GetTempFileName;

  hstrl := TStringList.Create;
  cmd := TStringList.Create;
  try
    hstrl.Text := _InvoiceXMLData;
    hstrl.SaveToFile(tmpFilename,TEncoding.UTF8);

    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));
    if _TrueIfUBL_FalseIfCII then
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(tmpFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-xr.xml')))
    else
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(tmpFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\cii-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-xr.xml')));
    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-xr.xml'))+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\xr-pdf.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-.fo'))); // geaendert von pdf auf fo

    cmd.SaveToFile(tmpFilename+'.bat',TEncoding.ANSI); //ToDo
    //cmd.SaveToFile(_InvoiceXMLFilename+'.bat');

    Result := ExecAndWait(tmpFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    DeleteFile(tmpFilename+'.bat');

    if not Result then
      exit;

    ////////////////////////////////////////////////////////////////////////////
    // Fopper aufrufen. Datei ist eine fo Datei. Saxon HE gibt eine fo-Datei zurueck!
    // cmd Inhalt aus der apache-fop\foop\fop.bat ausgelesen mit echo "%JAVACMD%" %JAVAOPTS% %LOGCHOICE% %LOGLEVEL% -cp "%LOCALCLASSPATH%" %FOP_OPTS% org.apache.fop.cli.Main %FOP_CMD_LINE_ARGS%
    if FileExists(ChangeFileExt(tmpFilename,'-.fo')) then
    begin
      cmd.Clear;
      cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(tmpFilename)));
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
      QuoteIfContainsSpace(FopLibPath+'fop\build\fop.jar;'+FopLibPath+'fop\lib\batik-all-1.16.jar;' +
                           FopLibPath+'fop\lib\commons-io-2.11.0.jar;'+FopLibPath+'fop\lib\commons-logging-1.0.4.jar;' +
                           FopLibPath+'fop\lib\fontbox-2.0.24.jar;'+FopLibPath+'fop\lib\serializer-2.7.2.jar;' +
                           FopLibPath+'fop\lib\xml-apis-1.4.01.jar;'+FopLibPath+'fop\lib\xml-apis-ext-1.3.04.jar;' +
                           FopLibPath+'fop\lib\xmlgraphics-commons-2.8.jar;') +
        ' org.apache.fop.cli.Main ' +
        QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-.fo')) + ' ' +
        QuoteIfContainsSpace(ChangeFileExt(tmpFilename,'-.pdf') ));

      cmd.SaveToFile(tmpFilename+'.bat');

      Result := ExecAndWait(tmpFilename+'.bat','');

     _CmdOutput := _CmdOutput + #13#10 + CmdOutput.Text;

     DeleteFile(tmpFilename+'.bat');
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
    hstrl.Free;
    cmd.Free;
  end;
end;

function TXRechnungValidationHelperJava.VisualizeFile(
  const _InvoiceXMLFilename: String; _TrueIfUBL_FalseIfCII: Boolean;
  out _CmdOutput, _VisualizationAsHTML: String): Boolean;
var
  hstrl,cmd: TStringList;
begin
  Result := false;
  if _InvoiceXMLFilename = '' then
    exit;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(ValidatorLibPath+'libs\Saxon-HE-11.4.jar') then
    exit;
  if _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;

  if not FileExists(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;

  hstrl := TStringList.Create;
  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(_InvoiceXMLFilename)));
    if _TrueIfUBL_FalseIfCII then
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(_InvoiceXMLFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml')))
    else
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(_InvoiceXMLFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\cii-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml')));
    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml'))+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\xrechnung-html.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-.html')));

    cmd.SaveToFile(_InvoiceXMLFilename+'.bat',TEncoding.ANSI);

    Result := ExecAndWait(_InvoiceXMLFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    DeleteFile(_InvoiceXMLFilename+'.bat');
    DeleteFile(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml'));

    if FileExists(ChangeFileExt(_InvoiceXMLFilename,'-.html')) then
    begin
      hstrl.LoadFromFile(ChangeFileExt(_InvoiceXMLFilename,'-.html'),TEncoding.UTF8);
      _VisualizationAsHTML := hstrl.Text;
      DeleteFile(ChangeFileExt(_InvoiceXMLFilename,'-.html'));
    end else
      Result := false;

  finally
    hstrl.Free;
    cmd.Free;
  end;
end;

function TXRechnungValidationHelperJava.VisualizeFileAsPdf(
  const _InvoiceXMLFilename: String; _TrueIfUBL_FalseIfCII: Boolean;
  out _CmdOutput: String; out _VisualizationAsPdf: TMemoryStream): Boolean;
var
  cmd: TStringList;
begin
  //Experimental - it does not work
  Result := false;
  if _InvoiceXMLFilename = '' then
    exit;
  if not FileExists(_InvoiceXMLFilename) then
    exit;
  if not FileExists(JavaRuntimeEnvironmentPath+'bin\java.exe') then
    exit;
  if not FileExists(ValidatorLibPath+'libs\Saxon-HE-11.4.jar') then
    exit;
  if _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl') then
    exit;
  if not _TrueIfUBL_FalseIfCII then
  if not FileExists(VisualizationLibPath+'xsl\cii-xr.xsl') then
    exit;
  if not FileExists(VisualizationLibPath+'xsl\xrechnung-html.xsl') then
    exit;
  if not FileExists(FopLibPath+'fop\build\fop.jar') then
    exit;

  cmd := TStringList.Create;
  try
    cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(_InvoiceXMLFilename)));
    if _TrueIfUBL_FalseIfCII then
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(_InvoiceXMLFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\ubl-invoice-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml')))
    else
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(_InvoiceXMLFilename)+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\cii-xr.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml')));
    cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
             QuoteIfContainsSpace(ValidatorLibPath+'libs\Saxon-HE-11.4.jar;'+ValidatorLibPath+'libs\xmlresolver-4.4.3.jar')+
             ' net.sf.saxon.Transform'+' -s:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml'))+
             ' -xsl:'+QuoteIfContainsSpace(VisualizationLibPath+'xsl\xr-pdf.xsl')+
             ' -o:'+QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-.fo'))); // geaendert von pdf auf fo

    cmd.SaveToFile(_InvoiceXMLFilename+'.bat',TEncoding.ANSI); //ToDo
    //cmd.SaveToFile(_InvoiceXMLFilename+'.bat');

    Result := ExecAndWait(_InvoiceXMLFilename+'.bat','');

    _CmdOutput := CmdOutput.Text;

    DeleteFile(_InvoiceXMLFilename+'.bat');

    if not Result then
      exit;

    ////////////////////////////////////////////////////////////////////////////
    // Fopper aufrufen. Datei ist eine fo Datei. Saxon HE gibt eine fo-Datei zurueck!
    // cmd Inhalt aus der apache-fop\foop\fop.bat ausgelesen mit echo "%JAVACMD%" %JAVAOPTS% %LOGCHOICE% %LOGLEVEL% -cp "%LOCALCLASSPATH%" %FOP_OPTS% org.apache.fop.cli.Main %FOP_CMD_LINE_ARGS%
    if FileExists(ChangeFileExt(_InvoiceXMLFilename,'-.fo')) then
    begin
      cmd.Clear;
      cmd.Add('pushd '+QuoteIfContainsSpace(ExtractFilePath(_InvoiceXMLFilename)));
      cmd.Add(QuoteIfContainsSpace(JavaRuntimeEnvironmentPath+'bin\java.exe')+' -cp '+
      QuoteIfContainsSpace(FopLibPath+'fop\build\fop.jar;'+FopLibPath+'fop\lib\batik-all-1.16.jar;' +
                           FopLibPath+'fop\lib\commons-io-2.11.0.jar;'+FopLibPath+'fop\lib\commons-logging-1.0.4.jar;' +
                           FopLibPath+'fop\lib\fontbox-2.0.24.jar;'+FopLibPath+'fop\lib\serializer-2.7.2.jar;' +
                           FopLibPath+'fop\lib\xml-apis-1.4.01.jar;'+FopLibPath+'fop\lib\xml-apis-ext-1.3.04.jar;' +
                           FopLibPath+'fop\lib\xmlgraphics-commons-2.8.jar;') +
        ' org.apache.fop.cli.Main ' +
        QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-.fo')) + ' ' +
        QuoteIfContainsSpace(ChangeFileExt(_InvoiceXMLFilename,'-.pdf') ));

      cmd.SaveToFile(_InvoiceXMLFilename+'.bat');

      Result := ExecAndWait(_InvoiceXMLFilename+'.bat','');

     _CmdOutput := _CmdOutput + #13#10 + CmdOutput.Text;

     DeleteFile(_InvoiceXMLFilename+'.bat');
     DeleteFile(ChangeFileExt(_InvoiceXMLFilename,'-.fo'));
    end else
      Result := false;

    DeleteFile(ChangeFileExt(_InvoiceXMLFilename,'-xr.xml'));
    ////////////////////////////////////////////////////////////////////////////
    if FileExists(ChangeFileExt(_InvoiceXMLFilename,'-.pdf')) then
    begin
      _VisualizationAsPdf := TMemoryStream.Create;
      _VisualizationAsPdf.LoadFromFile(ChangeFileExt(_InvoiceXMLFilename,'-.pdf'));
      _VisualizationAsPdf.Position := 0;
      DeleteFile(ChangeFileExt(_InvoiceXMLFilename,'-.pdf'));
    end else
      Result := false;
  finally
    cmd.Free;
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
