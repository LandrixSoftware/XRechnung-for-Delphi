{
Copyright (C) 2024 Landrix Software GmbH & Co. KG
Sven Harazim, info@landrix.de
Version 3.0.2

License
This file is not official part of the package XRechnung-for-Delphi.

This is provided as is, expressly without a warranty of any kind.
You use it at your own risc.
}

program XRechnungProject;

uses
  Vcl.Forms,
  XRechnungUnit1 in 'XRechnungUnit1.pas' {Form1},
  XRechnungUnit2TestCases in 'XRechnungUnit2TestCases.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
