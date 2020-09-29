program XRechnungProject;

uses
  Vcl.Forms,
  XRechnungUnit1 in 'XRechnungUnit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
