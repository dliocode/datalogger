program ProviderSendEmail;

uses
  Vcl.Forms,
  UProviderSendEmail in 'UProviderSendEmail.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
