program ProviderTwilioWhatsApp;

uses
  Vcl.Forms,
  UProviderTwilioWhatsApp in 'UProviderTwilioWhatsApp.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
