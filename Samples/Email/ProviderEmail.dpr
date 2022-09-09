program ProviderEmail;

uses
  Vcl.Forms,
  UProviderEmail in 'UProviderEmail.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
