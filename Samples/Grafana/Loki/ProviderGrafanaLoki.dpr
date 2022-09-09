program ProviderGrafanaLoki;

uses
  Vcl.Forms,
  UProviderGrafanaLoki in 'UProviderGrafanaLoki.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
