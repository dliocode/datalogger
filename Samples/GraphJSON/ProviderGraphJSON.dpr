program ProviderGraphJSON;

uses
  Vcl.Forms,
  UProviderGraphJSON in 'UProviderGraphJSON.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
