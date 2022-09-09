program ProviderElasticSearch;

uses
  Vcl.Forms,
  UProviderElasticSearch in 'UProviderElasticSearch.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
