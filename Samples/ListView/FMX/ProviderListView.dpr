program ProviderListView;

uses
  System.StartUpCopy,
  FMX.Forms,
  UProviderListView in 'UProviderListView.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
