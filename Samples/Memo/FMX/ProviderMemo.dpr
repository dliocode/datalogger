program ProviderMemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UProviderMemo in 'UProviderMemo.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
