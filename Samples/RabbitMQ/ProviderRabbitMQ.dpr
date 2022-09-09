program ProviderRabbitMQ;

uses
  Vcl.Forms,
  UProviderRabbitMQ in 'UProviderRabbitMQ.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
