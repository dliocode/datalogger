program ProviderRabbitMQ;

uses
  Vcl.Forms,
  UProviderRabbitMQ in 'UProviderRabbitMQ.pas' {Form2},
  DataLogger in '..\..\src\Core\DataLogger.pas',
  DataLogger.Provider in '..\..\src\Core\DataLogger.Provider.pas',
  DataLogger.Simple in '..\..\src\Core\DataLogger.Simple.pas',
  DataLogger.Types in '..\..\src\Core\DataLogger.Types.pas',
  DataLogger.Utils in '..\..\src\Core\DataLogger.Utils.pas',
  DataLogger.Provider.RabbitMQ in '..\..\src\Providers\DataLogger.Provider.RabbitMQ.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
