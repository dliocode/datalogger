program Model1;

uses
  System.SysUtils,
  DataLogger in '..\..\src\Core\DataLogger.pas',
  DataLogger.Provider in '..\..\src\Core\DataLogger.Provider.pas',
  DataLogger.Types in '..\..\src\Core\DataLogger.Types.pas',
  DataLogger.Utils in '..\..\src\Core\DataLogger.Utils.pas',
  DataLogger.Provider.Console in '..\..\src\Providers\DataLogger.Provider.Console.pas',
  DataLogger.Provider.ElasticSearch in '..\..\src\Providers\DataLogger.Provider.ElasticSearch.pas',
  DataLogger.Provider.Email in '..\..\src\Providers\DataLogger.Provider.Email.pas',
  DataLogger.Provider.EventLog in '..\..\src\Providers\DataLogger.Provider.EventLog.pas',
  DataLogger.Provider.Events in '..\..\src\Providers\DataLogger.Provider.Events.pas',
  DataLogger.Provider.ListBox in '..\..\src\Providers\DataLogger.Provider.ListBox.pas',
  DataLogger.Provider.ListView in '..\..\src\Providers\DataLogger.Provider.ListView.pas',
  DataLogger.Provider.Logstach in '..\..\src\Providers\DataLogger.Provider.Logstach.pas',
  DataLogger.Provider.Memo in '..\..\src\Providers\DataLogger.Provider.Memo.pas',
  DataLogger.Provider.OutputDebugString in '..\..\src\Providers\DataLogger.Provider.OutputDebugString.pas',
  DataLogger.Provider.Redis in '..\..\src\Providers\DataLogger.Provider.Redis.pas',
  DataLogger.Provider.REST.HTTPClient in '..\..\src\Providers\DataLogger.Provider.REST.HTTPClient.pas',
  DataLogger.Provider.REST.Indy in '..\..\src\Providers\DataLogger.Provider.REST.Indy.pas',
  DataLogger.Provider.REST.NetHTTPClient in '..\..\src\Providers\DataLogger.Provider.REST.NetHTTPClient.pas',
  DataLogger.Provider.Slack in '..\..\src\Providers\DataLogger.Provider.Slack.pas',
  DataLogger.Provider.SysLog in '..\..\src\Providers\DataLogger.Provider.SysLog.pas',
  DataLogger.Provider.Telegram in '..\..\src\Providers\DataLogger.Provider.Telegram.pas',
  DataLogger.Provider.TextFile in '..\..\src\Providers\DataLogger.Provider.TextFile.pas';


begin
  Logger.AddProvider(TProviderConsole.Create);

  // Quero exibir: [ Data e Hora ] [ Tipo ] [ Tag ] - Mensagem

  Logger.SetLogFormat(Format('[ %s ] [ %s ] [ %s ] - %s ',[TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_TYPE, TLoggerFormat.LOG_TAG, TLoggerFormat.LOG_MESSAGE]));

  // Definindo o formato do Timestamp
  Logger.SetFormatTimestamp('dd/mm/yyyy hh:mm:ss');

  Logger.Info('Minha mensagem no Log','CLASS_PRINCIPAL');

  // output: [ 19/05/2021 08:15:59 ] [ Info ] [ CLASS_PRINCIPAL ] - Minha mensagem no Log

  Readln;
end.
