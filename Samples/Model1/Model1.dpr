program Model1;

uses
  System.SysUtils,
  DataLogger in '..\..\src\Core\DataLogger.pas',
  DataLogger.Provider in '..\..\src\Core\DataLogger.Provider.pas',
  DataLogger.Types in '..\..\src\Core\DataLogger.Types.pas',
  DataLogger.Utils in '..\..\src\Core\DataLogger.Utils.pas',
  DataLogger.Provider.Console in '..\..\src\Providers\DataLogger.Provider.Console.pas';

begin
  Logger.AddProvider(TProviderConsole.Create);

  // Quero exibir: [ Data e Hora ] [ Tipo ] [ Tag ] - Mensagem

  Logger.SetLogFormat(Format('[ %s ] [ %s ] [ %s ] - %s ',[TLoggerFormat.LOG_TIMESTAMP, TLoggerFormat.LOG_TYPE, TLoggerFormat.LOG_TAG, TLoggerFormat.LOG_MESSAGE]));

  // Definindo o formato do Timestamp
  Logger.SetFormatTimestamp('dd/mm/yyyy hh:mm:ss');

  Logger
    .Trace('Minha mensagem no Log','CLASS_PRINCIPAL')
    .Debug('Minha mensagem no Log','CLASS_PRINCIPAL')
    .Info('Minha mensagem no Log','CLASS_PRINCIPAL')
    .Success('Minha mensagem no Log','CLASS_PRINCIPAL')
    .Warn('Minha mensagem no Log','CLASS_PRINCIPAL')
    .Error('Minha mensagem no Log','CLASS_PRINCIPAL')
    .Fatal('Minha mensagem no Log','CLASS_PRINCIPAL')
  ;

  // output: [ 19/05/2021 08:15:59 ] [ Info ] [ CLASS_PRINCIPAL ] - Minha mensagem no Log

  Readln;
end.
