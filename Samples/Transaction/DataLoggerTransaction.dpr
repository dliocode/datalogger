program DataLoggerTransaction;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DataLogger in '..\..\src\Core\DataLogger.pas',
  DataLogger.Provider in '..\..\src\Core\DataLogger.Provider.pas',
  DataLogger.Types in '..\..\src\Core\DataLogger.Types.pas',
  DataLogger.Utils in '..\..\src\Core\DataLogger.Utils.pas',
  DataLogger.Provider.Console in '..\..\src\Providers\DataLogger.Provider.Console.pas';

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${type}] ${message}');

  // Iniciando a transaction
  Logger.StartTransaction;

  Writeln('Iniciou a transaction');

  // Gerando o log
  Logger.Info('Minha mensagem no Log do tipo INFO 1 ');
  Logger.Info('Minha mensagem no Log do tipo INFO 2');
  Logger.Info('Minha mensagem no Log do tipo INFO 3');
  Logger.Info('Minha mensagem no Log do tipo INFO 4');
  Logger.Info('Minha mensagem no Log do tipo INFO 5');
  Logger.Info('Minha mensagem no Log do tipo INFO 6');
  Logger.Info('Minha mensagem no Log do tipo INFO 7');
  Logger.Info('Minha mensagem no Log do tipo INFO 8');
  Logger.Info('Minha mensagem no Log do tipo INFO 9');

  Writeln('Terminou os Logs');

  // Fazendo o commit
  Logger.CommitTransaction;
  Writeln('Fez os commits');

  Readln;
end.
