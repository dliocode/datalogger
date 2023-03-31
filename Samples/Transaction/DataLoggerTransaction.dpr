program DataLoggerTransaction;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  DataLogger,
  DataLogger.Provider.Console,
  System.SysUtils;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Definindo o formato do log
  Logger.SetTemplate('${timestamp} [${type}] ${message}');

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
