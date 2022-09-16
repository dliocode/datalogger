program DataLoggerTransactionAninhada;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  DataLogger,
  DataLogger.Provider.Console,
  System.SysUtils;

begin
  Logger.AddProvider(TProviderConsole.Create.UseTransaction(True));

  // Definindo o formato do log
  Logger.SetLogFormat('${timestamp} [${level}] ${message}');

  // Iniciando a transaction
  Logger.StartTransaction;
  try
    // Gerando o log
    Logger.Info('Minha mensagem no Log do tipo INFO 1 ');
    Logger.Info('Minha mensagem no Log do tipo INFO 2');
    Logger.Info('Minha mensagem no Log do tipo INFO 3');
    Logger.Info('Minha mensagem no Log do tipo INFO 4');

    // Iniciando 2 transaction
    Logger.StartTransaction;
    try
      Logger.Info('Minha mensagem no Log do tipo INFO 5');
      Logger.Info('Minha mensagem no Log do tipo INFO 6');
    finally
      // fazendo Rollback da segunda transaction
      Logger.RollbackTransaction;
    end;

    Logger.Info('Minha mensagem no Log do tipo INFO 7');
    Logger.Info('Minha mensagem no Log do tipo INFO 8');
    Logger.Info('Minha mensagem no Log do tipo INFO 9');
  finally
    // Fazendo o commit
    Logger.CommitTransaction;
  end;

  Readln;
end.


