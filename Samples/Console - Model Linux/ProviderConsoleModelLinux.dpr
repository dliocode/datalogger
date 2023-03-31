program ProviderConsoleModelLinux;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  DataLogger,
  DataLogger.Provider.Console,
  System.SysUtils;

begin
  // Add Provider
  Logger.AddProvider(TProviderConsole.Create);

  // Change the level name
  Logger.SetLevelName(TLoggerLevel.Success, '    OK    ');
  Logger.SetLevelName(TLoggerLevel.Error,   '   FAIL   ');
  Logger.SetLevelName(TLoggerLevel.Fatal,   ' CRITICAL ');
  Logger.SetLevelName(TLoggerLevel.Warn,    ' LOADING  ');
  Logger.SetLevelName(TLoggerLevel.Info,    '   INFO   ');

  // Defining the format
  Logger.SetTemplate(Format(' [%s] %s: %s', [TLoggerTemplate.LOG_LEVEL, TLoggerTemplate.LOG_TIMESTAMP, TLoggerTemplate.LOG_MESSAGE]));

  // Generating the logs
  Logger.Info('Test with the function: SetLevelName');
  Sleep(2000);
  Logger.SlineBreak;
  Logger.Success('Database connection');
  Sleep(2000);
  Logger.Error('save records');
  Sleep(2000);
  Logger.Fatal('System failure');
  Sleep(2000);
  Logger.SlineBreak;
  Logger.SlineBreak;

  // System loading simulation - using UndoLast
  Logger.Info('Test with the function: UndoLast');
  Sleep(2000);

  Logger.SlineBreak;
  Logger.Warn('Load System - Kernel');
  Sleep(2000);
  Logger.UndoLast;
  Logger.Success('Load System - Kernel');
  Sleep(2000);

  Logger.Warn('Load System - DataBase');
  Sleep(2000);
  Logger.UndoLast;
  Logger.Error('Load System - DataBase - Erro connection in DataBase');
  Sleep(2000);

  Logger.Warn('Load System - Drivers');
  Sleep(2000);
  Logger.UndoLast;
  Logger.Fatal('Load System - Driver - System Failure');

  Readln;
end.
