program ProviderConsoleSimple;

{$APPTYPE CONSOLE}


uses
  DataLogger,
  DataLogger.Simple,
  DataLogger.Provider.Console;

{$R *.res}


begin
  Logger.AddProvider(TProviderConsole.Create);

  // Log Format
  Logger.SetTemplate(TLoggerTemplate.LOG_TIMESTAMP + ' - [' + TLoggerTemplate.LOG_LEVEL + ']: ' + TLoggerTemplate.LOG_MESSAGE);

  TDataLoggerSimple.SetDataLogger(Logger);

  Trace('My Trace');
  Debug('My Debug');
  Info('My Info');
  Warn('My Warn');
  Error('My Error');
  Success('My Success');
  Fatal('My Fatal');
  Custom('Custom Level', 'My Custom');

  SlineBreak;

  // or

  T('My Trace');
  D('My Debug');
  I('My Info');
  W('My Warn');
  E('My Error');
  S('My Success');
  F('My Fatal');
  C('Custom Level', 'My Custom');

  Readln;
end.
