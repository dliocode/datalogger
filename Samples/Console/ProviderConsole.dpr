program ProviderConsole;

{$APPTYPE CONSOLE}

uses
  DataLogger,
  DataLogger.Provider.Console;             

{$R *.res}

begin
  Logger.AddProvider(
    TProviderConsole.Create
//    .UseColorInConsole(True)
//    .UseColorOnlyInTypes(False)
//    .ChangeColor(TLoggerType.Trace, TColor.Black, TColor.Red)
    );

  // Log Format
  Logger.SetLogFormat(TLoggerFormat.LOG_TIMESTAMP + ' - [' + TLoggerFormat.LOG_TYPE + ']: ' + TLoggerFormat.LOG_MESSAGE);

  Logger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal')
    .CustomType('CUSTOM TYPE','My Custom')    
    ;
  Readln;

end.
