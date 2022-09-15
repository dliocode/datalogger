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
//    .UseColorOnlyInTypes(True)

//    .ChangeColor(TLoggerType.Trace, TColor.Black, TColor.Magenta)
//    .ChangeColor(TLoggerType.Debug, TColor.Black, TColor.Cyan)
//    .ChangeColor(TLoggerType.Info, TColor.Black, TColor.White)
//    .ChangeColor(TLoggerType.Success, TColor.Black, TColor.Green)
//    .ChangeColor(TLoggerType.Warn, TColor.Black, TColor.Yellow)
//    .ChangeColor(TLoggerType.Error, TColor.Black, TColor.Red)
//    .ChangeColor(TLoggerType.Fatal, TColor.Black, TColor.DarkRed)
//    .ChangeColor(TLoggerType.Custom, TColor.Black, TColor.White)
    );

  // Log Format
  Logger.SetLogFormat(TLoggerFormat.LOG_TIMESTAMP + ' - [' + TLoggerFormat.LOG_LEVEL + ']: ' + TLoggerFormat.LOG_MESSAGE);

  Logger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal')
    .Custom('Custom Level','My Custom')
    ;

  Readln;
end.
