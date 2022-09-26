program ProviderConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  DataLogger,
  DataLogger.Provider.Console;

begin
  Logger.AddProvider(
    TProviderConsole.Create
    // .UseColorInConsole(True)
    // .UseColorOnlyInLevels(True)

    // .ChangeColor(TLoggerLevel.Trace, TColor.Black, TColor.Magenta)
    // .ChangeColor(TLoggerLevel.Debug, TColor.Black, TColor.Cyan)
    // .ChangeColor(TLoggerLevel.Info, TColor.Black, TColor.White)
    // .ChangeColor(TLoggerLevel.Success, TColor.Black, TColor.Green)
    // .ChangeColor(TLoggerLevel.Warn, TColor.Black, TColor.Yellow)
    // .ChangeColor(TLoggerLevel.Error, TColor.Black, TColor.Red)
    // .ChangeColor(TLoggerLevel.Fatal, TColor.Black, TColor.DarkRed)
    // .ChangeColor(TLoggerLevel.Custom, TColor.Black, TColor.White)
    );

  // Log Format
  Logger.SetLogFormat(TLoggerFormat.LOG_TIMESTAMP + ' - [' + TLoggerFormat.LOG_LEVEL + ']: ' + TLoggerFormat.LOG_MESSAGE);

  Logger
    .Trace('My Trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal')
    .Custom('Custom Level', 'My Custom')
    ;

  Logger.SlineBreak;

  // or

  Logger
    .T('My Trace')
    .D('My Debug')
    .I('My Info')
    .W('My Warn')
    .E('My Error')
    .S('My Success')
    .F('My Fatal')
    .C('Custom Level', 'My Custom')
    ;

  Readln;

end.
