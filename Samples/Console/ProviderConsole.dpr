program ProviderConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  DataLogger in '..\..\src\Core\DataLogger.pas',
  DataLogger.Provider in '..\..\src\Core\DataLogger.Provider.pas',
  DataLogger.Types in '..\..\src\Core\DataLogger.Types.pas',
  DataLogger.Utils in '..\..\src\Core\DataLogger.Utils.pas',
  DataLogger.Provider.Console in '..\..\src\Providers\DataLogger.Provider.Console.pas';

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
    .Trace('My Trace')
    .Debug('My Debug')
    .Info('My Info')
    .Success('My Success')
    .Warn('My Warn')
    .Error('My Error')
    .Fatal('My Fatal');

  Readln;

end.
