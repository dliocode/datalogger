program ProviderSysLog;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.StrUtils,
  DataLogger,
  DataLogger.Provider.SysLog;

begin
  Logger.AddProvider(TProviderSysLog.Create);

  // Log Format
  Logger.SetTemplate('[' + TLoggerTemplate.LOG_LEVEL + ']: ' + TLoggerTemplate.LOG_MESSAGE);

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

  Writeln('See logs in /var/log/syslog');
end.
