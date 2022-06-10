{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.SysLog;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  IdSysLog, IdSysLogMessage,
  System.SysUtils;

type
  TProviderSysLog = class(TDataLoggerProvider)
  private
    FSysLog: TIdSysLog;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Host(const AValue: string): TProviderSysLog;
    function Port(const AValue: Integer): TProviderSysLog;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSysLog }

constructor TProviderSysLog.Create;
begin
  inherited Create;

  FSysLog := TIdSysLog.Create(nil);
  FSysLog.Host := '';
  FSysLog.Port := 514;
end;

destructor TProviderSysLog.Destroy;
begin
  FSysLog.Free;
  inherited;
end;

function TProviderSysLog.Host(const AValue: string): TProviderSysLog;
begin
  Result := Self;
  FSysLog.Host := AValue;
end;

function TProviderSysLog.Port(const AValue: Integer): TProviderSysLog;
begin
  Result := Self;
  FSysLog.Port := AValue;
end;

procedure TProviderSysLog.Save(const ACache: TArray<TLoggerItem>);
var
  LRetryCount: Integer;
  LCache: TArray<TLoggerItem>;
  LItem: TLoggerItem;
  LLog: string;
  LSysLogMessage: TIdSysLogMessage;
begin
  if Length(ACache) = 0 then
    Exit;

  for LItem in LCache do
  begin
    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LSysLogMessage := TIdSysLogMessage.Create(nil);
    try
      LSysLogMessage.TimeStamp := LItem.TimeStamp;
      LSysLogMessage.Hostname := LItem.ComputerName;
      LSysLogMessage.Facility := TIdSyslogFacility.sfUserLevel;;
      LSysLogMessage.Msg.Process := LItem.ProcessId;
      LSysLogMessage.Msg.PID := LItem.ProcessId.ToInteger;

      case LItem.&Type of
        TLoggerType.Trace:
          LSysLogMessage.Severity := TIdSyslogSeverity.slInformational;
        TLoggerType.Debug:
          LSysLogMessage.Severity := TIdSyslogSeverity.slDebug;
        TLoggerType.Info:
          LSysLogMessage.Severity := TIdSyslogSeverity.slInformational;
        TLoggerType.Warn:
          LSysLogMessage.Severity := TIdSyslogSeverity.slWarning;
        TLoggerType.Error:
          LSysLogMessage.Severity := TIdSyslogSeverity.slError;
        TLoggerType.Success:
          LSysLogMessage.Severity := TIdSyslogSeverity.slNotice;
        TLoggerType.Fatal:
          LSysLogMessage.Severity := TIdSyslogSeverity.slCritical;
      end;

      LSysLogMessage.Msg.Text := LItem.Message;
      if LSysLogMessage.Msg.Text.Trim.IsEmpty then
        LSysLogMessage.Msg.Text := LItem.MessageJSON;

      if LSysLogMessage.Msg.Text.Trim.IsEmpty then
        Exit;

      LRetryCount := 0;

      while True do
        try
          if not FSysLog.Connected then
            FSysLog.Connect;

          FSysLog.SendLogMessage(LSysLogMessage, False);

          Break;
        except
          on E: Exception do
          begin
            Inc(LRetryCount);

            Sleep(50);

            if Assigned(FLogException) then
              FLogException(Self, LItem, E, LRetryCount);

            if Self.Terminated then
              Exit;

            if LRetryCount >= FMaxRetry then
              Break;
          end;
        end;
    finally
      LSysLogMessage.Free;
    end;
  end;
end;

end.
