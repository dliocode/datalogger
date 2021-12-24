{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.EventLog;

interface

uses
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils,
{$IF DEFINED(MSWINDOWS)}
  Vcl.SvcMgr, Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.Types;

type
  TProviderEventLog = class(TDataLoggerProvider)
  private
{$IF DEFINED(MSWINDOWS)}
    FEventLogger: TEventLogger;
{$ENDIF}
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AName: string = '');
    destructor Destroy; override;
  end;

implementation

{ TProviderEventLog }

constructor TProviderEventLog.Create(const AName: string = '');
{$IF DEFINED(MSWINDOWS)}
var
  LName: string;
{$ENDIF}
begin
  inherited Create;

{$IF DEFINED(MSWINDOWS)}
  if AName.Trim.IsEmpty then
    LName := TLoggerUtils.AppName
  else
    LName := AName;

  FEventLogger := TEventLogger.Create(LName);
{$ENDIF}
end;

destructor TProviderEventLog.Destroy;
begin
{$IF DEFINED(MSWINDOWS)}
  FEventLogger.Free;
{$ENDIF}
end;

procedure TProviderEventLog.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(MSWINDOWS)}
var
  LRetryCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LEventType: DWord;
begin
  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

    case LItem.&Type of
      TLoggerType.Debug:
        LEventType := EVENTLOG_INFORMATION_TYPE;
      TLoggerType.Info:
        LEventType := EVENTLOG_INFORMATION_TYPE;
      TLoggerType.Warn:
        LEventType := EVENTLOG_WARNING_TYPE;
      TLoggerType.Error:
        LEventType := EVENTLOG_ERROR_TYPE;
      TLoggerType.Success:
        LEventType := EVENTLOG_SUCCESS;
    else
      LEventType := EVENTLOG_INFORMATION_TYPE;
    end;

    LRetryCount := 0;

    while True do
      try
        FEventLogger.LogMessage(LLog, LEventType, 0, 0);
        Break;
      except
        on E: Exception do
        begin
          Inc(LRetryCount);

          if Assigned(LogException) then
            LogException(Self, LItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= GetMaxRetry then
            Break;
        end;
      end;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

end.
