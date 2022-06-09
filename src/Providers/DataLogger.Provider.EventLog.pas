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
    FName: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    property Name: string read FName write FName;

    constructor Create(const AName: string = '');
    destructor Destroy; override;
  end;

implementation

{ TProviderEventLog }

constructor TProviderEventLog.Create(const AName: string = '');
begin
  inherited Create;

{$IF DEFINED(MSWINDOWS)}
  if AName.Trim.IsEmpty then
    FName := TLoggerUtils.AppName
  else
    FName := AName;
{$ENDIF}
end;

destructor TProviderEventLog.Destroy;
begin
{$IF DEFINED(MSWINDOWS)}

{$ENDIF}
  inherited;
end;

procedure TProviderEventLog.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(MSWINDOWS)}
var
  LEventLogger: TEventLogger;
  LRetryCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LEventType: DWord;
begin
  if Length(ACache) = 0 then
    Exit;

  LEventLogger := TEventLogger.Create(FName);
  try
    for LItem in ACache do
    begin
      if LItem.&Type = TLoggerType.All then
        Continue;

      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

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
          LEventLogger.LogMessage(LLog, LEventType, 0, 0);
          Break;
        except
          on E: Exception do
          begin
            Inc(LRetryCount);

            if Assigned(FLogException) then
              FLogException(Self, LItem, E, LRetryCount);

            if Self.Terminated then
              Exit;

            if LRetryCount >= FMaxRetry then
              Break;
          end;
        end;
    end;
  finally
    LEventLogger.Free;
  end;
end;
{$ELSE}


begin
end;
{$ENDIF}

end.
