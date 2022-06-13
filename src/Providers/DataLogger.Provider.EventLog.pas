{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Administrative Tools\Event Viewer.lnk

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
    function Name(const AValue: string): TProviderEventLog;

    constructor Create; overload;
  end;

implementation

{ TProviderEventLog }

constructor TProviderEventLog.Create;
begin
  inherited Create;

  Name(TLoggerUtils.AppName);
end;

function TProviderEventLog.Name(const AValue: string): TProviderEventLog;
begin
  Result := Self;

  if AValue.Trim.IsEmpty then
    FName := TLoggerUtils.AppName
  else
    FName := AValue;
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

            Sleep(50);

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
