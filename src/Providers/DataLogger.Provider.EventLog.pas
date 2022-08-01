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
  System.SysUtils, System.Types, System.JSON;

type
  TProviderEventLog = class(TDataLoggerProvider)
  private
    FName: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Name(const AValue: string): TProviderEventLog;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

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

procedure TProviderEventLog.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
begin
  if AJSON.Trim.IsEmpty then
    Exit;

  try
    LJO := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  except
    on E: Exception do
      Exit;
  end;

  if not Assigned(LJO) then
    Exit;

  try
    Name(LJO.GetValue<string>('name', FName));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderEventLog.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('name', FName);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderEventLog.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(MSWINDOWS)}
var
  LEventLogger: TEventLogger;
  LRetriesCount: Integer;
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
      if LItem.InternalItem.TypeSlineBreak then
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

      LRetriesCount := 0;

      while True do
        try
          LEventLogger.LogMessage(LLog, LEventType, 0, 0);
          Break;
        except
          on E: Exception do
          begin
            Inc(LRetriesCount);

            Sleep(50);

            if Assigned(FLogException) then
              FLogException(Self, LItem, E, LRetriesCount);

            if Self.Terminated then
              Exit;

            if LRetriesCount <= 0 then
              Break;

            if LRetriesCount >= FMaxRetries then
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

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderEventLog);

end.
