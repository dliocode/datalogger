{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://sematext.com/
// https://sematext.com/docs/

unit DataLogger.Provider.Sematext.Logs;

interface

uses
{$IF DEFINED(DATALOGGER_SEMATEXT_LOGS_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SEMATEXT_LOGS_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON;

type
{$IF DEFINED(DATALOGGER_SEMATEXT_LOGS_USE_INDY)}
  TProviderSematextLogs = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_SEMATEXT_LOGS_USE_NETHTTPCLIENT)}
  TProviderSematextLogs = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderSematextLogs = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FAppToken: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function AppToken(const AValue: string): TProviderSematextLogs;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderSematextLogs }

constructor TProviderSematextLogs.Create;
begin
  inherited Create;

  ContentType('application/json');
end;

function TProviderSematextLogs.AppToken(const AValue: string): TProviderSematextLogs;
begin
  Result := Self;
  FAppToken := AValue;
end;

procedure TProviderSematextLogs.LoadFromJSON(const AJSON: string);
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
    AppToken(LJO.GetValue<string>('app_token', FAppToken));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSematextLogs.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('app_token', FAppToken);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSematextLogs.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsJsonObjectToString(FLogFormat, LItem, True);

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://logsene-receiver.sematext.com/%s/event', [FAppToken]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSematextLogs);

end.
