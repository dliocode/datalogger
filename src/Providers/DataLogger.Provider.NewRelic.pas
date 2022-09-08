{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://newrelic.com
// https://newrelic.com/instant-observability/logs-api/b40194c0-c7a0-497c-82c1-ca47b60726da

unit DataLogger.Provider.NewRelic;

interface

uses
{$IF DEFINED(DATALOGGER_NEW_RELIC_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_NEW_RELIC_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON;

type
{$IF DEFINED(DATALOGGER_NEW_RELIC_USE_INDY)}
  TProviderNewRelic = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_NEW_RELIC_USE_NETHTTPCLIENT)}
  TProviderNewRelic = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderNewRelic = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FApiKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderNewRelic;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderNewRelic }

constructor TProviderNewRelic.Create;
begin
  inherited Create;

  URL('https://log-api.newrelic.com/log/v1');
  ContentType('application/json');
end;

function TProviderNewRelic.ApiKey(const AValue: string): TProviderNewRelic;
begin
  Result := Self;
  FApiKey := AValue;
end;

procedure TProviderNewRelic.LoadFromJSON(const AJSON: string);
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
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderNewRelic.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', FApiKey);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderNewRelic.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LJO := TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True);
    try
      if LItem.Message.Trim.IsEmpty then
        LJO.AddPair('message', LItem.MessageJSON)
      else
        LJO.AddPair('message', LItem.Message);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('https://log-api.newrelic.com/log/v1?Api-Key=%s', [FApiKey]);
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderNewRelic);

end.
