{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://grafana.com/
// https://grafana.com/docs/oncall/latest/integrations/add-webhook-integration/

unit DataLogger.Provider.Grafana.OnCall.WebHook;

interface

uses
{$IF DEFINED(DATALOGGER_GRAFANAONCALL_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_GRAFANAONCALL_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON;

type
{$IF DEFINED(DATALOGGER_GRAFANAONCALL_USE_INDY)}
  TProviderGrafanaOnCall = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_GRAFANAONCALL_USE_NETHTTPCLIENT)}
  TProviderGrafanaOnCallWebHook = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderGrafanaOnCallWebHook = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderGrafanaOnCallWebHook;
    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderGrafanaOnCallWebHook }

constructor TProviderGrafanaOnCallWebHook.Create;
begin
  inherited Create;

  URL('https://a-prod-us-central-0.grafana.net/integrations/v1/webhook/xxxxxxxxxxxxxxxxxxx/');
  ContentType('application/json');
end;

function TProviderGrafanaOnCallWebHook.URL(const AValue: string): TProviderGrafanaOnCallWebHook;
begin
  Result := Self;
  inherited URL(AValue);
end;

procedure TProviderGrafanaOnCallWebHook.LoadFromJSON(const AJSON: string);
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
    URL(LJO.GetValue<string>('url', inherited URL));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderGrafanaOnCallWebHook.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', inherited URL);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderGrafanaOnCallWebHook.Save(const ACache: TArray<TLoggerItem>);
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
    LLogItemREST.URL := inherited URL;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderGrafanaOnCallWebHook);

end.
