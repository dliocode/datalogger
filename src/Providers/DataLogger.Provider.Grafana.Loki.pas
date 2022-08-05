{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://grafana.com/
// https://grafana.com/docs/loki/latest/api/#push-log-entries-to-loki

unit DataLogger.Provider.Grafana.Loki;

interface

uses
{$IF DEFINED(DATALOGGER_GRAFANALOKI_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_GRAFANALOKI_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_GRAFANALOKI_USE_INDY)}
  TProviderGrafanaLoki = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_GRAFANALOKI_USE_NETHTTPCLIENT)}
  TProviderGrafanaLoki = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderGrafanaLoki = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FServiceName: string;
    FBasicAuthUsername: string;
    FBasicAuthPassword: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderGrafanaLoki;
    function BasicAuth(const AUsername: string; const APassword: string): TProviderGrafanaLoki;
    function ServiceName(const AValue: string): TProviderGrafanaLoki;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderGrafanaLoki }

constructor TProviderGrafanaLoki.Create;
begin
  inherited Create;

  URL('https://logs-prod3.grafana.net');
  ContentType('application/json');
  ServiceName('Log');
end;

function TProviderGrafanaLoki.URL(const AValue: string): TProviderGrafanaLoki;
begin
  Result := Self;
  inherited URL(AValue);
end;

function TProviderGrafanaLoki.BasicAuth(const AUsername, APassword: string): TProviderGrafanaLoki;
begin
  Result := Self;

  FBasicAuthUsername := AUsername;
  FBasicAuthPassword := APassword;

  inherited BasicAuth(AUsername, APassword);
end;

function TProviderGrafanaLoki.ServiceName(const AValue: string): TProviderGrafanaLoki;
begin
  Result := Self;
  FServiceName := AValue;
end;

procedure TProviderGrafanaLoki.LoadFromJSON(const AJSON: string);
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
    BasicAuth(LJO.GetValue<string>('basic_auth_username', FBasicAuthUsername), LJO.GetValue<string>('basic_auth_password', FBasicAuthPassword));
    ServiceName(LJO.GetValue<string>('label_id', FServiceName));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderGrafanaLoki.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', inherited URL);
    LJO.AddPair('basic_auth_username', FBasicAuthUsername);
    LJO.AddPair('basic_auth_password', FBasicAuthPassword);
    LJO.AddPair('label_id', FServiceName);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderGrafanaLoki.Save(const ACache: TArray<TLoggerItem>);
  function SetZero(const AText: string; const ASize: Integer): string;
  var
    LSize: Integer;
  begin
    Result := AText;

    LSize := ASize - Length(Result);
    if (LSize > 0) then
       Result := Result + StringOfChar('0', LSize);
  end;

var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LDateUNIX: string;
  LJO: TJSONObject;
  LJOData: TJSONObject;
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
    LDateUNIX := IntToStr(DateTimeToUnix(LItem.TimeStamp, False));
    LDateUNIX := SetZero(LDateUNIX, 19);

    LJO := TJSONObject.Create;
    try
      LJOData := TJSONObject.Create;
      LJOData.AddPair('stream',
        TJSONObject.Create
          .AddPair('service', FServiceName)
          .AddPair('type', LItem.TypeString)
        );

      LJOData.AddPair('values',
        TJSONArray.Create
          .Add(
            TJSONArray.Create
            .Add(LDateUNIX)
            .Add(LLog)
          )
        );

      LJO.AddPair('streams', TJSONArray.Create.Add(LJOData));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('%s/loki/api/v1/push', [inherited URL]);
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

ForceReferenceToClass(TProviderGrafanaLoki);

end.
