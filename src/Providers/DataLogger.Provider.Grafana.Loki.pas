{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

// https://grafana.com/
// https://grafana.com/docs/loki/latest/api/#push-log-entries-to-loki

unit DataLogger.Provider.Grafana.Loki;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_GRAFANA_LOKI_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_GRAFANA_LOKI_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
  TProviderGrafanaLoki = class(TDataLoggerProvider<TProviderGrafanaLoki>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_GRAFANA_LOKI_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_GRAFANA_LOKI_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
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
    destructor Destroy; override;
  end;

implementation

{ TProviderGrafanaLoki }

constructor TProviderGrafanaLoki.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://logs-prod3.grafana.net');

  ServiceName('Log');
end;

destructor TProviderGrafanaLoki.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderGrafanaLoki.URL(const AValue: string): TProviderGrafanaLoki;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderGrafanaLoki.BasicAuth(const AUsername: string; const APassword: string): TProviderGrafanaLoki;
begin
  Result := Self;

  FBasicAuthUsername := AUsername;
  FBasicAuthPassword := APassword;

  FHTTP.BasicAuth(AUsername, APassword);
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
    URL(LJO.GetValue<string>('url', FHTTP.URL));
    BasicAuth(LJO.GetValue<string>('basic_auth_username', FBasicAuthUsername), LJO.GetValue<string>('basic_auth_password', FBasicAuthPassword));
    ServiceName(LJO.GetValue<string>('service_name', FServiceName));

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
    LJO.AddPair('url', FHTTP.URL);
    LJO.AddPair('basic_auth_username', FBasicAuthUsername);
    LJO.AddPair('basic_auth_password', FBasicAuthPassword);
    LJO.AddPair('service_name', FServiceName);

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
    if LItem.InternalItem.LevelSlineBreak then
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
        .AddPair('type', LItem.LevelString)
        );

      LJOData.AddPair('values', TJSONArray.Create.Add(TJSONArray.Create.Add(LDateUNIX).Add(LLog)));
      LJO.AddPair('streams', TJSONArray.Create.Add(LJOData));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('%s/loki/api/v1/push', [FHTTP.URL.Trim(['/'])]);
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderGrafanaLoki);

end.
