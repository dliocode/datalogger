{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://www.datadoghq.com/

unit DataLogger.Provider.Datadog;

interface

uses
{$IF DEFINED(DATALOGGER_DATADOG_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_DATADOG_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_DATADOG_USE_INDY)}
  TProviderDatadog = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_DATADOG_USE_NETHTTPCLIENT)}
  TProviderDatadog = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderDatadog = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FApiKey: string;
    FApplicationKey: string;
    FSource: string;
    FService: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderDatadog;
    function ApplicationKey(const AValue: string): TProviderDatadog;
    function Source(const AValue: string): TProviderDatadog;
    function Service(const AValue: string): TProviderDatadog;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderDatadog }

constructor TProviderDatadog.Create;
begin
  inherited Create;

  URL('https://http-intake.logs.datadoghq.com/api/v2/logs');
  ContentType('application/json');
  Source('datalogger');
  Service('');
end;

function TProviderDatadog.ApiKey(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FApiKey := AValue;
  AddHeader('DD-API-KEY', AValue);
end;

function TProviderDatadog.ApplicationKey(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FApplicationKey := AValue;
  AddHeader('DD-APPLICATION-KEY', AValue);
end;

function TProviderDatadog.Source(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FSource := AValue;
end;

function TProviderDatadog.Service(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FService := AValue;
end;

procedure TProviderDatadog.LoadFromJSON(const AJSON: string);
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
    ApplicationKey(LJO.GetValue<string>('application_key', FApplicationKey));
    Source(LJO.GetValue<string>('source', FSource));
    Service(LJO.GetValue<string>('service', FService));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderDatadog.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', FApiKey);
    LJO.AddPair('application_key', FApplicationKey);
    LJO.AddPair('source', FSource);
    LJO.AddPair('service', FService);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderDatadog.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJA: TJSONArray;
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

    LJA := TJSONArray.Create;
    try
      LJO := TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True);
      LJO.AddPair('ddsource', FSource);
      LJO.AddPair('ddtags', LItem.Tag);
      LJO.AddPair('host', LItem.ComputerName);
      LJO.AddPair('service', FService);
      LJO.AddPair('status', LItem.TypeString.ToUpper);

      LJA.Add(LJO);

      LLogItemREST.Stream := TStringStream.Create(LJA.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := 'https://http-intake.logs.datadoghq.com/api/v2/logs';
    finally
      LJA.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderDatadog);

end.
