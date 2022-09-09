{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://datalust.co/
// https://docs.datalust.co/docs

unit DataLogger.Provider.Datalust;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_DATALUST_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_DATALUST_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
  TProviderDatalust = class(TDataLoggerProvider<TProviderDatalust>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_DATALUST_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_DATALUST_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FApiKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderDatalust;
    function ApiKey(const AValue: string): TProviderDatalust;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderDatalust }

constructor TProviderDatalust.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('http://localhost:5431');
end;

destructor TProviderDatalust.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderDatalust.URL(const AValue: string): TProviderDatalust;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderDatalust.ApiKey(const AValue: string): TProviderDatalust;
begin
  Result := Self;

  FApiKey := AValue;
  FHTTP.AddHeader('X-Seq-ApiKey', AValue);
end;

procedure TProviderDatalust.LoadFromJSON(const AJSON: string);
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

function TProviderDatalust.ToJSON(const AFormat: Boolean): string;
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

procedure TProviderDatalust.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LJOEvents: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LJO := TJSONObject.Create;
    try
      LJOEvents := TJSONObject.Create;
      LJOEvents
        .AddPair('Timestamp', TJSONString.Create(DateToISO8601(LItem.TimeStamp, False)))
        .AddPair('Level', LItem.TypeString)
        .AddPair('Properties', TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True));

      if not LItem.Message.Trim.IsEmpty then
        LJOEvents.AddPair('MessageTemplate', LItem.Message)
      else
        LJOEvents.AddPair('MessageTemplate', LItem.MessageJSON);

      LJO.AddPair('Events', TJSONArray.Create.Add(LJOEvents));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('%s/api/events/raw', [FHTTP.URL.Trim(['/'])]);
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

ForceReferenceToClass(TProviderDatalust);

end.
