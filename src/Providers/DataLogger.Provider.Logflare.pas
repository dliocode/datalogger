{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://logflare.app/

unit DataLogger.Provider.Logflare;

interface

uses
{$IF DEFINED(DATALOGGER_LOGFLARE_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_LOGFLARE_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_LOGFLARE_USE_INDY)}
  TProviderLogflare = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_LOGFLARE_USE_NETHTTPCLIENT)}
  TProviderLogflare = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderLogflare = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FSourceKey: string;
    FApiKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function SourceKey(const AValue: string): TProviderLogflare;
    function ApiKey(const AValue: string): TProviderLogflare;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderLogflare }

constructor TProviderLogflare.Create;
begin
  inherited Create;

  URL('https://api.logflare.app/api/logs');
  ContentType('application/json');
end;

function TProviderLogflare.SourceKey(const AValue: string): TProviderLogflare;
begin
  Result := Self;
  FSourceKey := AValue;
end;

function TProviderLogflare.ApiKey(const AValue: string): TProviderLogflare;
begin
  Result := Self;
  FApiKey := AValue;
  AddHeader('X-API-KEY', AValue);
end;

procedure TProviderLogflare.LoadFromJSON(const AJSON: string);
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
    SourceKey(LJO.GetValue<string>('source_key', FSourceKey));
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderLogflare.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('source_key', FSourceKey);
    LJO.AddPair('api_key', FApiKey);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderLogflare.Save(const ACache: TArray<TLoggerItem>);
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

    LJO := TJSONObject.Create;
    try
      if LItem.Message.Trim.IsEmpty then
        LJO.AddPair('message', LItem.MessageJSON)
      else
        LJO.AddPair('message', LItem.Message);

      LJO.AddPair('metadata', TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('https://api.logflare.app/api/logs?source=%s', [FSourceKey]);
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

ForceReferenceToClass(TProviderLogflare);

end.
