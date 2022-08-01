{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://graphjson.com/
// https://docs.graphjson.com/

unit DataLogger.Provider.GraphJSON;

interface

uses
{$IF DEFINED(DATALOGGER_GRAPHJSON_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_GRAPHJSON_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_GRAPHJSON_USE_INDY)}
  TProviderGraphJSON = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_GRAPHJSON_USE_NETHTTPCLIENT)}
  TProviderGraphJSON = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderGraphJSON = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FApiKey: string;
    FCollection: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderGraphJSON;
    function Collection(const AValue: string): TProviderGraphJSON;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderGraphJSON }

constructor TProviderGraphJSON.Create;
begin
  inherited Create;

  URL('');
  ContentType('application/json');
  Collection('');
end;

function TProviderGraphJSON.ApiKey(const AValue: string): TProviderGraphJSON;
begin
  Result := Self;
  FApiKey := AValue;
end;

function TProviderGraphJSON.Collection(const AValue: string): TProviderGraphJSON;
begin
  Result := Self;
  FCollection := AValue;
end;

procedure TProviderGraphJSON.LoadFromJSON(const AJSON: string);
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
    Collection(LJO.GetValue<string>('collection', FCollection));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderGraphJSON.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', FApiKey);
    LJO.AddPair('collection', FCollection);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderGraphJSON.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
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

    LLog := TLoggerLogFormat.AsJsonObjectToString(FLogFormat, LItem, True);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('api_key', FApiKey);
      LJO.AddPair('collection', FCollection);
      LJO.AddPair('timestamp', TJSONNumber.Create(DateTimeToUnix(LItem.TimeStamp, False)));
      LJO.AddPair('json', LLog);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := 'https://api.graphjson.com/api/log';
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

ForceReferenceToClass(TProviderGraphJSON);

end.
