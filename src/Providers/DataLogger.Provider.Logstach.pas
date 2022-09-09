{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Logstach;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_LOGSTACH_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_LOGSTACH_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.JSON;

type
  TProviderLogstach = class(TDataLoggerProvider<TProviderLogstach>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_LOGSTACH_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_LOGSTACH_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FIndex: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderLogstach;
    function Index(const AValue: string): TProviderLogstach;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;  
  end;

implementation

{ TProviderLogstach }

constructor TProviderLogstach.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('http://localhost:5044');

  Index('logger');
end;

destructor TProviderLogstach.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderLogstach.URL(const AValue: string): TProviderLogstach;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderLogstach.Index(const AValue: string): TProviderLogstach;
begin
  Result := Self;
  FIndex := AValue;
end;

procedure TProviderLogstach.LoadFromJSON(const AJSON: string);
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
    Index(LJO.GetValue<string>('index', FIndex));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderLogstach.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', FHTTP.URL);
    LJO.AddPair('index', FIndex);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderLogstach.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLogItemREST.Stream := TLoggerLogFormat.AsStreamJsonObject(FLogFormat, LItem);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s/%s/doc', [FHTTP.URL.Trim(['/']), FIndex.ToLower]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderLogstach);

end.
