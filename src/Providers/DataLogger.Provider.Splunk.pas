{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://www.splunk.com/
// https://dev.splunk.com/enterprise/reference/
// https://docs.splunk.com/Documentation

unit DataLogger.Provider.Splunk;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_SPLUNK_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SPLUNK_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSplunk = class(TDataLoggerProvider<TProviderSplunk>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_SPLUNK_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_SPLUNK_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FToken: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderSplunk;
    function Token(const AValue: string): TProviderSplunk;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSplunk }

constructor TProviderSplunk.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://localhost:8088');
end;

destructor TProviderSplunk.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderSplunk.URL(const AValue: string): TProviderSplunk;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderSplunk.Token(const AValue: string): TProviderSplunk;
begin
  Result := Self;

  FToken := AValue;
  FHTTP.Token('Splunk ' + AValue);
end;

procedure TProviderSplunk.LoadFromJSON(const AJSON: string);
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
    Token(LJO.GetValue<string>('token', FToken));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSplunk.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', FHTTP.URL);
    LJO.AddPair('token', FToken);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSplunk.Save(const ACache: TArray<TLoggerItem>);
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
      LJO.AddPair('event', TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('%s/services/collector/event', [FHTTP.URL.Trim(['/'])]);
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

ForceReferenceToClass(TProviderSplunk);

end.
