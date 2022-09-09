{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://www.mailgun.com/
// https://documentation.mailgun.com/en/latest/user_manual.html#introduction

unit DataLogger.Provider.Mailgun.Api;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_MAILGUN_API_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_MAILGUN_API_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderMailgunApi = class(TDataLoggerProvider<TProviderMailgunApi>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_MAILGUN_API_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_MAILGUN_API_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FApiKey: string;
    FDomain: string;
    FEmailFrom: string;
    FEmailTo: TArray<string>;
    FSubject: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderMailgunApi;
    function Domain(const AValue: string): TProviderMailgunApi;
    function EmailFrom(const AValue: string): TProviderMailgunApi;
    function EmailTo(const AValue: TArray<string>): TProviderMailgunApi;
    function Subject(const AValue: string): TProviderMailgunApi;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderMailgunApi }

constructor TProviderMailgunApi.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  EmailFrom('');
  EmailTo([]);
  Subject('DataLogger');
end;

destructor TProviderMailgunApi.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderMailgunApi.ApiKey(const AValue: string): TProviderMailgunApi;
begin
  Result := Self;
  FApiKey := AValue;
  FHTTP.BasicAuth('api', AValue);
end;

function TProviderMailgunApi.Domain(const AValue: string): TProviderMailgunApi;
begin
  Result := Self;
  FDomain := AValue;
end;

function TProviderMailgunApi.EmailFrom(const AValue: string): TProviderMailgunApi;
begin
  Result := Self;
  FEmailFrom := AValue.Trim;
end;

function TProviderMailgunApi.EmailTo(const AValue: TArray<string>): TProviderMailgunApi;
begin
  Result := Self;
  FEmailTo := AValue;
end;

function TProviderMailgunApi.Subject(const AValue: string): TProviderMailgunApi;
begin
  Result := Self;
  FSubject := AValue;
end;

procedure TProviderMailgunApi.LoadFromJSON(const AJSON: string);
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
    Domain(LJO.GetValue<string>('domain', FDomain));
    EmailFrom(LJO.GetValue<string>('email_from', FEmailFrom));
    EmailTo(LJO.GetValue<string>('email_to', String.Join(',', FEmailTo)).Split([',']));
    Subject(LJO.GetValue<string>('subject', FSubject));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderMailgunApi.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', FApiKey);
    LJO.AddPair('domain', FDomain);
    LJO.AddPair('email_from', FEmailFrom);
    LJO.AddPair('email_to', String.Join(',', FEmailTo));
    LJO.AddPair('subject', FSubject);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderMailgunApi.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LLogItemREST: TLogItemREST;
  I: Integer;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.mailgun.net/v3/%s/messages', [FDomain]);
    LLogItemREST.FormData := [
      TLogFormData.Create('from', FEmailFrom),
      TLogFormData.Create('subject', FSubject),
      TLogFormData.Create('text', LLog)
      ];

    for I := Low(FEmailTo) to High(FEmailTo) do
      LLogItemREST.FormData := LLogItemREST.FormData + [TLogFormData.Create('to', FEmailTo[I])];

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP.InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderMailgunApi);

end.
