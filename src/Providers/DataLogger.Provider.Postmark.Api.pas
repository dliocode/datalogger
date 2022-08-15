{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://postmarkapp.com/
// https://postmarkapp.com/developer/user-guide/send-email-with-api

unit DataLogger.Provider.Postmark.Api;

interface

uses
{$IF DEFINED(DATALOGGER_POSTMARK_API_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_POSTMARK_API_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_POSTMARK_API_USE_INDY)}
  TProviderPostmarkApi = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_POSTMARK_API_USE_NETHTTPCLIENT)}
  TProviderPostmarkApi = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderPostmarkApi = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FApiToken: string;
    FEmailFrom: string;
    FEmailTo: TArray<string>;
    FSubject: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiToken(const AValue: string): TProviderPostmarkApi;
    function EmailFrom(const AValue: string): TProviderPostmarkApi;
    function EmailTo(const AValue: TArray<string>): TProviderPostmarkApi;
    function Subject(const AValue: string): TProviderPostmarkApi;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderPostmarkApi }

constructor TProviderPostmarkApi.Create;
begin
  inherited Create;

  ContentType('application/json');
  EmailFrom('');
  EmailTo([]);
  Subject('DataLogger');
end;

function TProviderPostmarkApi.ApiToken(const AValue: string): TProviderPostmarkApi;
begin
  Result := Self;
  FApiToken := AValue;
  inherited AddHeader('X-Postmark-Server-Token', AValue);
end;

function TProviderPostmarkApi.EmailFrom(const AValue: string): TProviderPostmarkApi;
begin
  Result := Self;
  FEmailFrom := AValue.Trim;
end;

function TProviderPostmarkApi.EmailTo(const AValue: TArray<string>): TProviderPostmarkApi;
begin
  Result := Self;
  FEmailTo := AValue;
end;

function TProviderPostmarkApi.Subject(const AValue: string): TProviderPostmarkApi;
begin
  Result := Self;
  FSubject := AValue;
end;

procedure TProviderPostmarkApi.LoadFromJSON(const AJSON: string);
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
    ApiToken(LJO.GetValue<string>('api_token', FApiToken));
    EmailFrom(LJO.GetValue<string>('email_from', FEmailFrom));
    EmailTo(LJO.GetValue<string>('email_to', String.Join(',', FEmailTo)).Split([',']));
    Subject(LJO.GetValue<string>('subject', FSubject));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderPostmarkApi.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_token', FApiToken);
    LJO.AddPair('email_from', FEmailFrom);
    LJO.AddPair('email_to', String.Join(',', FEmailTo));
    LJO.AddPair('subject', FSubject);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderPostmarkApi.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LJO: TJSONObject;
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

    for I := Low(FEmailTo) to High(FEmailTo) do
    begin
      LJO := TJSONObject.Create;
      try
        LJO.AddPair('From', FEmailFrom);
        LJO.AddPair('To', FEmailTo[I]);
        LJO.AddPair('Subject', FSubject);
        LJO.AddPair('HTMLBody', LLog);

        LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
        LLogItemREST.LogItem := LItem;
        LLogItemREST.URL := 'https://api.postmarkapp.com/email';

        LItemREST := Concat(LItemREST, [LLogItemREST]);
      finally
        LJO.Free;
      end;
    end;
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderPostmarkApi);

end.
