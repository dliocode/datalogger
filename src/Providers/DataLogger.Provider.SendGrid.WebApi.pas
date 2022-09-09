{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://sendgrid.com/

unit DataLogger.Provider.SendGrid.WebApi;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSendGridWebApi = class(TDataLoggerProvider<TProviderSendGridWebApi>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FEmailFrom: string;
    FEmailTo: TArray<string>;
    FSubject: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderSendGridWebApi;
    function EmailFrom(const AValue: string): TProviderSendGridWebApi;
    function EmailTo(const AValue: TArray<string>): TProviderSendGridWebApi;
    function Subject(const AValue: string): TProviderSendGridWebApi;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSendGridWebApi }

constructor TProviderSendGridWebApi.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://api.sendgrid.com/v3/mail/send');

  EmailFrom('');
  EmailTo([]);
  Subject('DataLogger');
end;

destructor TProviderSendGridWebApi.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderSendGridWebApi.ApiKey(const AValue: string): TProviderSendGridWebApi;
begin
  Result := Self;
  FHTTP.BearerToken(AValue);
end;

function TProviderSendGridWebApi.EmailFrom(const AValue: string): TProviderSendGridWebApi;
begin
  Result := Self;
  FEmailFrom := AValue.Trim;
end;

function TProviderSendGridWebApi.EmailTo(const AValue: TArray<string>): TProviderSendGridWebApi;
begin
  Result := Self;
  FEmailTo := AValue;
end;

function TProviderSendGridWebApi.Subject(const AValue: string): TProviderSendGridWebApi;
begin
  Result := Self;
  FSubject := AValue;
end;

procedure TProviderSendGridWebApi.LoadFromJSON(const AJSON: string);
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
    ApiKey(LJO.GetValue<string>('api_key', FHTTP.Token));
    EmailFrom(LJO.GetValue<string>('email_from', FEmailFrom));
    EmailTo(LJO.GetValue<string>('email_to', String.Join(',', FEmailTo)).Split([',']));
    Subject(LJO.GetValue<string>('subject', FSubject));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSendGridWebApi.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', FHTTP.Token);
    LJO.AddPair('email_from', FEmailFrom);
    LJO.AddPair('email_to', String.Join(',', FEmailTo));
    LJO.AddPair('subject', FSubject);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSendGridWebApi.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LJO: TJSONObject;
  LJAPersonalizations: TJSONArray;
  I: Integer;
  LJATo: TJSONArray;
  LJAContent: TJSONArray;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LJO := TJSONObject.Create;
    try
      LJAPersonalizations := TJSONArray.Create;

      for I := Low(FEmailTo) to High(FEmailTo) do
      begin
        LJATo := TJSONArray.Create;
        LJATo.Add(TJSONObject.Create.AddPair('email', FEmailTo[I].Trim));

        LJAPersonalizations.Add(TJSONObject.Create.AddPair('to', LJATo));
      end;

      LJO.AddPair('personalizations', LJAPersonalizations);
      LJO.AddPair('from', TJSONObject.Create.AddPair('email', FEmailFrom));
      LJO.AddPair('subject', FSubject);

      LJAContent := TJSONArray.Create;
      LJAContent.Add(
        TJSONObject.Create
        .AddPair('type', 'text/plain')
        .AddPair('value', LLog)
        );

      LJO.AddPair('content', LJAContent);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := FHTTP.URL;
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

ForceReferenceToClass(TProviderSendGridWebApi);

end.
