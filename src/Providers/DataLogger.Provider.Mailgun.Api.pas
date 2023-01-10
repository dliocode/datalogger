{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2023 Danilo Lucas

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
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));
    LJO.AddPair('domain', TJSONString.Create(FDomain));
    LJO.AddPair('email_from', TJSONString.Create(FEmailFrom));
    LJO.AddPair('email_to', TJSONString.Create(String.Join(',', FEmailTo)));
    LJO.AddPair('subject', TJSONString.Create(FSubject));

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

  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

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

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderMailgunApi);

end.
