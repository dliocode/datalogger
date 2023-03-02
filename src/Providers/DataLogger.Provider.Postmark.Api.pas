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

// https://postmarkapp.com/
// https://postmarkapp.com/developer/user-guide/send-email-with-api

unit DataLogger.Provider.Postmark.Api;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_POSTMARK_API_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_POSTMARK_API_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderPostmarkApi = class(TDataLoggerProvider<TProviderPostmarkApi>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_POSTMARK_API_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_POSTMARK_API_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
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

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderPostmarkApi }

constructor TProviderPostmarkApi.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  EmailFrom('');
  EmailTo([]);
  Subject('DataLogger');
end;

destructor TProviderPostmarkApi.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderPostmarkApi.ApiToken(const AValue: string): TProviderPostmarkApi;
begin
  Result := Self;

  FApiToken := AValue;
  FHTTP.AddHeader('X-Postmark-Server-Token', AValue);
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
    LJO.AddPair('api_token', TJSONString.Create(FApiToken));
    LJO.AddPair('email_from', TJSONString.Create(FEmailFrom));
    LJO.AddPair('email_to', TJSONString.Create(String.Join(',', FEmailTo)));
    LJO.AddPair('subject', TJSONString.Create(FSubject));

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

  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    for I := Low(FEmailTo) to High(FEmailTo) do
    begin
      LJO := TJSONObject.Create;
      try
        LJO.AddPair('From', TJSONString.Create(FEmailFrom));
        LJO.AddPair('To', TJSONString.Create(FEmailTo[I]));
        LJO.AddPair('Subject', TJSONString.Create(FSubject));
        LJO.AddPair('HTMLBody', TJSONString.Create(LLog));

        LLog := LJO.ToString;
      finally
        LJO.Free;
      end;

      LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := 'https://api.postmarkapp.com/email';

      LItemREST := Concat(LItemREST, [LLogItemREST]);
    end;
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

ForceReferenceToClass(TProviderPostmarkApi);

end.
