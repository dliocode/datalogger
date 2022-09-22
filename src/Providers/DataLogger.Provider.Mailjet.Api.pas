{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

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

// https://www.mailjet.com/
// https://dev.mailjet.com/email/guides/send-api-v31/
// https://app.mailjet.com/account/apikeys

unit DataLogger.Provider.Mailjet.Api;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_MAILJET_API_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_MAILJET_API_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderMailjetApi = class(TDataLoggerProvider<TProviderMailjetApi>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_MAILJET_API_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_MAILJET_API_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FBasicAuthUsername: string;
    FBasicAuthPassword: string;
    FCustomID: string;
    FEmailFrom: string;
    FEmailFromName: string;
    FEmailTo: TArray<string>;
    FEmailCc: TArray<string>;
    FEmailBcc: TArray<string>;
    FSubject: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function BasicAuth(const AUsername: string; const APassword: string): TProviderMailjetApi;
    function CustomID(const AValue: string): TProviderMailjetApi;
    function EmailFrom(const AValue: string; const AName: string): TProviderMailjetApi;
    function EmailTo(const AValue: TArray<string>): TProviderMailjetApi;
    function EmailCc(const AValue: TArray<string>): TProviderMailjetApi;
    function EmailBcc(const AValue: TArray<string>): TProviderMailjetApi;
    function Subject(const AValue: string): TProviderMailjetApi;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderMailjetApi }

constructor TProviderMailjetApi.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  CustomID('DataLogger');
  EmailFrom('', '');
  EmailTo([]);
  EmailCc([]);
  EmailBcc([]);
  Subject('DataLogger');
end;

destructor TProviderMailjetApi.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderMailjetApi.BasicAuth(const AUsername, APassword: string): TProviderMailjetApi;
begin
  Result := Self;

  FBasicAuthUsername := AUsername;
  FBasicAuthPassword := APassword;

  FHTTP.BasicAuth(AUsername, APassword);
end;

function TProviderMailjetApi.CustomID(const AValue: string): TProviderMailjetApi;
begin
  Result := Self;
  FCustomID := AValue;
end;

function TProviderMailjetApi.EmailFrom(const AValue: string; const AName: string): TProviderMailjetApi;
begin
  Result := Self;
  FEmailFrom := AValue.Trim;
  FEmailFromName := AName.Trim;
end;

function TProviderMailjetApi.EmailTo(const AValue: TArray<string>): TProviderMailjetApi;
begin
  Result := Self;
  FEmailTo := AValue;
end;

function TProviderMailjetApi.EmailCc(const AValue: TArray<string>): TProviderMailjetApi;
begin
  Result := Self;
  FEmailCc := AValue;
end;

function TProviderMailjetApi.EmailBcc(const AValue: TArray<string>): TProviderMailjetApi;
begin
  Result := Self;
  FEmailBcc := AValue;
end;

function TProviderMailjetApi.Subject(const AValue: string): TProviderMailjetApi;
begin
  Result := Self;
  FSubject := AValue;
end;

procedure TProviderMailjetApi.LoadFromJSON(const AJSON: string);
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
    BasicAuth(LJO.GetValue<string>('basic_auth_username', FBasicAuthUsername), LJO.GetValue<string>('basic_auth_password', FBasicAuthPassword));
    CustomID(LJO.GetValue<string>('custom_id', FCustomID));
    EmailFrom(LJO.GetValue<string>('email_from', FEmailFrom), LJO.GetValue<string>('email_from_name', FEmailFromName));
    EmailTo(LJO.GetValue<string>('email_to', String.Join(',', FEmailTo)).Split([',']));
    EmailCc(LJO.GetValue<string>('email_cc', String.Join(',', FEmailCc)).Split([',']));
    EmailBcc(LJO.GetValue<string>('email_bcc', String.Join(',', FEmailBcc)).Split([',']));
    Subject(LJO.GetValue<string>('subject', FSubject));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderMailjetApi.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('basic_auth_username', FBasicAuthUsername);
    LJO.AddPair('basic_auth_password', FBasicAuthPassword);
    LJO.AddPair('custom_id', FCustomID);
    LJO.AddPair('email_from', FEmailFrom);
    LJO.AddPair('email_from_name', FEmailFromName);
    LJO.AddPair('email_to', String.Join(',', FEmailTo));
    LJO.AddPair('email_cc', String.Join(',', FEmailCc));
    LJO.AddPair('email_bcc', String.Join(',', FEmailBcc));
    LJO.AddPair('subject', FSubject);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderMailjetApi.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LJO: TJSONObject;
  LJOMessage: TJSONObject;
  LJAEmails: TJSONArray;
  LLogItemREST: TLogItemREST;
  I: Integer;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.LevelSlineBreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    LJO := TJSONObject.Create;
    try
      LJOMessage := TJSONObject.Create;
      LJOMessage.AddPair('From', TJSONObject.Create.AddPair('Email', FEmailFrom).AddPair('Name', FEmailFromName));

      LJAEmails := TJSONArray.Create;
      for I := Low(FEmailTo) to High(FEmailTo) do
        LJAEmails.Add(TJSONObject.Create.AddPair('Email', FEmailTo[I]));
      LJOMessage.AddPair('To', LJAEmails);

      if Length(FEmailCc) > 0 then
      begin
        LJAEmails := TJSONArray.Create;
        for I := Low(FEmailCc) to High(FEmailCc) do
          LJAEmails.Add(TJSONObject.Create.AddPair('Email', FEmailCc[I]));
        LJOMessage.AddPair('Cc', LJAEmails);
      end;

      if Length(FEmailBcc) > 0 then
      begin
        LJAEmails := TJSONArray.Create;
        for I := Low(FEmailBcc) to High(FEmailBcc) do
          LJAEmails.Add(TJSONObject.Create.AddPair('Email', FEmailBcc[I]));
        LJOMessage.AddPair('Bcc', LJAEmails);
      end;

      LJOMessage.AddPair('Subject', FSubject);
      LJOMessage.AddPair('HTMLPart', LLog);

      if FCustomID.Trim.IsEmpty then
        LJOMessage.AddPair('CustomID', FCustomID);

      LJO.AddPair('Messages', TJSONArray.Create.Add(LJOMessage));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := 'https://api.mailjet.com/v3.1/send';
    finally
      LJO.Free;
    end;

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

ForceReferenceToClass(TProviderMailjetApi);

end.
