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

// https://www.twilio.com/

unit DataLogger.Provider.Twilio.WhatsApp;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_TWILIO_WHATSAPP_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_TWILIO_WHATSAPP_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.RegularExpressions;

type
  TProviderTwilioWhatsApp = class(TDataLoggerProvider<TProviderTwilioWhatsApp>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_TWILIO_WHATSAPP_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_TWILIO_WHATSAPP_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FAccountSID: string;
    FAuthToken: string;
    FMessagingServiceSID: string;
    FPhoneFrom: string;
    FPhoneTo: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function AccountSID(const AValue: string): TProviderTwilioWhatsApp;
    function AuthToken(const AValue: string): TProviderTwilioWhatsApp;
    function MessagingServiceSID(const AValue: string): TProviderTwilioWhatsApp;
    function PhoneFrom(const AValue: string): TProviderTwilioWhatsApp;
    function PhoneTo(const AValue: string): TProviderTwilioWhatsApp;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderTwilioWhatsApp }

constructor TProviderTwilioWhatsApp.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  AccountSID('');
  AuthToken('');
  MessagingServiceSID('');
  PhoneFrom('');
  PhoneTo('');
end;

destructor TProviderTwilioWhatsApp.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderTwilioWhatsApp.AccountSID(const AValue: string): TProviderTwilioWhatsApp;
begin
  Result := Self;
  FAccountSID := AValue;
end;

function TProviderTwilioWhatsApp.AuthToken(const AValue: string): TProviderTwilioWhatsApp;
begin
  Result := Self;
  FAuthToken := AValue;
end;

function TProviderTwilioWhatsApp.MessagingServiceSID(const AValue: string): TProviderTwilioWhatsApp;
begin
  Result := Self;
  FMessagingServiceSID := AValue;
end;

function TProviderTwilioWhatsApp.PhoneFrom(const AValue: string): TProviderTwilioWhatsApp;
begin
  Result := Self;
  FPhoneFrom := TRegEx.Match(AValue, '\d+').Value;
end;

function TProviderTwilioWhatsApp.PhoneTo(const AValue: string): TProviderTwilioWhatsApp;
begin
  Result := Self;
  FPhoneTo := TRegEx.Match(AValue, '\d+').Value;
end;

procedure TProviderTwilioWhatsApp.LoadFromJSON(const AJSON: string);
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
    AccountSID(LJO.GetValue<string>('account_sid', FAccountSID));
    AuthToken(LJO.GetValue<string>('auth_token', FAuthToken));
    MessagingServiceSID(LJO.GetValue<string>('messaging_service_sid', FMessagingServiceSID));
    PhoneFrom(LJO.GetValue<string>('phone_from', FPhoneFrom));
    PhoneTo(LJO.GetValue<string>('phone_to', FPhoneTo));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderTwilioWhatsApp.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('account_sid', TJSONString.Create(FAccountSID));
    LJO.AddPair('auth_token', TJSONString.Create(FAuthToken));
    LJO.AddPair('messaging_service_sid', TJSONString.Create(FMessagingServiceSID));
    LJO.AddPair('phone_from', TJSONString.Create(FPhoneFrom));
    LJO.AddPair('phone_to', TJSONString.Create(FPhoneTo));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderTwilioWhatsApp.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  FHTTP.BasicAuth(FAccountSID, FAuthToken);

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLastLine then
      Continue;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json', [FAccountSID]);
    LLogItemREST.FormData :=
      [
      TLogFormData.Create('From', 'whatsapp:' + FPhoneFrom),
      TLogFormData.Create('To', 'whatsapp:' + FPhoneTo),
      TLogFormData.Create('Body', LLog)
      ];

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

ForceReferenceToClass(TProviderTwilioWhatsApp);

end.
