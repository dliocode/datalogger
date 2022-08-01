{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://www.twilio.com/

unit DataLogger.Provider.Twilio.WhatsApp;

interface

uses
{$IF DEFINED(DATALOGGER_TWILIOWHATSAPP_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_TWILIOWHATSAPP_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_TWILIOWHATSAPP_USE_INDY)}
  TProviderTwilioWhatsApp = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_TWILIOWHATSAPP_USE_NETHTTPCLIENT)}
  TProviderTwilioWhatsApp = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderTwilioWhatsApp = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
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

    constructor Create; overload;
  end;

implementation

{ TProviderTwilioWhatsApp }

constructor TProviderTwilioWhatsApp.Create;
begin
  inherited Create;

  URL('');
  ContentType('application/json');
  PhoneFrom('');
  PhoneTo('');
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
  FPhoneFrom := AValue;
end;

function TProviderTwilioWhatsApp.PhoneTo(const AValue: string): TProviderTwilioWhatsApp;
begin
  Result := Self;
  FPhoneTo := AValue;
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
    LJO.AddPair('account_sid', FAccountSID);
    LJO.AddPair('auth_token', FAuthToken);
    LJO.AddPair('phone_from', FPhoneFrom);
    LJO.AddPair('phone_to', FPhoneTo);

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
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  BasicAuth(FAccountSID, FAuthToken);

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json', [FAccountSID]);
    LLogItemREST.FormData := [TLogFormData.Create('From', 'whatsapp:'+FPhoneFrom), TLogFormData.Create('To', 'whatsapp:'+FPhoneTo), TLogFormData.Create('Body', LLog)];

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderTwilioWhatsApp);

end.
