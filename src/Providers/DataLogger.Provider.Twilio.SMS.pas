{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://www.twilio.com/

unit DataLogger.Provider.Twilio.SMS;

interface

uses
{$IF DEFINED(DATALOGGER_TWILIOSMS_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_TWILIOSMS_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_TWILIOSMS_USE_INDY)}
  TProviderTwilioSMS = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_TWILIOSMS_USE_NETHTTPCLIENT)}
  TProviderTwilioSMS = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderTwilioSMS = class(TProviderRESTHTTPClient)
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
    function AccountSID(const AValue: string): TProviderTwilioSMS;
    function AuthToken(const AValue: string): TProviderTwilioSMS;
    function MessagingServiceSID(const AValue: string): TProviderTwilioSMS;
    function PhoneFrom(const AValue: string): TProviderTwilioSMS;
    function PhoneTo(const AValue: string): TProviderTwilioSMS;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderTwilioSMS }

constructor TProviderTwilioSMS.Create;
begin
  inherited Create;

  URL('');
  ContentType('application/json');
  PhoneFrom('');
  PhoneTo('');
end;

function TProviderTwilioSMS.AccountSID(const AValue: string): TProviderTwilioSMS;
begin
  Result := Self;
  FAccountSID := AValue;
end;

function TProviderTwilioSMS.AuthToken(const AValue: string): TProviderTwilioSMS;
begin
  Result := Self;
  FAuthToken := AValue;
end;

function TProviderTwilioSMS.MessagingServiceSID(const AValue: string): TProviderTwilioSMS;
begin
  Result := Self;
  FMessagingServiceSID := AValue;
end;

function TProviderTwilioSMS.PhoneFrom(const AValue: string): TProviderTwilioSMS;
begin
  Result := Self;
  FPhoneFrom := AValue;
end;

function TProviderTwilioSMS.PhoneTo(const AValue: string): TProviderTwilioSMS;
begin
  Result := Self;
  FPhoneTo := AValue;
end;

procedure TProviderTwilioSMS.LoadFromJSON(const AJSON: string);
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

function TProviderTwilioSMS.ToJSON(const AFormat: Boolean): string;
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

procedure TProviderTwilioSMS.Save(const ACache: TArray<TLoggerItem>);
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
    LLog := LLog.Replace('.', ' '); // Twilio bug SMS - Failed to receive message with dots - 2022-08-01 yyyy-mm-dd

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json', [FAccountSID]);

    if FMessagingServiceSID.Trim.IsEmpty then
      LLogItemREST.FormData := [TLogFormData.Create('From', FPhoneFrom)]
    else
      LLogItemREST.FormData := [TLogFormData.Create('MessagingServiceSid', FMessagingServiceSID)];

    LLogItemREST.FormData := LLogItemREST.FormData + [TLogFormData.Create('To', FPhoneTo), TLogFormData.Create('Body', LLog)];

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderTwilioSMS);

end.
