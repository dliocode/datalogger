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

// https://www.sendchamp.com/
// https://sendchamp.readme.io/reference/introduction
// https://sendchamp.readme.io/reference/send-whatsapp-text-api

unit DataLogger.Provider.SendChamp.WhatsApp;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_SENDCHAMP_WHATSAPP_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SENDCHAMP_WHATSAPP_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.RegularExpressions;

type
  TProviderSendChampWhatsApp = class(TDataLoggerProvider<TProviderSendChampWhatsApp>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_SENDCHAMP_WHATSAPP_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_SENDCHAMP_WHATSAPP_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FAccessKey: string;
    FPhoneFrom: string;
    FPhoneTo: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function AccessKey(const AValue: string): TProviderSendChampWhatsApp;
    function PhoneFrom(const AValue: string): TProviderSendChampWhatsApp;
    function PhoneTo(const AValue: string): TProviderSendChampWhatsApp;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSendChampWhatsApp }

constructor TProviderSendChampWhatsApp.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.AddHeader('accept', 'application/json');
  FHTTP.URL('https://api.sendchamp.com/api/v1/whatsapp/message/send');

  AccessKey('');
  PhoneFrom('');
  PhoneTo('');
end;

destructor TProviderSendChampWhatsApp.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderSendChampWhatsApp.AccessKey(const AValue: string): TProviderSendChampWhatsApp;
begin
  Result := Self;

  FAccessKey := AValue;
  FHTTP.BearerToken(FAccessKey);
end;

function TProviderSendChampWhatsApp.PhoneFrom(const AValue: string): TProviderSendChampWhatsApp;
begin
  Result := Self;
  FPhoneFrom := TRegEx.Match(AValue, '\d+').Value;
end;

function TProviderSendChampWhatsApp.PhoneTo(const AValue: string): TProviderSendChampWhatsApp;
begin
  Result := Self;
  FPhoneTo := TRegEx.Match(AValue, '\d+').Value;
end;

procedure TProviderSendChampWhatsApp.LoadFromJSON(const AJSON: string);
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
    AccessKey(LJO.GetValue<string>('access_key', FAccessKey));
    PhoneFrom(LJO.GetValue<string>('phone_from', FPhoneFrom));
    PhoneTo(LJO.GetValue<string>('phone_to', FPhoneTo));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSendChampWhatsApp.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('access_key', TJSONString.Create(FAccessKey));
    LJO.AddPair('phone_from', TJSONString.Create(FPhoneFrom));
    LJO.AddPair('phone_to', TJSONString.Create(FPhoneTo));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSendChampWhatsApp.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LJO: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLast then
      Continue;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('message', TJSONString.Create(LLog));
      LJO.AddPair('type', TJSONString.Create('text'));
      LJO.AddPair('sender', TJSONString.Create(FPhoneFrom));
      LJO.AddPair('recipient', TJSONString.Create(FPhoneTo));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := '';

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

ForceReferenceToClass(TProviderSendChampWhatsApp);

end.
