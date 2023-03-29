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

// https://ultramsg.com
// https://docs.ultramsg.com/api/post/messages/chat

unit DataLogger.Provider.UltraMSG.WhatsApp;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_ULTRAMSG_WHATSAPP_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_ULTRAMSG_WHATSAPP_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.RegularExpressions,
  System.NetEncoding;

type
  TProviderUltraMSGWhatsApp = class(TDataLoggerProvider<TProviderUltraMSGWhatsApp>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_ULTRAMSG_WHATSAPP_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_ULTRAMSG_WHATSAPP_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FToken: string;
    FInstanceID: string;
    FPhoneTo: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Token(const AValue: string): TProviderUltraMSGWhatsApp;
    function InstanceID(const AValue: string): TProviderUltraMSGWhatsApp;
    function PhoneTo(const AValue: string): TProviderUltraMSGWhatsApp;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderUltraMSGWhatsApp }

constructor TProviderUltraMSGWhatsApp.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/x-www-form-urlencoded');
  FHTTP.URL('https://api.ultramsg.com');

  Token('');
  InstanceID('');
  PhoneTo('');
end;

destructor TProviderUltraMSGWhatsApp.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderUltraMSGWhatsApp.Token(const AValue: string): TProviderUltraMSGWhatsApp;
begin
  Result := Self;
  FToken := AValue;
end;

function TProviderUltraMSGWhatsApp.InstanceID(const AValue: string): TProviderUltraMSGWhatsApp;
begin
  Result := Self;
  FInstanceID := Avalue;
end;

function TProviderUltraMSGWhatsApp.PhoneTo(const AValue: string): TProviderUltraMSGWhatsApp;
begin
  Result := Self;
  FPhoneTo := TRegEx.Match(AValue, '\d+').Value;
end;

procedure TProviderUltraMSGWhatsApp.LoadFromJSON(const AJSON: string);
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
    Token(LJO.GetValue<string>('token', FToken));
    InstanceID(LJO.GetValue<string>('instance_id', FInstanceID));
    PhoneTo(LJO.GetValue<string>('phone_to', FPhoneTo));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderUltraMSGWhatsApp.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('token', TJSONString.Create(FToken));
    LJO.AddPair('instance_id', TJSONString.Create(FInstanceID));
    LJO.AddPair('phone_to', TJSONString.Create(FPhoneTo));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderUltraMSGWhatsApp.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
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
    LLog := TNetEncoding.URL.Encode(LLog);

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s/%s/messages/chat?token=%s&to=%s&body=%s&priority=10', [FHTTP.URL, FInstanceID, FToken, FPhoneTo, LLog]);
    LLogItemREST.FormData := [];

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmGet, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderUltraMSGWhatsApp);

end.
