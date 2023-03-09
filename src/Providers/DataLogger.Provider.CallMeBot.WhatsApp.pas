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

// https://www.callmebot.com/
// https://www.callmebot.com/blog/free-api-whatsapp-messages/

unit DataLogger.Provider.CallMeBot.WhatsApp;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_CALLMEBOT_WHATSAPP_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_CALLMEBOT_WHATSAPP_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.RegularExpressions, System.NetEncoding;

type
  TProviderCallMeBotWhatsApp = class(TDataLoggerProvider<TProviderCallMeBotWhatsApp>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_CALLMEBOT_WHATSAPP_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_CALLMEBOT_WHATSAPP_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FPhone: string;
    FApiKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Phone(const AValue: string): TProviderCallMeBotWhatsApp;
    function ApiKey(const AValue: string): TProviderCallMeBotWhatsApp;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderCallMeBotWhatsApp }

constructor TProviderCallMeBotWhatsApp.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  Phone('');
  ApiKey('');
end;

destructor TProviderCallMeBotWhatsApp.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderCallMeBotWhatsApp.Phone(const AValue: string): TProviderCallMeBotWhatsApp;
begin
  Result := Self;
  FPhone := TRegEx.Match(AValue, '\d+').Value;
end;

function TProviderCallMeBotWhatsApp.ApiKey(const AValue: string): TProviderCallMeBotWhatsApp;
begin
  Result := Self;
  FApiKey := AValue;
end;

procedure TProviderCallMeBotWhatsApp.LoadFromJSON(const AJSON: string);
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
    Phone(LJO.GetValue<string>('phone', FPhone));
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderCallMeBotWhatsApp.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('phone', TJSONString.Create(FPhone));
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderCallMeBotWhatsApp.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);
    LLog := TNetEncoding.URL.Encode(LLog);

    LLogItemREST.Stream := nil;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.callmebot.com/whatsapp.php?phone=%s&text=%s&apikey=%s', [FPhone, LLog, FApiKey]);

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

ForceReferenceToClass(TProviderCallMeBotWhatsApp);

end.
