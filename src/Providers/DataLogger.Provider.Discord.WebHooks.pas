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

// https://discord.com
// https://discord.com/developers/docs/resources/webhook

unit DataLogger.Provider.Discord.WebHooks;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_DISCORD_WEB_HOOKS_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_DISCORD_WEB_HOOKS_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderDiscordWebHooks = class(TDataLoggerProvider<TProviderDiscordWebHooks>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_DISCORD_WEB_HOOKS_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_DISCORD_WEB_HOOKS_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FUsername: string;
    FAvatarURL: string;
    procedure HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
    procedure UndoLast;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderDiscordWebHooks;
    function Username(const AValue: string): TProviderDiscordWebHooks;
    function AvatarURL(const AValue: string): TProviderDiscordWebHooks;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderDiscordWebHooks }

constructor TProviderDiscordWebHooks.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://discord.com/api/webhooks/<ID_WEBHOOK>/<HASH>');
  FHTTP.ModeAsync(False);
  FHTTP.WaitTimeoutToSend(250);
  FHTTP.ExecuteFinally(HTTPExecuteFinally);

  Username('DataLogger');
  AvatarURL('');
end;

destructor TProviderDiscordWebHooks.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderDiscordWebHooks.URL(const AValue: string): TProviderDiscordWebHooks;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderDiscordWebHooks.Username(const AValue: string): TProviderDiscordWebHooks;
begin
  Result := Self;
  FUsername := AValue;
end;

function TProviderDiscordWebHooks.AvatarURL(const AValue: string): TProviderDiscordWebHooks;
begin
  Result := Self;
  FAvatarURL := AValue;
end;

procedure TProviderDiscordWebHooks.LoadFromJSON(const AJSON: string);
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
    URL(LJO.GetValue<string>('url', FHTTP.URL));
    Username(LJO.GetValue<string>('username', FUsername));
    AvatarURL(LJO.GetValue<string>('avatar_url', FAvatarURL));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderDiscordWebHooks.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', TJSONString.Create(FHTTP.URL));
    LJO.AddPair('username', TJSONString.Create(FUsername));
    LJO.AddPair('avatar_url', TJSONString.Create(FAvatarURL));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderDiscordWebHooks.Save(const ACache: TArray<TLoggerItem>);
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
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    if LItem.InternalItem.IsUndoLast then
    begin
      UndoLast;
      Continue;
    end;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('username', TJSONString.Create(FUsername));
      LJO.AddPair('avatar_url', TJSONString.Create(FAvatarURL));
      LJO.AddPair('content', TJSONString.Create(LLog));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := FHTTP.URL + '?wait=true';

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure TProviderDiscordWebHooks.HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
var
  LJO: TJSONObject;
begin
  if (AStatusCode <> 200) or (AMethod = TRESTMethod.tlmDelete) then
    Exit;

  LJO := TJSONObject.ParseJSONValue(AContent) as TJSONObject;
  if not Assigned(LJO) then
    Exit;

  try
    if not Assigned(LJO.Get('id')) then
      Exit;

    AddLastMessageId(LJO.GetValue<string>('id'));
  finally
    LJO.Free;
  end;
end;

procedure TProviderDiscordWebHooks.UndoLast;
var
  LLastMessageID: string;
  LLogItemREST: TLogItemREST;
  LItemREST: TArray<TLogItemREST>;
begin
  LLastMessageID := GetLastMessageId;
  if LLastMessageID.Trim.IsEmpty then
    Exit;

  LLogItemREST.Stream := nil;
  LLogItemREST.LogItem := Default (TLoggerItem);
  LLogItemREST.URL := FHTTP.URL + '/messages/' + LLastMessageID;

  LItemREST := Concat(LItemREST, [LLogItemREST]);

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmDelete, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderDiscordWebHooks);

end.
