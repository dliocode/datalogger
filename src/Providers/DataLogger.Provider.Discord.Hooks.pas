{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://discord.com/developers/docs/resources/webhook

unit DataLogger.Provider.Discord.Hooks;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_DISCORD_HOOKS_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_DISCORD_HOOKS_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderDiscordHooks = class(TDataLoggerProvider<TProviderDiscordHooks>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_DISCORD_HOOKS_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_DISCORD_HOOKS_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FUsername: string;
    FAvatarURL: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderDiscordHooks;
    function Username(const AValue: string): TProviderDiscordHooks;
    function AvatarURL(const AValue: string): TProviderDiscordHooks;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderDiscordHooks }

constructor TProviderDiscordHooks.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://discord.com/api/webhooks/<ID_WEBHOOK>/<HASH>');

  Username('DataLogger');
  AvatarURL('');
end;

destructor TProviderDiscordHooks.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderDiscordHooks.URL(const AValue: string): TProviderDiscordHooks;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderDiscordHooks.Username(const AValue: string): TProviderDiscordHooks;
begin
  Result := Self;
  FUsername := AValue;
end;

function TProviderDiscordHooks.AvatarURL(const AValue: string): TProviderDiscordHooks;
begin
  Result := Self;
  FAvatarURL := AValue;
end;

procedure TProviderDiscordHooks.LoadFromJSON(const AJSON: string);
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

function TProviderDiscordHooks.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', FHTTP.URL);
    LJO.AddPair('username', FUsername);
    LJO.AddPair('avatar_url', FAvatarURL);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderDiscordHooks.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;

  LItem: TLoggerItem;
  LLog: string;
  LJO: TJSONObject;

  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('username', FUsername);
      LJO.AddPair('avatar_url', FAvatarURL);
      LJO.AddPair('content', LLog);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := FHTTP.URL;
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP.InternalSave(TRESTMethod.tlmPost, LItemREST, 250);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderDiscordHooks);

end.
