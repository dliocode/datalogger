{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://api.mattermost.com/

unit DataLogger.Provider.Mattermost.Hooks;

interface

uses
{$IF DEFINED(DATALOGGER_MATTERMOST_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON;

type
{$IF DEFINED(DATALOGGER_MATTERMOST_USE_INDY)}
  TProviderMattermostHooks = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_USE_NETHTTPCLIENT)}
  TProviderMattermostHooks = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderMattermostHooks = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FChannelName: string;
    FUsername: string;
    FModePropsCard: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderMattermostHooks;
    function ChannelName(const AValue: string): TProviderMattermostHooks;
    function Username(const AValue: string): TProviderMattermostHooks;
    function ModePropsCard(const AValue: Boolean): TProviderMattermostHooks;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderMattermostHooks }

constructor TProviderMattermostHooks.Create;
begin
  inherited Create;

  URL('http://localhost');
  ContentType('application/json');
  ChannelName('');
  Username('');
  ModePropsCard(False);
end;

function TProviderMattermostHooks.URL(const AValue: string): TProviderMattermostHooks;
begin
  Result := Self;
  inherited URL(AValue);
end;

function TProviderMattermostHooks.ChannelName(const AValue: string): TProviderMattermostHooks;
begin
  Result := Self;
  FChannelName := AValue;
end;

function TProviderMattermostHooks.Username(const AValue: string): TProviderMattermostHooks;
begin
  Result := Self;
  FUsername := AValue;
end;

function TProviderMattermostHooks.ModePropsCard(const AValue: Boolean): TProviderMattermostHooks;
begin
  Result := Self;
  FModePropsCard := AValue;
end;

procedure TProviderMattermostHooks.LoadFromJSON(const AJSON: string);
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
    URL(LJO.GetValue<string>('url', inherited URL));
    ChannelName(LJO.GetValue<string>('channel_name', FChannelName));
    Username(LJO.GetValue<string>('username', FUsername));
    ModePropsCard(LJO.GetValue<Boolean>('mode_props_card', FModePropsCard));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderMattermostHooks.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', inherited URL);
    LJO.AddPair('channel_name', FChannelName);
    LJO.AddPair('username', FUsername);
    LJO.AddPair('mode_props_card', TJSONBool.Create(FModePropsCard));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderMattermostHooks.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LJO: TJSONObject;
  LLog: string;

  procedure Default;
  var
    LItem: TLoggerItem;
    LLogItemREST: TLogItemREST;
  begin
    for LItem in ACache do
    begin
      if LItem.InternalItem.TypeSlineBreak then
        Continue;

      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

      LJO := TJSONObject.Create;
      try
        LJO.AddPair('channel', FChannelName);
        LJO.AddPair('username', FUsername);
        LJO.AddPair('text', LLog);

        LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
        LLogItemREST.LogItem := LItem;
        LLogItemREST.URL := '';
      finally
        LJO.Free;
      end;

      LItemREST := Concat(LItemREST, [LLogItemREST]);
    end;
  end;

  procedure PropsCard;
  var
    LItem: TLoggerItem;
    LLogItemREST: TLogItemREST;
    LAppend: TStringBuilder;
  begin
    LJO := TJSONObject.Create;
    try
      LJO.AddPair('channel', FChannelName);
      LJO.AddPair('username', FUsername);

      LAppend := TStringBuilder.Create;
      try
        for LItem in ACache do
        begin
          if LItem.InternalItem.TypeSlineBreak then
            Continue;

          LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

          LAppend.Append(LLog);
          LAppend.AppendLine;
        end;

        LJO.AddPair('props', TJSONObject.Create(TJSONPair.Create('card', LAppend.ToString)))
      finally
        LAppend.Free;
      end;

      LJO.AddPair('text', LLog);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString.Replace('\r\n', '\r\n\r\n'), TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := '';
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

begin
  if Length(ACache) = 0 then
    Exit;

  LItemREST := [];

  if FModePropsCard and (Length(ACache) > 1) then
    PropsCard
  else
    Default;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderMattermostHooks);

end.
