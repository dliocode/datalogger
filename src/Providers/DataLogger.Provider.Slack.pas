{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://api.slack.com/tutorials/slack-apps-hello-world

unit DataLogger.Provider.Slack;

interface

uses
{$IF DEFINED(DATALOGGER_SLACK_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SLACK_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON;

type
{$IF DEFINED(DATALOGGER_SLACK_USE_INDY)}
  TProviderSlack = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_SLACK_USE_NETHTTPCLIENT)}
  TProviderSlack = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderSlack = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FServiceName: string;
    FChannel: string;
    FChannelId: string;
    FUsername: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ServiceName(const AValue: string): TProviderSlack;
    function Channel(const AValue: string): TProviderSlack;
    function ChannelId(const AValue: string): TProviderSlack;
    function Username(const AValue: string): TProviderSlack;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderSlack }

constructor TProviderSlack.Create;
begin
  inherited Create;

  ContentType('application/json');
  ServiceName('');
  Channel('');
  ChannelId('');
  Username('');
end;

function TProviderSlack.ServiceName(const AValue: string): TProviderSlack;
begin
  Result := Self;
  FServiceName := AValue;
end;

function TProviderSlack.Channel(const AValue: string): TProviderSlack;
begin
  Result := Self;

  FChannel := AValue;
  if not FChannel.Trim.IsEmpty then
    if not FChannel.StartsWith('#') then
      FChannel := '#' + AValue;
end;

function TProviderSlack.ChannelId(const AValue: string): TProviderSlack;
begin
  Result := Self;
  FChannelId := AValue;
end;

function TProviderSlack.Username(const AValue: string): TProviderSlack;
begin
  Result := Self;
  FUsername := AValue;
end;

procedure TProviderSlack.LoadFromJSON(const AJSON: string);
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
    ServiceName(LJO.GetValue<string>('service_name', FServiceName));
    Channel(LJO.GetValue<string>('channel', FChannel));
    ChannelId(LJO.GetValue<string>('channel_id', FChannelId));
    Username(LJO.GetValue<string>('username', FUsername));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSlack.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('service_name', FServiceName);
    LJO.AddPair('channel', FChannel);
    LJO.AddPair('channel_id', FChannelId);
    LJO.AddPair('username', FUsername);

    ToJSONInternal(LJO);

    if AFormat then
      Result := LJO.Format
    else
      Result := LJO.ToString;
  finally
    LJO.Free;
  end;
end;


procedure TProviderSlack.Save(const ACache: TArray<TLoggerItem>);
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
      LJO.AddPair('text', LLog);

      if not FChannelId.Trim.IsEmpty then
        LJO.AddPair('channel_id', FChannelId)
      else
        if not FChannel.Trim.IsEmpty then
          LJO.AddPair('channel', FChannel);

      if not FUsername.Trim.IsEmpty then
        LJO.AddPair('username', FUsername);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('https://hooks.slack.com/services/%s', [FServiceName]);
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSlack);

end.
