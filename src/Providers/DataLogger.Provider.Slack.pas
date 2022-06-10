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
    FUsername: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ServiceName(const AValue: string): TProviderSlack;
    function Channel(const AValue: string): TProviderSlack;
    function Username(const AValue: string): TProviderSlack;

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

function TProviderSlack.Username(const AValue: string): TProviderSlack;
begin
  Result := Self;
  FUsername := AValue;
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
    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('text', LLog);

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

  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
