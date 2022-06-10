{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://api.mattermost.com/

unit DataLogger.Provider.Mattermost;

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
  TProviderMattermost = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_USE_NETHTTPCLIENT)}
  TProviderMattermost = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderMattermost = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FChannelId: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderMattermost; overload;
    function URL: string; overload;
    function ChannelId(const AValue: string): TProviderMattermost;

    constructor Create; overload;
  end;

implementation

{ TProviderMattermost }

constructor TProviderMattermost.Create;
begin
  inherited Create;

  URL('http://localhost');
  ContentType('application/json');
  ChannelId('');
end;

function TProviderMattermost.URL(const AValue: string): TProviderMattermost;
begin
  Result := Self;
  inherited URL(AValue);
end;

function TProviderMattermost.URL: string;
begin
  Result := inherited URL;
end;

function TProviderMattermost.ChannelId(const AValue: string): TProviderMattermost;
begin
  Result := Self;
  FChannelId := AValue;
end;

procedure TProviderMattermost.Save(const ACache: TArray<TLoggerItem>);
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
      LJO.AddPair('channel_id', FChannelId);
      LJO.AddPair('message', LLog);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('%s/api/v4/posts', [ExcludeTrailingPathDelimiter(URL)]);
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
