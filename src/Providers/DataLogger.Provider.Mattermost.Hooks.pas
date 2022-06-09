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
  System.SysUtils, System.Classes, System.JSON, System.Net.URLClient, System.NetConsts, System.Net.HTTPClient;

type
{$IF DEFINED(DATALOGGER_MATTERMOST_USE_INDY)}
  TProviderMattermostHooks = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_USE_NETHTTPCLIENT)}
  TProviderMattermostHooks = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderMattermostHooks = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FURL: string;
    FChannelId: string;
    FUsername: string;
    FModePropsCard: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    property URL: string read FURL write FURL;
    property ChannelId: string read FChannelId write FChannelId;
    property Username: string read FUsername write FUsername;
    property ModePropsCard: Boolean read FModePropsCard write FModePropsCard;

    constructor Create(const AURL: string; const AChannelId: string; const AUserName: string = ''; const AModePropsCard: Boolean = False); reintroduce;
  end;

implementation

{ TProviderMattermostHooks }

constructor TProviderMattermostHooks.Create(const AURL: string; const AChannelId: string; const AUserName: string = ''; const AModePropsCard: Boolean = False);
begin
  FURL := AURL;
  FChannelId := AChannelId;
  FUsername := AUserName;
  FModePropsCard := AModePropsCard;

  inherited Create('', 'application/json');
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
      if LItem.&Type = TLoggerType.All then
        Continue;

      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

      LJO := TJSONObject.Create;
      try
        LJO.AddPair('channel_id', FChannelId);
        LJO.AddPair('username', FUsername);
        LJO.AddPair('text', LLog);

        LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
        LLogItemREST.LogItem := LItem;
        LLogItemREST.URL := FURL;
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
      LJO.AddPair('channel_id', FChannelId);
      LJO.AddPair('username', FUsername);

      LAppend := TStringBuilder.Create;
      try
        for LItem in ACache do
        begin
          if LItem.&Type = TLoggerType.All then
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
      LLogItemREST.URL := FURL;
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

  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
