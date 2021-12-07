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
  DataLogger.Provider.REST.HTTPClient, DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSlack = class(TProviderRESTHTTPClient)
  private
    FChannel: string;
    FUsername: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AServicesName: string; const AChannel: string = ''; const AUsername: string = ''); reintroduce;
  end;

implementation

{ TProviderSlack }

constructor TProviderSlack.Create(const AServicesName: string; const AChannel: string = ''; const AUsername: string = '');
var
  LURL: string;
begin
  LURL := Format('https://hooks.slack.com/services/%s', [AServicesName]);

  FChannel := AChannel;

  if not FChannel.Trim.IsEmpty then
    if not FChannel.StartsWith('#') then
      FChannel := '#' + AChannel;

  FUsername := AUsername;

  inherited Create(LURL, 'application/json', '');
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
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('text', TJSONString.Create(LLog));

      if not FChannel.Trim.IsEmpty then
        LJO.AddPair('channel', TJSONString.Create(FChannel));

      if not FUsername.Trim.IsEmpty then
        LJO.AddPair('username', TJSONString.Create(FUsername));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
