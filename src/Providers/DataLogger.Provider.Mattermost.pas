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
    function URL(const AValue: string): TProviderMattermost;
    function BearerToken(const AValue: string): TProviderMattermost;
    function ChannelId(const AValue: string): TProviderMattermost;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

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

function TProviderMattermost.ChannelId(const AValue: string): TProviderMattermost;
begin
  Result := Self;
  FChannelId := AValue;
end;

function TProviderMattermost.BearerToken(const AValue: string): TProviderMattermost;
begin
  Result := Self;
  inherited BearerToken(AValue);
end;

procedure TProviderMattermost.LoadFromJSON(const AJSON: string);
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
    BearerToken(LJO.GetValue<string>('token', inherited Token));
    ChannelId(LJO.GetValue<string>('channel_id', FChannelId));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderMattermost.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', inherited URL);
    LJO.AddPair('token', inherited Token);
    LJO.AddPair('channel_id', FChannelId);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
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
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('channel_id', FChannelId);
      LJO.AddPair('message', LLog);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('%s/api/v4/posts', [ExcludeTrailingPathDelimiter(inherited URL)]);
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

ForceReferenceToClass(TProviderMattermost);

end.
