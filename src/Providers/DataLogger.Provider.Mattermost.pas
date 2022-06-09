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
  System.SysUtils, System.Classes, System.JSON, System.Net.URLClient, System.NetConsts, System.Net.HTTPClient;

type
{$IF DEFINED(DATALOGGER_MATTERMOST_USE_INDY)}
  TProviderMattermost = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_USE_NETHTTPCLIENT)}
  TProviderMattermost = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderMattermost = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FURL: string;
    FTokenBearer: string;
    FChannelId: string;
    function Login(const AURL: string; const AUsername: string; const APassword: string): string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    property URL: string read FURL write FURL;
    property TokenBearer: string read FTokenBearer write FTokenBearer;
    property ChannelId: string read FChannelId write FChannelId;

    constructor Create(const AURL: string; const ATokenBearer: string; const AChannelId: string); reintroduce;
    constructor CreateLogin(const AURL: string; const AUsername: string; const APassword: string; const AChannelId: string);
  end;

implementation

{ TProviderMattermost }

constructor TProviderMattermost.Create(const AURL: string; const ATokenBearer: string; const AChannelId: string);
begin
  FURL := AURL;
  FTokenBearer := ATokenBearer;
  FChannelId := AChannelId;

  inherited Create('', 'application/json', ATokenBearer);
end;

constructor TProviderMattermost.CreateLogin(const AURL: string; const AUsername: string; const APassword: string; const AChannelId: string);
var
  LTokenBearer: string;
begin
  LTokenBearer := Login(AURL, AUsername, APassword);

  Create(AURL, LTokenBearer, AChannelId);
end;

function TProviderMattermost.Login(const AURL: string; const AUsername: string; const APassword: string): string;
var
  LHTTP: THTTPClient;
  LBodyJSON: TJSONObject;
  LBody: TStream;
  LResponse: IHTTPResponse;
begin
  LHTTP := THTTPClient.Create;
  try
{$IF RTLVersion > 32} // 32 = Delphi Tokyo (10.2)
    LHTTP.ConnectionTimeout := 60000;
    LHTTP.ResponseTimeout := 60000;
    LHTTP.SendTimeout := 60000;
{$ENDIF}
    LHTTP.HandleRedirects := True;
    LHTTP.UserAgent := 'DataLogger.Provider.Mattermost';
    LHTTP.ContentType := 'application/json';
    LHTTP.AcceptCharSet := 'utf-8';
    LHTTP.AcceptEncoding := 'utf-8';
    LHTTP.Accept := 'application/json';

    LBodyJSON := TJSONObject.Create;
    try
      LBodyJSON.AddPair('login_id', AUsername);
      LBodyJSON.AddPair('password', APassword);

      LBody := TStringStream.Create(LBodyJSON.ToString, TEncoding.UTF8);
    finally
      LBodyJSON.Free;
    end;

    try
      LResponse := LHTTP.Post(Format('%s/api/v4/users/login', [ExcludeTrailingPathDelimiter(AURL)]), LBody);
      if not Assigned(LResponse) then
        raise EDataLoggerException.Create('Erro: Invalid authentication in Provider Mattermost - Response nil!');

      if LResponse.StatusCode <> 200 then
        raise EDataLoggerException.CreateFmt('Error: Invalid authentication in Provider Mattermost - (%d) %s', [LResponse.StatusCode, LResponse.ContentAsString(TEncoding.UTF8)]);

      Result := LResponse.HeaderValue['Token'];
    finally
      LBody.Free;
    end;
  finally
    LHTTP.Free;
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
    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('channel_id', FChannelId);
      LJO.AddPair('message', LLog);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('%s/api/v4/posts', [ExcludeTrailingPathDelimiter(FURL)]);
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
