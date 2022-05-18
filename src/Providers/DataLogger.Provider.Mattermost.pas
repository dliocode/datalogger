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
  DataLogger.Provider.REST.HTTPClient, DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.Net.URLClient, System.NetConsts, System.Net.HttpClient;

type
  TProviderMattermost = class(TProviderRESTHTTPClient)
  private
    FChannel: string;
    function Login(const AURL: string; const AUsername: string; const APassword: string): string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AURL: string; const ATokenBearer: string; const AChannel: string); reintroduce;
    constructor CreateLogin(const AURL: string; const AUsername: string; const APassword: string; const AChannel: string);
  end;

implementation

{ TProviderMattermost }

constructor TProviderMattermost.Create(const AURL: string; const ATokenBearer: string; const AChannel: string);
var
  LURL: string;
begin
  FChannel := AChannel;

  LURL := Format('%s/api/v4/posts', [ExcludeTrailingPathDelimiter(AURL)]);
  inherited Create(LURL, 'application/json', ATokenBearer);
end;

constructor TProviderMattermost.CreateLogin(const AURL: string; const AUsername: string; const APassword: string; const AChannel: string);
var
  LTokenBearer: string;
begin
  LTokenBearer := Login(AURL, AUsername, APassword);

  Create(AURL, LTokenBearer, AChannel);
end;

function TProviderMattermost.Login(const AURL: string; const AUsername: string; const APassword: string): string;
var
  LHTTP: THTTPClient;
  LBodyJSON: TJSONOBject;
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
      LBodyJSON.AddPair('login_id',AUsername);
      LBodyJSON.AddPair('password',APassword);

      LBody := TStringStream.Create(LBodyJSON.ToString, TEncoding.UTF8);
    finally
      LBodyJSON.Free;
    end;

    try
      LResponse := LHTTP.Post(Format('%s/api/v4/users/login', [ExcludeTrailingPathDelimiter(AURL)]), LBody);
      if not Assigned(LResponse) then
        raise EDataLoggerException.Create('Erro: Invalid authentication in Provider Mattermost - Response nil!');

      if LResponse.StatusCode <> 200 then
        raise EDataLoggerException.CreateFmt('Error: Invalid authentication in Provider Mattermost - (%d) %s',[LResponse.StatusCode, LResponse.ContentAsString(TEncoding.UTF8)]);

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
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('channel_id', TJSONString.Create(FChannel));
      LJO.AddPair('message', TJSONString.Create(LLog));

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
