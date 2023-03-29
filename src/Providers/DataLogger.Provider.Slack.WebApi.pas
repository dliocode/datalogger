{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2023 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

// https://slack.com
// https://api.slack.com
// https://api.slack.com/apps
// https://api.slack.com/messaging/sending
// https://api.slack.com/web#basics

unit DataLogger.Provider.Slack.WebApi;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_SLACK_WEBAPI_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SLACK_WEBAPI_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSlackWebApi = class(TDataLoggerProvider<TProviderSlackWebApi>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_SLACK_WEBAPI_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_SLACK_WEBAPI_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FToken: string;
    FChannelID: string;
    procedure HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
    procedure UndoLast;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Token(const AValue: string): TProviderSlackWebApi;
    function ChannelID(const AValue: string): TProviderSlackWebApi;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSlack }

constructor TProviderSlackWebApi.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://slack.com/api');
  FHTTP.ExecuteFinally(HTTPExecuteFinally);
end;

destructor TProviderSlackWebApi.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderSlackWebApi.Token(const AValue: string): TProviderSlackWebApi;
begin
  Result := Self;

  FHTTP.BearerToken(AValue);
  FToken := AValue;
end;

function TProviderSlackWebApi.ChannelID(const AValue: string): TProviderSlackWebApi;
begin
  Result := Self;
  FChannelID := AValue;
end;

procedure TProviderSlackWebApi.LoadFromJSON(const AJSON: string);
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
    Token(LJO.GetValue<string>('token', FToken));
    ChannelID(LJO.GetValue<string>('channel_id', FChannelID));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSlackWebApi.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('token', TJSONString.Create(FToken));
    LJO.AddPair('channel_id', TJSONString.Create(FChannelID));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSlackWebApi.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LJO: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    if LItem.InternalItem.IsUndoLast then
    begin
      UndoLast;
      Continue;
    end;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('channel', TJSONString.Create(FChannelID));
      LJO.AddPair('text', TJSONString.Create(LLog));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := FHTTP.URL + '/chat.postMessage';

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure TProviderSlackWebApi.HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
var
  LJO: TJSONObject;
begin
  if (AStatusCode <> 200) or (AMethod = TRESTMethod.tlmDelete) then
    Exit;

  LJO := TJSONObject.ParseJSONValue(AContent) as TJSONObject;
  if not Assigned(LJO) then
    Exit;

  try
    if not Assigned(LJO.Get('ts')) then
      Exit;

    AddLastMessageId(LJO.GetValue<string>('ts'));
  finally
    LJO.Free;
  end;
end;

procedure TProviderSlackWebApi.UndoLast;
var
  LLastMessageID: string;
  LJO: TJSONObject;
  LLog: string;
  LLogItemREST: TLogItemREST;
  LItemREST: TArray<TLogItemREST>;
begin
  LLastMessageID := GetLastMessageId;
  if LLastMessageID.Trim.IsEmpty then
    Exit;

  LJO := TJSONObject.Create;
  try
    LJO.AddPair('channel', TJSONString.Create(FChannelID));
    LJO.AddPair('ts', TJSONString.Create(LLastMessageID));

    LLog := LJO.ToString;
  finally
    LJO.Free;
  end;

  LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
  LLogItemREST.LogItem := Default (TLoggerItem);
  LLogItemREST.URL := FHTTP.URL + '/chat.delete';

  LItemREST := Concat(LItemREST, [LLogItemREST]);

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSlackWebApi);

end.
