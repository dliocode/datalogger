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

// https://api.mattermost.com/

unit DataLogger.Provider.Mattermost;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_MATTERMOST_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderMattermost = class(TDataLoggerProvider<TProviderMattermost>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_MATTERMOST_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FChannelId: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderMattermost;
    function BearerToken(const AValue: string): TProviderMattermost;
    function ChannelId(const AValue: string): TProviderMattermost;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderMattermost }

constructor TProviderMattermost.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('http://localhost');

  ChannelId('');
end;

destructor TProviderMattermost.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderMattermost.URL(const AValue: string): TProviderMattermost;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderMattermost.ChannelId(const AValue: string): TProviderMattermost;
begin
  Result := Self;
  FChannelId := AValue;
end;

function TProviderMattermost.BearerToken(const AValue: string): TProviderMattermost;
begin
  Result := Self;
  FHTTP.BearerToken(AValue);
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
    URL(LJO.GetValue<string>('url', FHTTP.URL));
    BearerToken(LJO.GetValue<string>('token', FHTTP.Authorization));
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
    LJO.AddPair('url', TJSONString.Create(FHTTP.URL));
    LJO.AddPair('token', TJSONString.Create(FHTTP.Authorization));
    LJO.AddPair('channel_id', TJSONString.Create(FChannelId));

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
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('channel_id', TJSONString.Create(FChannelId));
      LJO.AddPair('message', TJSONString.Create(LLog));

      LLog := LJO.ToString;
      LLog := LLog.Replace(#$D#$A, '\n');
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s/api/v4/posts', [FHTTP.URL.Trim(['/'])]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderMattermost);

end.
