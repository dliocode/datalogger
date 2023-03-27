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

unit DataLogger.Provider.Mattermost.Hooks;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_MATTERMOST_HOOKS_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_HOOKS_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderMattermostHooks = class(TDataLoggerProvider<TProviderMattermostHooks>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_MATTERMOST_HOOKS_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_MATTERMOST_HOOKS_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FChannelName: string;
    FUsername: string;
    FModePropsCard: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderMattermostHooks;
    function ChannelName(const AValue: string): TProviderMattermostHooks;
    function Username(const AValue: string): TProviderMattermostHooks;
    function ModePropsCard(const AValue: Boolean): TProviderMattermostHooks;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderMattermostHooks }

constructor TProviderMattermostHooks.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('http://localhost');

  ChannelName('');
  Username('');
  ModePropsCard(False);
end;

destructor TProviderMattermostHooks.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderMattermostHooks.URL(const AValue: string): TProviderMattermostHooks;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderMattermostHooks.ChannelName(const AValue: string): TProviderMattermostHooks;
begin
  Result := Self;
  FChannelName := AValue;
end;

function TProviderMattermostHooks.Username(const AValue: string): TProviderMattermostHooks;
begin
  Result := Self;
  FUsername := AValue;
end;

function TProviderMattermostHooks.ModePropsCard(const AValue: Boolean): TProviderMattermostHooks;
begin
  Result := Self;
  FModePropsCard := AValue;
end;

procedure TProviderMattermostHooks.LoadFromJSON(const AJSON: string);
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
    ChannelName(LJO.GetValue<string>('channel_name', FChannelName));
    Username(LJO.GetValue<string>('username', FUsername));
    ModePropsCard(LJO.GetValue<Boolean>('mode_props_card', FModePropsCard));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderMattermostHooks.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', TJSONString.Create(FHTTP.URL));
    LJO.AddPair('channel_name', TJSONString.Create(FChannelName));
    LJO.AddPair('username', TJSONString.Create(FUsername));
    LJO.AddPair('mode_props_card', TJSONBool.Create(FModePropsCard));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
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
      if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLastLine then
        Continue;

      LLog := SerializeItem.LogItem(LItem).ToString;

      LJO := TJSONObject.Create;
      try
        LJO.AddPair('channel', TJSONString.Create(FChannelName));
        LJO.AddPair('username', TJSONString.Create(FUsername));
        LJO.AddPair('text', TJSONString.Create(LLog));

        LLog := LJO.ToString;
        LLog := LLog.Replace(#$D#$A, '\n');
      finally
        LJO.Free;
      end;

      LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := '';

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
      LJO.AddPair('channel', TJSONString.Create(FChannelName));
      LJO.AddPair('username', TJSONString.Create(FUsername));

      LAppend := TStringBuilder.Create;
      try
        for LItem in ACache do
        begin
          if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLastLine then
            Continue;

          LLog := SerializeItem.LogItem(LItem).ToString;

          LAppend.Append(LLog);
          LAppend.AppendLine;
        end;

        LJO.AddPair('props', TJSONObject.Create(TJSONPair.Create('card', TJSONString.Create(LAppend.ToString))))
      finally
        LAppend.Free;
      end;

      LJO.AddPair('text', TJSONString.Create(LLog));

      LLog := LJO.ToString;
      LLog := LLog.Replace(#$D#$A, '\n');
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := '';

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  if FModePropsCard and (Length(ACache) > 1) then
    PropsCard
  else
    Default;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderMattermostHooks);

end.
