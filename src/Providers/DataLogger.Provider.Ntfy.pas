{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

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

// https://ntfy.sh/
// https://docs.ntfy.sh/

unit DataLogger.Provider.Ntfy;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_NTFY_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_NTFY_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderNtfy = class(TDataLoggerProvider<TProviderNtfy>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_NTFY_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_NTFY_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});
  private
    FHTTP: TProviderHTTP;
    FServerURL: string;
    FTopic: string;
    FTitle: string;
    FIncludeLevelInTitle: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ServerURL(const AValue: string): TProviderNtfy;
    function Topic(const AValue: string): TProviderNtfy;
    function Title(const AValue: string; const AIncludeLevelInTitle: Boolean = True): TProviderNtfy;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderNtfy }

constructor TProviderNtfy.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('http://ntfy.sh');

  ServerURL('http://ntfy.sh');
  Topic('datalogger');
end;

procedure TProviderNtfy.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(False);
end;

destructor TProviderNtfy.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderNtfy.ServerURL(const AValue: string): TProviderNtfy;
begin
  Result := Self;
  FServerURL := AValue;
end;

function TProviderNtfy.Topic(const AValue: string): TProviderNtfy;
begin
  Result := Self;
  FTopic := AValue;
end;

function TProviderNtfy.Title(const AValue: string; const AIncludeLevelInTitle: Boolean): TProviderNtfy;
begin
  Result := Self;

  FTitle := AValue;
  FIncludeLevelInTitle := AIncludeLevelInTitle;
end;

procedure TProviderNtfy.LoadFromJSON(const AJSON: string);
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
    Topic(LJO.GetValue<string>('topic', FTopic));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderNtfy.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('topic', TJSONString.Create(FTopic));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderNtfy.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LTag: string;
  LPriority: Integer;
  LTitle: string;
  LJO: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  if FServerURL.Trim.IsEmpty then
    FServerURL := FHTTP.URL;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    // https://docs.ntfy.sh/emojis/
    // https://docs.ntfy.sh/publish/?h=priority#message-priority
    case LItem.Level of
      TLoggerLevel.Trace:
        begin
          LTag := 'purple_circle';
          LPriority := 1;
        end;

      TLoggerLevel.Debug:
        begin
          LTag := 'large_blue_circle';
          LPriority := 2;
        end;

      TLoggerLevel.Info:
        begin
          LTag := 'white_circle';
          LPriority := 3;
        end;

      TLoggerLevel.Success:
        begin
          LTag := 'green_circle';
          LPriority := 3;
        end;

      TLoggerLevel.Warn:
        begin
          LTag := 'yellow_circle';
          LPriority := 4;
        end;

      TLoggerLevel.Error:
        begin
          LTag := 'orange_circle';
          LPriority := 5;
        end;

      TLoggerLevel.Fatal:
        begin
          LTag := 'red_circle';
          LPriority := 5;
        end;

      TLoggerLevel.Custom:
        begin
          LTag := 'black_circle';
          LPriority := 5;
        end;
    else
      LTag := 'green_circle';
      LPriority := 3;
    end;

    if FIncludeLevelInTitle then
      LTitle := FTitle + ' ' + LItem.LevelString;

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('topic', TJSONString.Create(FTopic));
      LJO.AddPair('title', TJSONString.Create(LTitle));
      LJO.AddPair('tags', TJSONArray.Create.Add(LTag));
      LJO.AddPair('message', TJSONString.Create(LLog));
      LJO.AddPair('priority', TJSONNumber.Create(LPriority));

{$IF CompilerVersion > 32} // 32 = Delphi Tokyo (10.2)
      LLog := LJO.ToString;
{$ELSE}
      LLog := LJO.ToJSON;
{$ENDIF}
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := FServerURL;

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

ForceReferenceToClass(TProviderNtfy);

end.
