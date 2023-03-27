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

// https://www.dynatrace.com/
// https://www.dynatrace.com/support/help/dynatrace-api/environment-api/log-monitoring-v2/post-ingest-logs
// https://www.dynatrace.com/support/help/dynatrace-api/basics/dynatrace-api-authentication

unit DataLogger.Provider.Dynatrace;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_DYNATRACE_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_DYNATRACE_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.StrUtils;

type
  TProviderDynatrace = class(TDataLoggerProvider<TProviderDynatrace>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_DYNATRACE_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_DYNATRACE_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FToken: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderDynatrace;
    function Token(const AValue: string): TProviderDynatrace;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderDynatrace }

constructor TProviderDynatrace.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json; charset=utf-8');
end;

procedure TProviderDynatrace.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderDynatrace.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderDynatrace.URL(const AValue: string): TProviderDynatrace;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderDynatrace.Token(const AValue: string): TProviderDynatrace;
begin
  Result := Self;
  FToken := AValue;
  FHTTP.Authorization('Api-Token ' + AValue);
end;

procedure TProviderDynatrace.LoadFromJSON(const AJSON: string);
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
    Token(LJO.GetValue<string>('token', FToken));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderDynatrace.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', TJSONString.Create(FHTTP.URL));
    LJO.AddPair('token', TJSONString.Create(FToken));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderDynatrace.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LJA: TJSONArray;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  LJA := TJSONArray.Create;
  try
    for LItem in ACache do
    begin
      if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLastLine then
        Continue;

      LJO := SerializeItem.LogItem(LItem).ToJSONObject;
      try
        LJO.AddPair('timestamp', TJSONString.Create(LItem.TimeStampISO8601));

        if MatchText(LItem.LevelString.ToUpper, ['INFO', 'WARN', 'ERROR', 'FATAL']) then
          LJO.AddPair('loglevel', TJSONString.Create(LItem.LevelString));

        if LItem.Message.Trim.IsEmpty then
          LJO.AddPair('content', TJSONString.Create(LItem.MessageJSON))
        else
          LJO.AddPair('content', TJSONString.Create(LItem.Message));
      finally
        LJA.Add(LJO);
      end;
    end;

    LLog := LJA.ToString;
  finally
    LJA.Free;
  end;

  LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
  LLogItemREST.LogItem := LItem;
  LLogItemREST.URL := FHTTP.URL;

  LItemREST := Concat(LItemREST, [LLogItemREST]);

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderDynatrace);

end.
