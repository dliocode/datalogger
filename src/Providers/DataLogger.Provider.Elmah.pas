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

// https://elmah.io/
// https://docs.elmah.io/using-the-rest-api/
// https://api.elmah.io/swagger/index.html#/Messages/Messages_Create

unit DataLogger.Provider.Elmah;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_ELMAH_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_ELMAH_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderElmah = class(TDataLoggerProvider<TProviderElmah>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_ELMAH_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_ELMAH_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FLogID: string;
    FApiKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function LogID(const AValue: string): TProviderElmah;
    function ApiKey(const AValue: string): TProviderElmah;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderElmah }

constructor TProviderElmah.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
end;

procedure TProviderElmah.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(False);
end;

destructor TProviderElmah.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderElmah.LogID(const AValue: string): TProviderElmah;
begin
  Result := Self;
  FLogID := AValue;
end;

function TProviderElmah.ApiKey(const AValue: string): TProviderElmah;
begin
  Result := Self;
  FApiKey := AValue;
end;

procedure TProviderElmah.LoadFromJSON(const AJSON: string);
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
    LogID(LJO.GetValue<string>('log_id', FLogID));
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderElmah.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('log_id', TJSONString.Create(FLogID));
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderElmah.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LSeverity: string;
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

    case LItem.Level of
      TLoggerLevel.Trace:
        LSeverity := 'Verbose';

      TLoggerLevel.Debug:
        LSeverity := 'Debug';

      TLoggerLevel.Info, TLoggerLevel.Success, TLoggerLevel.Custom:
        LSeverity := 'Information';

      TLoggerLevel.Warn:
        LSeverity := 'Warning';

      TLoggerLevel.Error:
        LSeverity := 'Error';

      TLoggerLevel.Fatal:
        LSeverity := 'Fatal';
    end;

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('application', TJSONString.Create(LItem.AppName));
      LJO.AddPair('detail', TJSONString.Create(LLog));
      LJO.AddPair('hostname', TJSONString.Create(LItem.ComputerName));
      LJO.AddPair('title', TJSONString.Create(LItem.Message));
      LJO.AddPair('source', TJSONString.Create(LItem.Tag));
      LJO.AddPair('dateTime', TJSONString.Create(LItem.TimeStampISO8601));
      LJO.AddPair('type', TJSONString.Create(LItem.LevelString));
      LJO.AddPair('user', TJSONString.Create(LItem.Username));
      LJO.AddPair('severity', TJSONString.Create(LSeverity));
      LJO.AddPair('url', TJSONString.Create(LItem.AppPath));
      LJO.AddPair('version', TJSONString.Create(LItem.AppVersion.FileVersion));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.elmah.io/v3/messages/%s?api_key=%s', [FLogID, FApiKey]);

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

ForceReferenceToClass(TProviderElmah);

end.
