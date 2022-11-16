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

// https://www.mezmo.com/
// https://docs.mezmo.com/docs
// https://docs.mezmo.com/log-analysis-api/ref#ingest

unit DataLogger.Provider.Mezmo;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_LogDNA_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_LogDNA_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderLogDNA = class(TDataLoggerProvider<TProviderLogDNA>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_LogDNA_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_LogDNA_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FApiKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderLogDNA;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderLogDNA }

constructor TProviderLogDNA.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://logs.logdna.com/logs/ingest');
end;

procedure TProviderLogDNA.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderLogDNA.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderLogDNA.ApiKey(const AValue: string): TProviderLogDNA;
begin
  Result := Self;
  FApiKey := AValue;
  FHTTP.BasicAuth(AValue, '');
end;

procedure TProviderLogDNA.LoadFromJSON(const AJSON: string);
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
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderLogDNA.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderLogDNA.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LJOLog: TJSONObject;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsJsonObjectToString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat);

    LJO := TJSONObject.Create;
    try
      LJOLog := TJSONObject.Create;
      LJOLog.AddPair('timestamp', TJSONNumber.Create(LItem.TimestampUNIX));
      LJOLog.AddPair('line', TJSONString.Create(LLog));
      LJOLog.AddPair('app', TJSONString.Create(LItem.AppName));
      LJOLog.AddPair('level', TJSONString.Create(LItem.LevelString));

      LJO.AddPair('lines', TJSONArray.Create(LJOLog));

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('https://logs.logdna.com/logs/ingest?hostname=%s&mac=%s&ip=%s&now=%d', [LItem.ComputerName, LItem.MACAddress, LItem.IPLocal, LItem.TimestampUNIX]);
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderLogDNA);

end.
