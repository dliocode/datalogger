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

// https://www.datadoghq.com/

unit DataLogger.Provider.Datadog;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_DATADOG_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_DATADOG_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderDatadog = class(TDataLoggerProvider<TProviderDatadog>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_DATADOG_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_DATADOG_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FApiKey: string;
    FApplicationKey: string;
    FSource: string;
    FService: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderDatadog;
    function ApplicationKey(const AValue: string): TProviderDatadog;
    function Source(const AValue: string): TProviderDatadog;
    function Service(const AValue: string): TProviderDatadog;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderDatadog }

constructor TProviderDatadog.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://http-intake.logs.datadoghq.com/api/v2/logs');

  Source('datalogger');
  Service('');
end;

procedure TProviderDatadog.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderDatadog.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderDatadog.ApiKey(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FApiKey := AValue;
  FHTTP.AddHeader('DD-API-KEY', AValue);
end;

function TProviderDatadog.ApplicationKey(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FApplicationKey := AValue;
  FHTTP.AddHeader('DD-APPLICATION-KEY', AValue);
end;

function TProviderDatadog.Source(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FSource := AValue;
end;

function TProviderDatadog.Service(const AValue: string): TProviderDatadog;
begin
  Result := Self;
  FService := AValue;
end;

procedure TProviderDatadog.LoadFromJSON(const AJSON: string);
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
    ApplicationKey(LJO.GetValue<string>('application_key', FApplicationKey));
    Source(LJO.GetValue<string>('source', FSource));
    Service(LJO.GetValue<string>('service', FService));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderDatadog.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));
    LJO.AddPair('application_key', TJSONString.Create(FApplicationKey));
    LJO.AddPair('source', TJSONString.Create(FSource));
    LJO.AddPair('service', TJSONString.Create(FService));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderDatadog.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJA: TJSONArray;
  LJO: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LJA := TJSONArray.Create;
    try
      LJO := TLoggerSerializeItem.AsJsonObject(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat);
      LJO.AddPair('ddsource', TJSONString.Create(FSource));
      LJO.AddPair('ddtags', TJSONString.Create(LItem.Tag));
      LJO.AddPair('host', TJSONString.Create(LItem.ComputerName));
      LJO.AddPair('service', TJSONString.Create(FService));
      LJO.AddPair('status', TJSONString.Create(LItem.LevelString.ToUpper));

      LJA.Add(LJO);

      LLogItemREST.Stream := TStringStream.Create(LJA.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := 'https://http-intake.logs.datadoghq.com/api/v2/logs';
    finally
      LJA.Free;
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

ForceReferenceToClass(TProviderDatadog);

end.
