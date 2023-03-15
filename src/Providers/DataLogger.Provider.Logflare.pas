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

// https://logflare.app/

unit DataLogger.Provider.Logflare;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_LOGFLARE_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_LOGFLARE_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderLogflare = class(TDataLoggerProvider<TProviderLogflare>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_LOGFLARE_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_LOGFLARE_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FSourceKey: string;
    FApiKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function SourceKey(const AValue: string): TProviderLogflare;
    function ApiKey(const AValue: string): TProviderLogflare;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderLogflare }

constructor TProviderLogflare.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://api.logflare.app/api/logs');
end;

procedure TProviderLogflare.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderLogflare.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderLogflare.SourceKey(const AValue: string): TProviderLogflare;
begin
  Result := Self;
  FSourceKey := AValue;
end;

function TProviderLogflare.ApiKey(const AValue: string): TProviderLogflare;
begin
  Result := Self;

  FApiKey := AValue;
  FHTTP.AddHeader('X-API-KEY', AValue);
end;

procedure TProviderLogflare.LoadFromJSON(const AJSON: string);
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
    SourceKey(LJO.GetValue<string>('source_key', FSourceKey));
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderLogflare.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('source_key', TJSONString.Create(FSourceKey));
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderLogflare.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LJO := TJSONObject.Create;
    try
      if LItem.Message.Trim.IsEmpty then
        LJO.AddPair('message', TJSONString.Create(LItem.MessageJSON))
      else
        LJO.AddPair('message', TJSONString.Create(LItem.Message));

      LJO.AddPair('metadata', TLoggerSerializeItem.AsJsonObject(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('https://api.logflare.app/api/logs?source=%s', [FSourceKey]);

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

ForceReferenceToClass(TProviderLogflare);

end.
