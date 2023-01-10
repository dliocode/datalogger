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

// https://www.sumologic.com/
// https://help.sumologic.com/docs/send-data/hosted-collectors/http-source/logs-metrics

unit DataLogger.Provider.SumoLogic;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_SUMOLOGIC_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SUMOLOGIC_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSumoLogic = class(TDataLoggerProvider<TProviderSumoLogic>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_SUMOLOGIC_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_SUMOLOGIC_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderSumoLogic;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderSumoLogic }

constructor TProviderSumoLogic.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('text/plain');
end;

procedure TProviderSumoLogic.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderSumoLogic.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderSumoLogic.URL(const AValue: string): TProviderSumoLogic;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

procedure TProviderSumoLogic.LoadFromJSON(const AJSON: string);
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

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSumoLogic.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', TJSONString.Create(FHTTP.URL));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSumoLogic.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLogItemREST.Stream := TLoggerSerializeItem.AsStreamJsonObject(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := FHTTP.URL;

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

ForceReferenceToClass(TProviderSumoLogic);

end.
