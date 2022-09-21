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

// https://axiom.co/
// https://axiom.co/docs/usage/ingest#ingest-api

unit DataLogger.Provider.Axiom;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_AXIOM_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_AXIOM_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
  TProviderAxiom = class(TDataLoggerProvider<TProviderAxiom>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_AXIOM_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_AXIOM_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});
  private
    FHTTP: TProviderHTTP;
    FDataset: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiToken(const AValue: string): TProviderAxiom;
    function Dataset(const AValue: string): TProviderAxiom;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderAxiom }

constructor TProviderAxiom.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  Dataset('');
end;

procedure TProviderAxiom.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderAxiom.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderAxiom.ApiToken(const AValue: string): TProviderAxiom;
begin
  Result := Self;
  FHTTP.BearerToken(AValue);
end;

function TProviderAxiom.Dataset(const AValue: string): TProviderAxiom;
begin
  Result := Self;
  FDataset := AValue;
end;

procedure TProviderAxiom.LoadFromJSON(const AJSON: string);
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
    ApiToken(LJO.GetValue<string>('api_token', FHTTP.Token));
    Dataset(LJO.GetValue<string>('dataset', FDataset));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderAxiom.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_token', FHTTP.Token);
    LJO.AddPair('dataset', FDataset);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderAxiom.Save(const ACache: TArray<TLoggerItem>);
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
    if LItem.InternalItem.LevelSlineBreak then
      Continue;

    LJA := TJSONArray.Create;
    try
      LJO := TLoggerSerializeItem.AsJsonObject(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat);
      LJO.AddPair('_time', TJSONString.Create(DateToISO8601(LItem.TimeStamp, False)));

      LJA.Add(LJO);

      LLogItemREST.Stream := TStringStream.Create(LJA.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('https://cloud.axiom.co/api/v1/datasets/%s/ingest', [FDataset]);
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

ForceReferenceToClass(TProviderAxiom);

end.
