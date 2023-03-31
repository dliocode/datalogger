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

// https://datalust.co/
// https://docs.datalust.co/docs

unit DataLogger.Provider.Datalust;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_DATALUST_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_DATALUST_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderDatalust = class(TDataLoggerProvider<TProviderDatalust>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_DATALUST_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_DATALUST_USE_NETHTTPCLIENT)}
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
    function URL(const AValue: string): TProviderDatalust;
    function ApiKey(const AValue: string): TProviderDatalust;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderDatalust }

constructor TProviderDatalust.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('http://localhost:5431');
end;

procedure TProviderDatalust.AfterConstruction;
begin
  inherited;

  SetIgnoreTemplate(True);
end;

destructor TProviderDatalust.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderDatalust.URL(const AValue: string): TProviderDatalust;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderDatalust.ApiKey(const AValue: string): TProviderDatalust;
begin
  Result := Self;

  FApiKey := AValue;
  FHTTP.AddHeader('X-Seq-ApiKey', AValue);
end;

procedure TProviderDatalust.LoadFromJSON(const AJSON: string);
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

function TProviderDatalust.ToJSON(const AFormat: Boolean): string;
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

procedure TProviderDatalust.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LJOEvents: TJSONObject;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLast then
      Continue;

    LJO := TJSONObject.Create;
    try
      LJOEvents := TJSONObject.Create;
      LJOEvents
        .AddPair('Timestamp', TJSONString.Create(LItem.TimestampISO8601))
        .AddPair('Level', TJSONString.Create(LItem.LevelString))
        .AddPair('Properties', SerializeItem.LogItem(LItem).ToJSONObject);

      if not LItem.Message.Trim.IsEmpty then
        LJOEvents.AddPair('MessageTemplate', TJSONString.Create(LItem.Message))
      else
        LJOEvents.AddPair('MessageTemplate', TJSONString.Create(LItem.MessageJSON));

      LJO.AddPair('Events', TJSONArray.Create.Add(LJOEvents));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s/api/events/raw', [FHTTP.URL.Trim(['/'])]);

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

ForceReferenceToClass(TProviderDatalust);

end.
