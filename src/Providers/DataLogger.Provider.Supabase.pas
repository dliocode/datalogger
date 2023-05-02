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

// https://supabase.com/
// https://supabase.com/docs/guides/api

unit DataLogger.Provider.Supabase;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_SUPABASE_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SUPABASE_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
  TProviderSupabase = class(TDataLoggerProvider<TProviderSupabase>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_SUPABASE_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_SUPABASE_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FApiKey: string;
    FURL: string;
    FTableName: string;
    FFieldNameID: string;
    FFieldNameMessage: string;
    procedure HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
    procedure UndoLast;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderSupabase;
    function URL(const AValue: string): TProviderSupabase;
    function TableName(const AValue: string): TProviderSupabase;
    function FieldNameID(const AValue: string): TProviderSupabase;
    function FieldNameMessage(const AValue: string): TProviderSupabase;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSupabse }

constructor TProviderSupabase.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.ExecuteFinally(HTTPExecuteFinally);

  URL('');
  ApiKey('');
  TableName('datalogger');
  FieldNameID('id');
  FieldNameMessage('message');
end;

destructor TProviderSupabase.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderSupabase.ApiKey(const AValue: string): TProviderSupabase;
begin
  Result := Self;

  FApiKey := AValue;

  FHTTP.BearerToken(AValue);
  FHTTP.AddHeader('apikey', AValue);
end;

function TProviderSupabase.URL(const AValue: string): TProviderSupabase;
begin
  Result := Self;
  FURL := AValue;
end;

function TProviderSupabase.TableName(const AValue: string): TProviderSupabase;
begin
  Result := Self;
  FTableName := AValue;
end;

function TProviderSupabase.FieldNameID(const AValue: string): TProviderSupabase;
begin
  Result := Self;
  FFieldNameID := AValue;
end;

function TProviderSupabase.FieldNameMessage(const AValue: string): TProviderSupabase;
begin
  Result := Self;
  FFieldNameMessage := AValue;
end;

procedure TProviderSupabase.LoadFromJSON(const AJSON: string);
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
    URL(LJO.GetValue<string>('url', FURL));
    TableName(LJO.GetValue<string>('tablename', FTableName));
    FieldNameID(LJO.GetValue<string>('fieldname_id', FFieldNameID));
    FieldNameMessage(LJO.GetValue<string>('fieldname_message', FFieldNameMessage));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSupabase.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));
    LJO.AddPair('url', TJSONString.Create(FURL));
    LJO.AddPair('tablename', TJSONString.Create(FTableName));
    LJO.AddPair('fieldname_id', TJSONString.Create(FFieldNameID));
    LJO.AddPair('fieldname_message', TJSONString.Create(FFieldNameMessage));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSupabase.Save(const ACache: TArray<TLoggerItem>);
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

    if LItem.InternalItem.IsUndoLast then
    begin
      UndoLast;
      Continue;
    end;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LJO := TJSONObject.Create;
    try
      LJO
        .AddPair(FFieldNameID, TJSONString.Create(LItem.Id))
        .AddPair(FFieldNameMessage, TJSONString.Create(LLog));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s/rest/v1/%s', [FURL, FTableName]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure TProviderSupabase.HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
begin
  if (AStatusCode <> 201) and (AMethod = TRESTMethod.tlmDelete) then
    Exit;

  AddLastMessageId(ALogItem.Id);
end;

procedure TProviderSupabase.UndoLast;
var
  LLastMessageID: string;
  LLogItemREST: TLogItemREST;
  LItemREST: TArray<TLogItemREST>;
begin
  LLastMessageID := GetLastMessageId;
  if LLastMessageID.Trim.IsEmpty then
    Exit;

  LLogItemREST.Stream := nil;
  LLogItemREST.LogItem := Default (TLoggerItem);
  LLogItemREST.URL := Format('%s/rest/v1/%s?%s=eq.%s', [FURL, FTableName, FFieldNameID, LLastMessageID]);

  LItemREST := Concat(LItemREST, [LLogItemREST]);

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmDelete, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSupabase);

end.
