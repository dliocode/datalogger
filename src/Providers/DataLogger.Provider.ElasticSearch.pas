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

// https://www.elastic.co
// https://www.elastic.co/guide/index.html#viewall
// https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-index_.html

unit DataLogger.Provider.ElasticSearch;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_ELASTICSEARCH_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_ELASTICSEARCH_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.JSON, System.StrUtils;

type
  TProviderElasticSearch = class(TDataLoggerProvider<TProviderElasticSearch>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_ELASTICSEARCH_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_ELASTICSEARCH_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FBasicAuthUsername: string;
    FBasicAuthPassword: string;
    FIndex: string;
    FLastMessageID: string;
    procedure HTTPExecuteFinally(const ALogItem: TLoggerItem; const AContent: string; const AStatusCode: Integer);
    procedure UndoLastLine;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderElasticSearch;
    function BasicAuth(const AUsername: string; const APassword: string): TProviderElasticSearch;
    function Index(const AValue: string): TProviderElasticSearch;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderElasticSearch }

constructor TProviderElasticSearch.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://localhost:9200');
  FHTTP.ExecuteFinally(HTTPExecuteFinally);

  Index('logger');

  FLastMessageID := '';
end;

procedure TProviderElasticSearch.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderElasticSearch.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderElasticSearch.URL(const AValue: string): TProviderElasticSearch;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderElasticSearch.BasicAuth(const AUsername: string; const APassword: string): TProviderElasticSearch;
begin
  Result := Self;

  FBasicAuthUsername := AUsername;
  FBasicAuthPassword := APassword;

  FHTTP.BasicAuth(AUsername, APassword);
end;

function TProviderElasticSearch.Index(const AValue: string): TProviderElasticSearch;
const
  C_REMOVE: array [0 .. 10] of string = ('\', '/', '*', '?', '"', '<', '>', '|', ' ', ',', '#');
var
  LIndex: string;
  I: Integer;
begin
  Result := Self;

  LIndex := AValue;

  // https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html
  for I := Low(C_REMOVE) to High(C_REMOVE) do
    LIndex := LIndex.Replace(C_REMOVE[I], '');

  LIndex := LIndex.TrimRight(['-', '+', '_']);

  if LIndex.Trim.IsEmpty or MatchText(LIndex, ['.', '..']) then
    raise EDataLoggerException.CreateFmt('%s -> Index %s invalid!', [Self.ClassName, AValue]);

  FIndex := LIndex;
end;

procedure TProviderElasticSearch.LoadFromJSON(const AJSON: string);
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
    BasicAuth(LJO.GetValue<string>('basic_auth_username', FBasicAuthUsername), LJO.GetValue<string>('basic_auth_password', FBasicAuthPassword));
    Index(LJO.GetValue<string>('index', FIndex));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderElasticSearch.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', TJSONString.Create(FHTTP.URL));
    LJO.AddPair('basic_auth_username', TJSONString.Create());
    LJO.AddPair('basic_auth_password', TJSONString.Create(FBasicAuthPassword));
    LJO.AddPair('index', TJSONString.Create(FIndex));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderElasticSearch.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    if LItem.InternalItem.IsUndoLastLine then
    begin
      UndoLastLine;
      Continue;
    end;

    LLogItemREST.Stream := SerializeItem.LogItem(LItem).ToJSONStream;
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s/%s/_doc', [FHTTP.URL.Trim(['/']), FIndex.ToLower]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure TProviderElasticSearch.HTTPExecuteFinally(const ALogItem: TLoggerItem; const AContent: string; const AStatusCode: Integer);
var
  LJO: TJSONObject;
begin
  FLastMessageID := '';

  if AStatusCode <> 201 then
    Exit;

  LJO := TJSONObject.ParseJSONValue(AContent) as TJSONObject;
  if not Assigned(LJO) then
    Exit;

  try
    if not Assigned(LJO.Get('_id')) then
      Exit;

    FLastMessageID := LJO.GetValue<string>('_id');
  finally
    LJO.Free;
  end;
end;

procedure TProviderElasticSearch.UndoLastLine;
var
  LLogItemREST: TLogItemREST;
  LItemREST: TArray<TLogItemREST>;
begin
  if FLastMessageID.Trim.IsEmpty then
    Exit;

  LLogItemREST.Stream := nil;
  LLogItemREST.LogItem := Default (TLoggerItem);
  LLogItemREST.URL := Format('%s/%s/_doc/%s', [FHTTP.URL.Trim(['/']), FIndex.ToLower, FLastMessageID]);

  LItemREST := Concat(LItemREST, [LLogItemREST]);

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmDelete, LItemREST);

  FLastMessageID := '';
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderElasticSearch);

end.
