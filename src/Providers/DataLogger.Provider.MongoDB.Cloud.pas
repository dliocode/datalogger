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

// https://www.mongodb.com/cloud
// https://www.mongodb.com/docs/atlas/api/data-api/

unit DataLogger.Provider.MongoDB.Cloud;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_MONGODB_CLOUD_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_MONGODB_CLOUD_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderMongoDBCloud = class(TDataLoggerProvider<TProviderMongoDBCloud>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_MONGODB_CLOUD_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_MONGODB_CLOUD_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FApiKey: string;
    FDataSource: string;
    FDataBase: string;
    FCollection: string;
    procedure HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
    procedure UndoLast;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URLEndpoint(const AValue: string): TProviderMongoDBCloud;
    function ApiKey(const AValue: string): TProviderMongoDBCloud;
    function DataSource(const AValue: string): TProviderMongoDBCloud;
    function DataBase(const AValue: string): TProviderMongoDBCloud;
    function Collection(const AValue: string): TProviderMongoDBCloud;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderMongoDBCloud }

constructor TProviderMongoDBCloud.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.ExecuteFinally(HTTPExecuteFinally);

  URLEndpoint('https://sa-east-1.aws.data.mongodb-api.com/app/<APP-ID>/endpoint/data/v1');
  DataSource('Cluster0');
  DataBase('db_datalogger');
  Collection('logger');
end;

procedure TProviderMongoDBCloud.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderMongoDBCloud.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderMongoDBCloud.URLEndpoint(const AValue: string): TProviderMongoDBCloud;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderMongoDBCloud.ApiKey(const AValue: string): TProviderMongoDBCloud;
begin
  Result := Self;

  FApiKey := AValue;
  FHTTP.AddHeader('api-key', AValue);
end;

function TProviderMongoDBCloud.DataSource(const AValue: string): TProviderMongoDBCloud;
begin
  Result := Self;
  FDataSource := AValue;
end;

function TProviderMongoDBCloud.DataBase(const AValue: string): TProviderMongoDBCloud;
begin
  Result := Self;
  FDataBase := AValue;
end;

function TProviderMongoDBCloud.Collection(const AValue: string): TProviderMongoDBCloud;
begin
  Result := Self;
  FCollection := AValue;
end;

procedure TProviderMongoDBCloud.LoadFromJSON(const AJSON: string);
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
    URLEndpoint(LJO.GetValue<string>('url_endpoint', FHTTP.URL));
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));
    DataSource(LJO.GetValue<string>('data_source', FDataSource));
    DataBase(LJO.GetValue<string>('data_base', FDataBase));
    Collection(LJO.GetValue<string>('collection', FCollection));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderMongoDBCloud.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url_endpoint', TJSONString.Create(FHTTP.URL));
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));
    LJO.AddPair('data_source', TJSONString.Create(FDataSource));
    LJO.AddPair('data_base', TJSONString.Create(FDataBase));
    LJO.AddPair('collection', TJSONString.Create(FCollection));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderMongoDBCloud.Save(const ACache: TArray<TLoggerItem>);
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

    LJO := TJSONObject.Create;
    try
      LJO.AddPair('dataSource', TJSONString.Create(FDataSource));
      LJO.AddPair('database', TJSONString.Create(FDataBase));
      LJO.AddPair('collection', TJSONString.Create(FCollection));
      LJO.AddPair('document', SerializeItem.LogItem(LItem).ToJSONObject);

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := FHTTP.URL + '/action/insertOne';

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure TProviderMongoDBCloud.HTTPExecuteFinally(const ALogItem: TLoggerItem; const AMethod: TRESTMethod; const AContent: string; const AStatusCode: Integer);
begin
  if (AStatusCode <> 201) then
    Exit;

  AddLastMessageId(ALogItem.Id);
end;

procedure TProviderMongoDBCloud.UndoLast;
var
  LLastMessageID: string;
  LJO: TJSONObject;
  LLog: string;
  LLogItemREST: TLogItemREST;
  LItemREST: TArray<TLogItemREST>;
begin
  LLastMessageID := GetLastMessageId;
  if LLastMessageID.Trim.IsEmpty then
    Exit;

  LJO := TJSONObject.Create;
  try
    LJO.AddPair('dataSource', TJSONString.Create(FDataSource));
    LJO.AddPair('database', TJSONString.Create(FDataBase));
    LJO.AddPair('collection', TJSONString.Create(FCollection));
    LJO.AddPair('filter', TJSONObject.Create.AddPair('log_id', TJSONString.Create(LLastMessageID)));

    LLog := LJO.ToString;
  finally
    LJO.Free;
  end;

  LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
  LLogItemREST.LogItem := Default (TLoggerItem);
  LLogItemREST.URL := FHTTP.URL + '/action/deleteOne';

  LItemREST := Concat(LItemREST, [LLogItemREST]);

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderMongoDBCloud);

end.
