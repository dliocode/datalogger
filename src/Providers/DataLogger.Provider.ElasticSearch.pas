{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.ElasticSearch;

interface

uses
{$IF DEFINED(DATALOGGER_ELASTICSEARCH_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_ELASTICSEARCH_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils;

type
{$IF DEFINED(DATALOGGER_ELASTICSEARCH_USE_INDY)}
  TProviderElasticSearch = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_ELASTICSEARCH_USE_NETHTTPCLIENT)}
  TProviderElasticSearch = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderElasticSearch = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FPort: Integer;
    FIndex: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderElasticSearch;
    function Port(const AValue: Integer): TProviderElasticSearch;
    function Index(const AValue: string): TProviderElasticSearch;

    constructor Create;
  end;

implementation

{ TProviderElasticSearch }

constructor TProviderElasticSearch.Create;
begin
  inherited Create;

  URL('http://localhost');
  ContentType('application/json');
  Port(9200);
  Index('logger');
end;

function TProviderElasticSearch.URL(const AValue: string): TProviderElasticSearch;
begin
  Result := Self;
  inherited URL(AVAlue);
end;

function TProviderElasticSearch.Port(const AValue: Integer): TProviderElasticSearch;
begin
  Result := Self;
  FPort := AValue;
end;

function TProviderElasticSearch.Index(const AValue: string): TProviderElasticSearch;
begin
  Result := Self;
  FIndex := AValue;
end;

procedure TProviderElasticSearch.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.&Type = TLoggerType.All then
      Continue;

    LLogItemREST.Stream := TLoggerLogFormat.AsStreamJsonObject(FLogFormat, LItem);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s:%d/%s/_doc', [inherited URL, FPort, FIndex.ToLower]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);;
  end;

  InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

end.
