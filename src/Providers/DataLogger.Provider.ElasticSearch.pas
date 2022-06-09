{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.ElasticSearch;

interface

uses
{$IF DEFINED(DATALOGGER_TELEGRAM_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_TELEGRAM_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils;

type
{$IF DEFINED(DATALOGGER_TELEGRAM_USE_INDY)}
  TProviderElasticSearch = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_TELEGRAM_USE_NETHTTPCLIENT)}
  TProviderElasticSearch = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderElasticSearch = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FHost: string;
    FPort: Integer;
    FIndex: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property &Index: string read FIndex write FIndex;

    constructor Create(const AHost: string = 'http://localhost'; const APort: Integer = 9200; const AIndex: string = 'logger'); reintroduce;
  end;

implementation

{ TProviderElasticSearch }

constructor TProviderElasticSearch.Create(const AHost: string = 'http://localhost'; const APort: Integer = 9200; const AIndex: string = 'logger');
begin
  FHost := AHost;
  FPort := APort;
  FIndex := AIndex;

  inherited Create('', 'application/json', '');
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
    LLogItemREST.URL := Format('%s:%d/%s/_doc', [FHost, FPort, FIndex.ToLower]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);;
  end;

  InternalSaveAsync(TLoggerMethod.tlmPost, LItemREST);
end;

end.
