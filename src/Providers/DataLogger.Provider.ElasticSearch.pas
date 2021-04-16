{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.ElasticSearch;

interface

uses
  DataLogger.Provider.REST.Indy, DataLogger.Types,
  System.SysUtils;

type
  TProviderElasticSearch = class(TProviderRESTIndy)
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AElasticHOST: string = 'http://localhost'; const AElasticPORT: Integer = 9200; const AElasticIndex: string = 'logger'); reintroduce;
  end;

implementation

{ TProviderElasticSearch }

constructor TProviderElasticSearch.Create(const AElasticHOST: string = 'http://localhost'; const AElasticPORT: Integer = 9200; const AElasticIndex: string = 'logger');
var
  LURL: string;
begin
  LURL := Format('%s:%d/%s/_doc', [AElasticHOST, AElasticPORT, AElasticIndex.ToLower]);
  inherited Create(LURL, 'application/json', '');
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
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    LLogItemREST.Stream := TLoggerLogFormat.AsStreamJsonObject(GetLogFormat, LItem);
    LLogItemREST.LogItem := LItem;

    LItemREST := Concat(LItemREST, [LLogItemREST]);;
  end;

  InternalSaveAsync(TLoggerMethod.tlmPost, LItemREST);
end;

end.
