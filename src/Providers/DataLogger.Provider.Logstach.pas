{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Logstach;

interface

uses
  DataLogger.Provider.REST.HTTPClient, DataLogger.Types,
  System.SysUtils;

type
  TProviderLogstach = class(TProviderRESTHTTPClient)
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const ALogstachHOST: string = 'http://localhost'; const ALogstachPORT: Integer = 5044; const ALogstachIndex: string = 'logger'); reintroduce;
  end;

implementation

{ TProviderLogstach }

constructor TProviderLogstach.Create(const ALogstachHOST: string = 'http://localhost'; const ALogstachPORT: Integer = 5044; const ALogstachIndex: string = 'logger');
var
  LURL: string;
begin
  LURL := Format('%s:%d/%s/doc', [ALogstachHOST, ALogstachPORT, ALogstachIndex.ToLower]);
  inherited Create(LURL, 'application/json', '');
end;

procedure TProviderLogstach.Save(const ACache: TArray<TLoggerItem>);
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

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
