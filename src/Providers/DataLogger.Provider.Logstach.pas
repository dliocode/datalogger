{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Logstach;

interface

uses
{$IF DEFINED(DATALOGGER_LOGSTACH_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_LOGSTACH_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils;

type
{$IF DEFINED(DATALOGGER_LOGSTACH_USE_INDY)}
  TProviderLogstach = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_LOGSTACH_USE_NETHTTPCLIENT)}
  TProviderLogstach = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderLogstach = class(TProviderRESTHTTPClient)
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

    constructor Create(const AHost: string = 'http://localhost'; const APort: Integer = 5044; const AIndex: string = 'logger'); reintroduce;
  end;

implementation

{ TProviderLogstach }

constructor TProviderLogstach.Create(const AHost: string = 'http://localhost'; const APort: Integer = 5044; const AIndex: string = 'logger');
begin
  FHost := AHost;
  FPort := APort;
  FIndex := AIndex;

  inherited Create('', 'application/json');
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
    if LItem.&Type = TLoggerType.All then
      Continue;

    LLogItemREST.Stream := TLoggerLogFormat.AsStreamJsonObject(FLogFormat, LItem);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s:%d/%s/doc', [FHost, FPort, FIndex.ToLower]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;
  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
