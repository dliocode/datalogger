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
    FPort: Integer;
    FIndex: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderLogstach; overload;
    function URL: string; overload;
    function Port(const AValue: Integer): TProviderLogstach;
    function Index(const AValue: string): TProviderLogstach;

    constructor Create; overload;
    constructor Create(const AHost: string; const APort: Integer = 5044; const AIndex: string = 'logger'); overload; deprecated 'Use TProviderLogstach.Create.Host(''http://localhost'').Port(5044).Index(''logger'') - This function will be removed in future versions';
  end;

implementation

{ TProviderLogstach }

constructor TProviderLogstach.Create;
begin
  inherited Create;

  URL('http://localhost');
  ContentType('application/json');
  Port(5044);
  Index('logger');
end;

constructor TProviderLogstach.Create(const AHost: string; const APort: Integer = 5044; const AIndex: string = 'logger');
begin
  Create;

  URL(AHost);
  Port(APort);
  Index(AIndex);
end;

function TProviderLogstach.URL(const AValue: string): TProviderLogstach;
begin
  Result := Self;
  inherited URL(AValue);
end;

function TProviderLogstach.URL: string;
begin
  Result := inherited URL;
end;

function TProviderLogstach.Port(const AValue: Integer): TProviderLogstach;
begin
  Result := Self;
  FPort := AValue;
end;

function TProviderLogstach.Index(const AValue: string): TProviderLogstach;
begin
  Result := Self;
  FIndex := AValue;
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
    LLogItemREST.URL := Format('%s:%d/%s/doc', [URL, FPort, FIndex.ToLower]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;
  InternalSave(TLoggerMethod.tlmPost, LItemREST);
end;

end.
