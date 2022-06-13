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
    function URL(const AValue: string): TProviderLogstach;
    function Port(const AValue: Integer): TProviderLogstach;
    function Index(const AValue: string): TProviderLogstach;

    constructor Create; overload;
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

function TProviderLogstach.URL(const AValue: string): TProviderLogstach;
begin
  Result := Self;
  inherited URL(AValue);
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
    LLogItemREST.URL := Format('%s:%d/%s/doc', [inherited URL, FPort, FIndex.ToLower]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;
  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

end.
