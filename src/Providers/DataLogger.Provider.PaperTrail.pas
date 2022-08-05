{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://papertrailapp.com
// https://papertrailapp.com/account/destinations
// https://www.papertrail.com/help/log-destinations

unit DataLogger.Provider.PaperTrail;

interface

uses
{$IF DEFINED(DATALOGGER_PAPERTRAIL_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_PAPERTRAIL_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON;

type
{$IF DEFINED(DATALOGGER_PAPERTRAIL_USE_INDY)}
  TProviderPaperTrail = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_PAPERTRAIL_USE_NETHTTPCLIENT)}
  TProviderPaperTrail = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderPaperTrail = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FToken: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Token(const AValue: string): TProviderPaperTrail;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderPaperTrail }

constructor TProviderPaperTrail.Create;
begin
  inherited Create;

  URL('https://logs.collector.solarwinds.com/v1/log');
  ContentType('application/json');
end;

function TProviderPaperTrail.Token(const AValue: string): TProviderPaperTrail;
begin
  Result := Self;

  FToken := AValue;
  inherited BasicAuth('', AValue);
end;

procedure TProviderPaperTrail.LoadFromJSON(const AJSON: string);
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
    Token(LJO.GetValue<string>('token', FToken));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderPaperTrail.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('token', FToken);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderPaperTrail.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsJsonObjectToString(FLogFormat, LItem, True);

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := 'https://logs.collector.solarwinds.com/v1/log';

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderPaperTrail);

end.
