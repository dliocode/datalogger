{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://betterstack.com/logtail
// https://docs.logtail.com/integrations/rest-api

unit DataLogger.Provider.Logtail;

interface

uses
{$IF DEFINED(DATALOGGER_LOGTAIL_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_LOGTAIL_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_LOGTAIL_USE_INDY)}
  TProviderLogtail = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_LOGTAIL_USE_NETHTTPCLIENT)}
  TProviderLogtail = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderLogtail = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function SourceToken(const AValue: string): TProviderLogtail;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderLogtail }

constructor TProviderLogtail.Create;
begin
  inherited Create;

  URL('https://in.logtail.com');
  ContentType('application/json');
end;

function TProviderLogtail.SourceToken(const AValue: string): TProviderLogtail;
begin
  Result := Self;
  inherited BearerToken(AValue);
end;

procedure TProviderLogtail.LoadFromJSON(const AJSON: string);
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
    BearerToken(LJO.GetValue<string>('source_token', inherited Token));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderLogtail.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('source_token', inherited Token);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderLogtail.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LJO := TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True);
    try
      if LItem.Message.Trim.IsEmpty then
        LJO.AddPair('message', LItem.MessageJSON)
      else
        LJO.AddPair('message', LItem.Message);

      LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := 'https://in.logtail.com';
    finally
      LJO.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderLogtail);

end.
