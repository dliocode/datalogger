{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://axiom.co/
// https://axiom.co/docs/usage/ingest#ingest-api

unit DataLogger.Provider.Axiom;

interface

uses
{$IF DEFINED(DATALOGGER_AXIOM_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_AXIOM_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_AXIOM_USE_INDY)}
  TProviderAxiom = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_AXIOM_USE_NETHTTPCLIENT)}
  TProviderAxiom = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderAxiom = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FDatasets: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function BearerToken(const AValue: string): TProviderAxiom;
    function Datasets(const AValue: string): TProviderAxiom;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderAxiom }

constructor TProviderAxiom.Create;
begin
  inherited Create;

  URL('');
  ContentType('application/json');
  Datasets('');
end;

function TProviderAxiom.Datasets(const AValue: string): TProviderAxiom;
begin
  Result := Self;
  FDatasets := AValue;
end;

function TProviderAxiom.BearerToken(const AValue: string): TProviderAxiom;
begin
  Result := Self;
  inherited BearerToken(AValue);
end;

procedure TProviderAxiom.LoadFromJSON(const AJSON: string);
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
    BearerToken(LJO.GetValue<string>('token', inherited Token));
    Datasets(LJO.GetValue<string>('datasets', FDatasets));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderAxiom.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('token', inherited Token);
    LJO.AddPair('datasets', FDatasets);

    ToJSONInternal(LJO);

    if AFormat then
      Result := LJO.Format
    else
      Result := LJO.ToString;
  finally
    LJO.Free;
  end;
end;

procedure TProviderAxiom.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJA: TJSONArray;
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

    LJA := TJSONArray.Create;
    try
      LJO := TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True);
      LJO.AddPair('_time', TJSONString.Create(DateToISO8601(LItem.TimeStamp, False)));

      LJA.Add(LJO);

      LLogItemREST.Stream := TStringStream.Create(LJA.ToString, TEncoding.UTF8);
      LLogItemREST.LogItem := LItem;
      LLogItemREST.URL := Format('https://cloud.axiom.co/api/v1/datasets/%s/ingest', [FDatasets]);
    finally
      LJA.Free;
    end;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderAxiom);

end.
