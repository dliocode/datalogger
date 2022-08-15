{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://novu.co/
// https://docs.novu.co/overview/introduction

unit DataLogger.Provider.Novu;

interface

uses
{$IF DEFINED(DATALOGGER_NOVU_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_NOVU_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  DataLogger.Types,
  System.SysUtils, System.Classes, System.JSON, System.DateUtils;

type
{$IF DEFINED(DATALOGGER_NOVU_USE_INDY)}
  TProviderNovu = class(TProviderRESTIndy)
{$ELSEIF DEFINED(DATALOGGER_NOVU_USE_NETHTTPCLIENT)}
  TProviderNovu = class(TProviderRESTNetHTTPClient)
{$ELSE}
  TProviderNovu = class(TProviderRESTHTTPClient)
{$ENDIF}
  private
    FApiKey: string;
    FTemplateID: string;
    FSubscriberID: string;
    FEmailTo: TArray<string>;
    FSubject: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderNovu;
    function TemplateID(const AValue: string): TProviderNovu;
    function SubscriberID(const AValue: string): TProviderNovu;
    function EmailTo(const AValue: TArray<string>): TProviderNovu;
    function Subject(const AValue: string): TProviderNovu;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderMailjetApi }

constructor TProviderNovu.Create;
begin
  inherited Create;

  ContentType('application/json');

  TemplateID('');
  SubscriberID('');
  EmailTo([]);
  Subject('DataLogger');
end;

function TProviderNovu.ApiKey(const AValue: string): TProviderNovu;
begin
  Result := Self;
  FApiKey := AValue;
  inherited Token('ApiKey ' + AValue);
end;

function TProviderNovu.TemplateID(const AValue: string): TProviderNovu;
begin
  Result := Self;
  FTemplateID := AValue;
end;

function TProviderNovu.SubscriberID(const AValue: string): TProviderNovu;
begin
  Result := Self;
  FSubscriberID := AValue;
end;

function TProviderNovu.EmailTo(const AValue: TArray<string>): TProviderNovu;
begin
  Result := Self;
  FEmailTo := AValue;
end;

function TProviderNovu.Subject(const AValue: string): TProviderNovu;
begin
  Result := Self;
  FSubject := AValue;
end;

procedure TProviderNovu.LoadFromJSON(const AJSON: string);
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
    ApiKey(LJO.GetValue<string>('api_key', FApiKey));
    TemplateID(LJO.GetValue<string>('template_id', FTemplateID));
    SubscriberID(LJO.GetValue<string>('subscriber_id', FSubscriberID));
    EmailTo(LJO.GetValue<string>('email_to', String.Join(',', FEmailTo)).Split([',']));
    Subject(LJO.GetValue<string>('subject', FSubject));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderNovu.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', FApiKey);
    LJO.AddPair('template_id', FTemplateID);
    LJO.AddPair('subscriber_id', FSubscriberID);
    LJO.AddPair('email_to', String.Join(',', FEmailTo));
    LJO.AddPair('subject', FSubject);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderNovu.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJOLog: TJSONObject;
  LJO: TJSONObject;
  LJOTo: TJSONObject;
  LLogItemREST: TLogItemREST;
  I: Integer;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LJOLog := TLoggerLogFormat.AsJsonObject(FLogFormat, LItem, True);
    try
      for I := Low(FEmailTo) to High(FEmailTo) do
      begin
        LJO := TJSONObject.Create;
        try
          LJO.AddPair('name', FTemplateID);

          LJOTo := TJSONObject.Create;
          LJOTo.AddPair('subscriberId', FSubscriberID);
          LJOTo.AddPair('email', FEmailTo[I]);
          LJO.AddPair('to', LJOTo);

          LJOLog.RemovePair('subject').Free;
          LJOLog.AddPair('subject', FSubject);
          LJO.AddPair('payload', LJOLog.Clone as TJSONObject);

          LLogItemREST.Stream := TStringStream.Create(LJO.ToString, TEncoding.UTF8);
          LLogItemREST.LogItem := LItem;
          LLogItemREST.URL := 'https://api.novu.co/v1/events/trigger';
        finally
          LJO.Free;
        end;

        LItemREST := Concat(LItemREST, [LLogItemREST]);
      end;
    finally
      LJOLog.Free;
    end;
  end;

  InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderNovu);

end.
