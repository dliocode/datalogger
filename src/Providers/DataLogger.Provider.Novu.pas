{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2023 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

// https://novu.co/
// https://docs.novu.co/overview/introduction

unit DataLogger.Provider.Novu;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_NOVU_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_NOVU_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderNovu = class(TDataLoggerProvider<TProviderNovu>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_NOVU_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_NOVU_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
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

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderMailjetApi }

constructor TProviderNovu.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');

  TemplateID('');
  SubscriberID('');
  EmailTo([]);
  Subject('DataLogger');
end;

procedure TProviderNovu.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderNovu.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderNovu.ApiKey(const AValue: string): TProviderNovu;
begin
  Result := Self;

  FApiKey := AValue;
  FHTTP.Authorization('ApiKey ' + AValue);
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
    LJO.AddPair('api_key', TJSONString.Create(FApiKey));
    LJO.AddPair('template_id', TJSONString.Create(FTemplateID));
    LJO.AddPair('subscriber_id', TJSONString.Create(FSubscriberID));
    LJO.AddPair('email_to', TJSONString.Create(String.Join(',', FEmailTo)));
    LJO.AddPair('subject', TJSONString.Create(FSubject));

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
  LLog: string;
  LLogItemREST: TLogItemREST;
  I: Integer;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLast then
      Continue;

    LJOLog := SerializeItem.LogItem(LItem).ToJSONObject;
    try
      for I := Low(FEmailTo) to High(FEmailTo) do
      begin
        LJO := TJSONObject.Create;
        try
          LJO.AddPair('name', TJSONString.Create(FTemplateID));

          LJOTo := TJSONObject.Create;
          LJOTo.AddPair('subscriberId', TJSONString.Create(FSubscriberID));
          LJOTo.AddPair('email', TJSONString.Create(FEmailTo[I]));
          LJO.AddPair('to', LJOTo);

          LJOLog.RemovePair('subject').Free;
          LJOLog.AddPair('subject', TJSONString.Create(FSubject));
          LJO.AddPair('payload', LJOLog.Clone as TJSONObject);

          LLog := LJO.ToString;
        finally
          LJO.Free;
        end;

        LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
        LLogItemREST.LogItem := LItem;
        LLogItemREST.URL := 'https://api.novu.co/v1/events/trigger';

        LItemREST := Concat(LItemREST, [LLogItemREST]);
      end;
    finally
      LJOLog.Free;
    end;
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveSync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderNovu);

end.
