{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

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

// https://sendgrid.com/

unit DataLogger.Provider.SendGrid.WebApi;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSendGridWebApi = class(TDataLoggerProvider<TProviderSendGridWebApi>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_SENDGRID_WEBAPI_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FEmailFrom: string;
    FEmailTo: TArray<string>;
    FSubject: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ApiKey(const AValue: string): TProviderSendGridWebApi;
    function EmailFrom(const AValue: string): TProviderSendGridWebApi;
    function EmailTo(const AValue: TArray<string>): TProviderSendGridWebApi;
    function Subject(const AValue: string): TProviderSendGridWebApi;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSendGridWebApi }

constructor TProviderSendGridWebApi.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://api.sendgrid.com/v3/mail/send');

  EmailFrom('');
  EmailTo([]);
  Subject('DataLogger');
end;

destructor TProviderSendGridWebApi.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderSendGridWebApi.ApiKey(const AValue: string): TProviderSendGridWebApi;
begin
  Result := Self;
  FHTTP.BearerToken(AValue);
end;

function TProviderSendGridWebApi.EmailFrom(const AValue: string): TProviderSendGridWebApi;
begin
  Result := Self;
  FEmailFrom := AValue.Trim;
end;

function TProviderSendGridWebApi.EmailTo(const AValue: TArray<string>): TProviderSendGridWebApi;
begin
  Result := Self;
  FEmailTo := AValue;
end;

function TProviderSendGridWebApi.Subject(const AValue: string): TProviderSendGridWebApi;
begin
  Result := Self;
  FSubject := AValue;
end;

procedure TProviderSendGridWebApi.LoadFromJSON(const AJSON: string);
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
    ApiKey(LJO.GetValue<string>('api_key', FHTTP.Authorization));
    EmailFrom(LJO.GetValue<string>('email_from', FEmailFrom));
    EmailTo(LJO.GetValue<string>('email_to', String.Join(',', FEmailTo)).Split([',']));
    Subject(LJO.GetValue<string>('subject', FSubject));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSendGridWebApi.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('api_key', TJSONString.Create(FHTTP.Authorization));
    LJO.AddPair('email_from', TJSONString.Create(FEmailFrom));
    LJO.AddPair('email_to', TJSONString.Create(String.Join(',', FEmailTo)));
    LJO.AddPair('subject', TJSONString.Create(FSubject));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSendGridWebApi.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LJO: TJSONObject;
  LJAPersonalizations: TJSONArray;
  I: Integer;
  LJATo: TJSONArray;
  LJAContent: TJSONArray;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    LJO := TJSONObject.Create;
    try
      LJAPersonalizations := TJSONArray.Create;

      for I := Low(FEmailTo) to High(FEmailTo) do
      begin
        LJATo := TJSONArray.Create;
        LJATo.Add(TJSONObject.Create.AddPair('email', TJSONString.Create(FEmailTo[I].Trim)));

        LJAPersonalizations.Add(TJSONObject.Create.AddPair('to', LJATo));
      end;

      LJO.AddPair('personalizations', LJAPersonalizations);
      LJO.AddPair('from', TJSONObject.Create.AddPair('email', TJSONString.Create(FEmailFrom)));
      LJO.AddPair('subject', TJSONString.Create(FSubject));

      LJAContent := TJSONArray.Create;
      LJAContent.Add(
        TJSONObject.Create
        .AddPair('type', TJSONString.Create('text/plain'))
        .AddPair('value', TJSONString.Create(LLog))
        );

      LJO.AddPair('content', LJAContent);

{$IF CompilerVersion > 32} // 32 = Delphi Tokyo (10.2)
      LLog := LJO.ToString;
{$ELSE}
      LLog := LJO.ToJSON;
{$ENDIF}
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := FHTTP.URL;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSendGridWebApi);

end.
