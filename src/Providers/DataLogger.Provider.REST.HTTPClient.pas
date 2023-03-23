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

unit DataLogger.Provider.REST.HTTPClient;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  System.SysUtils, System.Classes, System.Threading, System.Net.HTTPClient, System.Net.URLClient, System.NetConsts, System.JSON, System.TypInfo, System.NetEncoding, System.Net.Mime;

type
  TLoggerJSON = DataLogger.Provider.TLoggerJSON;
  THTTPClient = System.Net.HTTPClient.THTTPClient;

  TLogHeader = record
    Key: string;
    Value: string;

    constructor Create(const AKey: string; const AValue: string);
  end;

  TLogFormData = record
    Field: string;
    Value: string;
    ContentType: string;

    constructor Create(const AField: string; const AValue: string; const AContentType: string = '');
  end;

  TLogItemREST = record
    Stream: TStream;
    LogItem: TLoggerItem;
    URL: string;
    Header: TArray<TLogHeader>;
    FormData: TArray<TLogFormData>;
  end;

  TLogItemResponse = record
    LogItem: TLoggerItem;
    Content: string;
  end;

  TExecuteFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string);
  TContentValidation = reference to function(const AContent: string; var AMessageException: string): Boolean;
  TRESTMethod = (tlmGet, tlmPost);

  TProviderRESTHTTPClient = class(TDataLoggerProvider<TProviderRESTHTTPClient>)
  private
    FURL: string;
    FContentType: string;
    FAuthorization: string;
    FMethod: TRESTMethod;
    FHeader: TArray<TLogHeader>;
    FModeAsync: Boolean;
    FWaitTimeoutToSend: Integer;
    FContentValidation: TContentValidation;
    FExecuteFinally: TExecuteFinally;
    procedure HTTP(const AMethod: TRESTMethod; const AItemREST: TLogItemREST);
  protected
    function URL: string; overload;
    function Authorization: string; overload;
    function ContentType: string; overload;
    function Header(const AKey: string): string;
    function Method: TRESTMethod; overload;
    function MethodString: string;

    procedure InternalSaveSync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
    procedure InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string; const AWithProtocol: Boolean = True): TProviderRESTHTTPClient; overload;
    function ContentType(const AValue: string): TProviderRESTHTTPClient; overload;
    function Authorization(const AValue: string): TProviderRESTHTTPClient; overload;
    function BearerToken(const AValue: string): TProviderRESTHTTPClient;
    function BasicAuth(const AUsername: string; const APassword: string): TProviderRESTHTTPClient;
    function Method(const AValue: TRESTMethod): TProviderRESTHTTPClient; overload;
    function AddHeader(const AKey: string; const AValue: string): TProviderRESTHTTPClient;
    function ModeAsync(const AValue: Boolean): TProviderRESTHTTPClient;
    function WaitTimeoutToSend(const AValue: Integer): TProviderRESTHTTPClient;
    function ContentValidation(const AContentValidation: TContentValidation): TProviderRESTHTTPClient;
    function ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTHTTPClient;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderRESTHTTPClient }

constructor TProviderRESTHTTPClient.Create;
begin
  inherited Create;

  URL('');
  ContentType('text/plain');
  Authorization('');
  Method(tlmPost);
  ModeAsync(True);
  WaitTimeoutToSend(0);
  ContentValidation(nil);
  ExecuteFinally(nil);
end;

function TProviderRESTHTTPClient.URL(const AValue: string; const AWithProtocol: Boolean = True): TProviderRESTHTTPClient;
var
  LProtocol: string;
begin
  Result := Self;

  FURL := AValue;

  if AWithProtocol then
  begin
    LProtocol := 'http://';
    if not AValue.ToLower.StartsWith('http://') and not AValue.ToLower.StartsWith('https://') then
      FURL := LProtocol + AValue;
  end;
end;

function TProviderRESTHTTPClient.URL: string;
begin
  Result := FURL;
end;

function TProviderRESTHTTPClient.ContentType(const AValue: string): TProviderRESTHTTPClient;
begin
  Result := Self;
  FContentType := AValue;
end;

function TProviderRESTHTTPClient.ContentType: string;
begin
  Result := FContentType;
end;

function TProviderRESTHTTPClient.Authorization(const AValue: string): TProviderRESTHTTPClient;
begin
  Result := Self;
  FAuthorization := AValue;
end;

function TProviderRESTHTTPClient.Authorization: string;
begin
  Result := FAuthorization;
end;

function TProviderRESTHTTPClient.BearerToken(const AValue: string): TProviderRESTHTTPClient;
begin
  Result := Self;

  if AValue.Trim.ToLower.Contains('bearer ') then
    Authorization(AValue)
  else
    Authorization('Bearer ' + AValue);
end;

function TProviderRESTHTTPClient.BasicAuth(const AUsername: string; const APassword: string): TProviderRESTHTTPClient;
begin
  Result := Self;
  FAuthorization := 'Basic ' + TNetEncoding.Base64.Encode(Format('%s:%s', [AUsername, APassword]));
end;

function TProviderRESTHTTPClient.Method(const AValue: TRESTMethod): TProviderRESTHTTPClient;
begin
  Result := Self;
  FMethod := AValue;
end;

function TProviderRESTHTTPClient.Method: TRESTMethod;
begin
  Result := FMethod;
end;

function TProviderRESTHTTPClient.MethodString: string;
begin
  Result := 'GET';

  case FMethod of
    tlmGet:
      Result := 'GET';
    tlmPost:
      Result := 'POST';
  end;
end;

function TProviderRESTHTTPClient.AddHeader(const AKey: string; const AValue: string): TProviderRESTHTTPClient;
var
  LIsFound: Boolean;
  LHeader: TLogHeader;
  I: Integer;
begin
  Result := Self;

  LIsFound := False;

  LHeader.Key := AKey;
  LHeader.Value := AValue;

  for I := Low(FHeader) to High(FHeader) do
    if (FHeader[I].Key = AKey) then
    begin
      FHeader[I].Value := AValue;
      LIsFound := True;

      Break;
    end;

  if not LIsFound then
    FHeader := FHeader + [LHeader];
end;

function TProviderRESTHTTPClient.Header(const AKey: string): string;
var
  I: Integer;
begin
  Result := '';

  for I := Low(FHeader) to High(FHeader) do
    if (FHeader[I].Key = AKey) then
    begin
      Result := FHeader[I].Value;
      Break;
    end;
end;

function TProviderRESTHTTPClient.ModeAsync(const AValue: Boolean): TProviderRESTHTTPClient;
begin
  Result := Self;
  FModeAsync := AValue;
end;

function TProviderRESTHTTPClient.WaitTimeoutToSend(const AValue: Integer): TProviderRESTHTTPClient;
begin
  Result := Self;
  FWaitTimeoutToSend := AValue;
end;

function TProviderRESTHTTPClient.ContentValidation(const AContentValidation: TContentValidation): TProviderRESTHTTPClient;
begin
  Result := Self;
  FContentValidation := AContentValidation;
end;

function TProviderRESTHTTPClient.ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTHTTPClient;
begin
  Result := Self;
  FExecuteFinally := AExecuteFinally;
end;

procedure TProviderRESTHTTPClient.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
  LValue: string;
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
    URL(LJO.GetValue<string>('url', FURL));
    ContentType(LJO.GetValue<string>('content_type', FContentType));
    Authorization(LJO.GetValue<string>('token', FAuthorization));

    LValue := GetEnumName(TypeInfo(TRESTMethod), Integer(FMethod));
    Method(TRESTMethod(GetEnumValue(TypeInfo(TRESTMethod), LJO.GetValue<string>('method', LValue))));

    ModeAsync(LJO.GetValue<Boolean>('mode_async', FModeAsync));
    WaitTimeoutToSend(LJO.GetValue<Integer>('wait_timeout_to_send', FWaitTimeoutToSend));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderRESTHTTPClient.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', TJSONString.Create(FURL));
    LJO.AddPair('content_type', TJSONString.Create(FContentType));
    LJO.AddPair('token', TJSONString.Create(FAuthorization));
    LJO.AddPair('method', TJSONString.Create(GetEnumName(TypeInfo(TRESTMethod), Integer(FMethod))));
    LJO.AddPair('mode_async', TJSONBool.Create(FModeAsync));
    LJO.AddPair('wait_timeout_to_send', TJSONNumber.Create(FWaitTimeoutToSend));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderRESTHTTPClient.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    if (Trim(LowerCase(FContentType)) = 'application/json') then
      LLogItemREST.Stream := SerializeItem.LogItem(LItem).ToJSONStream
    else
      LLogItemREST.Stream := SerializeItem.LogItem(LItem).ToStream;

    LLogItemREST.LogItem := LItem;
    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  if FModeAsync then
    InternalSaveAsync(FMethod, LItemREST)
  else
    InternalSaveSync(FMethod, LItemREST);
end;

procedure TProviderRESTHTTPClient.InternalSaveSync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
var
  I: Integer;
begin
  if (Length(ALogItemREST) = 0) then
    Exit;

  for I := Low(ALogItemREST) to High(ALogItemREST) do
  begin
    HTTP(AMethod, ALogItemREST[I]);

    if (FWaitTimeoutToSend > 0) then
      Sleep(FWaitTimeoutToSend);
  end;
end;

procedure TProviderRESTHTTPClient.InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
begin
  if (Length(ALogItemREST) = 0) then
    Exit;

  TParallel.For(Low(ALogItemREST), High(ALogItemREST),
    procedure(Index: Integer)
    begin
      HTTP(AMethod, ALogItemREST[Index]);
    end);
end;

procedure TProviderRESTHTTPClient.HTTP(const AMethod: TRESTMethod; const AItemREST: TLogItemREST);
var
  LRetriesCount: Integer;
  LURL: string;
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
  LResponseContent: string;
  I: Integer;
  LFormData: TMultipartFormData;
  LExceptionMessage: string;
begin
  try
    LURL := AItemREST.URL;

    if LURL.Trim.IsEmpty then
      LURL := FURL;

    if LURL.Trim.IsEmpty then
      raise EDataLoggerException.Create('URL is empty');

    LHTTP := THTTPClient.Create;
  except
    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    Exit
  end;

  try
{$IF RTLVersion > 32} // 32 = Delphi Tokyo (10.2)
    LHTTP.ConnectionTimeout := 60000;
    LHTTP.ResponseTimeout := 60000;
    LHTTP.SendTimeout := 60000;
{$ENDIF}
    LHTTP.HandleRedirects := True;
    LHTTP.UserAgent := 'DataLogger.Provider.REST.HTTPClient';
    LHTTP.ContentType := FContentType;
    LHTTP.AcceptCharSet := 'utf-8';
    LHTTP.AcceptEncoding := 'gzip, deflate, br';
    LHTTP.Accept := '*/*';

    if not FAuthorization.Trim.IsEmpty then
      LHTTP.CustomHeaders['Authorization'] := FAuthorization;

    for I := Low(FHeader) to High(FHeader) do
      LHTTP.CustomHeaders[FHeader[I].Key] := FHeader[I].Value;

    for I := Low(AItemREST.Header) to High(AItemREST.Header) do
      LHTTP.CustomHeaders[AItemREST.Header[I].Key] := AItemREST.Header[I].Value;

    LRetriesCount := 0;

    while True do
      try
        case AMethod of
          tlmGet:
            LResponse := LHTTP.Get(LURL);

          tlmPost:
            begin
              if (Length(AItemREST.FormData) = 0) then
                LResponse := LHTTP.Post(LURL, AItemREST.Stream)
              else
              begin
                LFormData := TMultipartFormData.Create;
                try
                  for I := Low(AItemREST.FormData) to High(AItemREST.FormData) do
                    LFormData.AddField(AItemREST.FormData[I].Field, AItemREST.FormData[I].Value {$IF RTLVersion > 34}, AItemREST.FormData[I].ContentType{$ENDIF});

                  LResponse := LHTTP.Post(LURL, LFormData);
                finally
                  LFormData.Free;
                end;
              end;
            end;
        end;

        try
          LResponseContent := LResponse.ContentAsString(TEncoding.UTF8);
        except
          LResponseContent := LResponse.ContentAsString(TEncoding.ASCII);
        end;

        if not(LResponse.StatusCode in [200, 201, 202, 204]) then
          raise EDataLoggerException.Create(LResponseContent);

        LExceptionMessage := '';
        if Assigned(FContentValidation) then
          if not FContentValidation(LResponseContent, LExceptionMessage) then
            raise EDataLoggerException.Create(LExceptionMessage);

        Break;
      except
        on E: Exception do
        begin
          Inc(LRetriesCount);

          Sleep(50);

          if Assigned(FLogException) then
            FLogException(Self, AItemREST.LogItem, E, LRetriesCount);

          if Self.Terminated then
            Exit;

          if (LRetriesCount <= 0) then
            Break;

          if (LRetriesCount >= FMaxRetries) then
            Break;
        end;
      end;
  finally
    LHTTP.Free;

    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    if Assigned(FExecuteFinally) then
      FExecuteFinally(AItemREST.LogItem, LResponseContent);
  end;
end;

{ TLogHeader }

constructor TLogHeader.Create(const AKey: string; const AValue: string);
begin
  Self.Key := AKey;
  Self.Value := AValue;
end;

{ TLogFormData }

constructor TLogFormData.Create(const AField: string; const AValue: string; const AContentType: string = '');
begin
  Self.Field := AField;
  Self.Value := AValue;
  Self.ContentType := AContentType;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderRESTHTTPClient);

end.
