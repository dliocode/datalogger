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

unit DataLogger.Provider.REST.Indy;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  IdHTTP, IdSSLOpenSSL, IdSSLOpenSSLHeaders, IdMultipartFormData,
  System.SysUtils, System.Classes, System.Threading, System.JSON, System.TypInfo, System.NetEncoding;

type
  TLoggerJSON = DataLogger.Provider.TLoggerJSON;
  TIdHTTP = IdHTTP.TIdHTTP;

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

  TExecuteFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string; const AStatusCode: Integer);
  TContentValidation = reference to function(const AContent: string; var AMessageException: string): Boolean;
  TRESTMethod = (tlmGet, tlmPost, tlmDelete);

  TProviderRESTIndy = class(TDataLoggerProvider<TProviderRESTIndy>)
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
    function URL(const AValue: string; const AWithProtocol: Boolean = True): TProviderRESTIndy; overload;
    function ContentType(const AValue: string): TProviderRESTIndy; overload;
    function Authorization(const AValue: string): TProviderRESTIndy; overload;
    function BearerToken(const AValue: string): TProviderRESTIndy;
    function BasicAuth(const AUsername: string; const APassword: string): TProviderRESTIndy;
    function Method(const AValue: TRESTMethod): TProviderRESTIndy; overload;
    function AddHeader(const AKey: string; const AValue: string): TProviderRESTIndy;
    function ModeAsync(const AValue: Boolean): TProviderRESTIndy;
    function WaitTimeoutToSend(const AValue: Integer): TProviderRESTIndy;
    function ContentValidation(const AContentValidation: TContentValidation): TProviderRESTIndy;
    function ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTIndy;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderRESTIndy }

constructor TProviderRESTIndy.Create;
begin
  inherited Create;

  URL('');
  ContentType('text/plain');
  Authorization('');
  ExecuteFinally(nil);
  ModeAsync(True);
  WaitTimeoutToSend(0);
end;

function TProviderRESTIndy.URL(const AValue: string; const AWithProtocol: Boolean = True): TProviderRESTIndy;
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

function TProviderRESTIndy.URL: string;
begin
  Result := FURL;
end;

function TProviderRESTIndy.ContentType(const AValue: string): TProviderRESTIndy;
begin
  Result := Self;
  FContentType := AValue;
end;

function TProviderRESTIndy.ContentType: string;
begin
  Result := FContentType;
end;

function TProviderRESTIndy.Authorization(const AValue: string): TProviderRESTIndy;
begin
  Result := Self;
  FAuthorization := AValue;
end;

function TProviderRESTIndy.Authorization: string;
begin
  Result := FAuthorization;
end;

function TProviderRESTIndy.BearerToken(const AValue: string): TProviderRESTIndy;
begin
  Result := Self;

  if AValue.Trim.ToLower.Contains('bearer ') then
    Authorization(AValue)
  else
    FAuthorization := 'Bearer ' + AValue;
end;

function TProviderRESTIndy.BasicAuth(const AUsername: string; const APassword: string): TProviderRESTIndy;
begin
  Result := Self;
  FAuthorization := 'Basic ' + TNetEncoding.Base64.Encode(Format('%s:%s', [AUsername, APassword]));
end;

function TProviderRESTIndy.Method(const AValue: TRESTMethod): TProviderRESTIndy;
begin
  Result := Self;
  FMethod := AValue;
end;

function TProviderRESTIndy.AddHeader(const AKey, AValue: string): TProviderRESTIndy;
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

function TProviderRESTIndy.Header(const AKey: string): string;
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

function TProviderRESTIndy.Method: TRESTMethod;
begin
  Result := FMethod;
end;

function TProviderRESTIndy.MethodString: string;
begin
  Result := 'GET';

  case FMethod of
    tlmGet:
      Result := 'GET';
    tlmPost:
      Result := 'POST';
  end;
end;

function TProviderRESTIndy.ModeAsync(const AValue: Boolean): TProviderRESTIndy;
begin
  Result := Self;
  FModeAsync := AValue;
end;

function TProviderRESTIndy.WaitTimeoutToSend(const AValue: Integer): TProviderRESTIndy;
begin
  Result := Self;
  FWaitTimeoutToSend := AValue;
end;

function TProviderRESTIndy.ContentValidation(const AContentValidation: TContentValidation): TProviderRESTIndy;
begin
  Result := Self;
  FContentValidation := AContentValidation;
end;

function TProviderRESTIndy.ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTIndy;
begin
  Result := Self;
  FExecuteFinally := AExecuteFinally;
end;

procedure TProviderRESTIndy.LoadFromJSON(const AJSON: string);
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

function TProviderRESTIndy.ToJSON(const AFormat: Boolean): string;
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

procedure TProviderRESTIndy.Save(const ACache: TArray<TLoggerItem>);
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
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLastLine then
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

procedure TProviderRESTIndy.InternalSaveSync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
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

procedure TProviderRESTIndy.InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
begin
  if (Length(ALogItemREST) = 0) then
    Exit;

  TParallel.For(Low(ALogItemREST), High(ALogItemREST),
    procedure(Index: Integer)
    begin
      HTTP(AMethod, ALogItemREST[Index]);
    end);
end;

procedure TProviderRESTIndy.HTTP(const AMethod: TRESTMethod; const AItemREST: TLogItemREST);
var
  LRetriesCount: Integer;
  LURL: string;
  LHTTP: TIdHTTP;
  LSSL: TIdSSLIOHandlerSocketOpenSSL;
  I: Integer;
  LResponseContent: string;
  LFormData: TIdMultiPartFormDataStream;
  LExceptionMessage: string;
begin
  try
    LURL := AItemREST.URL;
    if LURL.Trim.IsEmpty then
      LURL := FURL;

    if LURL.Trim.IsEmpty then
      raise EDataLoggerException.Create('URL is empty');

    LHTTP := TIdHTTP.Create(nil);
    LSSL := nil;
  except
    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    Exit
  end;

  try
    LHTTP.ConnectTimeout := 60000;
    LHTTP.ReadTimeout := 60000;

    LHTTP.HandleRedirects := True;
    LHTTP.Request.UserAgent := 'DataLogger.Provider.REST.Indy';
    LHTTP.Request.ContentType := FContentType;
    LHTTP.Request.AcceptCharSet := 'utf-8';
    LHTTP.Request.AcceptEncoding := 'gzip, deflate, br';
    LHTTP.Request.Accept := '*/*';
    LHTTP.Request.Connection := 'Keep-Alive';

    if not FAuthorization.Trim.IsEmpty then
      LHTTP.Request.CustomHeaders.AddValue('Authorization', FAuthorization);

    for I := Low(FHeader) to High(FHeader) do
      LHTTP.Request.CustomHeaders.Values[FHeader[I].Key] := FHeader[I].Value;

    for I := Low(AItemREST.Header) to High(AItemREST.Header) do
      LHTTP.Request.CustomHeaders.Values[AItemREST.Header[I].Key] := AItemREST.Header[I].Value;

    LRetriesCount := 0;

    while True do
      try
        if LURL.ToLower.Contains('https://') then
        begin
          if not LoadOpenSSLLibrary then
            raise EDataLoggerException.Create(Self.ClassName + ' > ' + WhichFailedToLoad);

          LSSL := TIdSSLIOHandlerSocketOpenSSL.Create(LHTTP);
          LSSL.SSLOptions.Method := sslvTLSv1_2;
        end;

        LHTTP.IOHandler := LSSL;

        case AMethod of
          tlmGet:
            LHTTP.Get(LURL);

          tlmPost:
            begin
              if (Length(AItemREST.FormData) = 0) then
                LHTTP.Post(LURL, AItemREST.Stream)
              else
              begin
                LFormData := TIdMultiPartFormDataStream.Create;
                try
                  for I := Low(AItemREST.FormData) to High(AItemREST.FormData) do
                    LFormData.AddFormField(AItemREST.FormData[I].Field, AItemREST.FormData[I].Value, AItemREST.FormData[I].ContentType);

                  LHTTP.Post(LURL, LFormData);
                finally
                  LFormData.Free;
                end;
              end;
            end;

          tlmDelete:
            LHTTP.Delete(LURL);
        else
          raise EDataLoggerException.CreateFmt('%s > Invalid method!', [Self.ClassName]);
        end;

        LResponseContent := LHTTP.Response.ResponseText;

        if not(LHTTP.Response.ResponseCode in [200, 201, 202, 204]) then
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
    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    if Assigned(FExecuteFinally) then
      FExecuteFinally(AItemREST.LogItem, LResponseContent, LHTTP.Response.ResponseCode);

    LHTTP.Free;
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

ForceReferenceToClass(TProviderRESTIndy);

end.
