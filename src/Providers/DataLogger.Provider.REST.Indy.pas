{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
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
  end;

  TLogFormData = record
    Field: string;
    Value: string;
    ContentType: string;

    class function Create(const AField: string; const AValue: string; const AContentType: string = ''): TLogFormData; static;
  end;

  TLogItemREST = record
    Stream: TStream;
    LogItem: TLoggerItem;
    URL: string;
    Header: TArray<TLogHeader>;
    FormData: TArray<TLogFormData>;
  end;

  TExecuteFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string);
  TRESTMethod = (tlmGet, tlmPost);

  TProviderRESTIndy = class(TDataLoggerProvider)
  private
    FURL: string;
    FContentType: string;
    FToken: string;
    FMethod: TRESTMethod;
    FHeader: TArray<TLogHeader>;
    FExecuteFinally: TExecuteFinally;
    procedure HTTP(const AMethod: TRESTMethod; const AItemREST: TLogItemREST);
  protected
    procedure InternalSave(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>; const ASleep: Integer = 0);
    procedure InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderRESTIndy; overload;
    function URL: string; overload;
    function ContentType(const AValue: string): TProviderRESTIndy;
    function Token(const AValue: string): TProviderRESTIndy; overload;
    function Token: string; overload;
    function BearerToken(const AValue: string): TProviderRESTIndy;
    function BasicAuth(const AUsername: string; const APassword: string): TProviderRESTIndy;
    function Method(const AValue: TRESTMethod): TProviderRESTIndy;
    function AddHeader(const AKey: string; const AValue: string): TProviderRESTIndy;
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
  Token('');
  ExecuteFinally(nil);
end;

function TProviderRESTIndy.URL(const AValue: string): TProviderRESTIndy;
var
  LProtocol: string;
begin
  Result := Self;

  LProtocol := 'http://';

  FURL := AValue;
  if not AValue.ToLower.StartsWith('http://') and not AValue.ToLower.StartsWith('https://') then
    FURL := LProtocol + AValue;
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

function TProviderRESTIndy.Token(const AValue: string): TProviderRESTIndy;
begin
  Result := Self;
  FToken := AValue;
end;

function TProviderRESTIndy.Token: string;
begin
  Result := FToken;
end;

function TProviderRESTIndy.BearerToken(const AValue: string): TProviderRESTIndy;
begin
  Result := Self;

  if AValue.Trim.ToLower.Contains('bearer ') then
    Token(AValue)
  else
    FToken := 'Bearer ' + AValue;
end;

function TProviderRESTIndy.BasicAuth(const AUsername: string; const APassword: string): TProviderRESTIndy;
begin
  Result := Self;

  FToken := 'Basic ' + TNetencoding.Base64.Encode(Format('%s:%s', [AUsername, APassword]));
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
    if FHeader[I].Key = AKey then
    begin
      FHeader[I].Value := AValue;
      LIsFound := True;
      Break;
    end;

  if not LIsFound then
    FHeader := FHeader + [LHeader];
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
    Token(LJO.GetValue<string>('token', FToken));

    LValue := GetEnumName(TypeInfo(TRESTMethod), Integer(FMethod));
    Method(TRESTMethod(GetEnumValue(TypeInfo(TRESTMethod), LJO.GetValue<string>('method', LValue))));

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
    LJO.AddPair('url', FURL);
    LJO.AddPair('content_type', FContentType);
    LJO.AddPair('token', FToken);
    LJO.AddPair('method', GetEnumName(TypeInfo(TRESTMethod), Integer(FMethod)));

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
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    if Trim(LowerCase(FContentType)) = 'application/json' then
      LLogItemREST.Stream := TLoggerLogFormat.AsStreamJsonObject(FLogFormat, LItem)
    else
      LLogItemREST.Stream := TLoggerLogFormat.AsStream(FLogFormat, LItem, FFormatTimestamp);

    LLogItemREST.LogItem := LItem;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSaveAsync(FMethod, LItemREST);
end;

procedure TProviderRESTIndy.InternalSave(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>; const ASleep: Integer = 0);
var
  I: Integer;
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  for I := Low(ALogItemREST) to High(ALogItemREST) do
  begin
    HTTP(AMethod, ALogItemREST[I]);
    Sleep(ASleep);
  end;
end;

procedure TProviderRESTIndy.InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
begin
  if Length(ALogItemREST) = 0 then
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
  LResponseContent: string;
  I: Integer;
  LFormData: TIdMultiPartFormDataStream;
begin
  if Self.Terminated then
  begin
    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    Exit;
  end;

  LURL := AItemREST.URL;
  if LURL.Trim.IsEmpty then
    LURL := FURL;

  if LURL.Trim.IsEmpty then
    raise EDataLoggerException.Create('URL is empty');

  try
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

    if not FToken.Trim.IsEmpty then
      LHTTP.Request.CustomHeaders.AddValue('Authorization', FToken);

    for I := Low(FHeader) to High(FHeader) do
      LHTTP.Request.CustomHeaders.Values[FHeader[I].Key] := FHeader[I].Value;

    for I := Low(AItemREST.Header) to High(AItemREST.Header) do
      LHTTP.Request.CustomHeaders.Values[AItemREST.Header[I].Key] := AItemREST.Header[I].Value;

    LRetriesCount := 0;

    while True do
      try
        if Self.Terminated then
          Exit;

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
              if Length(AItemREST.FormData) = 0 then
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
        end;

        LResponseContent := LHTTP.Response.ResponseText;

        if not(LHTTP.Response.ResponseCode in [200, 201, 202, 204]) then
          raise EDataLoggerException.Create(LResponseContent);

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

          if LRetriesCount <= 0 then
            Break;

          if LRetriesCount >= FMaxRetries then
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

procedure ForceReferenceToClass(C: TClass);
begin
end;

{ TLogFormData }

class function TLogFormData.Create(const AField: string; const AValue: string; const AContentType: string = ''): TLogFormData;
begin
  Result.Field := AField;
  Result.Value := AValue;
  Result.ContentType := AContentType;
end;

initialization

ForceReferenceToClass(TProviderRESTIndy);

end.
