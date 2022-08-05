{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
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

  TLogItemResponse = record
    LogItem: TLoggerItem;
    Content: string;
  end;

  TExecuteFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string);
  TRESTMethod = (tlmGet, tlmPost);

  TProviderRESTHTTPClient = class(TDataLoggerProvider)
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
    function URL(const AValue: string): TProviderRESTHTTPClient; overload;
    function URL: string; overload;
    function ContentType(const AValue: string): TProviderRESTHTTPClient;
    function Token(const AValue: string): TProviderRESTHTTPClient; overload;
    function Token: string; overload;
    function BearerToken(const AValue: string): TProviderRESTHTTPClient;
    function BasicAuth(const AUsername: string; const APassword: string): TProviderRESTHTTPClient;
    function Method(const AValue: TRESTMethod): TProviderRESTHTTPClient;
    function AddHeader(const AKey: string; const AValue: string): TProviderRESTHTTPClient;
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
  Token('');
  Method(tlmPost);
  ExecuteFinally(nil);
end;

function TProviderRESTHTTPClient.URL(const AValue: string): TProviderRESTHTTPClient;
var
  LProtocol: string;
begin
  Result := Self;
  LProtocol := 'http://';
  FURL := AValue;

  if not AValue.ToLower.StartsWith('http://') and not AValue.ToLower.StartsWith('https://') then
    FURL := LProtocol + AValue;
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

function TProviderRESTHTTPClient.Token(const AValue: string): TProviderRESTHTTPClient;
begin
  Result := Self;
  FToken := AValue;
end;

function TProviderRESTHTTPClient.Token: string;
begin
  Result := FToken;
end;

function TProviderRESTHTTPClient.BearerToken(const AValue: string): TProviderRESTHTTPClient;
begin
  Result := Self;

  if AValue.Trim.ToLower.Contains('bearer') then
    Token(AValue)
  else
    Token('Bearer ' + AValue);
end;

function TProviderRESTHTTPClient.BasicAuth(const AUsername: string; const APassword: string): TProviderRESTHTTPClient;
var
  LBase64: TBase64Encoding;
begin
  LBase64 := TBase64Encoding.Create(0, '');
  try
    Result := Token('Basic ' + LBase64.Encode(Format('%s:%s', [AUsername, APassword])));
  finally
    LBase64.Free;
  end;
end;

function TProviderRESTHTTPClient.Method(const AValue: TRESTMethod): TProviderRESTHTTPClient;
begin
  Result := Self;
  FMethod := AValue;
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
    if FHeader[I].Key = AKey then
    begin
      FHeader[I].Value := AValue;
      LIsFound := True;
      Break;
    end;

  if not LIsFound then
    FHeader := FHeader + [LHeader];
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
    Token(LJO.GetValue<string>('token', FToken));
    LValue := GetEnumName(TypeInfo(TRESTMethod), Integer(FMethod));
    Method(TRESTMethod(GetEnumValue(TypeInfo(TRESTMethod), LJO.GetValue<string>('method', LValue))));
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

procedure TProviderRESTHTTPClient.Save(const ACache: TArray<TLoggerItem>);
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

procedure TProviderRESTHTTPClient.InternalSave(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>; const ASleep: Integer = 0);
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

procedure TProviderRESTHTTPClient.InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
begin
  if Length(ALogItemREST) = 0 then
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

    if not FToken.Trim.IsEmpty then
      LHTTP.CustomHeaders['Authorization'] := FToken;

    for I := Low(FHeader) to High(FHeader) do
      LHTTP.CustomHeaders[FHeader[I].Key] := FHeader[I].Value;

    for I := Low(AItemREST.Header) to High(AItemREST.Header) do
      LHTTP.CustomHeaders[AItemREST.Header[I].Key] := AItemREST.Header[I].Value;

    LRetriesCount := 0;

    while True do
      try
        if Self.Terminated then
          Exit;

        case AMethod of
          tlmGet:
            LResponse := LHTTP.Get(LURL);

          tlmPost:
            begin
              if Length(AItemREST.FormData) = 0 then
                LResponse := LHTTP.Post(LURL, AItemREST.Stream)
              else
              begin
                LFormData := TMultipartFormData.Create;
                try
                  for I := Low(AItemREST.FormData) to High(AItemREST.FormData) do
                    LFormData.AddField(AItemREST.FormData[I].Field, AItemREST.FormData[I].Value {$IF RTLVersion > 32}, AItemREST.FormData[I].ContentType{$ENDIF});
                  LResponse := LHTTP.Post(LURL, LFormData);
                finally
                  LFormData.Free;
                end;
              end;
            end;
        end;

        LResponseContent := LResponse.ContentAsString(TEncoding.UTF8);

        if not(LResponse.StatusCode in [200, 201, 202, 204]) then
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

ForceReferenceToClass(TProviderRESTHTTPClient);

end.
