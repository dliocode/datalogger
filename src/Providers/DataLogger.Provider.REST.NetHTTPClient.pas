{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.REST.NetHTTPClient;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  System.SysUtils, System.Classes, System.Threading, System.Net.HttpClientComponent, System.Net.HttpClient, System.JSON, System.TypInfo;

type
  TNetHTTPClient = System.Net.HttpClientComponent.TNetHTTPClient;

  TLogItemREST = record
    Stream: TStream;
    LogItem: TLoggerItem;
    URL: string;
  end;

  TExecuteFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string);

  TRESTMethod = (tlmGet, tlmPost);

  TProviderRESTNetHTTPClient = class(TDataLoggerProvider)
  private
    FURL: string;
    FContentType: string;
    FToken: string;
    FMethod: TRESTMethod;
    FExecuteFinally: TExecuteFinally;
    procedure HTTP(const AMethod: TRESTMethod; const AItemREST: TLogItemREST);
  protected
    procedure InternalSave(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
    procedure InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);

    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderRESTNetHTTPClient; overload;
    function URL: string; overload;
    function ContentType(const AValue: string): TProviderRESTNetHTTPClient;
    function BearerToken(const AValue: string): TProviderRESTNetHTTPClient;
    function Token(const AValue: string): TProviderRESTNetHTTPClient; overload;
    function Token: string; overload;
    function Method(const AValue: TRESTMethod): TProviderRESTNetHTTPClient;
    function ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTNetHTTPClient;

    procedure SetJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
    constructor Create(const AURL: string; const AContentType: string = 'text/plain'; const AToken: string = ''); overload; deprecated 'Use TProviderRESTNetHTTPClient.Create.URL('').ContentType(''application/json'').BearerToken(''aaaa'') - This function will be removed in future versions';
  end;

implementation

{ TProviderRESTNetHTTPClient }

constructor TProviderRESTNetHTTPClient.Create;
begin
  inherited Create;

  URL('');
  ContentType('text/plain');
  Token('');
end;

constructor TProviderRESTNetHTTPClient.Create(const AURL: string; const AContentType: string = 'text/plain'; const AToken: string = '');
var
  LProtocol: string;
  LHost: string;
begin
  Create;

  LProtocol := 'http://';
  LHost := AURL;

  if not LHost.ToLower.StartsWith('http://') and not LHost.ToLower.StartsWith('https://') then
    LHost := LProtocol + AURL;

  URL(LHost);
  Token(AToken);
  ContentType(AContentType);

  if FContentType.Trim.IsEmpty then
    ContentType('text/plain');
end;

function TProviderRESTNetHTTPClient.URL(const AValue: string): TProviderRESTNetHTTPClient;
var
  LProtocol: string;
begin
  Result := Self;

  LProtocol := 'http://';

  FURL := AValue;
  if not AValue.ToLower.StartsWith('http://') and not AValue.ToLower.StartsWith('https://') then
    FURL := LProtocol + AValue;
end;

function TProviderRESTNetHTTPClient.URL: string;
begin
  Result := FURL;
end;

function TProviderRESTNetHTTPClient.ContentType(const AValue: string): TProviderRESTNetHTTPClient;
begin
  Result := Self;
  FContentType := AValue;
end;

function TProviderRESTNetHTTPClient.BearerToken(const AValue: string): TProviderRESTNetHTTPClient;
begin
  Result := Self;

  if AValue.Trim.ToLower.Contains('bearer ') then
    Token(AValue)
  else
    FToken := 'Bearer ' + AValue;
end;

function TProviderRESTNetHTTPClient.Token(const AValue: string): TProviderRESTNetHTTPClient;
begin
  Result := Self;
  FToken := AValue;
end;

function TProviderRESTNetHTTPClient.Token: string;
begin
  Result := FToken;
end;

function TProviderRESTNetHTTPClient.Method(const AValue: TRESTMethod): TProviderRESTNetHTTPClient;
begin
  Result := Self;
  FMethod := AValue;
end;

function TProviderRESTNetHTTPClient.ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTNetHTTPClient;
begin
  Result := Self;
  FExecuteFinally := AExecuteFinally;
end;

procedure TProviderRESTNetHTTPClient.SetJSON(const AJSON: string);
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

    inherited SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderRESTNetHTTPClient.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', FURL);
    LJO.AddPair('content_type', FContentType);
    LJO.AddPair('token', FToken);
    LJO.AddPair('method', GetEnumName(TypeInfo(TRESTMethod), Integer(FMethod)));

    inherited ToJSONInternal(LJO);

    if AFormat then
      Result := LJO.Format
    else
      Result := LJO.ToString;
  finally
    LJO.Free;
  end;
end;

procedure TProviderRESTNetHTTPClient.Save(const ACache: TArray<TLoggerItem>);
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
    if LItem.&Type = TLoggerType.All then
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

procedure TProviderRESTNetHTTPClient.InternalSave(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
var
  I: Integer;
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  for I := Low(ALogItemREST) to High(ALogItemREST) do
    HTTP(AMethod, ALogItemREST[I]);
end;

procedure TProviderRESTNetHTTPClient.InternalSaveAsync(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  TParallel.For(Low(ALogItemREST), High(ALogItemREST),
    procedure(Index: Integer)
    begin
      HTTP(AMethod, ALogItemREST[Index]);
    end);
end;

procedure TProviderRESTNetHTTPClient.HTTP(const AMethod: TRESTMethod; const AItemREST: TLogItemREST);
var
  LRetriesCount: Integer;
  LURL: string;
  LHTTP: TNetHTTPClient;
  LResponse: IHTTPResponse;
  LResponseContent: string;
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
    LHTTP := TNetHTTPClient.Create(nil);
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

    LHTTP.AcceptCharSet := 'utf-8';
    LHTTP.AcceptEncoding := 'utf-8';

    LHTTP.UserAgent := 'DataLogger.Provider.REST.NetHTTPClient';

    LHTTP.ContentType := FContentType;
    LHTTP.Accept := FContentType;

    if not FToken.Trim.IsEmpty then
      LHTTP.CustomHeaders['Authorization'] := FToken;

    LRetriesCount := 0;

    while True do
      try
        if Self.Terminated then
          Exit;

        case AMethod of
          tlmGet:
            LResponse := LHTTP.Get(LURL);
          tlmPost:
            LResponse := LHTTP.Post(LURL, AItemREST.Stream);
        end;

        LResponseContent := LResponse.ContentAsString(TEncoding.UTF8);

        if not(LResponse.StatusCode in [200, 201]) then
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

          if LRetriesCount = -1 then
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

initialization

ForceReferenceToClass(TProviderRESTNetHTTPClient);

end.
