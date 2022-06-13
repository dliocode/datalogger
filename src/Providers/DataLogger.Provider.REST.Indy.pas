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
  System.SysUtils, System.Classes, System.Threading,
  IdHTTP, IdSSLOpenSSL;

type
  TIdHTTP = IdHTTP.TIdHTTP;

  TLogItemREST = record
    Stream: TStream;
    LogItem: TLoggerItem;
    URL: string;
  end;

  TExecuteFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string);
  TRESTMethod = (tlmGet, tlmPost);

  TProviderRESTIndy = class(TDataLoggerProvider)
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
    function URL(const AValue: string): TProviderRESTIndy; overload;
    function URL: string; overload;
    function ContentType(const AValue: string): TProviderRESTIndy;
    function BearerToken(const AValue: string): TProviderRESTIndy;
    function Token(const AValue: string): TProviderRESTIndy;
    function Method(const AValue: TRESTMethod): TProviderRESTIndy;
    function ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTIndy;

    constructor Create; overload;
    constructor Create(const AURL: string; const AContentType: string = 'text/plain'; const AToken: string = ''); overload; deprecated 'Use TProviderRESTIndy.Create.URL('').ContentType(''application/json'').BearerToken(''aaaa'') - This function will be removed in future versions';
  end;

implementation

{ TProviderRESTIndy }

constructor TProviderRESTIndy.Create;
begin
  inherited Create;

  URL('');
  ContentType('text/plain');
  Token('');
end;

constructor TProviderRESTIndy.Create(const AURL: string; const AContentType: string = 'text/plain'; const AToken: string = '');
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

function TProviderRESTIndy.BearerToken(const AValue: string): TProviderRESTIndy;
begin
  Result := Self;
  FToken := 'Bearer ' + AValue;
end;

function TProviderRESTIndy.Token(const AValue: string): TProviderRESTIndy;
begin
  Result := Self;
  FToken := AValue;
end;

function TProviderRESTIndy.Method(const AValue: TRESTMethod): TProviderRESTIndy;
begin
  Result := Self;
  FMethod := AValue;
end;

function TProviderRESTIndy.ExecuteFinally(const AExecuteFinally: TExecuteFinally): TProviderRESTIndy;
begin
  Result := Self;
  FExecuteFinally := AExecuteFinally;
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

procedure TProviderRESTIndy.InternalSave(const AMethod: TRESTMethod; const ALogItemREST: TArray<TLogItemREST>);
var
  I: Integer;
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  for I := Low(ALogItemREST) to High(ALogItemREST) do
    HTTP(AMethod, ALogItemREST[I]);
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
  LRetryCount: Integer;
  LURL: string;
  LHTTP: TIdHTTP;
  LSSL: TIdSSLIOHandlerSocketOpenSSL;
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
    LHTTP.Request.AcceptCharSet := 'utf-8';
    LHTTP.Request.AcceptEncoding := 'utf-8';
    LHTTP.Request.UserAgent := 'DataLogger.Provider.REST.Indy';
    LHTTP.Request.ContentType := FContentType;
    LHTTP.Request.Accept := FContentType;
    LHTTP.Request.Connection := 'Keep-Alive';

    if not FToken.Trim.IsEmpty then
      LHTTP.Request.CustomHeaders.AddValue('Authorization', FToken);

    if LURL.ToLower.Contains('https://') then
    begin
      if not LoadOpenSSLLibrary then
        raise EDataLoggerException.Create('DLL''s not compatible or not found (ssleay32 e libeay32)');

      LSSL := TIdSSLIOHandlerSocketOpenSSL.Create(LHTTP);
      LSSL.SSLOptions.Method := sslvTLSv1_2;
    end;

    LHTTP.IOHandler := LSSL;

    LRetryCount := 0;

    while True do
      try
        if Self.Terminated then
          Exit;

        case AMethod of
          tlmGet:
            LHTTP.Get(LURL);
          tlmPost:
            LHTTP.Post(LURL, AItemREST.Stream);
        end;

        LResponseContent := LHTTP.Response.ResponseText;

        if not(LHTTP.Response.ResponseCode in [200, 201]) then
          raise EDataLoggerException.Create(LResponseContent);

        Break;
      except
        on E: Exception do
        begin
          Inc(LRetryCount);

          Sleep(50);

          if Assigned(FLogException) then
            FLogException(Self, AItemREST.LogItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= FMaxRetry then
            Break;
        end;
      end;
  finally
    LHTTP.Free;

    if Assigned(LSSL) then
      UnLoadOpenSSLLibrary;

    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    if Assigned(FExecuteFinally) then
      FExecuteFinally(AItemREST.LogItem, LResponseContent);
  end;
end;

end.
