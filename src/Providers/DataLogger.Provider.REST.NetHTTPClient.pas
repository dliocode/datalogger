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
  System.SysUtils, System.Classes, System.Threading,
  System.Net.HttpClientComponent, System.Net.HttpClient;

type
  TLogItemREST = record
    Stream: TStream;
    LogItem: TLoggerItem;
    URL: string;
  end;

  TSaveFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string);

  TLoggerMethod = (tlmGet, tlmPost);

  TProviderRESTNetHTTPClient = class(TDataLoggerProvider)
  private
    FURL: string;
    FBearerToken: string;
    FContentType: string;
    FSaveFinally: TSaveFinally;
    procedure HTTP(const AMethod: TLoggerMethod; const AItemREST: TLogItemREST);
  protected
    procedure SetURL(const AURL: string);

    procedure InternalSave(const AMethod: TLoggerMethod; const ALogItemREST: TArray<TLogItemREST>);
    procedure InternalSaveAsync(const AMethod: TLoggerMethod; const ALogItemREST: TArray<TLogItemREST>);

    procedure SetSendFinally(const ASaveFinally: TSaveFinally);
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AURL: string; const AContentType: string = 'text/plain'; const ABearerToken: string = '');
    destructor Destroy; override; final;
  end;

implementation

{ TProviderRESTNetHTTPClient }

constructor TProviderRESTNetHTTPClient.Create(const AURL: string; const AContentType: string = 'text/plain'; const ABearerToken: string = '');
var
  LProtocol: string;
  LHost: string;
begin
  inherited Create;

  LProtocol := 'http://';
  LHost := AURL;

  if not LHost.ToLower.StartsWith('http://') and not LHost.ToLower.StartsWith('https://') then
    LHost := LProtocol + AURL;

  FURL := LHost;
  FBearerToken := ABearerToken;
  FContentType := AContentType;

  if FContentType.Trim.IsEmpty then
    FContentType := 'text/plain';
end;

destructor TProviderRESTNetHTTPClient.Destroy;
begin
  inherited;
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
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    if Trim(LowerCase(FContentType)) = 'application/json' then
      LLogItemREST.Stream := TLoggerLogFormat.AsStreamJsonObject(GetLogFormat, LItem)
    else
      LLogItemREST.Stream := TLoggerLogFormat.AsStream(GetLogFormat, LItem, GetFormatTimestamp);

    LLogItemREST.LogItem := LItem;

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  InternalSaveAsync(TLoggerMethod.tlmPost, LItemREST);
end;

procedure TProviderRESTNetHTTPClient.SetSendFinally(const ASaveFinally: TSaveFinally);
begin
  FSaveFinally := ASaveFinally;
end;

procedure TProviderRESTNetHTTPClient.SetURL(const AURL: string);
begin
  FURL := AURL;
end;

procedure TProviderRESTNetHTTPClient.InternalSave(const AMethod: TLoggerMethod; const ALogItemREST: TArray<TLogItemREST>);
var
  I: Integer;
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  for I := Low(ALogItemREST) to High(ALogItemREST) do
    HTTP(AMethod, ALogItemREST[I]);
end;

procedure TProviderRESTNetHTTPClient.InternalSaveAsync(const AMethod: TLoggerMethod; const ALogItemREST: TArray<TLogItemREST>);
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  TParallel.For(Low(ALogItemREST), High(ALogItemREST),
    procedure(Index: Integer)
    begin
      HTTP(AMethod, ALogItemREST[Index]);
    end);
end;

procedure TProviderRESTNetHTTPClient.HTTP(const AMethod: TLoggerMethod; const AItemREST: TLogItemREST);
var
  LRetryCount: Integer;
  LURL: string;
  LHTTP: TNetHTTPClient;
  LResponse: IHTTPResponse;
  LResponseContent: string;
begin
  try
    if Self.Terminated then
      Exit;

    LHTTP := TNetHTTPClient.Create(nil);
  except
    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    Exit
  end;

  try
    LHTTP.HandleRedirects := True;
    LHTTP.ConnectionTimeout := 3000;
    LHTTP.ResponseTimeout := 3000;
    LHTTP.AcceptCharSet := 'utf-8';
    LHTTP.AcceptEncoding := 'utf-8';
    LHTTP.UserAgent := 'DataLoggerRest';
    LHTTP.ContentType := FContentType;
    LHTTP.Accept := FContentType;

    if not FBearerToken.Trim.IsEmpty then
      LHTTP.CustomHeaders['Authorization'] := 'Bearer ' + FBearerToken;

    LRetryCount := 0;

    LURL := AItemREST.URL;
    if LURL.Trim.IsEmpty then
      LURL := FURL;

    if LURL.Trim.IsEmpty then
      raise EDataLoggerException.Create('URL is empty!');

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
          Inc(LRetryCount);

          if Assigned(LogException) then
            LogException(Self, AItemREST.LogItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= GetMaxRetry then
            Break;
        end;
      end;
  finally
    LHTTP.Free;

    if Assigned(FSaveFinally) then
      FSaveFinally(AItemREST.LogItem, LResponseContent);

    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;
  end;
end;

end.
