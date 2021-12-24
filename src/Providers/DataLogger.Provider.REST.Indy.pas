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
  IdHTTP;

type
  TLogItemREST = record
    Stream: TStream;
    LogItem: TLoggerItem;
    URL: string;
  end;

  TSaveFinally = reference to procedure(const ALogItem: TLoggerItem; const AContent: string);
  TLoggerMethod = (tlmGet, tlmPost);

  TProviderRESTIndy = class(TDataLoggerProvider)
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

{ TProviderRESTIndy }

constructor TProviderRESTIndy.Create(const AURL: string; const AContentType: string = 'text/plain'; const ABearerToken: string = '');
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

destructor TProviderRESTIndy.Destroy;
begin
  inherited;
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

procedure TProviderRESTIndy.SetSendFinally(const ASaveFinally: TSaveFinally);
begin
  FSaveFinally := ASaveFinally;
end;

procedure TProviderRESTIndy.SetURL(const AURL: string);
begin
  FURL := AURL;
end;

procedure TProviderRESTIndy.InternalSave(const AMethod: TLoggerMethod; const ALogItemREST: TArray<TLogItemREST>);
var
  I: Integer;
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  for I := Low(ALogItemREST) to High(ALogItemREST) do
    HTTP(AMethod, ALogItemREST[I]);
end;

procedure TProviderRESTIndy.InternalSaveAsync(const AMethod: TLoggerMethod; const ALogItemREST: TArray<TLogItemREST>);
begin
  if Length(ALogItemREST) = 0 then
    Exit;

  TParallel.For(Low(ALogItemREST), High(ALogItemREST),
    procedure(Index: Integer)
    begin
      HTTP(AMethod, ALogItemREST[Index]);
    end);
end;

procedure TProviderRESTIndy.HTTP(const AMethod: TLoggerMethod; const AItemREST: TLogItemREST);
var
  LRetryCount: Integer;
  LURL: string;
  LHTTP: TIdHTTP;
  LResponseContent: string;
begin
  try
    if Self.Terminated then
      Exit;

    LHTTP := TIdHTTP.Create(nil);
  except
    if Assigned(AItemREST.Stream) then
      AItemREST.Stream.Free;

    Exit
  end;

  try
    LHTTP.Request.Connection := 'Keep-Alive';
    LHTTP.HandleRedirects := True;
    LHTTP.ConnectTimeout := 3000;
    LHTTP.ReadTimeout := 3000;
    LHTTP.Request.AcceptCharSet := 'utf-8';
    LHTTP.Request.AcceptEncoding := 'utf-8';
    LHTTP.Request.UserAgent := 'DataLoggerRest';
    LHTTP.Request.ContentType := FContentType;
    LHTTP.Request.Accept := FContentType;

    if not FBearerToken.Trim.IsEmpty then
      LHTTP.Request.CustomHeaders.AddValue('Authorization', 'Bearer ' + FBearerToken);

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
