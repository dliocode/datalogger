{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Socket;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  IdCustomTCPServer, IdTCPConnection, IdContext, IdIOHandler, IdGlobal, IdCoderMIME, IdHashSHA, IdSSL, IdSSLOpenSSL,
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON, System.DateUtils, System.Threading;

type
  TProviderSocketCustomMessage = reference to function(const AItem: TLoggerItem): string;

  TProviderSocket = class(TDataLoggerProvider)
  private
    FListClients: TDictionary<string, TIdContext>;
    FSocket: TIdCustomTCPServer;
    FCheckHeartBeat: TThread;
    FAutoStart: Boolean;
    FCustomMessage: TProviderSocketCustomMessage;

    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);
    procedure SendMessage(const AContext: TIdContext; const AMessage: string);
    procedure CheckHeartBeat;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Port(const AValue: Integer): TProviderSocket;
    function MaxConnection(const AValue: Integer): TProviderSocket;
    function InitSSL(const AValue: TIdServerIOHandlerSSLOpenSSL): TProviderSocket;
    function AutoStart(const AValue: Boolean): TProviderSocket;
    function CustomMessage(const AMessage: TProviderSocketCustomMessage): TProviderSocket;

    function Start: TProviderSocket;
    function Stop: TProviderSocket;

    function IsActive: Boolean;
    function ClientCount: Integer;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

type
  TMyData = class
    LastRecvTime: TDateTime;
  end;

  TDataLoggerSocketServer = class(TIdCustomTCPServer)
  protected
    procedure DoConnect(AContext: TIdContext); override;
    function DoExecute(AContext: TIdContext): Boolean; override;
  public
    function InitSSL(const AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL): TDataLoggerSocketServer;
  end;

  TDataLoggerSocketIOHandler = class(TIdIOHandler)
  public
    function ReadMessage: string;
    function ReadMessageBytes: TBytes;
    procedure WriteMessage(const AMessage: string);
    procedure WriteMessageBytes(const AMessage: TBytes);
  end;

  { TProviderSocket }

constructor TProviderSocket.Create;
begin
  inherited Create;

  FListClients := TDictionary<string, TIdContext>.Create;
  FSocket := TDataLoggerSocketServer.Create;
  FSocket.OnConnect := OnConnect;
  FSocket.OnDisconnect := OnDisconnect;
  TDataLoggerSocketServer(FSocket).OnExecute := OnExecute;
end;

procedure TProviderSocket.AfterConstruction;
begin
  inherited;

  Port(55666);
  AutoStart(True);
  MaxConnection(0);
  CustomMessage(nil);

  CheckHeartBeat;
end;

procedure TProviderSocket.BeforeDestruction;
begin
  Terminate;
  FCheckHeartBeat.Terminate;

  Lock;
  try
    FListClients.Free;
    FSocket.Free;
  finally
    UnLock;
  end;

  inherited;
end;

function TProviderSocket.Port(const AValue: Integer): TProviderSocket;
begin
  Result := Self;

  Lock;
  try
    FSocket.DefaultPort := AValue;
  finally
    UnLock;
  end;
end;

function TProviderSocket.MaxConnection(const AValue: Integer): TProviderSocket;
begin
  Result := Self;

  Lock;
  try
    FSocket.MaxConnections := AValue;
  finally
    UnLock;
  end;
end;

function TProviderSocket.InitSSL(const AValue: TIdServerIOHandlerSSLOpenSSL): TProviderSocket;
begin
  Result := Self;

  Lock;
  try
    TDataLoggerSocketServer(FSocket).InitSSL(AValue);
  finally
    UnLock;
  end;
end;

function TProviderSocket.AutoStart(const AValue: Boolean): TProviderSocket;
begin
  Result := Self;
  FAutoStart := AValue;
end;

function TProviderSocket.CustomMessage(const AMessage: TProviderSocketCustomMessage): TProviderSocket;
begin
  Result := Self;
  FCustomMessage := AMessage;
end;

function TProviderSocket.Start: TProviderSocket;
begin
  Result := Self;

  Lock;
  try
    if FSocket.Active then
      Exit;

    try
      FSocket.Active := True;
    except
      on E: Exception do
        raise EDataLoggerException.CreateFmt('ProviderSocker -> Start -> Port: %d | Message: %s', [FSocket.DefaultPort, E.Message]);
    end;
  finally
    UnLock;
  end;
end;

function TProviderSocket.Stop: TProviderSocket;
begin
  Result := Self;

  Lock;
  try
    if not FSocket.Active then
      Exit;

    FSocket.Active := False;
    AutoStart(False);

    FListClients.Clear;
    FListClients.TrimExcess;
  finally
    UnLock;
  end;
end;

function TProviderSocket.IsActive: Boolean;
begin
  Lock;
  try
    Result := FSocket.Active;
  finally
    UnLock;
  end;
end;

function TProviderSocket.ClientCount: Integer;
begin
  Lock;
  try
    Result := FListClients.Count;
  finally
    UnLock;
  end;
end;

procedure TProviderSocket.LoadFromJSON(const AJSON: string);
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
    Port(LJO.GetValue<Integer>('port', FSocket.DefaultPort));
    AutoStart(LJO.GetValue<Boolean>('auto_start', FAutoStart));
    MaxConnection(LJO.GetValue<Integer>('max_connections', FSocket.MaxConnections));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSocket.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('port', TJSONNumber.Create(FSocket.DefaultPort));
    LJO.AddPair('auto_start', TJSONBool.Create(FAutoStart));
    LJO.AddPair('max_connections', TJSONNumber.Create(FSocket.MaxConnections));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSocket.Save(const ACache: TArray<TLoggerItem>);
var
  LContexts: TArray<TIdContext>;
  LItem: TLoggerItem;
  LLog: string;
begin
  if Length(ACache) = 0 then
    Exit;

  if FAutoStart then
    Start
  else
    if not IsActive then
      Exit;

  Lock;
  try
    LContexts := FListClients.Values.ToArray;
    if Length(LContexts) = 0 then
      Exit;
  finally
    UnLock;
  end;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    if Assigned(FCustomMessage) then
      LLog := FCustomMessage(LItem)
    else
      LLog := TLoggerLogFormat.AsJsonObjectToString(FLogFormat, LItem, True);

    TParallel.For(Low(LContexts), High(LContexts),
      procedure(I: Integer)
      var
        LContext: TIdContext;
        LRetriesCount: Integer;
      begin
        LContext := LContexts[I];

        LRetriesCount := 0;

        while True do
          try
            SendMessage(LContext, LLog);
            Break;
          except
            on E: Exception do
            begin
              Inc(LRetriesCount);

              Sleep(50);

              if Assigned(FLogException) then
                FLogException(Self, LItem, E, LRetriesCount);

              if Self.Terminated then
                Exit;

              if LRetriesCount <= 0 then
                Break;

              if LRetriesCount >= FMaxRetries then
                Break;
            end;
          end;
      end);
  end;
end;

procedure TProviderSocket.OnConnect(AContext: TIdContext);
var
  LMyData: TMyData;
  LID: Int64;
begin
  if Terminated then
    Exit;

  Lock;
  try
    LMyData := TMyData.Create;
    LMyData.LastRecvTime := Now;

    AContext.Data := LMyData;

    LID := Integer(@AContext);

    FListClients.AddOrSetValue(LID.ToString, AContext);
  finally
    UnLock;
  end;
end;

procedure TProviderSocket.OnDisconnect(AContext: TIdContext);
begin

end;

procedure TProviderSocket.OnExecute(AContext: TIdContext);
var
  LMessage: string;
begin
  if (not Assigned(AContext)) or (not Assigned(AContext.Connection)) or (not Assigned(AContext.Connection.IOHandler)) then
    Exit;

  AContext.Connection.IOHandler.CheckForDataOnSource(10);
  LMessage := TDataLoggerSocketIOHandler(AContext.Connection.IOHandler).ReadMessage;

  if LMessage.Trim.IsEmpty then
    Exit;

  if Assigned(AContext.Data) then
    TMyData(AContext.Data).LastRecvTime := Now;
end;

procedure TProviderSocket.SendMessage(const AContext: TIdContext; const AMessage: string);
begin
  if Assigned(AContext.Connection) then
    TDataLoggerSocketIOHandler(AContext.Connection.IOHandler).WriteMessage(AMessage);
end;

procedure TProviderSocket.CheckHeartBeat;
begin
  FCheckHeartBeat :=
    TThread.CreateAnonymousThread(
    procedure
    var
      LIDs: TArray<string>;
      LContexts: TArray<TIdContext>;
      I: Integer;
      LContext: TIdContext;
    begin
      while True do
      begin
        Sleep(500);

        if Terminated then
          Exit;

        try
          Lock;
          try
            if Terminated then
              Exit;

            if FListClients.Count = 0 then
              Continue;

            LIDs := [];
            if FListClients.Keys.Count > 0 then;
            LIDs := FListClients.Keys.ToArray;

            LContexts := [];
            if FListClients.Values.Count > 0 then;
            LContexts := FListClients.Values.ToArray;

            for I := Low(LIDs) to High(LIDs) do
            begin
              LContext := LContexts[I];

              if not Assigned(LContext.Connection) or not Assigned(LContext.Data) then
              begin
                FListClients.Remove(LIDs[I]);
                Continue;
              end;

              if SecondsBetween(Now, TMyData(LContext.Data).LastRecvTime) >= 20 then
              begin
                FListClients.Remove(LIDs[I]);
                Continue;
              end;
            end;

            if Terminated then
              Exit;
          finally
            UnLock;
          end;
        except
        end;
      end;
    end);

  FCheckHeartBeat.Start;
end;

{ TDataLoggerSocketServer }

function TDataLoggerSocketServer.InitSSL(const AIdServerIOHandlerSSLOpenSSL: TIdServerIOHandlerSSLOpenSSL): TDataLoggerSocketServer;
var
  LCurrentActive: Boolean;
begin
  Result := Self;

  LCurrentActive := Active;
  try
    Active := False;
    IOHandler := AIdServerIOHandlerSSLOpenSSL;
  finally
    Active := LCurrentActive;
  end;
end;

procedure TDataLoggerSocketServer.DoConnect(AContext: TIdContext);
begin
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := False;

  AContext.Connection.IOHandler.Tag := -1;

  inherited;
end;

function TDataLoggerSocketServer.DoExecute(AContext: TIdContext): Boolean;
  function HeadersParse(const AMessage: string): TDictionary<string, string>;
  var
    LLines: TArray<string>;
    LLine: string;
    LSplittedLine: TArray<string>;
  begin
    Result := TDictionary<string, string>.Create;

    LLines := AMessage.Split([#13#10]);
    for LLine in LLines do
    begin
      LSplittedLine := LLine.Split([': ']);

      if Length(LSplittedLine) > 1 then
        Result.AddOrSetValue(Trim(LSplittedLine[0]).ToLower, Trim(LSplittedLine[1]));
    end;
  end;

var
  LHandle: TIdIOHandler;
  LBytes: TBytes;
  LMessage: string;
  LSecretKey: string;
  LHashSHA1: TIdHashSHA1;
  LHash: string;
  LParsedHeaders: TDictionary<string, string>;
begin
  LHandle := AContext.Connection.IOHandler;

  if LHandle.Tag = -1 then
  begin
    LHandle.CheckForDataOnSource(10);

    if not LHandle.InputBufferIsEmpty then
    begin
      try
        LHandle.InputBuffer.ExtractToBytes(TIdBytes(LBytes));
        LMessage := IndyTextEncoding_UTF8.GetString(TIdBytes(LBytes));
      except
      end;

      LParsedHeaders := HeadersParse(LMessage);
      try
        if LParsedHeaders.ContainsKey('upgrade') and LParsedHeaders.ContainsKey('sec-websocket-key') then
          if (LParsedHeaders['upgrade'] = 'websocket') then
          begin
            LSecretKey := LParsedHeaders['sec-websocket-key'];

            LHashSHA1 := TIdHashSHA1.Create;
            try
              LHash := TIdEncoderMIME.EncodeBytes(LHashSHA1.HashString(LSecretKey + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
            finally
              LHashSHA1.Free;
            end;

            try
              LHandle.Write(
                'HTTP/1.1 101 Switching Protocols'#13#10 +
                'Upgrade: websocket'#13#10 +
                'Connection: Upgrade'#13#10 +
                'Sec-WebSocket-Accept: ' + LHash +
                #13#10#13#10, IndyTextEncoding_UTF8);
            except
            end;

            LHandle.Tag := 1;
          end;
      finally
        LParsedHeaders.Free;
      end;
    end;
  end;

  Result := inherited;
end;

{ TDataLoggerSocketIOHandler }

function TDataLoggerSocketIOHandler.ReadMessage: string;
begin
  Result := IndyTextEncoding_UTF8.GetString(TIdBytes(ReadMessageBytes));
end;

function TDataLoggerSocketIOHandler.ReadMessageBytes: TBytes;
var
  LReadByte: Byte;
  LByte: array [0 .. 7] of Byte;
  I: Int64;
  LDecodedSize: Int64;
  LMask: array [0 .. 3] of Byte;
begin
  try
    if ReadByte = $81 then
    begin
      LReadByte := ReadByte;

      case LReadByte of
        $FE:
          begin
            LByte[1] := ReadByte;
            LByte[0] := ReadByte;
            LByte[2] := 0;
            LByte[3] := 0;
            LByte[4] := 0;
            LByte[5] := 0;
            LByte[6] := 0;
            LByte[7] := 0;
            LDecodedSize := Int64(LByte);
          end;

        $FF:
          begin
            LByte[7] := ReadByte;
            LByte[6] := ReadByte;
            LByte[5] := ReadByte;
            LByte[4] := ReadByte;
            LByte[3] := ReadByte;
            LByte[2] := ReadByte;
            LByte[1] := ReadByte;
            LByte[0] := ReadByte;
            LDecodedSize := Int64(LByte);
          end;
      else
        LDecodedSize := LReadByte - 128;
      end;

      LMask[0] := ReadByte;
      LMask[1] := ReadByte;
      LMask[2] := ReadByte;
      LMask[3] := ReadByte;

      if LDecodedSize < 1 then
      begin
        Result := [];
        Exit;
      end;

      SetLength(Result, LDecodedSize);

      inherited ReadBytes(TIdBytes(Result), LDecodedSize, False);

      for I := 0 to Pred(LDecodedSize) do
        Result[I] := Result[I] xor LMask[I mod 4];
    end;
  except
  end;
end;

procedure TDataLoggerSocketIOHandler.WriteMessage(const AMessage: string);
begin
  WriteMessageBytes(TBytes(IndyTextEncoding_UTF8.GetBytes(AMessage)));
end;

procedure TDataLoggerSocketIOHandler.WriteMessageBytes(const AMessage: TBytes);
var
  LBytes: TBytes;
begin
  LBytes := [$81];

  if Length(AMessage) <= 125 then
    LBytes := LBytes + [Length(AMessage)]
  else
    if (Length(AMessage) >= 126) and (Length(AMessage) <= 65535) then
      LBytes := LBytes + [126, (Length(AMessage) shr 8) and 255, Length(AMessage) and 255]
    else
      LBytes := LBytes + [127,
        (Int64(Length(AMessage)) shr 56) and 255,
        (Int64(Length(AMessage)) shr 48) and 255,
        (Int64(Length(AMessage)) shr 40) and 255,
        (Int64(Length(AMessage)) shr 32) and 255,
        (Length(AMessage) shr 24) and 255,
        (Length(AMessage) shr 16) and 255,
        (Length(AMessage) shr 8) and 255,
        Length(AMessage) and 255];

  LBytes := LBytes + AMessage;

  try
    Write(TIdBytes(LBytes), Length(LBytes));
  except
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSocket);

end.
