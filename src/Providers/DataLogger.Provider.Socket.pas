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

unit DataLogger.Provider.Socket;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  IdTCPServer, IdHashSHA, IdSSLOpenSSL, IdContext, IdSSL, IdIOHandler, IdGlobal, IdCoderMIME, IdComponent,
  System.SysUtils, System.Generics.Collections, System.JSON, System.Threading, System.SyncObjs, System.Classes, System.Types;

type
  TProviderSocketCustomMessage = reference to function(const AItem: TLoggerItem): string;
  TProviderSocketOnExecute = reference to procedure(const AConnectionID: string);

  TProviderSocket = class(TDataLoggerProvider<TProviderSocket>)
  strict private
    FIdTCPServer: TIdTCPServer;
    FListConnection: TObject;
    FOnConnection: TProviderSocketOnExecute;
    FOnDisconnection: TProviderSocketOnExecute;

    FAutoStart: Boolean;
    FCustomMessage: TProviderSocketCustomMessage;

    procedure DoOnConnect(AContext: TIdContext);
    procedure DoOnExecute(AContext: TIdContext);
    procedure DoOnDisconnect(AContext: TIdContext);
    procedure CheckConnection(const AContext: TIdContext);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function OnConnection(const AValue: TProviderSocketOnExecute): TProviderSocket;
    function OnDisconnect(const AValue: TProviderSocketOnExecute): TProviderSocket;

    function InitSSL(const AValue: TIdServerIOHandlerSSLOpenSSL): TProviderSocket;
    function Port(const AValue: Integer): TProviderSocket;
    function MaxConnection(const AValue: Integer): TProviderSocket;

    function AutoStart(const AValue: Boolean): TProviderSocket;
    function CustomMessage(const AMessage: TProviderSocketCustomMessage): TProviderSocket;

    function Start: TProviderSocket;
    function Stop: TProviderSocket;

    function IsActive: Boolean;
    function DisconnectID(const AConnectionID: string): TProviderSocket;
    function DisconnectAll: TProviderSocket;
    function CountConnections: Integer;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

type
{$SCOPEDENUMS ON}
  TDataLoggerSocketMessageOperationCode = (CONTINUE, TEXT_FRAME, BINARY_FRAME, CONNECTION_CLOSE, PING, PONG, TIMEOUT, ERROR);
{$SCOPEDENUMS OFF}

  TDataLoggerSocketMessage = record
    OperationCode: TDataLoggerSocketMessageOperationCode;
    Message: string;
  end;

  TDataLoggerSocketConnection = class
  strict private
    FIdTCPServer: TIdTCPServer;
    FIdContext: TIdContext;
    FID: string;
    FDateTimeConnected: TDateTime;
  public
    function ID: string;
    function IPAddress: string;
    function DateTimeConnected: TDateTime;

    function Connected: Boolean;
    procedure Disconnect;

    function CheckForDataOnSource(const ATimeout: Integer): Boolean;
    function IsEquals(const AIdContext: TIdContext): Boolean;
    function HandShaked: Boolean;

    procedure SendMessage(const AMessage: string);
    procedure SendFile(const AFilename: string);
    procedure PING;

    function ReadMessage: TDataLoggerSocketMessage; overload;
    function ReadMessage(const ATimeout: Integer): TDataLoggerSocketMessage; overload;

    function Context: TIdContext;

    constructor Create(const AIdTCPServer: TIdTCPServer; const AIdContext: TIdContext);
  end;

  TDataLoggerSocketListConnection = class
  strict private
    FCriticalSection: TCriticalSection;
    FList: TObjectList<TDataLoggerSocketConnection>;
    FIdTCPServer: TIdTCPServer;

    procedure Lock;
    procedure UnLock;
  public
    function Add(const AValue: TDataLoggerSocketConnection): TDataLoggerSocketListConnection;
    function Remove(const AValue: TDataLoggerSocketConnection): TDataLoggerSocketListConnection;
    function RemoveAll: TDataLoggerSocketListConnection;
    function Last: TDataLoggerSocketConnection;
    function ToArray: TArray<TDataLoggerSocketConnection>;
    function IsEquals(const AContext: TIdContext): TDataLoggerSocketConnection;
    function Count: Integer;
    function List: TObjectList<TDataLoggerSocketConnection>;

    function GetFromID(const AID: string): TDataLoggerSocketConnection;

    constructor Create(const AIdTCPServer: TIdTCPServer);
    destructor Destroy; override;
  end;

  TDataLoggerSocketConnectionHandler = class(TIdIOHandler)
  private
    procedure Send(const ABytes: TBytes; const AMessage: TBytes);
  public
    function HandShaked: Boolean;

    function ReadMessage: TDataLoggerSocketMessage;
    procedure WriteMessage(const AMessage: string);
    procedure WriteMessageBytes(const AMessage: TBytes);

    procedure PING;
  end;

  { TProviderSocket }

constructor TProviderSocket.Create;
begin
  inherited Create;

  FIdTCPServer := TIdTCPServer.Create;
  FIdTCPServer.OnConnect := DoOnConnect;
  FIdTCPServer.OnDisconnect := DoOnDisconnect;
  FIdTCPServer.OnExecute := DoOnExecute;
  FIdTCPServer.Active := False;

  FListConnection := TDataLoggerSocketListConnection.Create(FIdTCPServer);

  OnConnection(nil);
  OnDisconnect(nil);
  Port(8080);
  AutoStart(True);
  MaxConnection(0);
  CustomMessage(nil);
end;

procedure TProviderSocket.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderSocket.Destroy;
begin
  Stop;

  FListConnection.Free;
  FIdTCPServer.Free;

  inherited;
end;

function TProviderSocket.OnConnection(const AValue: TProviderSocketOnExecute): TProviderSocket;
begin
  Result := Self;
  FOnConnection := AValue;
end;

function TProviderSocket.OnDisconnect(const AValue: TProviderSocketOnExecute): TProviderSocket;
begin
  Result := Self;
  FOnDisconnection := AValue;
end;

function TProviderSocket.InitSSL(const AValue: TIdServerIOHandlerSSLOpenSSL): TProviderSocket;
var
  LCurrentActive: Boolean;
begin
  Result := Self;

  LCurrentActive := FIdTCPServer.Active;
  try
    FIdTCPServer.Active := False;
    FIdTCPServer.IOHandler := AValue;
  finally
    FIdTCPServer.Active := LCurrentActive;
  end;
end;

function TProviderSocket.Port(const AValue: Integer): TProviderSocket;
begin
  Result := Self;
  FIdTCPServer.DefaultPort := AValue;
end;

function TProviderSocket.MaxConnection(const AValue: Integer): TProviderSocket;
begin
  Result := Self;
  FIdTCPServer.MaxConnections := AValue
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

  try
    FIdTCPServer.Active := True;
  except
    on E: Exception do
      raise EDataLoggerException.CreateFmt('ProviderSocket -> Start -> Port: %d | Message: %s', [FIdTCPServer.DefaultPort, E.Message]);
  end;
end;

function TProviderSocket.Stop: TProviderSocket;
begin
  Result := Self;

  FIdTCPServer.Active := False;
end;

function TProviderSocket.IsActive: Boolean;
begin
  Result := FIdTCPServer.Active;
end;

function TProviderSocket.DisconnectID(const AConnectionID: string): TProviderSocket;
var
  LConnection: TDataLoggerSocketConnection;
begin
  Result := Self;

  LConnection := TDataLoggerSocketListConnection(FListConnection).GetFromID(AConnectionID);

  if Assigned(LConnection) then
    LConnection.Disconnect;
end;

function TProviderSocket.DisconnectAll: TProviderSocket;
var
  LConnections: TArray<TDataLoggerSocketConnection>;
  LConnection: TDataLoggerSocketConnection;
begin
  Result := Self;

  LConnections := TDataLoggerSocketListConnection(FListConnection).ToArray;

  try
    for LConnection in LConnections do
      LConnection.Disconnect;
  finally
    TDataLoggerSocketListConnection(FListConnection).RemoveAll;
  end;

  Stop;
  Start
end;

function TProviderSocket.CountConnections: Integer;
begin
  Result := TDataLoggerSocketListConnection(FListConnection).Count;
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
    Port(LJO.GetValue<Integer>('port', FIdTCPServer.DefaultPort));
    AutoStart(LJO.GetValue<Boolean>('auto_start', FAutoStart));
    MaxConnection(LJO.GetValue<Integer>('max_connections', FIdTCPServer.MaxConnections));

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
    LJO.AddPair('port', TJSONNumber.Create(FIdTCPServer.DefaultPort));
    LJO.AddPair('auto_start', TJSONBool.Create(FAutoStart));
    LJO.AddPair('max_connections', TJSONNumber.Create(FIdTCPServer.MaxConnections));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSocket.Save(const ACache: TArray<TLoggerItem>);
var
  LContexts: TArray<TDataLoggerSocketConnection>;
  LItem: TLoggerItem;
  LLog: string;
begin
  if (Length(ACache) = 0) then
    Exit;

  if FAutoStart then
    Start
  else
    if not IsActive then
      Exit;

  if (TDataLoggerSocketListConnection(FListConnection).Count = 0) then
    Exit;

  LContexts := TDataLoggerSocketListConnection(FListConnection).ToArray;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLast then
      Continue;

    if Assigned(FCustomMessage) then
      LLog := FCustomMessage(LItem)
    else
      LLog := SerializeItem.LogItem(LItem).ToJSON;

    TParallel.For(Low(LContexts), High(LContexts),
      procedure(I: Integer)
      var
        LContext: TDataLoggerSocketConnection;
        LRetriesCount: Integer;
      begin
        LContext := LContexts[I];

        LRetriesCount := 0;

        while True do
          try
            LContext.SendMessage(LLog);
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

              if (LRetriesCount <= 0) then
                Break;

              if (LRetriesCount >= FMaxRetries) then
                Break;
            end;
          end;
      end);
  end;
end;

procedure TProviderSocket.DoOnConnect(AContext: TIdContext);
var
  LConnection: TDataLoggerSocketConnection;
begin
  if AContext.Connection.IOHandler is TIdSSLIOHandlerSocketBase then
    TIdSSLIOHandlerSocketBase(AContext.Connection.IOHandler).PassThrough := False;

  AContext.Connection.IOHandler.Tag := -1;

  LConnection := TDataLoggerSocketConnection.Create(FIdTCPServer, AContext);
  TDataLoggerSocketListConnection(FListConnection).Add(LConnection);

  CheckConnection(AContext);

  if Assigned(FOnConnection) then
    FOnConnection(LConnection.ID);
end;

procedure TProviderSocket.DoOnExecute(AContext: TIdContext);
var
  LConnection: TDataLoggerSocketConnection;
  LReadMessage: TDataLoggerSocketMessage;
begin
  LConnection := TDataLoggerSocketListConnection(FListConnection).IsEquals(AContext);

  LReadMessage := LConnection.ReadMessage;
  if (LReadMessage.OperationCode = TDataLoggerSocketMessageOperationCode.CONNECTION_CLOSE) then
  begin
    LConnection.Disconnect;
    Exit;
  end;
end;

procedure TProviderSocket.DoOnDisconnect(AContext: TIdContext);
var
  LConnection: TDataLoggerSocketConnection;
begin
  LConnection := TDataLoggerSocketListConnection(FListConnection).IsEquals(AContext);
  try
    if Assigned(FOnDisconnection) then
      FOnDisconnection(LConnection.ID);
  finally
    TDataLoggerSocketListConnection(FListConnection).Remove(LConnection);
  end;
end;

procedure TProviderSocket.CheckConnection(const AContext: TIdContext);
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

      if (Length(LSplittedLine) > 1) then
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

  if (LHandle.Tag = -1) then
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
          if (LParsedHeaders['upgrade'].Trim.ToLower.Equals('websocket')) then
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
                'Sec-WebSocket-Accept: ' + LHash + #13#10#13#10,
                IndyTextEncoding_UTF8);
            except
            end;

            LHandle.Tag := 1;
          end;
      finally
        LParsedHeaders.Free;
      end;
    end;
  end;
end;

{ TDataLoggerSocketConnection }

constructor TDataLoggerSocketConnection.Create(const AIdTCPServer: TIdTCPServer; const AIdContext: TIdContext);
begin
  FIdTCPServer := AIdTCPServer;
  FIdContext := AIdContext;

  FID := '';
  FDateTimeConnected := Now;
end;

function TDataLoggerSocketConnection.ID: string;
begin
  if (FID = '') then
    FID := IntToStr(Int64(@FIdContext));

  Result := FID
end;

function TDataLoggerSocketConnection.IPAddress: string;
begin
  Result := '';

  if not Assigned(FIdContext) or not Assigned(FIdContext.Connection) or not Assigned(FIdContext.Connection.Socket) or not Assigned(FIdContext.Connection.Socket.Binding) then
    Exit;

  Result := FIdContext.Connection.Socket.Binding.PeerIP;
end;

function TDataLoggerSocketConnection.DateTimeConnected: TDateTime;
begin
  Result := FDateTimeConnected;
end;

function TDataLoggerSocketConnection.Connected: Boolean;
begin
  Result := False;

  if not Assigned(FIdContext) or not Assigned(FIdContext.Connection) then
    Exit;

  Result := FIdContext.Connection.Connected;
end;

procedure TDataLoggerSocketConnection.Disconnect;
begin
  if not Assigned(FIdContext) or not Assigned(FIdContext.Connection) then
    Exit;

  FIdContext.Connection.Disconnect;
  FIdTCPServer.Contexts.Remove(FIdContext);
end;

function TDataLoggerSocketConnection.Context: TIdContext;
begin
  Result := FIdContext;
end;

function TDataLoggerSocketConnection.CheckForDataOnSource(const ATimeout: Integer): Boolean;
begin
  Result := False;

  if not Assigned(FIdContext) or not Assigned(FIdContext.Connection) or not Assigned(FIdContext.Connection.IOHandler) then
    Exit;

  Result := FIdContext.Connection.IOHandler.CheckForDataOnSource(ATimeout);
end;

function TDataLoggerSocketConnection.IsEquals(const AIdContext: TIdContext): Boolean;
begin
  Result := False;

  if not Assigned(FIdContext) then
    Exit;

  Result := (FIdContext = AIdContext);
end;

function TDataLoggerSocketConnection.HandShaked: Boolean;
begin
  Result := False;

  if Assigned(FIdContext) and Assigned(FIdContext.Connection) and Assigned(FIdContext.Connection.IOHandler) then
    Result := TDataLoggerSocketConnectionHandler(FIdContext.Connection.IOHandler).HandShaked;
end;

procedure TDataLoggerSocketConnection.SendMessage(const AMessage: string);
begin
  if Assigned(FIdContext) and Assigned(FIdContext.Connection) and Assigned(FIdContext.Connection.IOHandler) then
    TDataLoggerSocketConnectionHandler(FIdContext.Connection.IOHandler).WriteMessage(AMessage);
end;

procedure TDataLoggerSocketConnection.SendFile(const AFilename: string);
begin
  if Assigned(FIdContext) and Assigned(FIdContext.Connection) and Assigned(FIdContext.Connection.IOHandler) then
    TDataLoggerSocketConnectionHandler(FIdContext.Connection.IOHandler).WriteFile(AFilename, True);
end;

procedure TDataLoggerSocketConnection.PING;
begin
  if Assigned(FIdContext) and Assigned(FIdContext.Connection) and Assigned(FIdContext.Connection.IOHandler) then
    TDataLoggerSocketConnectionHandler(FIdContext.Connection.IOHandler).PING;
end;

function TDataLoggerSocketConnection.ReadMessage: TDataLoggerSocketMessage;
begin
  Result.OperationCode := TDataLoggerSocketMessageOperationCode.ERROR;
  Result.Message := '';

  if not Assigned(FIdContext) or not Assigned(FIdContext.Connection) or not Assigned(FIdContext.Connection.IOHandler) then
    Exit;

  FIdContext.Connection.IOHandler.CheckForDataOnSource(50);

  Result := TDataLoggerSocketConnectionHandler(FIdContext.Connection.IOHandler).ReadMessage;
end;

function TDataLoggerSocketConnection.ReadMessage(const ATimeout: Integer): TDataLoggerSocketMessage;
var
  LOldReadTimeout: Integer;
begin
  Result.OperationCode := TDataLoggerSocketMessageOperationCode.ERROR;
  Result.Message := '';

  if not Assigned(FIdContext) or not Assigned(FIdContext.Connection) or not Assigned(FIdContext.Connection.IOHandler) then
    Exit;

  FIdContext.Connection.IOHandler.CheckForDataOnSource(50);

  LOldReadTimeout := FIdContext.Connection.IOHandler.ReadTimeout;
  try
    FIdContext.Connection.IOHandler.ReadTimeout := ATimeout;

    Result := TDataLoggerSocketConnectionHandler(FIdContext.Connection.IOHandler).ReadMessage;

    if FIdContext.Connection.IOHandler.ReadLnTimedout then
      Result.OperationCode := TDataLoggerSocketMessageOperationCode.TIMEOUT;
  finally
    FIdContext.Connection.IOHandler.ReadTimeout := LOldReadTimeout;
  end;
end;

{ TDataLoggerSocketListConnection }

constructor TDataLoggerSocketListConnection.Create(const AIdTCPServer: TIdTCPServer);
begin
  FIdTCPServer := AIdTCPServer;

  FCriticalSection := TCriticalSection.Create;
  FList := TObjectList<TDataLoggerSocketConnection>.Create(True);
end;

destructor TDataLoggerSocketListConnection.Destroy;
begin
  FList.Free;
  FCriticalSection.Free;

  inherited;
end;

function TDataLoggerSocketListConnection.Add(const AValue: TDataLoggerSocketConnection): TDataLoggerSocketListConnection;
begin
  Result := Self;

  Lock;
  try
    FList.Add(AValue);
  finally
    UnLock;
  end;
end;

function TDataLoggerSocketListConnection.Remove(const AValue: TDataLoggerSocketConnection): TDataLoggerSocketListConnection;
begin
  Result := Self;

  Lock;
  try
    FList.Remove(AValue);
    FList.TrimExcess;
  finally
    UnLock;
  end;
end;

function TDataLoggerSocketListConnection.RemoveAll: TDataLoggerSocketListConnection;
begin
  Result := Self;

  Lock;
  try
    FList.Clear;
    FList.TrimExcess;
  finally
    UnLock;
  end;
end;

function TDataLoggerSocketListConnection.Last: TDataLoggerSocketConnection;
begin
  Lock;
  try
    Result := FList.Last;
  finally
    UnLock;
  end;
end;

function TDataLoggerSocketListConnection.List: TObjectList<TDataLoggerSocketConnection>;
begin
  Result := FList;
end;

function TDataLoggerSocketListConnection.ToArray: TArray<TDataLoggerSocketConnection>;
begin
  Lock;
  try
    Result := FList.ToArray;
  finally
    UnLock;
  end;
end;

function TDataLoggerSocketListConnection.IsEquals(const AContext: TIdContext): TDataLoggerSocketConnection;
var
  LConnection: TDataLoggerSocketConnection;
begin
  Lock;
  try
    for LConnection in FList do
      if LConnection.IsEquals(AContext) then
      begin
        Result := LConnection;
        Exit
      end;

    Result := TDataLoggerSocketConnection.Create(FIdTCPServer, AContext);
    FList.Add(Result);
  finally
    UnLock;
  end;
end;

function TDataLoggerSocketListConnection.Count: Integer;
begin
  Lock;
  try
    Result := FList.Count;
  finally
    UnLock;
  end;
end;

function TDataLoggerSocketListConnection.GetFromID(const AID: string): TDataLoggerSocketConnection;
var
  LConnection: TDataLoggerSocketConnection;
begin
  Result := nil;

  Lock;
  try
    for LConnection in FList do
      if LConnection.ID.Equals(AID) then
      begin
        Result := LConnection;
        Break;
      end;
  finally
    UnLock;
  end;
end;

procedure TDataLoggerSocketListConnection.Lock;
begin
  FCriticalSection.Acquire;
end;

procedure TDataLoggerSocketListConnection.UnLock;
begin
  FCriticalSection.Release;
end;

{ TDataLoggerSocketConnectionHandler }

function TDataLoggerSocketConnectionHandler.HandShaked: Boolean;
begin
  Result := (Tag = 1);
end;

function TDataLoggerSocketConnectionHandler.ReadMessage: TDataLoggerSocketMessage;
var
  LReadByte: Byte;
  LByte: array [0 .. 7] of Byte;
  I: Int64;
  LDecodedSize: Int64;
  LMask: array [0 .. 3] of Byte;
  LMessage: TBytes;
begin
  Result.OperationCode := TDataLoggerSocketMessageOperationCode.TIMEOUT;
  Result.Message := '';

  try
    LReadByte := ReadByte;

    case LReadByte of
      129:
        Result.OperationCode := TDataLoggerSocketMessageOperationCode.TEXT_FRAME;

      130:
        Result.OperationCode := TDataLoggerSocketMessageOperationCode.BINARY_FRAME;

      136:
        Result.OperationCode := TDataLoggerSocketMessageOperationCode.CONNECTION_CLOSE;

      137:
        Result.OperationCode := TDataLoggerSocketMessageOperationCode.PING;

      138:
        Result.OperationCode := TDataLoggerSocketMessageOperationCode.PONG;
    else
      Result.OperationCode := TDataLoggerSocketMessageOperationCode.CONTINUE;
    end;

    if LReadByte in [129, 137, 138] then
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

      if (LDecodedSize < 1) then
        Exit;

      LMessage := [];
      SetLength(LMessage, LDecodedSize);

      inherited ReadBytes(TIdBytes(LMessage), LDecodedSize, False);

      for I := 0 to Pred(LDecodedSize) do
        LMessage[I] := LMessage[I] xor LMask[I mod 4];

      Result.Message := IndyTextEncoding_UTF8.GetString(TIdBytes(LMessage));
    end;
  except
  end;
end;

procedure TDataLoggerSocketConnectionHandler.WriteMessage(const AMessage: string);
begin
  WriteMessageBytes(TBytes(IndyTextEncoding_UTF8.GetBytes(AMessage)));
end;

procedure TDataLoggerSocketConnectionHandler.WriteMessageBytes(const AMessage: TBytes);
begin
  Send([$81], AMessage);
end;

procedure TDataLoggerSocketConnectionHandler.PING;
begin
  Send([$89], TBytes(IndyTextEncoding_UTF8.GetBytes('PING')));
end;

procedure TDataLoggerSocketConnectionHandler.Send(const ABytes: TBytes; const AMessage: TBytes);
var
  LBytes: TBytes;
begin
  LBytes := ABytes;

  if (Length(AMessage) <= 125) then
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
