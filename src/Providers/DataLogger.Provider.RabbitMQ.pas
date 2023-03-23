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

// https://www.rabbitmq.com/
// https://github.com/danieleteti/delphistompclient

unit DataLogger.Provider.RabbitMQ;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  StompClient,
  System.SysUtils, System.JSON;

type
  TProviderRabbitMQ = class(TDataLoggerProvider<TProviderRabbitMQ>)
  private
    FHost: string;
    FPort: Integer;
    FVirtualHost: string;
    FClientID: string;
    FTopicName: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Host(const AValue: string): TProviderRabbitMQ;
    function Port(const AValue: Integer): TProviderRabbitMQ;
    function VirtualHost(const AValue: string): TProviderRabbitMQ;
    function ClientID(const AValue: string): TProviderRabbitMQ;
    function TopicName(const AValue: string): TProviderRabbitMQ;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderRabbitMQ }

constructor TProviderRabbitMQ.Create;
begin
  inherited Create;

  Host('127.0.0.1');
  Port(61613);
  VirtualHost('');
  ClientID('DataLogger-RabbitMQ');
  TopicName('datalogger');
end;

function TProviderRabbitMQ.Host(const AValue: string): TProviderRabbitMQ;
begin
  Result := Self;
  FHost := AValue;
end;

function TProviderRabbitMQ.Port(const AValue: Integer): TProviderRabbitMQ;
begin
  Result := Self;
  FPort := AValue;
end;

function TProviderRabbitMQ.VirtualHost(const AValue: string): TProviderRabbitMQ;
begin
  Result := Self;
  FVirtualHost := AValue;
end;

function TProviderRabbitMQ.ClientID(const AValue: string): TProviderRabbitMQ;
begin
  Result := Self;
  FClientID := AValue;
end;

function TProviderRabbitMQ.TopicName(const AValue: string): TProviderRabbitMQ;
begin
  Result := Self;
  FTopicName := AValue;
end;

procedure TProviderRabbitMQ.LoadFromJSON(const AJSON: string);
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
    Host(LJO.GetValue<string>('host', FHost));
    Port(LJO.GetValue<Integer>('port', FPort));
    VirtualHost(LJO.GetValue<string>('virtual_host', FVirtualHost));
    ClientID(LJO.GetValue<string>('client_id', FClientID));
    TopicName(LJO.GetValue<string>('topic_name', FTopicName));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderRabbitMQ.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('host', TJSONString.Create(FHost));
    LJO.AddPair('port', TJSONNumber.Create(FPort));
    LJO.AddPair('virtual_host', TJSONString.Create(FVirtualHost));
    LJO.AddPair('client_id', TJSONString.Create(FClientID));
    LJO.AddPair('topic_name', TJSONString.Create(FTopicName));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderRabbitMQ.Save(const ACache: TArray<TLoggerItem>);
var
  LStompPub: IStompClient;
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
  if (Length(ACache) = 0) then
    Exit;

  LStompPub := StompUtils.StompClientAndConnect(FHost, FPort, FVirtualHost, FClientID);

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := SerializeItem.LogItem(LItem).ToJSON;

    LRetriesCount := 0;

    while True do
      try
        if not LStompPub.Connected then
          LStompPub.Connect;

        LStompPub.Send('/queue/' + FTopicName, LLog);

        Break;
      except
        on E: Exception do
        begin
          if LStompPub.Connected then
            try
              LStompPub.Disconnect;
            except
            end;

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
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderRabbitMQ);

end.
