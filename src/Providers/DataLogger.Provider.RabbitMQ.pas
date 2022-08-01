{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://github.com/danieleteti/delphistompclient

unit DataLogger.Provider.RabbitMQ;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  StompClient,
  System.SysUtils, System.JSON;

type
  TProviderRabbitMQ = class(TDataLoggerProvider)
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
    LJO.AddPair('host', FHost);
    LJO.AddPair('port', TJSONNumber.Create(FPort));
    LJO.AddPair('virtual_host', FVirtualHost);
    LJO.AddPair('client_id', FClientID);
    LJO.AddPair('topic_name', FTopicName);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderRabbitMQ.Save(const ACache: TArray<TLoggerItem>);
var
  LStompPub: IStompClient;
  LConnected: Boolean;
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
  if Length(ACache) = 0 then
    Exit;

  LStompPub := StompUtils.StompClientAndConnect(FHost, FPort, FVirtualHost, FClientID);
  LConnected := False;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetriesCount := 0;

    while True do
      try
        if not LConnected then
        begin
          LStompPub.Connect;
          LConnected := True;
        end;

        LStompPub.Send('/queue/' + FTopicName, LLog);

        Break;
      except
        on E: Exception do
        begin
          if LConnected then
          begin
            try
              try
                LStompPub.Disconnect;
              except
              end;
            finally
              LConnected := False;
            end;
          end;

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
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderRabbitMQ);

end.
