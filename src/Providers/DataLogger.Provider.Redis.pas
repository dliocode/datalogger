{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Redis;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  Redis.Commons, Redis.Client, Redis.NetLib.INDY,
  System.SysUtils;

type
  TProviderRedis = class(TDataLoggerProvider)
  private
    FHost: string;
    FPort: Integer;
    FKeyPrefix: string;
    FMaxSize: Int64;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Host(const AValue: string): TProviderRedis;
    function Port(const AValue: Integer): TProviderRedis;
    function KeyPrefix(const AValue: string): TProviderRedis;
    function MaxSize(const AValue: Int64): TProviderRedis;

    constructor Create;
  end;

implementation

{ TProviderRedis }

constructor TProviderRedis.Create;
begin
  inherited Create;

  Host('127.0.0.1');
  Port(6379);
  KeyPrefix('DataLoggerRedis');
  MaxSize(10000);
end;

function TProviderRedis.Host(const AValue: string): TProviderRedis;
begin
  Result := Self;
  FHost := AValue;
end;

function TProviderRedis.Port(const AValue: Integer): TProviderRedis;
begin
  Result := Self;
  FPort := AValue;
end;

function TProviderRedis.KeyPrefix(const AValue: string): TProviderRedis;
begin
  Result := Self;
  FKeyPrefix := AValue;
end;

function TProviderRedis.MaxSize(const AValue: Int64): TProviderRedis;
begin
  Result := Self;
  FMaxSize := AValue;
end;

procedure TProviderRedis.Save(const ACache: TArray<TLoggerItem>);
var
  LRedisClient: IRedisClient;
  LConnected: Boolean;
  LRetryCount: Integer;
  LKey: string;
  LItem: TLoggerItem;
  LLog: string;
begin
  if Length(ACache) = 0 then
    Exit;

  LKey := FKeyPrefix + '::DataLoggerRedis::';

  LRedisClient := TRedisClient.Create(FHost, FPort);
  LConnected := False;

  for LItem in ACache do
  begin
    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetryCount := 0;

    while True do
      try
        if not LConnected then
        begin
          LRedisClient.Connect;
          LConnected := True;
        end;

        LRedisClient.RPUSH(LKey, [LLog]);

        Break;
      except
        on E: Exception do
        begin
          if LConnected then
          begin
            try
              try
                LRedisClient.Disconnect;
              except
              end;
            finally
              LConnected := False;
            end;
          end;

          Inc(LRetryCount);

          Sleep(50);

          if Assigned(FLogException) then
            FLogException(Self, LItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= FMaxRetry then
            Break;
        end;
      end;
  end;

  try
    LRedisClient.LTRIM(LKey, -FMaxSize, -1);
  except
  end;
end;

end.
