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
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property KeyPrefix: string read FKeyPrefix write FKeyPrefix;
    property MaxSize: Int64 read FMaxSize write FMaxSize;

    constructor Create(const AHost: string = '127.0.0.1'; const APort: Integer = 6379; const AKeyPrefix: string = 'DataLogger'; const AMaxSize: Int64 = 10000);
  end;

implementation

{ TProviderRedis }

constructor TProviderRedis.Create(const AHost: string = '127.0.0.1'; const APort: Integer = 6379; const AKeyPrefix: string = 'DataLogger'; const AMaxSize: Int64 = 10000);
begin
  inherited Create;

  FHost := AHost;
  FPort := APort;
  FKeyPrefix := AKeyPrefix;
  FMaxSize := AMaxSize;
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

  LKey := FKeyPrefix + '::DataLoggerRedis';

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
