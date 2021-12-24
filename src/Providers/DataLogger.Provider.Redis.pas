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
    FKeyPrefix: string;
    FMaxSize: Int64;
    FRedisClient: IRedisClient;
    FConnected: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AHost: string = '127.0.0.1'; const APort: Integer = 6379; const AKeyPrefix: string = 'DataLogger'; const AMaxSize: Int64 = 10000);
    destructor Destroy; override;
  end;

implementation

{ TProviderRedis }

constructor TProviderRedis.Create(const AHost: string = '127.0.0.1'; const APort: Integer = 6379; const AKeyPrefix: string = 'DataLogger'; const AMaxSize: Int64 = 10000);
begin
  inherited Create;

  FRedisClient := TRedisClient.Create(AHost, APort);
  FConnected := False;

  FKeyPrefix := AKeyPrefix;
  FMaxSize := AMaxSize;
end;

destructor TProviderRedis.Destroy;
begin
  inherited;
end;

procedure TProviderRedis.Save(const ACache: TArray<TLoggerItem>);
var
  LRetryCount: Integer;
  LKey: string;
  LItem: TLoggerItem;
  LLog: string;
begin
  if Length(ACache) = 0 then
    Exit;

  LKey := FKeyPrefix + '::DataLogger';

  for LItem in ACache do
  begin
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

    LRetryCount := 0;

    while True do
      try
        if not FConnected then
        begin
          FRedisClient.Connect;
          FConnected := True;
        end;

        FRedisClient.RPUSH(LKey, [LLog]);

        Break;
      except
        on E: Exception do
        begin
          if FConnected then
          begin
            try
              try
                FRedisClient.Disconnect;
              except
              end;
            finally
              FConnected := False;
            end;
          end;

          Inc(LRetryCount);

          if Assigned(LogException) then
            LogException(Self, LItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= GetMaxRetry then
            Break;
        end;
      end;
  end;

  try
    FRedisClient.LTRIM(LKey, -FMaxSize, -1);
  except
  end;
end;

end.
