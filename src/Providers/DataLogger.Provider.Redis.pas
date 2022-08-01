{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://github.com/danieleteti/delphiredisclient

unit DataLogger.Provider.Redis;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  Redis.Commons, Redis.Client, Redis.NetLib.INDY,
  System.SysUtils, System.JSON;

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

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

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

procedure TProviderRedis.LoadFromJSON(const AJSON: string);
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
    KeyPrefix(LJO.GetValue<string>('key_prefix', FKeyPrefix));
    MaxSize(LJO.GetValue<Integer>('max_size', FMaxSize));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderRedis.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('host', FHost);
    LJO.AddPair('port', TJSONNumber.Create(FPort));
    LJO.AddPair('key_prefix', FKeyPrefix);
    LJO.AddPair('max_size', FMaxSize);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderRedis.Save(const ACache: TArray<TLoggerItem>);
var
  LRedisClient: IRedisClient;
  LConnected: Boolean;
  LRetriesCount: Integer;
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
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetriesCount := 0;

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

  try
    LRedisClient.LTRIM(LKey, -FMaxSize, -1);
  except
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderRedis);

end.
