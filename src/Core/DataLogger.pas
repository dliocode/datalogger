{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger;

interface

uses
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils, System.Classes,
  System.SyncObjs, System.Generics.Collections, System.SysUtils, System.Threading, System.JSON;

type
  TLoggerItem = DataLogger.Types.TLoggerItem;
  TLoggerType = DataLogger.Types.TLoggerType;
  TLoggerTypes = DataLogger.Types.TLoggerTypes;
  TOnLogException = DataLogger.Types.TOnLogException;
  TDataLoggerProvider = DataLogger.Provider.TDataLoggerProvider;
  TLoggerFormat = DataLogger.Types.TLoggerFormat;
  Exception = System.SysUtils.Exception;

  TDataLogger = class sealed(TThread)
  strict private
    FCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FList: TList<TLoggerItem>;
    FProviders: TArray<TDataLoggerProvider>;
    FLogLevel: TLoggerType;
    FDisableLogType: TLoggerTypes;
    FOnlyLogType: TLoggerTypes;
    FSequence: UInt64;
    function AddCache(const AType: TLoggerType; const AMessageString: string; const AMessageJSON: string; const ATag: string): TDataLogger; overload;
    function AddCache(const AType: TLoggerType; const AMessage: string; const ATag: string): TDataLogger; overload;
    function AddCache(const AType: TLoggerType; const AMessage: TJsonObject; const ATag: string): TDataLogger; overload;
    function ExtractCache: TArray<TLoggerItem>;
    procedure CloseProvider;
  protected
    procedure Execute; override;
  public
    function AddProvider(const AProvider: TDataLoggerProvider): TDataLogger;
    function SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;
    function SetLogFormat(const ALogFormat: string): TDataLogger;
    function SetLogLevel(const ALogLevel: TLoggerType): TDataLogger;
    function SetOnlyLogType(const ALogType: TLoggerTypes): TDataLogger;
    function SetDisableLogType(const ALogType: TLoggerTypes): TDataLogger;
    function SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
    function SetLogException(const AException: TOnLogException): TDataLogger;
    function SetMaxRetry(const AMaxRetry: Integer): TDataLogger;
    function Trace(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Trace(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
    function Trace(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Debug(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Debug(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
    function Debug(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Info(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Info(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
    function Info(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Success(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Success(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
    function Success(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Warn(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Warn(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
    function Warn(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Error(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Error(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
    function Error(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Fatal(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Fatal(const AMessage: string; const AArgs: array of const; const ATag: string = ''): TDataLogger; overload;
    function Fatal(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function SlineBreak: TDataLogger;

    constructor Create; reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function Builder: TDataLogger;
  end;

function Logger: TDataLogger;
function TLogger: TDataLogger; deprecated 'Use Logger - This function will be removed in future versions';

implementation

var
  FLoggerDefault: TDataLogger;

function Logger: TDataLogger;
begin
  if not Assigned(FLoggerDefault) then
    FLoggerDefault := TDataLogger.Builder;

  Result := FLoggerDefault;
end;

function TLogger: TDataLogger;
begin
  Result := Logger;
end;

{ TDataLogger }

class function TDataLogger.Builder: TDataLogger;
begin
  Result := TDataLogger.Create;
end;

constructor TDataLogger.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Start;
  NameThreadForDebugging(Self.ClassName);
end;

procedure TDataLogger.AfterConstruction;
begin
  FCriticalSection := TCriticalSection.Create;
  FEvent := TEvent.Create;
  FList := TList<TLoggerItem>.Create;
  FProviders := [];
  FLogLevel := TLoggerType.All;
  FDisableLogType := [];
  FOnlyLogType := [TLoggerType.All];
  FSequence := 0;
end;

procedure TDataLogger.BeforeDestruction;
begin
  Terminate;
  FEvent.SetEvent;
  WaitFor;
  CloseProvider;
  FList.DisposeOf;
  FEvent.DisposeOf;
  FCriticalSection.DisposeOf;
end;

function TDataLogger.AddProvider(const AProvider: TDataLoggerProvider): TDataLogger;
begin
  Result := Self;
  FProviders := Concat(FProviders, [AProvider]);
end;

function TDataLogger.SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;
begin
  Result := Self;
  FProviders := AProviders;
end;

function TDataLogger.SetLogFormat(const ALogFormat: string): TDataLogger;
begin
  Result := Self;

  if Length(FProviders) = 0 then
    raise EDataLoggerException.Create('Provider not defined!');

  TParallel.for(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].SetLogFormat(ALogFormat);
    end);
end;

function TDataLogger.SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
begin
  Result := Self;

  if Length(FProviders) = 0 then
    raise EDataLoggerException.Create('Provider not defined!');

  TParallel.for(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].SetFormatTimestamp(AFormatTimestamp);
    end);
end;

function TDataLogger.SetLogLevel(const ALogLevel: TLoggerType): TDataLogger;
begin
  Result := Self;
  FLogLevel := ALogLevel;
end;

function TDataLogger.SetDisableLogType(const ALogType: TLoggerTypes): TDataLogger;
begin
  Result := Self;
  FDisableLogType := ALogType;
end;

function TDataLogger.SetOnlyLogType(const ALogType: TLoggerTypes): TDataLogger;
begin
  Result := Self;
  FOnlyLogType := ALogType;
end;

function TDataLogger.SetLogException(const AException: TOnLogException): TDataLogger;
begin
  Result := Self;

  if Length(FProviders) = 0 then
    raise EDataLoggerException.Create('Provider not defined!');

  TParallel.for(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].SetLogException(AException);
    end);
end;

function TDataLogger.SetMaxRetry(const AMaxRetry: Integer): TDataLogger;
begin
  Result := Self;

  if Length(FProviders) = 0 then
    raise EDataLoggerException.Create('Provider not defined!');

  TParallel.for(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].SetMaxRetry(AMaxRetry);
    end);
end;

function TDataLogger.Trace(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Trace, AMessage, ATag);
end;

function TDataLogger.Trace(const AMessage: string; const AArgs: array of const; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Trace, Format(AMessage, AArgs), ATag);
end;

function TDataLogger.Trace(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Trace, AMessage, ATag);
end;

function TDataLogger.Debug(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, AMessage, ATag);
end;

function TDataLogger.Debug(const AMessage: string; const AArgs: array of const; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, Format(AMessage, AArgs), ATag);
end;

function TDataLogger.Debug(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, AMessage, ATag);
end;

function TDataLogger.Info(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, AMessage, ATag);
end;

function TDataLogger.Info(const AMessage: string; const AArgs: array of const; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, Format(AMessage, AArgs), ATag);
end;

function TDataLogger.Info(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, AMessage, ATag);
end;

function TDataLogger.Success(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, AMessage, ATag);
end;

function TDataLogger.Success(const AMessage: string; const AArgs: array of const; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, Format(AMessage, AArgs), ATag);
end;

function TDataLogger.Success(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, AMessage, ATag);
end;

function TDataLogger.Warn(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, AMessage, ATag);
end;

function TDataLogger.Warn(const AMessage: string; const AArgs: array of const; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, Format(AMessage, AArgs), ATag);
end;

function TDataLogger.Warn(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, AMessage, ATag);
end;

function TDataLogger.Error(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, AMessage, ATag);
end;

function TDataLogger.Error(const AMessage: string; const AArgs: array of const; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, Format(AMessage, AArgs), ATag);
end;

function TDataLogger.Error(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, AMessage, ATag);
end;

function TDataLogger.Fatal(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Fatal, AMessage, ATag);
end;

function TDataLogger.Fatal(const AMessage: string; const AArgs: array of const; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Fatal, Format(AMessage, AArgs), ATag);
end;

function TDataLogger.Fatal(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Fatal, AMessage, ATag);
end;

function TDataLogger.SlineBreak: TDataLogger;
begin
  Result := AddCache(TLoggerType.All, '', '');
end;

function TDataLogger.AddCache(const AType: TLoggerType; const AMessageString: string; const AMessageJSON: string; const ATag: string): TDataLogger;

  procedure DefineSequence;
  begin
    if FSequence = 18446744073709551615 then
      FSequence := 0;

    Inc(FSequence);
  end;

var
  LLogItem: TLoggerItem;
begin
  Result := Self;

  if Length(FProviders) = 0 then
    raise EDataLoggerException.Create('Provider not defined!');

  if (TLoggerType.All in FDisableLogType) or (AType in FDisableLogType) then
    Exit;

  if not(TLoggerType.All in FOnlyLogType) and not(AType in FOnlyLogType) then
    Exit;

  if not(AType in FOnlyLogType) then
    if Ord(FLogLevel) > Ord(AType) then
      Exit;

  FCriticalSection.Enter;
  try
    if not (AType = TLoggerType.All) then
      DefineSequence;

    LLogItem := Default(TLoggerItem);
    LLogItem.Sequence := FSequence;
    LLogItem.TimeStamp := Now;
    LLogItem.ThreadID := TThread.Current.ThreadID;
    LLogItem.&Type := AType;
    LLogItem.Tag := ATag;
    LLogItem.Message := AMessageString;
    LLogItem.MessageJSON := AMessageJSON;
    LLogItem.AppName := TLoggerUtils.AppName;
    LLogItem.AppPath := TLoggerUtils.AppPath;
    LLogItem.AppVersion := TLoggerUtils.AppVersion;
    LLogItem.ComputerName := TLoggerUtils.ComputerName;
    LLogItem.Username := TLoggerUtils.Username;
    LLogItem.OSVersion := TLoggerUtils.OS;
    LLogItem.ProcessID := TLoggerUtils.ProcessID.ToString;

    FList.Add(LLogItem);
  finally
    FCriticalSection.Leave;
  end;

  FEvent.SetEvent;
end;

function TDataLogger.AddCache(const AType: TLoggerType; const AMessage: string; const ATag: string): TDataLogger;
begin
  Result := AddCache(AType, AMessage, '', ATag);
end;

function TDataLogger.AddCache(const AType: TLoggerType; const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(AType, '', AMessage.ToString, ATag);
end;

function TDataLogger.ExtractCache: TArray<TLoggerItem>;
var
  LLogCacheArray: TArray<TLoggerItem>;
begin
  FCriticalSection.Enter;
  try
    LLogCacheArray := FList.ToArray;
    FList.Clear;
    FList.TrimExcess;
  finally
    FCriticalSection.Leave;
  end;

  Result := LLogCacheArray;
end;

procedure TDataLogger.CloseProvider;
begin
  TParallel.for(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].DisposeOf;
    end);
end;

procedure TDataLogger.Execute;
var
  LWait: TWaitResult;
  LCache: TArray<TLoggerItem>;
begin
  while not Terminated do
  begin
    LWait := FEvent.WaitFor(INFINITE);
    FEvent.ResetEvent;

    if LWait = wrSignaled then
    begin
      FCriticalSection.Enter;
      try
        LCache := ExtractCache;
      finally
        FCriticalSection.Leave;
      end;

      if Length(LCache) = 0 then
        Continue;

      TParallel.for(Low(FProviders), High(FProviders),
        procedure(Index: Integer)
        begin
          FProviders[Index].AddCache(LCache);
        end);
    end;
  end;
end;

initialization

finalization

if Assigned(FLoggerDefault) then
  FLoggerDefault.DisposeOf;

end.
