{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger;

interface

uses
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils,
  System.Classes, System.SyncObjs, System.Generics.Collections, System.SysUtils, System.Threading, System.JSON;

type
  TLoggerItem = DataLogger.Types.TLoggerItem;
  TLoggerType = DataLogger.Types.TLoggerType;
  TLoggerTypes = DataLogger.Types.TLoggerTypes;
  TOnLogException = DataLogger.Types.TOnLogException;
  TDataLoggerProvider = DataLogger.Provider.TDataLoggerProvider;

  Exception = System.SysUtils.Exception;
  TFormatSettings = System.SysUtils.TFormatSettings;

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
    function SetFormatSettings(const AFormatSettings: string): TDataLogger;
    function SetLogException(const AException: TOnLogException): TDataLogger;
    function SetMaxRetry(const AMaxRetry: Integer): TDataLogger;

    function Trace(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Trace(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Debug(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Debug(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Info(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Info(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Success(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Success(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Warn(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Warn(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Error(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Error(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function Fatal(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    function Fatal(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    function SlineBreak: TDataLogger;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function Builder: TDataLogger;
  end;

  TLogger = class
  private
    FDataLogger: TDataLogger;
    class var FInstance: TLogger;

    class function New: TLogger;
    class destructor UnInitialize;
  public
    class function Trace(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    class function Trace(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    class function Debug(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    class function Debug(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    class function Info(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    class function Info(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    class function Success(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    class function Success(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    class function Warn(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    class function Warn(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    class function Error(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    class function Error(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    class function Fatal(const AMessage: string; const ATag: string = ''): TDataLogger; overload;
    class function Fatal(const AMessage: TJsonObject; const ATag: string = ''): TDataLogger; overload;
    class function SlineBreak: TDataLogger;

    class function AddProvider(const AProvider: TDataLoggerProvider): TDataLogger;
    class function SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;
    class function SetLogFormat(const ALogFormat: string): TDataLogger;
    class function SetLogLevel(const ALogLevel: TLoggerType): TDataLogger;
    class function SetOnlyLogType(const ALogType: TLoggerTypes): TDataLogger;
    class function SetDisableLogType(const ALogType: TLoggerTypes): TDataLogger;
    class function SetFormatSettings(const AFormatSettings: string): TDataLogger;
    class function SetLogException(const AException: TOnLogException): TDataLogger;
    class function SetMaxRetry(const AMaxRetry: Integer): TDataLogger;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TDataLogger }

class function TDataLogger.Builder: TDataLogger;
begin
  Result := TDataLogger.Create(True);
  Result.FreeOnTerminate := False;
  Result.Start;
  Result.NameThreadForDebugging(Self.ClassName);
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

  TParallel.For(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].SetLogFormat(ALogFormat);
    end);
end;

function TDataLogger.SetFormatSettings(const AFormatSettings: string): TDataLogger;
begin
  Result := Self;

  if Length(FProviders) = 0 then
    raise EDataLoggerException.Create('Provider not defined!');

  TParallel.For(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].SetFormatSettings(AFormatSettings);
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

  TParallel.For(Low(FProviders), High(FProviders),
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

  TParallel.For(Low(FProviders), High(FProviders),
    procedure(Index: Integer)
    begin
      FProviders[Index].SetMaxRetry(AMaxRetry);
    end);
end;

function TDataLogger.Trace(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Trace, AMessage, ATag);
end;

function TDataLogger.Trace(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Trace, AMessage, ATag);
end;

function TDataLogger.Debug(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, AMessage, ATag);
end;

function TDataLogger.Debug(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, AMessage, ATag);
end;

function TDataLogger.Info(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, AMessage, ATag);
end;

function TDataLogger.Info(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, AMessage, ATag);
end;

function TDataLogger.Success(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, AMessage, ATag);
end;

function TDataLogger.Success(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, AMessage, ATag);
end;

function TDataLogger.Warn(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, AMessage, ATag);
end;

function TDataLogger.Warn(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, AMessage, ATag);
end;

function TDataLogger.Error(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, AMessage, ATag);
end;

function TDataLogger.Error(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, AMessage, ATag);
end;

function TDataLogger.Fatal(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Fatal, AMessage, ATag);
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

  DefineSequence;

  LLogItem := Default (TLoggerItem);

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

  FCriticalSection.Enter;
  try
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
  TParallel.For(Low(FProviders), High(FProviders),
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
  while not(Terminated) do
  begin
    LWait := FEvent.WaitFor(INFINITE);
    FEvent.ResetEvent;

    case LWait of
      wrSignaled:
        begin
          FCriticalSection.Enter;
          try
            LCache := ExtractCache;
          finally
            FCriticalSection.Leave;
          end;

          if Length(LCache) = 0 then
            Exit;

          TParallel.For(Low(FProviders), High(FProviders),
            procedure(Index: Integer)
            begin
              FProviders[Index].AddCache(LCache);
            end);
        end;
    else
      Continue;
    end;
  end;
end;

{ TLogger }

class function TLogger.New: TLogger;
begin
  if not Assigned(FInstance) then
    FInstance := TLogger.Create;

  Result := FInstance;
end;

class destructor TLogger.UnInitialize;
begin
  if Assigned(FInstance) then
    FInstance.DisposeOf;
end;

constructor TLogger.Create;
begin
  if Assigned(FInstance) then
    raise Exception.Create('TLogger is already created!');

  FDataLogger := TDataLogger.Builder;
end;

destructor TLogger.Destroy;
begin
  FDataLogger.DisposeOf;
  inherited;
end;

class function TLogger.AddProvider(const AProvider: TDataLoggerProvider): TDataLogger;
begin
  Result := New.FDataLogger.AddProvider(AProvider);
end;

class function TLogger.SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;
begin
  Result := New.FDataLogger.SetProvider(AProviders);
end;

class function TLogger.SetLogFormat(const ALogFormat: string): TDataLogger;
begin
  Result := New.FDataLogger.SetLogFormat(ALogFormat);
end;

class function TLogger.SetLogLevel(const ALogLevel: TLoggerType): TDataLogger;
begin
  Result := New.FDataLogger.SetLogLevel(ALogLevel);
end;

class function TLogger.SetFormatSettings(const AFormatSettings: string): TDataLogger;
begin
  Result := New.FDataLogger.SetFormatSettings(AFormatSettings);
end;

class function TLogger.SetOnlyLogType(const ALogType: TLoggerTypes): TDataLogger;
begin
  Result := New.FDataLogger.SetOnlyLogType(ALogType);
end;

class function TLogger.SetDisableLogType(const ALogType: TLoggerTypes): TDataLogger;
begin
  Result := New.FDataLogger.SetDisableLogType(ALogType);
end;

class function TLogger.SetLogException(const AException: TOnLogException): TDataLogger;
begin
  Result := New.FDataLogger.SetLogException(AException);
end;

class function TLogger.SetMaxRetry(const AMaxRetry: Integer): TDataLogger;
begin
  Result := New.FDataLogger.SetMaxRetry(AMaxRetry);
end;

class function TLogger.Trace(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := New.FDataLogger.Trace(AMessage, ATag);
end;

class function TLogger.Trace(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := New.FDataLogger.Trace(AMessage, ATag);
end;

class function TLogger.Debug(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := New.FDataLogger.Debug(AMessage, ATag);
end;

class function TLogger.Debug(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := New.FDataLogger.Debug(AMessage, ATag);
end;

class function TLogger.Info(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := New.FDataLogger.Info(AMessage, ATag);
end;

class function TLogger.Info(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := New.FDataLogger.Info(AMessage, ATag);
end;

class function TLogger.Success(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := New.FDataLogger.Success(AMessage, ATag);
end;

class function TLogger.Success(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := New.FDataLogger.Success(AMessage, ATag);
end;

class function TLogger.Warn(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := New.FDataLogger.Warn(AMessage, ATag);
end;

class function TLogger.Warn(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := New.FDataLogger.Warn(AMessage, ATag);
end;

class function TLogger.Error(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := New.FDataLogger.Error(AMessage, ATag);
end;

class function TLogger.Error(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := New.FDataLogger.Error(AMessage, ATag);
end;

class function TLogger.Fatal(const AMessage: string; const ATag: string = ''): TDataLogger;
begin
  Result := New.FDataLogger.Fatal(AMessage, ATag);
end;

class function TLogger.Fatal(const AMessage: TJsonObject; const ATag: string): TDataLogger;
begin
  Result := New.FDataLogger.Fatal(AMessage, ATag);
end;

class function TLogger.SlineBreak: TDataLogger;
begin
  Result := New.FDataLogger.SlineBreak;
end;

end.
