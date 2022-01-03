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

  TDataLogger = class sealed(TThread)
  strict private
    FCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FListLoggerItem: TList<TLoggerItem>;
    FListProviders: TObjectList<TDataLoggerProvider>;
    FLogLevel: TLoggerType;
    FDisableLogType: TLoggerTypes;
    FOnlyLogType: TLoggerTypes;
    FSequence: UInt64;
    function AddCache(const AType: TLoggerType; const AMessageString: string; const AMessageJSON: string; const ATag: string): TDataLogger; overload;
    function AddCache(const AType: TLoggerType; const AMessage: string; const ATag: string): TDataLogger; overload;
    function AddCache(const AType: TLoggerType; const AMessage: TJsonObject; const ATag: string): TDataLogger; overload;
    function ExtractCache: TArray<TLoggerItem>;
    procedure CloseProvider;
    procedure CheckProviders;
    function GetProviders: TArray<TDataLoggerProvider>;
  protected
    procedure Execute; override;
  public
    function AddProvider(const AProvider: TDataLoggerProvider): TDataLogger;
    function SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;

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

    function SetLogFormat(const ALogFormat: string): TDataLogger;

    function SetLogLevel(const ALogLevel: TLoggerType): TDataLogger;
    function SetOnlyLogType(const ALogType: TLoggerTypes): TDataLogger;
    function SetDisableLogType(const ALogType: TLoggerTypes): TDataLogger;

    function SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
    function SetLogException(const AException: TOnLogException): TDataLogger;
    function SetMaxRetry(const AMaxRetry: Integer): TDataLogger;

    function Clear: TDataLogger;

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
  NameThreadForDebugging(Self.ClassName);
end;

procedure TDataLogger.AfterConstruction;
begin
  FCriticalSection := TCriticalSection.Create;
  FEvent := TEvent.Create;
  FListLoggerItem := TList<TLoggerItem>.Create;
  FListProviders := TObjectList<TDataLoggerProvider>.Create;

  SetLogLevel(TLoggerType.All);
  SetDisableLogType([]);
  SetOnlyLogType([TLoggerType.All]);

  FSequence := 0;

  Start;
end;

procedure TDataLogger.BeforeDestruction;
begin
  SetDisableLogType([TLoggerType.All]);

  Terminate;
  FEvent.SetEvent;
  WaitFor;

  CloseProvider;

  FListProviders.Free;
  FListLoggerItem.Free;
  FEvent.Free;
  FCriticalSection.Free;
end;

function TDataLogger.AddProvider(const AProvider: TDataLoggerProvider): TDataLogger;
begin
  Result := Self;

  FCriticalSection.Acquire;
  try
    FListProviders.Add(AProvider);
  finally
    FCriticalSection.Release;
  end;
end;

function TDataLogger.SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;
var
  LItem: TDataLoggerProvider;
begin
  Result := Self;

  FCriticalSection.Acquire;
  try
    FListProviders.Clear;
    FListProviders.TrimExcess;
  finally
    FCriticalSection.Release;
  end;

  for LItem in AProviders do
    AddProvider(LItem);
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

function TDataLogger.SetLogFormat(const ALogFormat: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
begin
  Result := Self;

  CheckProviders;

  LProviders := GetProviders;

  TParallel.For(Low(LProviders), High(LProviders),
    procedure(Index: Integer)
    begin
      LProviders[Index].SetLogFormat(ALogFormat);
    end);
end;

function TDataLogger.SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
begin
  Result := Self;

  CheckProviders;

  LProviders := GetProviders;

  TParallel.For(Low(LProviders), High(LProviders),
    procedure(Index: Integer)
    begin
      LProviders[Index].SetFormatTimestamp(AFormatTimestamp);
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
var
  LProviders: TArray<TDataLoggerProvider>;
begin
  Result := Self;

  CheckProviders;

  LProviders := GetProviders;

  TParallel.For(Low(LProviders), High(LProviders),
    procedure(Index: Integer)
    begin
      LProviders[Index].SetLogException(AException);
    end);
end;

function TDataLogger.SetMaxRetry(const AMaxRetry: Integer): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
begin
  Result := Self;

  CheckProviders;

  LProviders := GetProviders;

  TParallel.For(Low(LProviders), High(LProviders),
    procedure(Index: Integer)
    begin
      LProviders[Index].SetMaxRetry(AMaxRetry);
    end);
end;

function TDataLogger.Clear: TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
begin
  Result := Self;

  CheckProviders;

  LProviders := GetProviders;

  FCriticalSection.Acquire;
  try
    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;
  finally
    FCriticalSection.Release;
  end;

  TParallel.For(Low(LProviders), High(LProviders),
    procedure(Index: Integer)
    begin
      LProviders[Index].Clear;
    end);
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

  CheckProviders;

  FCriticalSection.Acquire;
  try
    if (TLoggerType.All in FDisableLogType) or (AType in FDisableLogType) then
      Exit;

    if not(TLoggerType.All in FOnlyLogType) and not(AType in FOnlyLogType) then
      Exit;

    if not(AType in FOnlyLogType) then
      if Ord(FLogLevel) > Ord(AType) then
        Exit;

    if not(AType = TLoggerType.All) then
      DefineSequence;

    LLogItem := default (TLoggerItem);
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

    FListLoggerItem.Add(LLogItem);
  finally
    FCriticalSection.Release;
    FEvent.SetEvent;
  end;
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
  LCache: TArray<TLoggerItem>;
begin
  FCriticalSection.Acquire;
  try
    LCache := FListLoggerItem.ToArray;

    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;
  finally
    FCriticalSection.Release;
  end;

  Result := LCache;
end;

procedure TDataLogger.CloseProvider;
var
  LProviders: TArray<TDataLoggerProvider>;
begin
  CheckProviders;

  LProviders := GetProviders;

  TParallel.For(Low(LProviders), High(LProviders),
    procedure(Index: Integer)
    begin
      LProviders[Index].NotifyEvent;
    end);
end;

procedure TDataLogger.CheckProviders;
var
  LCount: Integer;
begin
  FCriticalSection.Acquire;
  try
    LCount := FListProviders.Count;
  finally
    FCriticalSection.Release;
  end;

  if LCount = 0 then
    raise EDataLoggerException.Create('Provider not defined!');
end;

function TDataLogger.GetProviders: TArray<TDataLoggerProvider>;
var
  LProviders: TArray<TDataLoggerProvider>;
begin
  FCriticalSection.Acquire;
  try
    LProviders := FListProviders.ToArray;
  finally
    FCriticalSection.Release;
  end;

  Result := LProviders;
end;

procedure TDataLogger.Execute;
var
  LWait: TWaitResult;
  LCache: TArray<TLoggerItem>;
  LProviders: TArray<TDataLoggerProvider>;
begin
  while not Terminated do
  begin
    LWait := FEvent.WaitFor(INFINITE);
    FEvent.ResetEvent;

    if LWait = wrSignaled then
    begin
      LCache := ExtractCache;

      if Length(LCache) = 0 then
        Continue;

      LProviders := GetProviders;

      TParallel.For(Low(LProviders), High(LProviders),
        procedure(Index: Integer)
        begin
          LProviders[Index].AddCache(LCache);
        end);
    end;
  end;
end;

initialization

finalization

if Assigned(FLoggerDefault) then
  FLoggerDefault.Free;

end.
