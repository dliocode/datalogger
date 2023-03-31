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

unit DataLogger;

interface

uses
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils, DataLogger.SerializeItem,
  System.Classes, System.SyncObjs, System.Generics.Collections, System.SysUtils, System.JSON, System.DateUtils;

type
  TDataLoggerProviderBase = DataLogger.Provider.TDataLoggerProviderBase;

  TLoggerItem = DataLogger.Types.TLoggerItem;
  TLoggerLevel = DataLogger.Types.TLoggerLevel;
  TLoggerLevels = DataLogger.Types.TLoggerLevels;
  TLoggerTransactionTypeCommit = DataLogger.Types.TLoggerTransactionTypeCommit;
  TLoggerOnException = DataLogger.Types.TLoggerOnException;
  TLoggerTemplate = DataLogger.Types.TLoggerTemplate;
  TLoggerSerializeItem = DataLogger.SerializeItem.TLoggerSerializeItem;

  EDataLoggerException = DataLogger.Types.EDataLoggerException;
  Exception = System.SysUtils.Exception;

  TDataLogger = class
  private
    FCriticalSection: TCriticalSection;
    FSemaphore: TSemaphore;
    FLocked: Boolean;

    FEvent: TEvent;
    FThreadExecute: TThread;
    FThreadTerminated: Boolean;
    FLoggerItems: TList<TLoggerItem>;
    FProviders: TObjectList<TDataLoggerProviderBase>;
    FLevel: TLoggerLevel;
    FDisableLevel: TLoggerLevels;
    FOnlyLevel: TLoggerLevels;
    FName: string;
    FTagNameIsRequired: Boolean;
    FGenerateLogWithoutProvider: Boolean;
    FSequence: UInt64;
    FLiveMode: Boolean;
    FHasProvider: Boolean;
    FLevelName: TDictionary<TLoggerLevel, string>;

    constructor Create;

    function AddCacheBase(const ALevel: TLoggerLevel; const AMessageString: string; const AMessageJSON: string; const ATagName: string; const ACustom: string; const AIsSlinebreak: Boolean; const AIsUndoLast: Boolean): TDataLogger; overload;
    function AddCache(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function AddCache(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;
    function ExtractCache(const AUseLock: Boolean = True): TArray<TLoggerItem>;
    procedure SaveForced(const AUseLock: Boolean = True);
    procedure CloseProvider;
    function GetProviders(const AUseLock: Boolean = True): TArray<TDataLoggerProviderBase>;
    procedure Start;
    procedure NotifyEvent(const AUseLock: Boolean = True);
    function HasProvider(const AUseLock: Boolean = True): Boolean;
    procedure Lock;
    procedure UnLock;
    procedure LockCache;
    procedure UnLockCache;

    class constructor Create;
    class destructor Destroy;
  public
    function AddProvider(const AProviders: TArray<TDataLoggerProviderBase>): TDataLogger; overload;
    function AddProvider(const AProvider: TDataLoggerProviderBase): TDataLogger; overload;
    function RemoveProvider(const AProvider: TDataLoggerProviderBase): TDataLogger;
    function SetProvider(const AProviders: TArray<TDataLoggerProviderBase>): TDataLogger;
    function ProviderCount: Integer;

    function Trace(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Trace(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Trace(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Debug(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Debug(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Debug(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Info(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Info(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Info(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Success(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Success(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Success(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Warn(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Warn(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Warn(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Error(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Error(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Error(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Fatal(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Fatal(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Fatal(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Custom(const ALevel: string; const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Custom(const ALevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Custom(const ALevel: string; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Log(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Log(const ALevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Log(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function T(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function T(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function T(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function D(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function D(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function D(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function I(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function I(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function I(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function S(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function S(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function S(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function W(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function W(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function W(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function E(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function E(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function E(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function F(const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function F(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function F(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function C(const ALevel: string; const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function C(const ALevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function C(const ALevel: string; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function L(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function L(const ALevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function L(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function SlineBreak: TDataLogger;
    function UndoLast: TDataLogger;

    function StartTransaction: TDataLogger;
    function CommitTransaction: TDataLogger;
    function RollbackTransaction: TDataLogger;
    function InTransaction: Boolean;

    function SetTemplate(const ATemplate: string; const AForced: Boolean = False): TDataLogger; overload;
    function SetTemplate(const ATemplate: string; AArgs: array of const; const AForced: Boolean = False): TDataLogger; overload;
    function SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
    function SetLevelName(const ALevel: TLoggerLevel; const AName: string): TDataLogger;
    function SetLevel(const ALevel: TLoggerLevel): TDataLogger;
    function SetDisableLevel(const ALevels: TLoggerLevels): TDataLogger;
    function SetOnlyLevel(const ALevels: TLoggerLevels): TDataLogger;
    function SetLogException(const AException: TLoggerOnException): TDataLogger;
    function SetMaxRetries(const AMaxRetries: Integer): TDataLogger;
    function SetIgnoreTemplate(const AIgnoreTemplate: Boolean; const ASeparator: string = ' '; const AIncludeKey: Boolean = False; const AIncludeKeySeparator: string = ' -> '): TDataLogger;
    function SetName(const AName: string): TDataLogger;
    function SetLiveMode(const ALiveMode: Boolean): TDataLogger;
    function SetTagNameIsRequired(const ATagNameIsRequired: Boolean): TDataLogger;
    function SetGenerateLogWithoutProvider(const AGenerateLogWithoutProvider: Boolean): TDataLogger;

    function Clear: TDataLogger;
    function CountLogInCache: Int64;
    function LoadFromJSON(const AJSON: string): Boolean;
    function ToJSON(const AFormat: Boolean = False): string;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function Builder: TDataLogger;
  end;

function Logger: TDataLogger;

implementation

type
  TThreadExecute = class(TThread);

const
  C_MAX_LOG_IN_CACHE = 200000;

var
  FLoggerDefault: TDataLogger;

function Logger: TDataLogger;
begin
  if not Assigned(FLoggerDefault) then
    FLoggerDefault := TDataLogger.Builder;

  Result := FLoggerDefault;
end;

{ TDataLogger }

class constructor TDataLogger.Create;
begin
  FLoggerDefault := nil;
end;

class destructor TDataLogger.Destroy;
begin
  FLoggerDefault.Free;
  FLoggerDefault := nil;
end;

class function TDataLogger.Builder: TDataLogger;
begin
  Result := TDataLogger.Create;
end;

constructor TDataLogger.Create;
begin

end;

procedure TDataLogger.AfterConstruction;
var
  LLevel: TLoggerLevel;
begin
  inherited;

  FCriticalSection := TCriticalSection.Create;
  FSemaphore := TSemaphore.Create(nil, 1, 1, '');
  FLocked := False;

  FEvent := TEvent.Create;
  FLoggerItems := TList<TLoggerItem>.Create;
  FProviders := TObjectList<TDataLoggerProviderBase>.Create(True);

  SetLevel(TLoggerLevel.All);
  SetDisableLevel([]);
  SetOnlyLevel([TLoggerLevel.All]);
  SetName('');
  SetTagNameIsRequired(False);
  SetGenerateLogWithoutProvider(True);
  FLiveMode := IsLibrary or ModuleIsLib;

  FHasProvider := False;
  FSequence := 0;

  FThreadExecute := nil;
  FThreadTerminated := False;
  FLevelName := TDictionary<TLoggerLevel, string>.Create;
  for LLevel := Low(TLoggerLevel) to High(TLoggerLevel) do
    FLevelName.Add(LLevel, LLevel.ToString);
end;

procedure TDataLogger.BeforeDestruction;
begin
  SetDisableLevel([TLoggerLevel.All]);

  if Assigned(FThreadExecute) then
  begin
    FThreadExecute.Terminate;
    FThreadTerminated := True;
    NotifyEvent;
    FThreadExecute.WaitFor;
    FThreadExecute.Free;
  end
  else
    if FLiveMode then
      NotifyEvent;

  CloseProvider;

  Lock;
  try
    FProviders.Free;
    FLoggerItems.Free;
    FEvent.Free;
  finally
    UnLock;
  end;

  FLevelName.Free;

  FSemaphore.Free;
  FCriticalSection.Free;

  inherited;
end;

function TDataLogger.AddProvider(const AProviders: TArray<TDataLoggerProviderBase>): TDataLogger;
begin
  Result := Self;

  if (Length(AProviders) = 0) then
    Exit;

  Lock;
  try
    FProviders.AddRange(AProviders);
    FHasProvider := True;
  finally
    UnLock;
  end;
end;

function TDataLogger.AddProvider(const AProvider: TDataLoggerProviderBase): TDataLogger;
begin
  Result := AddProvider([AProvider]);
end;

function TDataLogger.RemoveProvider(const AProvider: TDataLoggerProviderBase): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FProviders.Remove(AProvider);
    FProviders.TrimExcess;
    FHasProvider := FProviders.Count > 0;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetProvider(const AProviders: TArray<TDataLoggerProviderBase>): TDataLogger;
var
  LItem: TDataLoggerProviderBase;
begin
  Result := Self;

  Lock;
  try
    FProviders.Clear;
    FProviders.TrimExcess;
    FHasProvider := FProviders.Count > 0;
  finally
    UnLock;
  end;

  for LItem in AProviders do
    AddProvider(LItem);
end;

function TDataLogger.ProviderCount: Integer;
begin
  Lock;
  try
    Result := FProviders.Count;
  finally
    UnLock;
  end;
end;

function TDataLogger.Trace(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Trace, AMessage, ATagName);
end;

function TDataLogger.Trace(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Trace, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Trace(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Trace, AMessage, ATagName);
end;

function TDataLogger.Debug(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Debug, AMessage, ATagName);
end;

function TDataLogger.Debug(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Debug, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Debug(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Debug, AMessage, ATagName);
end;

function TDataLogger.Info(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Info, AMessage, ATagName);
end;

function TDataLogger.Info(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Info, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Info(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Info, AMessage, ATagName);
end;

function TDataLogger.Success(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Success, AMessage, ATagName);
end;

function TDataLogger.Success(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Success, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Success(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Success, AMessage, ATagName);
end;

function TDataLogger.Warn(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Warn, AMessage, ATagName);
end;

function TDataLogger.Warn(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Warn, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Warn(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Warn, AMessage, ATagName);
end;

function TDataLogger.Error(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Error, AMessage, ATagName);
end;

function TDataLogger.Error(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Error, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Error(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Error, AMessage, ATagName);
end;

function TDataLogger.Fatal(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Fatal, AMessage, ATagName);
end;

function TDataLogger.Fatal(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Fatal, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Fatal(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Fatal, AMessage, ATagName);
end;

function TDataLogger.Custom(const ALevel: string; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCacheBase(TLoggerLevel.Custom, AMessage, '', ATagName, ALevel, False, False);
end;

function TDataLogger.Custom(const ALevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCacheBase(TLoggerLevel.Custom, Format(AMessage, AArgs), '', ATagName, ALevel, False, False);
end;

function TDataLogger.Custom(const ALevel: string; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCacheBase(TLoggerLevel.Custom, '', AMessage.ToString, ATagName, ALevel, False, False);
end;

function TDataLogger.Log(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(ALevel, AMessage, ATagName);
end;

function TDataLogger.Log(const ALevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(ALevel, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Log(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(ALevel, AMessage, ATagName);
end;

function TDataLogger.T(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Trace(AMessage, ATagName);
end;

function TDataLogger.T(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Trace(AMessage, AArgs, ATagName);
end;

function TDataLogger.T(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Trace(AMessage, ATagName);
end;

function TDataLogger.D(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Debug(AMessage, ATagName);
end;

function TDataLogger.D(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Debug(AMessage, AArgs, ATagName);
end;

function TDataLogger.D(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Debug(AMessage, ATagName);
end;

function TDataLogger.I(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Info(AMessage, ATagName);
end;

function TDataLogger.I(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Info(AMessage, AArgs, ATagName);
end;

function TDataLogger.I(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Info(AMessage, ATagName);
end;

function TDataLogger.S(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Success(AMessage, ATagName);
end;

function TDataLogger.S(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Success(AMessage, AArgs, ATagName);
end;

function TDataLogger.S(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Success(AMessage, ATagName);
end;

function TDataLogger.W(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Warn(AMessage, ATagName);
end;

function TDataLogger.W(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Warn(AMessage, AArgs, ATagName);
end;

function TDataLogger.W(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Warn(AMessage, ATagName);
end;

function TDataLogger.E(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Error(AMessage, ATagName);
end;

function TDataLogger.E(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Error(AMessage, AArgs, ATagName);
end;

function TDataLogger.E(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Error(AMessage, ATagName);
end;

function TDataLogger.F(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Fatal(AMessage, ATagName);
end;

function TDataLogger.F(const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Fatal(AMessage, AArgs, ATagName);
end;

function TDataLogger.F(const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Fatal(AMessage, ATagName);
end;

function TDataLogger.C(const ALevel: string; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Custom(ALevel, AMessage, ATagName);
end;

function TDataLogger.C(const ALevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Custom(ALevel, AMessage, AArgs, ATagName);
end;

function TDataLogger.C(const ALevel: string; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Custom(ALevel, AMessage, ATagName);
end;

function TDataLogger.L(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := Log(ALevel, AMessage, ATagName);
end;

function TDataLogger.L(const ALevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger;
begin
  Result := Log(ALevel, AMessage, AArgs, ATagName);
end;

function TDataLogger.L(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := Log(ALevel, AMessage, ATagName);
end;

function TDataLogger.SlineBreak: TDataLogger;
begin
  Result := AddCacheBase(TLoggerLevel.All, '', '', '', '', True, False);
end;

function TDataLogger.UndoLast: TDataLogger;
begin
  Result := AddCacheBase(TLoggerLevel.All, '', '', '', '', False, True);
end;

function TDataLogger.StartTransaction: TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    Exit;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).StartTransaction(LID);
end;

function TDataLogger.CommitTransaction: TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    Exit;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).CommitTransaction(LID);
end;

function TDataLogger.RollbackTransaction: TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    Exit;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).RollbackTransaction(LID);
end;

function TDataLogger.InTransaction: Boolean;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  LID: string;
  LProvider: TDataLoggerProviderBase;
begin
  Result := False;

  if not HasProvider then
    Exit;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;
  for LProvider in LProviders do
  begin
    Result := TDataLoggerProvider<TDataLoggerProviderBase>(LProvider).InTransaction(LID);
    if Result then
      Break;
  end;
end;

function TDataLogger.SetTemplate(const ATemplate: string; const AForced: Boolean = False): TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    raise EDataLoggerException.Create('Not defined Provider!');

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).SetTemplate(ATemplate, AForced);
end;

function TDataLogger.SetTemplate(const ATemplate: string; AArgs: array of const; const AForced: Boolean = False): TDataLogger;
begin
  Result := SetTemplate(Format(ATemplate, AArgs), AForced);
end;

function TDataLogger.SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    raise EDataLoggerException.Create('Not defined Provider!');

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).SetFormatTimestamp(AFormatTimestamp);
end;

function TDataLogger.SetLevelName(const ALevel: TLoggerLevel; const AName: string): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FLevelName.Items[ALevel] := AName;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetLevel(const ALevel: TLoggerLevel): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FLevel := ALevel;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetDisableLevel(const ALevels: TLoggerLevels): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FDisableLevel := ALevels;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetOnlyLevel(const ALevels: TLoggerLevels): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FOnlyLevel := ALevels;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetLogException(const AException: TLoggerOnException): TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    raise EDataLoggerException.Create('Not defined Provider!');

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).SetLogException(AException);
end;

function TDataLogger.SetMaxRetries(const AMaxRetries: Integer): TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    raise EDataLoggerException.Create('Not defined Provider!');

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).SetMaxRetries(AMaxRetries);
end;

function TDataLogger.SetIgnoreTemplate(const AIgnoreTemplate: Boolean; const ASeparator: string; const AIncludeKey: Boolean; const AIncludeKeySeparator: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    raise EDataLoggerException.Create('Not defined Provider!');

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).SetIgnoreTemplate(AIgnoreTemplate, ASeparator, AIncludeKey, AIncludeKeySeparator);
end;

function TDataLogger.SetName(const AName: string): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FName := AName;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetLiveMode(const ALiveMode: Boolean): TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  Result := Self;

  if not HasProvider then
    raise EDataLoggerException.Create('Not defined Provider!');

  Lock;
  try
    if (IsLibrary or ModuleIsLib) then
      FLiveMode := True
    else
      FLiveMode := ALiveMode;
  finally
    UnLock;
  end;

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).SetLiveMode(ALiveMode);
end;

function TDataLogger.SetTagNameIsRequired(const ATagNameIsRequired: Boolean): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FTagNameIsRequired := ATagNameIsRequired;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetGenerateLogWithoutProvider(const AGenerateLogWithoutProvider: Boolean): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FGenerateLogWithoutProvider := AGenerateLogWithoutProvider;
  finally
    UnLock;
  end;
end;

function TDataLogger.Clear: TDataLogger;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  Result := Self;

  Lock;
  try
    FLoggerItems.Clear;
    FLoggerItems.TrimExcess;
  finally
    UnLock;
  end;

  LProviders := GetProviders;
  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).Clear;
end;

function TDataLogger.CountLogInCache: Int64;
begin
  Lock;
  try
    Result := FLoggerItems.Count;
  finally
    UnLock;
  end;
end;

function TDataLogger.LoadFromJSON(const AJSON: string): Boolean;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  LJSON: string;
  LJV: TJSONValue;
  LJO: TJSONObject;
  LJA: TJSONArray;
  LJAName: string;
  LProvider: TDataLoggerProviderBase;
  I: Integer;
  J: Integer;
begin
  Result := False;

  LJSON := AJSON.Replace(#$D#$A, '');

  if AJSON.Trim.IsEmpty then
    Exit;

  if (AJSON.Trim = '{}') then
    Exit;

  try
    LJV := TJSONObject.ParseJSONValue(LJSON);

    if not(LJV is TJSONObject) then
    begin
      LJV.Free;
      Exit;
    end;

    LJO := LJV as TJSONObject;
  except
    on E: Exception do
      raise EDataLoggerException.Create('JSON invalid in LoadFromJSON!');
  end;

  if not Assigned(LJO) then
    Exit;

  Result := True;

  LProviders := GetProviders;

  try
    for I := Pred(LJO.Count) downto 0 do
    begin
      if not(LJO.Pairs[I].JsonValue is TJSONArray) then
        Continue;

      LJA := LJO.Pairs[I].JsonValue as TJSONArray;
      LJAName := LJO.Pairs[I].JsonString.Value;

      if (LJA.Count = 0) then
        Continue;

      for LProvider in LProviders do
        if (LProvider.ClassName.ToLower = LJAName.ToLower) then
        begin
          LProvider.LoadFromJSON(LJA.Items[0].ToString);
          LJA.Remove(0).Free;

          if (LJA.Count = 0) then
            Break;
        end;

      if (LJA.Count = 0) then
        LJO.RemovePair(LJAName).Free;
    end;

    if (LJO.Count = 0) then
      Exit;

    Lock;
    try
      for I := 0 to Pred(LJO.Count) do
      begin
        LJA := LJO.Pairs[I].JsonValue as TJSONArray;
        LJAName := LJO.Pairs[I].JsonString.Value;

        for J := 0 to Pred(LJA.Count) do
        begin
          LProvider := TLoggerRTTI.CreateObject(LJAName) as TDataLoggerProviderBase;

          if not Assigned(LProvider) then
            Continue;

          try
            LProvider.LoadFromJSON(LJA.Items[J].ToString);
          except
            on E: Exception do
            begin
              LProvider.Free;
              Continue;
            end;
          end;

          AddProvider(LProvider);
        end;
      end;
    finally
      UnLock;
    end;
  finally
    LJO.Free;
  end;
end;

function TDataLogger.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
  LJA: TJSONArray;
  LProviders: TArray<TDataLoggerProviderBase>;
  LProvider: TDataLoggerProviderBase;
begin
  if not HasProvider then
    Exit('{}');

  Lock;
  try
    LJO := TJSONObject.Create;
    try
      LProviders := GetProviders(False);
      for LProvider in LProviders do
      begin
        if not Assigned(LJO.Get(LProvider.ClassName)) then
        begin
          LJA := TJSONArray.Create;
          LJO.AddPair(LProvider.ClassName, LJA);
        end
        else
          LJA := LJO.Get(LProvider.ClassName).JsonValue as TJSONArray;

        LJA.Add(TJSONObject.ParseJSONValue(LProvider.ToJSON) as TJSONObject);
      end;

      Result := TLoggerJSON.Format(LJO, AFormat);
    finally
      LJO.Free;
    end;
  finally
    UnLock;
  end;
end;

function TDataLogger.AddCacheBase(const ALevel: TLoggerLevel; const AMessageString: string; const AMessageJSON: string; const ATagName: string; const ACustom: string; const AIsSlinebreak: Boolean; const AIsUndoLast: Boolean): TDataLogger;
var
  LItem: TLoggerItem;
  LMessage: string;
begin
  Result := Self;

  if Assigned(FThreadExecute) then
    if FThreadTerminated then
      Exit;

  if FTagNameIsRequired and not AIsSlinebreak then
    if ATagName.Trim.IsEmpty then
    begin
      LMessage := AMessageString;
      if AMessageString.Trim.IsEmpty then
        LMessage := AMessageJSON;

      raise EDataLoggerException.CreateFmt('Tag name is required in message "%s"!', [LMessage]);
    end;

  LockCache;
  Lock;
  try
    if not HasProvider(False) and not FGenerateLogWithoutProvider then
      Exit;

    if (not AIsSlinebreak) and (not AIsUndoLast) then
    begin
      if (TLoggerLevel.All in FDisableLevel) or (ALevel in FDisableLevel) then
        Exit;

      if not(TLoggerLevel.All in FOnlyLevel) and not(ALevel in FOnlyLevel) then
        Exit;

      if (Ord(FLevel) > Ord(ALevel)) then
        Exit;

      if not(ALevel = TLoggerLevel.All) then
      begin
        if (FSequence = 18446744073709551615) then
          FSequence := 0;

        Inc(FSequence);
      end;
    end;

    LItem := default (TLoggerItem);
    LItem.Id := TGUID.NewGuid.ToString;
    LItem.Name := FName;
    LItem.Sequence := FSequence;
    LItem.TimeStamp := Now;
    LItem.ThreadID := TThread.Current.ThreadID;
    LItem.Level := ALevel;

    LItem.LevelString := ACustom;
    if LItem.LevelString.Trim.IsEmpty then
      LItem.LevelString := FLevelName.Items[ALevel];

    LItem.LevelValue := Ord(ALevel);
    LItem.Tag := ATagName;
    LItem.Message := AMessageString;
    LItem.MessageJSON := AMessageJSON;

    LItem.InternalItem.IsSlinebreak := AIsSlinebreak;
    LItem.InternalItem.IsUndoLast := AIsUndoLast;
    LItem.InternalItem.TransactionID := LItem.ThreadID.ToString;

    FLoggerItems.Add(LItem);

    NotifyEvent(False);
  finally
    UnLock;
    UnLockCache;
  end;
end;

function TDataLogger.AddCache(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCacheBase(ALevel, AMessage, '', ATagName, '', False, False);
end;

function TDataLogger.AddCache(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCacheBase(ALevel, '', AMessage.ToString, ATagName, '', False, False);
end;

function TDataLogger.ExtractCache(const AUseLock: Boolean = True): TArray<TLoggerItem>;
begin
  if AUseLock then
    Lock;
  try
    Result := FLoggerItems.ToArray;
    FLoggerItems.Clear;
    FLoggerItems.TrimExcess;
  finally
    if AUseLock then
      UnLock;
  end;
end;

procedure TDataLogger.SaveForced(const AUseLock: Boolean = True);
var
  LProviders: TArray<TDataLoggerProviderBase>;
  LCache: TArray<TLoggerItem>;
  I: Integer;
begin
  if AUseLock then
    Lock;
  try
    LCache := ExtractCache(False);
    if (Length(LCache) = 0) then
      Exit;

    LProviders := GetProviders(False);
    for I := Low(LProviders) to High(LProviders) do
      TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).AddCache(LCache);
  finally
    if AUseLock then
      UnLock;
  end;
end;

procedure TDataLogger.CloseProvider;
var
  LProviders: TArray<TDataLoggerProviderBase>;
  I: Integer;
begin
  if not HasProvider then
    Exit;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).NotifyEvent;
end;

function TDataLogger.GetProviders(const AUseLock: Boolean = True): TArray<TDataLoggerProviderBase>;
begin
  if AUseLock then
    Lock;
  try
    Result := FProviders.ToArray;
  finally
    if AUseLock then
      UnLock;
  end;
end;

procedure TDataLogger.Start;
begin
  FThreadExecute :=
    TThreadExecute.CreateAnonymousThread(
    procedure
    var
      LProviders: TArray<TDataLoggerProviderBase>;
      LCache: TArray<TLoggerItem>;
      I: Integer;
    begin
      while not FThreadTerminated do
      begin
        UnLockCache;

        FEvent.WaitFor(INFINITE);
        FEvent.ResetEvent;

        if not HasProvider then
          Continue;

        LCache := ExtractCache;
        if (Length(LCache) = 0) then
          Continue;

        UnLockCache;

        LProviders := GetProviders;
        for I := Low(LProviders) to High(LProviders) do
          TDataLoggerProvider<TDataLoggerProviderBase>(LProviders[I]).AddCache(LCache);
      end;
    end);

{$WARN SYMBOL_PLATFORM OFF}
{$IF DEFINED(MSWINDOWS)}
  FThreadExecute.Priority := TThreadPriority.tpHighest;
{$ENDIF}
{$WARN SYMBOL_PLATFORM ON}
  FThreadExecute.FreeOnTerminate := False;
  FThreadExecute.Start;
end;

procedure TDataLogger.NotifyEvent(const AUseLock: Boolean = True);
begin
  if AUseLock then
    Lock;
  try
    if not HasProvider(False) then
      Exit;

    if FLiveMode then
      SaveForced(False)
    else
    begin
      if not Assigned(FThreadExecute) then
        Start;

      FEvent.SetEvent;
    end;
  finally
    if AUseLock then
      UnLock;
  end;
end;

function TDataLogger.HasProvider(const AUseLock: Boolean = True): Boolean;
begin
  if AUseLock then
    Lock;
  try
    Result := FHasProvider;
  finally
    if AUseLock then
      UnLock;
  end;
end;

procedure TDataLogger.Lock;
begin
  FCriticalSection.Acquire;
end;

procedure TDataLogger.UnLock;
begin
  FCriticalSection.Release;
end;

procedure TDataLogger.LockCache;
var
  LCount: Int64;
begin
  LCount := CountLogInCache;

  if LCount >= C_MAX_LOG_IN_CACHE then
  begin
    FSemaphore.Acquire;
    FLocked := True;
  end;
end;

procedure TDataLogger.UnLockCache;
var
  LCount: Int64;
begin
  LCount := CountLogInCache;

  if (LCount < C_MAX_LOG_IN_CACHE) and (FLocked) then
  begin
    FSemaphore.Release;
    FLocked := False;
  end;
end;

end.
