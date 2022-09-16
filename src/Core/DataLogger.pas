{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

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
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils,
  System.Classes, System.SyncObjs, System.Generics.Collections, System.SysUtils, System.JSON;

type
  TLoggerItem = DataLogger.Types.TLoggerItem;
  TLoggerLevel = DataLogger.Types.TLoggerLevel;
  TLoggerLevels = DataLogger.Types.TLoggerLevels;
  TLoggerTransactionTypeCommit = DataLogger.Types.TLoggerTransactionTypeCommit;
  TOnLogException = DataLogger.Types.TOnLogException;

  TLoggerFormat = DataLogger.Types.TLoggerFormat;
  EDataLoggerException = DataLogger.Types.EDataLoggerException;

  TDataLogger = class sealed(TThread)
  strict private
    FCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FListLoggerItem: TList<TLoggerItem>;
    FListProviders: TObjectList<TThread>;
    FLogLevel: TLoggerLevel;
    FDisableLogLevel: TLoggerLevels;
    FOnlyLogLevel: TLoggerLevels;
    FSequence: UInt64;
    FName: string;
    FTagNameIsRequired: Boolean;

    constructor Create;
    procedure Start;

    function AddCache(const ALevel: TLoggerLevel; const AMessageString: string; const AMessageJSON: string; const ATagName: string; const ACustom: string; const ALevelSlineBreak: Boolean): TDataLogger; overload;
    function AddCache(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string): TDataLogger; overload;
    function AddCache(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string): TDataLogger; overload;
    function ExtractCache: TArray<TLoggerItem>;
    procedure SaveForced;
    procedure CloseProvider;
    function GetProviders: TArray<TThread>;
    procedure Lock;
    procedure UnLock;
  protected
    procedure Execute; override;
  public
    function AddProvider(const AProviders: TArray<TThread>): TDataLogger; overload;
    function AddProvider(const AProvider: TThread): TDataLogger; overload;
    function RemoveProvider(const AProvider: TThread): TDataLogger;
    function SetProvider(const AProviders: TArray<TThread>): TDataLogger;

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

    function SlineBreak: TDataLogger;

    function StartTransaction: TDataLogger;
    function CommitTransaction: TDataLogger;
    function RollbackTransaction: TDataLogger;
    function InTransaction: Boolean;

    function SetLogFormat(const ALogFormat: string): TDataLogger;
    function SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
    function SetLogLevel(const ALogLevel: TLoggerLevel): TDataLogger;
    function SetDisableLogLevel(const ALogLevels: TLoggerLevels): TDataLogger;
    function SetOnlyLogLevel(const ALogLevels: TLoggerLevels): TDataLogger;
    function SetLogException(const AException: TOnLogException): TDataLogger;
    function SetMaxRetries(const AMaxRetries: Integer): TDataLogger;
    function SetInitialMessage(const AMessage: string): TDataLogger;
    function SetFinalMessage(const AMessage: string): TDataLogger;
    function SetIgnoreLogFormat(const AIgnoreLogFormat: Boolean; const ASeparator: string = ' '; const AIncludeKey: Boolean = False; const AIncludeKeySeparator: string = ' -> '): TDataLogger;
    function SetName(const AName: string): TDataLogger;
    function SetTagNameIsRequired(const ATagNameIsRequired: Boolean): TDataLogger;

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

var
  FLoggerDefault: TDataLogger;

function Logger: TDataLogger;
begin
  if not Assigned(FLoggerDefault) then
    FLoggerDefault := TDataLogger.Builder;

  Result := FLoggerDefault;
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
end;

procedure TDataLogger.AfterConstruction;
begin
  inherited;

  FCriticalSection := TCriticalSection.Create;

  FEvent := TEvent.Create;
  FListLoggerItem := TList<TLoggerItem>.Create;
  FListProviders := TObjectList<TThread>.Create(True);

  SetLogLevel(TLoggerLevel.All);
  SetDisableLogLevel([]);
  SetOnlyLogLevel([TLoggerLevel.All]);
  SetName('');
  SetTagNameIsRequired(False);

  FSequence := 0;

  Start;
end;

procedure TDataLogger.BeforeDestruction;
begin
  SetDisableLogLevel([TLoggerLevel.All]);

  Terminate;
  FEvent.SetEvent;
  WaitFor;

  CloseProvider;

  Lock;
  try
    FListProviders.Free;
    FListLoggerItem.Free;
    FEvent.Free;
  finally
    UnLock;
  end;

  FCriticalSection.Free;

  inherited;
end;

procedure TDataLogger.Start;
begin
  inherited Start;
end;

procedure TDataLogger.Execute;
var
  LCache: TArray<TLoggerItem>;
  LProviders: TArray<TThread>;
  I: Integer;
begin
  while not Terminated do
  begin
    FEvent.WaitFor(INFINITE);
    FEvent.ResetEvent;

    LProviders := GetProviders;
    if Length(LProviders) = 0 then
      Continue;

    LCache := ExtractCache;
    if Length(LCache) = 0 then
      Continue;

    for I := Low(LProviders) to High(LProviders) do
      TDataLoggerProvider<TObject>(LProviders[I]).AddCache(LCache);
  end;
end;

function TDataLogger.AddProvider(const AProviders: TArray<TThread>): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FListProviders.AddRange(AProviders);
  finally
    UnLock;
  end;
end;

function TDataLogger.AddProvider(const AProvider: TThread): TDataLogger;
begin
  Result := AddProvider([AProvider]);
end;

function TDataLogger.RemoveProvider(const AProvider: TThread): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FListProviders.Remove(AProvider);
  finally
    UnLock;
  end;
end;

function TDataLogger.SetProvider(const AProviders: TArray<TThread>): TDataLogger;
var
  LItem: TThread;
begin
  Result := Self;

  Lock;
  try
    FListProviders.Clear;
    FListProviders.TrimExcess;
  finally
    UnLock;
  end;

  for LItem in AProviders do
    AddProvider(LItem);
end;

function TDataLogger.Trace(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Trace, AMessage, ATagName);
end;

function TDataLogger.Trace(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Trace, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Trace(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Trace, AMessage, ATagName);
end;

function TDataLogger.Debug(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Debug, AMessage, ATagName);
end;

function TDataLogger.Debug(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Debug, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Debug(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Debug, AMessage, ATagName);
end;

function TDataLogger.Info(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Info, AMessage, ATagName);
end;

function TDataLogger.Info(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Info, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Info(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Info, AMessage, ATagName);
end;

function TDataLogger.Success(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Success, AMessage, ATagName);
end;

function TDataLogger.Success(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Success, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Success(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Success, AMessage, ATagName);
end;

function TDataLogger.Warn(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Warn, AMessage, ATagName);
end;

function TDataLogger.Warn(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Warn, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Warn(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Warn, AMessage, ATagName);
end;

function TDataLogger.Error(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Error, AMessage, ATagName);
end;

function TDataLogger.Error(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Error, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Error(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Error, AMessage, ATagName);
end;

function TDataLogger.Fatal(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Fatal, AMessage, ATagName);
end;

function TDataLogger.Fatal(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Fatal, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Fatal(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Fatal, AMessage, ATagName);
end;

function TDataLogger.Custom(const ALevel: string; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Custom, AMessage, '', ATagName, ALevel, False);
end;

function TDataLogger.Custom(const ALevel: string; const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Custom, Format(AMessage, AArgs), '', ATagName, ALevel, False);
end;

function TDataLogger.Custom(const ALevel: string; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerLevel.Custom, '', AMessage.ToString, ATagName, ALevel, False);
end;

function TDataLogger.Log(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(ALevel, AMessage, ATagName);
end;

function TDataLogger.Log(const ALevel: TLoggerLevel; const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(ALevel, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Log(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(ALevel, AMessage, ATagName);
end;

function TDataLogger.SlineBreak: TDataLogger;
begin
  Result := AddCache(TLoggerLevel.All, '', '', '', '', True);
end;

function TDataLogger.StartTransaction: TDataLogger;
var
  LProviders: TArray<TThread>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).StartTransaction(LID);
end;

function TDataLogger.CommitTransaction: TDataLogger;
var
  LProviders: TArray<TThread>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).CommitTransaction(LID);
end;

function TDataLogger.RollbackTransaction: TDataLogger;
var
  LProviders: TArray<TThread>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).RollbackTransaction(LID);
end;

function TDataLogger.InTransaction: Boolean;
var
  LProviders: TArray<TThread>;
  LProvider: TThread;
  LID: string;
begin
  Result := False;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for LProvider in LProviders do
  begin
    Result := TDataLoggerProvider<TObject>(LProvider).InTransaction(LID);
    if Result then
      Break;
  end;
end;

function TDataLogger.SetLogFormat(const ALogFormat: string): TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).SetLogFormat(ALogFormat);
end;

function TDataLogger.SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).SetFormatTimestamp(AFormatTimestamp);
end;

function TDataLogger.SetLogLevel(const ALogLevel: TLoggerLevel): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FLogLevel := ALogLevel;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetDisableLogLevel(const ALogLevels: TLoggerLevels): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FDisableLogLevel := ALogLevels;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetOnlyLogLevel(const ALogLevels: TLoggerLevels): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FOnlyLogLevel := ALogLevels;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetLogException(const AException: TOnLogException): TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).SetLogException(AException);
end;

function TDataLogger.SetMaxRetries(const AMaxRetries: Integer): TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).SetMaxRetries(AMaxRetries);
end;

function TDataLogger.SetInitialMessage(const AMessage: string): TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).SetInitialMessage(AMessage);
end;

function TDataLogger.SetFinalMessage(const AMessage: string): TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).SetFinalMessage(AMessage);
end;

function TDataLogger.SetIgnoreLogFormat(const AIgnoreLogFormat: Boolean; const ASeparator: string; const AIncludeKey: Boolean; const AIncludeKeySeparator: string): TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).SetIgnoreLogFormat(AIgnoreLogFormat, ASeparator, AIncludeKey, AIncludeKeySeparator);
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

function TDataLogger.Clear: TDataLogger;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  Result := Self;

  Lock;
  try
    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;
  finally
    UnLock;
  end;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).Clear;
end;

function TDataLogger.CountLogInCache: Int64;
begin
  Lock;
  try
    Result := FListLoggerItem.Count;
  finally
    UnLock;
  end;
end;

function TDataLogger.LoadFromJSON(const AJSON: string): Boolean;
var
  LProviders: TArray<TThread>;
  LJSON: string;
  LJV: TJSONValue;
  LJO: TJSONObject;
  LJA: TJSONArray;
  LJAName: string;
  LProvider: TThread;
  I: Integer;
  J: Integer;
begin
  Result := False;

  LJSON := AJSON.Replace(#$D#$A, '');

  if AJSON.Trim.IsEmpty then
    Exit;

  if AJSON.Trim = '{}' then
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

      if LJA.Count = 0 then
        Continue;

      for LProvider in LProviders do
        if LProvider.ClassName.ToLower = LJAName.ToLower then
        begin
          TDataLoggerProvider<TObject>(LProvider).LoadFromJSON(LJA.Items[0].ToString);
          LJA.Remove(0).Free;

          if LJA.Count = 0 then
            Break;
        end;

      if LJA.Count = 0 then
        LJO.RemovePair(LJAName).Free;
    end;

    if LJO.Count = 0 then
      Exit;

    Lock;
    try
      for I := 0 to Pred(LJO.Count) do
      begin
        LJA := LJO.Pairs[I].JsonValue as TJSONArray;
        LJAName := LJO.Pairs[I].JsonString.Value;

        for J := 0 to Pred(LJA.Count) do
        begin
          LProvider := TLoggerRTTI.CreateObject(LJAName) as TThread;

          if not Assigned(LProvider) then
            Continue;

          try
            TDataLoggerProvider<TObject>(LProvider).LoadFromJSON(LJA.Items[J].ToString);
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
  LProviders: TArray<TThread>;
  LProvider: TThread;
begin
  LProviders := GetProviders;

  if Length(LProviders) = 0 then
    Exit('{}');

  Lock;
  try
    LJO := TJSONObject.Create;
    try
      for LProvider in LProviders do
      begin
        if not Assigned(LJO.Get(LProvider.ClassName)) then
        begin
          LJA := TJSONArray.Create;
          LJO.AddPair(LProvider.ClassName, LJA);
        end
        else
          LJA := LJO.Get(LProvider.ClassName).JsonValue as TJSONArray;

        LJA.Add(TJSONObject.ParseJSONValue(TDataLoggerProvider<TObject>(LProvider).ToJSON) as TJSONObject);
      end;

      Result := TLoggerJSON.Format(LJO, AFormat);
    finally
      LJO.Free;
    end;
  finally
    UnLock;
  end;
end;

function TDataLogger.AddCache(const ALevel: TLoggerLevel; const AMessageString: string; const AMessageJSON: string; const ATagName: string; const ACustom: string; const ALevelSlineBreak: Boolean): TDataLogger;
var
  LLogItem: TLoggerItem;
begin
  Result := Self;

  if Terminated then
    Exit;

  if FTagNameIsRequired and not(ALevel = TLoggerLevel.All) then
    if ATagName.Trim.IsEmpty then
      raise EDataLoggerException.CreateFmt('DataLogger -> %s -> Tag name is required!', [ALevel.ToString]);

  Lock;
  try
    if not ALevelSlineBreak then
    begin
      if (TLoggerLevel.All in FDisableLogLevel) or (ALevel in FDisableLogLevel) then
        Exit;

      if not(TLoggerLevel.All in FOnlyLogLevel) and not(ALevel in FOnlyLogLevel) then
        Exit;

      if not(ALevel in FOnlyLogLevel) then
        if Ord(FLogLevel) > Ord(ALevel) then
          Exit;

      if not(ALevel = TLoggerLevel.All) then
      begin
        if FSequence = 18446744073709551615 then
          FSequence := 0;

        Inc(FSequence);
      end;
    end;

    LLogItem := default (TLoggerItem);
    LLogItem.Name := FName;
    LLogItem.Sequence := FSequence;
    LLogItem.TimeStamp := Now;
    LLogItem.ThreadID := TThread.Current.ThreadID;
    LLogItem.Level := ALevel;

    LLogItem.LevelString := ACustom;
    if LLogItem.LevelString.Trim.IsEmpty then
      LLogItem.LevelString := ALevel.ToString;

    LLogItem.LevelValue := Ord(ALevel);

    LLogItem.Tag := ATagName;
    LLogItem.Message := AMessageString;
    LLogItem.MessageJSON := AMessageJSON;

    LLogItem.AppName := TLoggerUtils.AppName;
    LLogItem.AppPath := TLoggerUtils.AppPath;
    LLogItem.AppVersion := TLoggerUtils.AppVersion;
    LLogItem.AppSize := TLoggerUtils.AppSize;

    LLogItem.ComputerName := TLoggerUtils.ComputerName;
    LLogItem.Username := TLoggerUtils.Username;
    LLogItem.OSVersion := TLoggerUtils.OS;
    LLogItem.ProcessID := TLoggerUtils.ProcessID;
    LLogItem.IPLocal := TLoggerUtils.IPLocal;

    LLogItem.InternalItem.LevelSlineBreak := ALevelSlineBreak;
    LLogItem.InternalItem.TransactionID := TThread.Current.ThreadID.ToString;

    FListLoggerItem.Add(LLogItem);
  finally
    FEvent.SetEvent;
    UnLock;
  end;
end;

function TDataLogger.AddCache(const ALevel: TLoggerLevel; const AMessage: string; const ATagName: string): TDataLogger;
begin
  Result := AddCache(ALevel, AMessage, '', ATagName, '', False);
end;

function TDataLogger.AddCache(const ALevel: TLoggerLevel; const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(ALevel, '', AMessage.ToString, ATagName, '', False);
end;

function TDataLogger.ExtractCache: TArray<TLoggerItem>;
begin
  Lock;
  try
    Result := FListLoggerItem.ToArray;

    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;
  finally
    UnLock;
  end;
end;

procedure TDataLogger.SaveForced;
var
  LCount: Integer;
begin
  LCount := CountLogInCache;
  if LCount = 0 then
    Exit;

  Lock;
  try
    FEvent.SetEvent;
  finally
    UnLock;
  end;

  while LCount > 0 do
  begin
    Sleep(1);
    LCount := CountLogInCache;
  end;
end;

procedure TDataLogger.CloseProvider;
var
  LProviders: TArray<TThread>;
  I: Integer;
begin
  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    TDataLoggerProvider<TObject>(LProviders[I]).NotifyEvent;
end;

function TDataLogger.GetProviders: TArray<TThread>;
var
  LProviders: TArray<TThread>;
begin
  Result := [];

  Lock;
  try
    LProviders := FListProviders.ToArray;
  finally
    UnLock;
  end;

  Result := LProviders;
end;

procedure TDataLogger.Lock;
begin
  FCriticalSection.Acquire;
end;

procedure TDataLogger.UnLock;
begin
  FCriticalSection.Release;
end;

initialization

FLoggerDefault := nil;

finalization

if Assigned(FLoggerDefault) then
begin
  FLoggerDefault.Free;
  FLoggerDefault := nil;
end;

end.
