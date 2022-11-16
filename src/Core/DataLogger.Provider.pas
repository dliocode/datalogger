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

unit DataLogger.Provider;

interface

uses
  DataLogger.Types, DataLogger.Transaction, DataLogger.Utils,
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections, System.JSON, System.TypInfo;

type
  TLoggerJSON = DataLogger.Utils.TLoggerJSON;

  TDataLoggerProvider<T: class> = class
  private
    FOwner: T;
    FCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FExecute: TThread;
    FTerminated: Boolean;

    FListLoggerBase: TDataLoggerListItem;
    FListTransaction: TDataLoggerListTransaction;

    FLevel: TLoggerLevel;
    FDisableLevel: TLoggerLevels;
    FOnlyLevel: TLoggerLevels;

    FUseTransaction: Boolean;
    FUseTransactionModeMultThread: Boolean;
    FTransactionAutoCommitLevel: TLoggerLevels;
    FTransactionAutoCommitType: TLoggerTransactionTypeCommit;

    FInitialMessage: string;
    FFinalMessage: string;

    function ExtractCache: TArray<TLoggerItem>;
    procedure Start;
  protected
    FLogFormat: string;
    FFormatTimestamp: string;
    FLogException: TLoggerOnException;
    FMaxRetries: Integer;
    FIgnoreLogFormat: Boolean;
    FIgnoreLogFormatSeparator: string;
    FIgnoreLogFormatIncludeKey: Boolean;
    FIgnoreLogFormatIncludeKeySeparator: string;

    procedure Save(const ACache: TArray<TLoggerItem>); virtual; abstract;

    procedure SetJSONInternal(const AJO: TJSONObject);
    procedure ToJSONInternal(const AJO: TJSONObject);
    procedure Lock;
    procedure UnLock;
    function Terminated: Boolean;
  public
    function SetLogFormat(const ALogFormat: string): T;
    function SetFormatTimestamp(const AFormatTimestamp: string): T;
    function SetLevel(const ALevel: TLoggerLevel): T;
    function SetDisableLevel(const ALevels: TLoggerLevels): T;
    function SetOnlyLevel(const ALevels: TLoggerLevels): T;
    function SetLogLevel(const ALevel: TLoggerLevel): T; deprecated 'Use SetLevel instead - This function will be removed in future versions';
    function SetDisableLogLevel(const ALevels: TLoggerLevels): T; deprecated 'Use SetDisableLevel instead - This function will be removed in future versions';
    function SetOnlyLogLevel(const ALevels: TLoggerLevels): T; deprecated 'Use SetOnlyLevel instead - This function will be removed in future versions';
    function SetLogException(const AException: TLoggerOnException): T;
    function SetMaxRetries(const AMaxRetries: Integer): T;
    function SetInitialMessage(const AMessage: string): T;
    function SetFinalMessage(const AMessage: string): T;
    function SetIgnoreLogFormat(const AIgnoreLogFormat: Boolean; const ASeparator: string = ' '; const AIncludeKey: Boolean = False; const AIncludeKeySeparator: string = ' -> '): T;

    function UseTransaction(const AUseTransaction: Boolean; const AModeMultThread: Boolean = True): T;
    function TransactionAutoCommit(const ALevels: TLoggerLevels; const ATypeAutoCommit: TLoggerTransactionTypeCommit = TLoggerTransactionTypeCommit.tcBlock): T;

    function StartTransaction(const AID: string; const AUseLock: Boolean = True): T;
    function CommitTransaction(const AID: string; const ALevelCommit: TLoggerTransactionTypeCommit = TLoggerTransactionTypeCommit.tcBlock; const AUseLock: Boolean = True): T;
    function RollbackTransaction(const AID: string; const ALevelCommit: TLoggerTransactionTypeCommit = TLoggerTransactionTypeCommit.tcBlock): T;
    function InTransaction(const AID: string): Boolean;
    function CountTransaction(const AID: string): Integer;

    function Clear: T;
    function CountLogInCache: Int64;

    procedure LoadFromJSON(const AJSON: string); virtual; abstract;
    function ToJSON(const AFormat: Boolean = False): string; virtual; abstract;

    function AddCache(const AValues: TArray<TLoggerItem>): T;
    function NotifyEvent: T;

    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{ TDataLoggerProvider }

constructor TDataLoggerProvider<T>.Create;
begin

end;

procedure TDataLoggerProvider<T>.AfterConstruction;
begin
  inherited;

  FOwner := Self as T;
  FCriticalSection := TCriticalSection.Create;
  FEvent := TEvent.Create;

  FListLoggerBase := TDataLoggerListItem.Create;
  FListTransaction := TDataLoggerListTransaction.Create([doOwnsValues]);

  SetLogFormat(TLoggerFormat.DEFAULT_LOG_FORMAT);
  SetFormatTimestamp('yyyy-mm-dd hh:nn:ss.zzz');
  SetLevel(TLoggerLevel.All);
  SetDisableLevel([]);
  SetOnlyLevel([TLoggerLevel.All]);
  SetLogException(nil);
  SetMaxRetries(5);
  SetInitialMessage('');
  SetFinalMessage('');
  SetIgnoreLogFormat(False);

  UseTransaction(False);
  TransactionAutoCommit([], TLoggerTransactionTypeCommit.tcBlock);

  FTerminated := False;
  Start;
end;

procedure TDataLoggerProvider<T>.BeforeDestruction;
begin
  FExecute.Terminate;
  FTerminated := True;
  FEvent.SetEvent;
  FExecute.WaitFor;
  FExecute.Free;

  Lock;
  try
    FListTransaction.Free;
    FListLoggerBase.Free;
    FEvent.Free;
  finally
    UnLock;
  end;

  FCriticalSection.Free;

  inherited;
end;

function TDataLoggerProvider<T>.SetLogFormat(const ALogFormat: string): T;
begin
  Result := FOwner;
  FLogFormat := ALogFormat;
end;

function TDataLoggerProvider<T>.SetFormatTimestamp(const AFormatTimestamp: string): T;
begin
  Result := FOwner;
  FFormatTimestamp := AFormatTimestamp;
end;

function TDataLoggerProvider<T>.SetLevel(const ALevel: TLoggerLevel): T;
begin
  Result := FOwner;
  FLevel := ALevel;
end;

function TDataLoggerProvider<T>.SetDisableLevel(const ALevels: TLoggerLevels): T;
begin
  Result := FOwner;
  FDisableLevel := ALevels;
end;

function TDataLoggerProvider<T>.SetOnlyLevel(const ALevels: TLoggerLevels): T;
begin
  Result := FOwner;
  FOnlyLevel := ALevels;
end;

function TDataLoggerProvider<T>.SetLogLevel(const ALevel: TLoggerLevel): T;
begin
  Result := SetLevel(ALevel);
end;

function TDataLoggerProvider<T>.SetDisableLogLevel(const ALevels: TLoggerLevels): T;
begin
  Result := SetDisableLevel(ALevels);
end;

function TDataLoggerProvider<T>.SetOnlyLogLevel(const ALevels: TLoggerLevels): T;
begin
  Result := SetOnlyLevel(ALevels);
end;

function TDataLoggerProvider<T>.SetLogException(const AException: TLoggerOnException): T;
begin
  Result := FOwner;
  FLogException := AException;
end;

function TDataLoggerProvider<T>.SetMaxRetries(const AMaxRetries: Integer): T;
begin
  Result := FOwner;
  FMaxRetries := AMaxRetries;
end;

function TDataLoggerProvider<T>.SetInitialMessage(const AMessage: string): T;
begin
  Result := FOwner;
  FInitialMessage := AMessage;
end;

function TDataLoggerProvider<T>.SetFinalMessage(const AMessage: string): T;
begin
  Result := FOwner;
  FFinalMessage := AMessage;
end;

function TDataLoggerProvider<T>.SetIgnoreLogFormat(const AIgnoreLogFormat: Boolean; const ASeparator: string = ' '; const AIncludeKey: Boolean = False; const AIncludeKeySeparator: string = ' -> '): T;
begin
  Result := FOwner;

  FIgnoreLogFormat := AIgnoreLogFormat;
  FIgnoreLogFormatSeparator := ASeparator;
  FIgnoreLogFormatIncludeKey := AIncludeKey;
  FIgnoreLogFormatIncludeKeySeparator := AIncludeKeySeparator;
end;

function TDataLoggerProvider<T>.UseTransaction(const AUseTransaction: Boolean; const AModeMultThread: Boolean = True): T;
begin
  Result := FOwner;

  FUseTransaction := AUseTransaction;
  FUseTransactionModeMultThread := AModeMultThread;
end;

function TDataLoggerProvider<T>.TransactionAutoCommit(const ALevels: TLoggerLevels; const ATypeAutoCommit: TLoggerTransactionTypeCommit): T;
begin
  Result := FOwner;

  FTransactionAutoCommitLevel := ALevels;
  FTransactionAutoCommitType := ATypeAutoCommit;
end;

function TDataLoggerProvider<T>.StartTransaction(const AID: string; const AUseLock: Boolean = True): T;
var
  LTransaction: TDataLoggerTransaction;
  LCountTransaction: Integer;
  LID: string;
begin
  Result := FOwner;

  if not FUseTransaction then
    Exit;

  if AUseLock then
    Lock;
  try
    LID := AID;
    if not FUseTransactionModeMultThread then
      LID := TLoggerConst.TRANSACTION_ID;

    if not FListTransaction.TryGetValue(LID, LTransaction) then
    begin
      LTransaction := TDataLoggerTransaction.Create;
      FListTransaction.Add(LID, LTransaction);

      LTransaction.ListItemTransaction := TDataLoggerListItemTransaction.Create([doOwnsValues]);
    end;
  finally
    if AUseLock then
      UnLock;
  end;

  LCountTransaction := LTransaction.ListItemTransaction.Count;
  if LCountTransaction = 0 then
    LTransaction.InTransaction := True;

  LTransaction.ListItemTransaction.Add(LCountTransaction + 1, TDataLoggerListItem.Create);
end;

function TDataLoggerProvider<T>.CommitTransaction(const AID: string; const ALevelCommit: TLoggerTransactionTypeCommit = TLoggerTransactionTypeCommit.tcBlock; const AUseLock: Boolean = True): T;
var
  LTransaction: TDataLoggerTransaction;
  LCountTransaction: Integer;
  LCurrent: TDataLoggerListItem;
  LCurrentValues: TArray<TLoggerItem>;
  LID: string;
begin
  Result := FOwner;

  if not FUseTransaction then
    Exit;

  if AUseLock then
    Lock;
  try
    LID := AID;
    if not FUseTransactionModeMultThread then
      LID := TLoggerConst.TRANSACTION_ID;

    if not FListTransaction.TryGetValue(LID, LTransaction) then
    begin
      LTransaction := TDataLoggerTransaction.Create;
      FListTransaction.Add(LID, LTransaction);

      LTransaction.ListItemTransaction := TDataLoggerListItemTransaction.Create([doOwnsValues]);
    end;
  finally
    if AUseLock then
      UnLock;
  end;

  while True do
  begin
    LCountTransaction := LTransaction.ListItemTransaction.Count;
    if LCountTransaction = 0 then
      Exit;

    LTransaction.ListItemTransaction.TryGetValue(LCountTransaction, LCurrent);
    LCurrentValues := LCurrent.ToArray;

    if LCountTransaction > 1 then
    begin
      LTransaction.ListItemTransaction.TryGetValue(LCountTransaction - 1, LCurrent);
      LCurrent.AddRange(LCurrentValues);
    end;

    LTransaction.ListItemTransaction.Remove(LCountTransaction);

    if LCountTransaction = 1 then
    begin
      if AUseLock then
        Lock;
      try
        FListLoggerBase.AddRange(LCurrentValues);
        FEvent.SetEvent;
      finally
        if AUseLock then
          UnLock;
      end;

      LTransaction.InTransaction := False;

      Break;
    end;

    if ALevelCommit = TLoggerTransactionTypeCommit.tcBlock then
      Break;
  end;
end;

function TDataLoggerProvider<T>.RollbackTransaction(const AID: string; const ALevelCommit: TLoggerTransactionTypeCommit = TLoggerTransactionTypeCommit.tcBlock): T;
var
  LTransaction: TDataLoggerTransaction;
  LCountTransaction: Integer;
  LID: string;
begin
  Result := FOwner;

  if not FUseTransaction then
    Exit;

  Lock;
  try
    LID := AID;
    if not FUseTransactionModeMultThread then
      LID := TLoggerConst.TRANSACTION_ID;

    if not FListTransaction.TryGetValue(LID, LTransaction) then
    begin
      LTransaction := TDataLoggerTransaction.Create;
      FListTransaction.Add(LID, LTransaction);

      LTransaction.ListItemTransaction := TDataLoggerListItemTransaction.Create([doOwnsValues]);
    end;
  finally
    UnLock;
  end;

  while True do
  begin
    LCountTransaction := LTransaction.ListItemTransaction.Count;
    if LCountTransaction = 0 then
      Exit;

    LTransaction.ListItemTransaction.Remove(LCountTransaction);

    if LCountTransaction = 1 then
    begin
      NotifyEvent;

      LTransaction.InTransaction := False;
      Break;
    end;

    if ALevelCommit = TLoggerTransactionTypeCommit.tcBlock then
      Break;
  end;
end;

function TDataLoggerProvider<T>.InTransaction(const AID: string): Boolean;
var
  LTransaction: TDataLoggerTransaction;
  LID: string;
begin
  Result := False;

  Lock;
  try
    LID := AID;
    if not FUseTransactionModeMultThread then
      LID := TLoggerConst.TRANSACTION_ID;

    if not FListTransaction.TryGetValue(LID, LTransaction) then
      Exit;
  finally
    UnLock;
  end;

  Result := LTransaction.InTransaction;
end;

function TDataLoggerProvider<T>.CountTransaction(const AID: string): Integer;
var
  LTransaction: TDataLoggerTransaction;
  LID: string;
begin
  Result := 0;

  Lock;
  try
    LID := AID;
    if not FUseTransactionModeMultThread then
      LID := TLoggerConst.TRANSACTION_ID;

    if not FListTransaction.TryGetValue(LID, LTransaction) then
      Exit;
  finally
    UnLock;
  end;

  Result := LTransaction.ListItemTransaction.Count;
end;

function TDataLoggerProvider<T>.Clear: T;
begin
  Result := FOwner;

  Lock;
  try
    FListLoggerBase.Clear;
    FListLoggerBase.TrimExcess;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider<T>.CountLogInCache: Int64;
begin
  Lock;
  try
    Result := FListLoggerBase.Count;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider<T>.AddCache(const AValues: TArray<TLoggerItem>): T;
var
  I: Integer;
  LItem: TLoggerItem;
  LMessage: string;
  LTransaction: TDataLoggerTransaction;
  LListLoggerItem: TDataLoggerListItem;
begin
  Result := FOwner;

  Lock;
  try
    try
      for I := Low(AValues) to High(AValues) do
      begin
        LItem := AValues[I];

        if not LItem.InternalItem.IsSlinebreak then
        begin
          if (TLoggerLevel.All in FDisableLevel) or (LItem.Level in FDisableLevel) then
            Continue;

          if not(TLoggerLevel.All in FOnlyLevel) and not(LItem.Level in FOnlyLevel) then
            Continue;

          if not(LItem.Level in FOnlyLevel) then
            if Ord(FLevel) > Ord(LItem.Level) then
              Continue;
        end;

        LMessage := LItem.Message;
        try
          if not FInitialMessage.Trim.IsEmpty then
            LMessage := FInitialMessage + LMessage;

          if not FFinalMessage.Trim.IsEmpty then
            LMessage := LMessage + FFinalMessage;
        finally
          LItem.Message := LMessage;
        end;

        if not FUseTransactionModeMultThread then
          LItem.InternalItem.TransactionID := TLoggerConst.TRANSACTION_ID;

        LTransaction := nil;
        if FListTransaction.TryGetValue(LItem.InternalItem.TransactionID, LTransaction) then
        begin
          LTransaction.ListItemTransaction.TryGetValue(LTransaction.ListItemTransaction.Count, LListLoggerItem);

          if not Assigned(LListLoggerItem) then
            Exit;

          LListLoggerItem.Add(LItem);

          if not LItem.InternalItem.IsSlinebreak then
            if FUseTransaction and LTransaction.InTransaction then
              if LItem.Level in FTransactionAutoCommitLevel then
              begin
                CommitTransaction(LItem.InternalItem.TransactionID, FTransactionAutoCommitType, False);
                StartTransaction(LItem.InternalItem.TransactionID, False);
              end;
        end
        else
          FListLoggerBase.Add(LItem);
      end;
    finally
      if not FUseTransaction or not Assigned(LTransaction) then
        FEvent.SetEvent;
    end;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider<T>.NotifyEvent: T;
begin
  Result := FOwner;

  Lock;
  try
    FEvent.SetEvent;
  finally
    UnLock;
  end;
end;

procedure TDataLoggerProvider<T>.SetJSONInternal(const AJO: TJSONObject);
var
  LJOInternal: TJSONObject;
  LValue: string;
  LLoggerLevel: TLoggerLevel;
  LJSONValue: TJSONValue;
  LJSONObjectValue: TJSONValue;
  I: Integer;
begin
  if not Assigned(AJO) then
    Exit;

  if not Assigned(AJO.Get('internal')) then
    Exit;

  LJOInternal := AJO.GetValue<TJSONObject>('internal');

  SetLogFormat(LJOInternal.GetValue<string>('log_format', FLogFormat));
  SetFormatTimestamp(LJOInternal.GetValue<string>('format_timestamp', FFormatTimestamp));

  LValue := FLevel.ToString;
  FLevel.SetLevelName(LJOInternal.GetValue<string>('log_level', LValue));

  // Disable Log Level
  LJSONValue := LJOInternal.GetValue('disable_level');
  if Assigned(LJSONValue) then
  begin
    SetDisableLevel([]);

    for I := 0 to Pred(TJSONArray(LJSONValue).Count) do
    begin
      LValue := TJSONArray(LJSONValue).Items[I].Value;
      LLoggerLevel.SetLevelName(LValue);

      FDisableLevel := FDisableLevel + [LLoggerLevel];
    end;
  end;

  // Only Log Level
  LJSONValue := LJOInternal.GetValue('only_level');
  if Assigned(LJSONValue) then
  begin
    SetOnlyLevel([]);

    for I := 0 to Pred(TJSONArray(LJSONValue).Count) do
    begin
      LValue := TJSONArray(LJSONValue).Items[I].Value;
      LLoggerLevel.SetLevelName(LValue);

      FOnlyLevel := FOnlyLevel + [LLoggerLevel];
    end;
  end;

  SetMaxRetries(LJOInternal.GetValue<Int64>('max_retries', FMaxRetries));
  SetInitialMessage(LJOInternal.GetValue<string>('initial_message', FInitialMessage));
  SetFinalMessage(LJOInternal.GetValue<string>('final_message', FFinalMessage));
  UseTransaction(LJOInternal.GetValue<Boolean>('use_transaction', FUseTransaction));

  // Auto Commit
  LJSONObjectValue := LJOInternal.GetValue<TJSONObject>('transaction_auto_commit');
  if Assigned(LJSONObjectValue) then
  begin
    LJSONValue := TJSONObject(LJSONObjectValue).GetValue('level');
    if Assigned(LJSONValue) then
    begin
      FTransactionAutoCommitLevel := [];

      for I := 0 to Pred(TJSONArray(LJSONValue).Count) do
      begin
        LValue := TJSONArray(LJSONValue).Items[I].Value;
        LLoggerLevel.SetLevelName(LValue);

        FTransactionAutoCommitLevel := FTransactionAutoCommitLevel + [LLoggerLevel];
      end;
    end;

    LValue := GetEnumName(TypeInfo(TLoggerLevel), Integer(FTransactionAutoCommitType));
    SetLevel(TLoggerLevel(GetEnumValue(TypeInfo(TLoggerLevel), TJSONObject(LJSONObjectValue).GetValue<string>('type', LValue))));
  end;
end;

procedure TDataLoggerProvider<T>.ToJSONInternal(const AJO: TJSONObject);
var
  LJOInternal: TJSONObject;
  I: TLoggerLevel;
  LJADisableLevel: TJSONArray;
  LJAOnlyLevel: TJSONArray;
  LJOTransactionAutoCommit: TJSONObject;
  LJATransactionAutoCommitLevel: TJSONArray;
begin
  if not Assigned(AJO) then
    Exit;

  LJOInternal := TJSONObject.Create;
  AJO.AddPair('internal', LJOInternal);

  LJOInternal.AddPair('log_format', TJSONString.Create(FLogFormat));
  LJOInternal.AddPair('format_timestamp', TJSONString.Create(FFormatTimestamp));
  LJOInternal.AddPair('level', TJSONString.Create(FLevel.ToString));

  // Disable Log Level
  LJADisableLevel := TJSONArray.Create;
  LJOInternal.AddPair('disable_level', LJADisableLevel);

  if not(FDisableLevel = []) then
    for I := Low(TLoggerLevel) to High(TLoggerLevel) do
      if TLoggerLevel(I) in FDisableLevel then
        LJADisableLevel.Add(TLoggerLevel(I).ToString);

  // Only Log Level
  LJAOnlyLevel := TJSONArray.Create;
  LJOInternal.AddPair('only_level', LJAOnlyLevel);

  if not(FOnlyLevel = []) then
    for I := Low(TLoggerLevel) to High(TLoggerLevel) do
      if TLoggerLevel(I) in FOnlyLevel then
        LJAOnlyLevel.Add(TLoggerLevel(I).ToString);

  LJOInternal.AddPair('max_retries', TJSONNumber.Create(FMaxRetries));
  LJOInternal.AddPair('initial_message', TJSONString.Create(FInitialMessage));
  LJOInternal.AddPair('final_message', TJSONString.Create(FFinalMessage));
  LJOInternal.AddPair('use_transaction', TJSONBool.Create(FUseTransaction));

  // Auto Commit
  LJOTransactionAutoCommit := TJSONObject.Create;
  LJOInternal.AddPair('transaction_auto_commit', LJOTransactionAutoCommit);

  LJATransactionAutoCommitLevel := TJSONArray.Create;
  LJOTransactionAutoCommit.AddPair('level', LJATransactionAutoCommitLevel);

  if not(FTransactionAutoCommitLevel = []) then
    for I := Low(TLoggerLevel) to High(TLoggerLevel) do
      if TLoggerLevel(I) in FTransactionAutoCommitLevel then
        LJATransactionAutoCommitLevel.Add(TLoggerLevel(I).ToString);

  LJOTransactionAutoCommit.AddPair('type', TJSONString.Create(GetEnumName(TypeInfo(TLoggerTransactionTypeCommit), Integer(FTransactionAutoCommitType))));
end;

procedure TDataLoggerProvider<T>.Lock;
begin
  FCriticalSection.Acquire;
end;

procedure TDataLoggerProvider<T>.UnLock;
begin
  FCriticalSection.Release;
end;

function TDataLoggerProvider<T>.Terminated: Boolean;
begin
  Result := FTerminated;
end;

function TDataLoggerProvider<T>.ExtractCache: TArray<TLoggerItem>;
begin
  Lock;
  try
    Result := FListLoggerBase.ToArray;

    FListLoggerBase.Clear;
    FListLoggerBase.TrimExcess;
  finally
    UnLock;
  end;
end;

procedure TDataLoggerProvider<T>.Start;
begin
  FExecute :=
    TThread.CreateAnonymousThread(
    procedure
    var
      LCache: TArray<TLoggerItem>;
    begin
      while not FTerminated do
      begin
        FEvent.WaitFor(INFINITE);
        FEvent.ResetEvent;

        LCache := ExtractCache;
        if Length(LCache) = 0 then
          Continue;

        Save(LCache);
      end;
    end);

  FExecute.FreeOnTerminate := False;
  FExecute.Start;
end;

end.
