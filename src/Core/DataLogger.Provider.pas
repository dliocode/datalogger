{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider;

interface

uses
  DataLogger.Types, DataLogger.Transaction, DataLogger.Utils,
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections, System.JSON, System.TypInfo;

type
  TLoggerJSON = DataLogger.Utils.TLoggerJSON;

  TDataLoggerProvider<T: class> = class(TThread)
  private
    FOwner: T;
    FCriticalSection: TCriticalSection;
    FEvent: TEvent;

    FListLoggerBase: TDataLoggerListItem;
    FListTransaction: TDataLoggerListTransaction;

    FLogLevel: TLoggerType;
    FDisableLogType: TLoggerTypes;
    FOnlyLogType: TLoggerTypes;

    FUseTransaction: Boolean;
    FAutoCommit: TLoggerTypes;
    FTypeAutoCommit: TLoggerTypeAutoCommit;

    FInitialMessage: string;
    FFinalMessage: string;

    function ExtractCache: TArray<TLoggerItem>;
  protected
    FLogFormat: string;
    FFormatTimestamp: string;
    FLogException: TOnLogException;
    FMaxRetries: Integer;

    procedure Execute; override;
    procedure Save(const ACache: TArray<TLoggerItem>); virtual; abstract;

    procedure SetJSONInternal(const AJO: TJSONObject);
    procedure ToJSONInternal(const AJO: TJSONObject);
    procedure Lock;
    procedure UnLock;
  public
    function SetLogFormat(const ALogFormat: string): T;
    function SetFormatTimestamp(const AFormatTimestamp: string): T;
    function SetLogLevel(const ALogLevel: TLoggerType): T;
    function SetDisableLogType(const ALogTypes: TLoggerTypes): T;
    function SetOnlyLogType(const ALogTypes: TLoggerTypes): T;
    function SetLogException(const AException: TOnLogException): T;
    function SetMaxRetries(const AMaxRetries: Integer): T;
    function SetInitialMessage(const AMessage: string): T;
    function SetFinalMessage(const AMessage: string): T;

    function UseTransaction(const AUseTransaction: Boolean): T;
    function AutoCommit(const ALogTypes: TLoggerTypes; const ATypeAutoCommit: TLoggerTypeAutoCommit = TLoggerTypeAutoCommit.tcBlock): T;

    function StartTransaction(const AID: string; const AUseLock: Boolean = True): T;
    function CommitTransaction(const AID: string; const ATypeCommit: TLoggerTypeAutoCommit = TLoggerTypeAutoCommit.tcBlock; const AUseLock: Boolean = True): T;
    function RollbackTransaction(const AID: string; const ATypeCommit: TLoggerTypeAutoCommit = TLoggerTypeAutoCommit.tcBlock): T;
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
  inherited Create(True);
  FreeOnTerminate := False;
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
  SetFormatTimestamp('yyyy-mm-dd hh:nn:ss:zzz');
  SetLogLevel(TLoggerType.All);
  SetDisableLogType([]);
  SetOnlyLogType([TLoggerType.All]);
  SetLogException(nil);
  SetMaxRetries(5);
  UseTransaction(False);
  AutoCommit([], TLoggerTypeAutoCommit.tcBlock);

  Start;
end;

procedure TDataLoggerProvider<T>.BeforeDestruction;
begin
  Terminate;
  FEvent.SetEvent;
  WaitFor;

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

procedure TDataLoggerProvider<T>.Execute;
var
  LCache: TArray<TLoggerItem>;
begin
  while not Terminated do
  begin
    FEvent.WaitFor(INFINITE);
    FEvent.ResetEvent;

    LCache := ExtractCache;
    if Length(LCache) = 0 then
      Continue;

    Save(LCache);
  end;
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

function TDataLoggerProvider<T>.SetLogLevel(const ALogLevel: TLoggerType): T;
begin
  Result := FOwner;
  FLogLevel := ALogLevel;
end;

function TDataLoggerProvider<T>.SetDisableLogType(const ALogTypes: TLoggerTypes): T;
begin
  Result := FOwner;
  FDisableLogType := ALogTypes;
end;

function TDataLoggerProvider<T>.SetOnlyLogType(const ALogTypes: TLoggerTypes): T;
begin
  Result := FOwner;
  FOnlyLogType := ALogTypes;
end;

function TDataLoggerProvider<T>.SetLogException(const AException: TOnLogException): T;
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

function TDataLoggerProvider<T>.UseTransaction(const AUseTransaction: Boolean): T;
begin
  Result := FOwner;
  FUseTransaction := AUseTransaction;
end;

function TDataLoggerProvider<T>.AutoCommit(const ALogTypes: TLoggerTypes; const ATypeAutoCommit: TLoggerTypeAutoCommit = TLoggerTypeAutoCommit.tcBlock): T;
begin
  Result := FOwner;

  FAutoCommit := ALogTypes;
  FTypeAutoCommit := ATypeAutoCommit;
end;

function TDataLoggerProvider<T>.StartTransaction(const AID: string; const AUseLock: Boolean = True): T;
var
  LTransaction: TDataLoggerTransaction;
  LCountTransaction: Integer;
begin
  Result := FOwner;

  if not FUseTransaction then
    Exit;

  if AUseLock then
    Lock;
  try
    if not FListTransaction.TryGetValue(AID, LTransaction) then
    begin
      LTransaction := TDataLoggerTransaction.Create;
      FListTransaction.Add(AID, LTransaction);

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

function TDataLoggerProvider<T>.CommitTransaction(const AID: string; const ATypeCommit: TLoggerTypeAutoCommit = TLoggerTypeAutoCommit.tcBlock; const AUseLock: Boolean = True): T;
var
  LTransaction: TDataLoggerTransaction;
  LCountTransaction: Integer;
  LCurrent: TDataLoggerListItem;
  LCurrentValues: TArray<TLoggerItem>;
begin
  Result := FOwner;

  if not FUseTransaction then
    Exit;

  if AUseLock then
    Lock;
  try
    if not FListTransaction.TryGetValue(AID, LTransaction) then
    begin
      LTransaction := TDataLoggerTransaction.Create;
      FListTransaction.Add(AID, LTransaction);

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

    if ATypeCommit = TLoggerTypeAutoCommit.tcBlock then
      Break;
  end;
end;

function TDataLoggerProvider<T>.RollbackTransaction(const AID: string; const ATypeCommit: TLoggerTypeAutoCommit = TLoggerTypeAutoCommit.tcBlock): T;
var
  LTransaction: TDataLoggerTransaction;
  LCountTransaction: Integer;
begin
  Result := FOwner;

  if not FUseTransaction then
    Exit;

  Lock;
  try
    if not FListTransaction.TryGetValue(AID, LTransaction) then
    begin
      LTransaction := TDataLoggerTransaction.Create;
      FListTransaction.Add(AID, LTransaction);

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

    if ATypeCommit = TLoggerTypeAutoCommit.tcBlock then
      Break;
  end;
end;

function TDataLoggerProvider<T>.InTransaction(const AID: string): Boolean;
var
  LTransaction: TDataLoggerTransaction;
begin
  Result := False;

  Lock;
  try
    if not FListTransaction.TryGetValue(AID, LTransaction) then
      Exit;
  finally
    UnLock;
  end;

  Result := LTransaction.InTransaction;
end;

function TDataLoggerProvider<T>.CountTransaction(const AID: string): Integer;
var
  LTransaction: TDataLoggerTransaction;
begin
  Result := 0;

  Lock;
  try
    if not FListTransaction.TryGetValue(AID, LTransaction) then
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

        if not LItem.InternalItem.TypeSlineBreak then
        begin
          if (TLoggerType.All in FDisableLogType) or (LItem.&Type in FDisableLogType) then
            Continue;

          if not(TLoggerType.All in FOnlyLogType) and not(LItem.&Type in FOnlyLogType) then
            Continue;

          if not(LItem.&Type in FOnlyLogType) then
            if Ord(FLogLevel) > Ord(LItem.&Type) then
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

        LTransaction := nil;
        if FListTransaction.TryGetValue(LItem.InternalItem.TransactionID, LTransaction) then
        begin
          LTransaction.ListItemTransaction.TryGetValue(LTransaction.ListItemTransaction.Count, LListLoggerItem);

          if not Assigned(LListLoggerItem) then
            Exit;

          LListLoggerItem.Add(LItem);

          if not LItem.InternalItem.TypeSlineBreak then
            if FUseTransaction and LTransaction.InTransaction then
              if LItem.&Type in FAutoCommit then
              begin
                CommitTransaction(LItem.InternalItem.TransactionID, FTypeAutoCommit, False);
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

procedure TDataLoggerProvider<T>.SetJSONInternal(const AJO: TJSONObject);
var
  LJOInternal: TJSONObject;
  LValue: string;
  LLoggerType: TLoggerType;
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

  LValue := FLogLevel.ToString;
  FLogLevel.SetName(LJOInternal.GetValue<string>('log_level', LValue));

  // Disable Log Type
  LJSONValue := LJOInternal.GetValue('disable_log_level');
  if Assigned(LJSONValue) then
  begin
    SetDisableLogType([]);

    for I := 0 to Pred(TJSONArray(LJSONValue).Count) do
    begin
      LValue := TJSONArray(LJSONValue).Items[I].Value;
      LLoggerType.SetName(LValue);

      FDisableLogType := FDisableLogType + [LLoggerType];
    end;
  end;

  // Only Log Type
  LJSONValue := LJOInternal.GetValue('only_log_type');
  if Assigned(LJSONValue) then
  begin
    SetOnlyLogType([]);

    for I := 0 to Pred(TJSONArray(LJSONValue).Count) do
    begin
      LValue := TJSONArray(LJSONValue).Items[I].Value;
      LLoggerType.SetName(LValue);

      FOnlyLogType := FOnlyLogType + [LLoggerType];
    end;
  end;

  SetMaxRetries(LJOInternal.GetValue<Int64>('max_retries', FMaxRetries));
  SetInitialMessage(LJOInternal.GetValue<string>('initial_message', FInitialMessage));
  SetFinalMessage(LJOInternal.GetValue<string>('final_message', FFinalMessage));
  UseTransaction(LJOInternal.GetValue<Boolean>('use_transaction', FUseTransaction));

  // Auto Commit
  LJSONObjectValue := LJOInternal.GetValue<TJSONObject>('auto_commit');
  if Assigned(LJSONObjectValue) then
  begin
    LJSONValue := TJSONObject(LJSONObjectValue).GetValue('log_type');
    if Assigned(LJSONValue) then
    begin
      FAutoCommit := [];

      for I := 0 to Pred(TJSONArray(LJSONValue).Count) do
      begin
        LValue := TJSONArray(LJSONValue).Items[I].Value;
        LLoggerType.SetName(LValue);

        FAutoCommit := FAutoCommit + [LLoggerType];
      end;
    end;

    LValue := GetEnumName(TypeInfo(TLoggerType), Integer(FTypeAutoCommit));
    SetLogLevel(TLoggerType(GetEnumValue(TypeInfo(TLoggerType), TJSONObject(LJSONObjectValue).GetValue<string>('type_commit', LValue))));
  end;
end;

procedure TDataLoggerProvider<T>.ToJSONInternal(const AJO: TJSONObject);
var
  LJOInternal: TJSONObject;
  I: TLoggerType;
  LJADisableLogLevel: TJSONArray;
  LJAOnlyLogType: TJSONArray;
  LJOAutoCommit: TJSONObject;
  LJAAutoCommitLogType: TJSONArray;
begin
  if not Assigned(AJO) then
    Exit;

  LJOInternal := TJSONObject.Create;
  AJO.AddPair('internal', LJOInternal);

  LJOInternal.AddPair('log_format', FLogFormat);
  LJOInternal.AddPair('format_timestamp', FFormatTimestamp);
  LJOInternal.AddPair('log_level', FLogLevel.ToString);

  // Disable Log Level
  LJADisableLogLevel := TJSONArray.Create;
  LJOInternal.AddPair('disable_log_level', LJADisableLogLevel);

  if not(FDisableLogType = []) then
    for I := Low(TLoggerType) to High(TLoggerType) do
      if TLoggerType(I) in FDisableLogType then
        LJADisableLogLevel.Add(TLoggerType(I).ToString);

  // Only Log Type
  LJAOnlyLogType := TJSONArray.Create;
  LJOInternal.AddPair('only_log_type', LJAOnlyLogType);

  if not(FOnlyLogType = []) then
    for I := Low(TLoggerType) to High(TLoggerType) do
      if TLoggerType(I) in FOnlyLogType then
        LJAOnlyLogType.Add(TLoggerType(I).ToString);

  LJOInternal.AddPair('max_retries', TJSONNumber.Create(FMaxRetries));
  LJOInternal.AddPair('initial_message', FInitialMessage);
  LJOInternal.AddPair('final_message', FFinalMessage);
  LJOInternal.AddPair('use_transaction', TJSONBool.Create(FUseTransaction));

  // Auto Commit
  LJOAutoCommit := TJSONObject.Create;
  LJOInternal.AddPair('auto_commit', LJOAutoCommit);

  LJAAutoCommitLogType := TJSONArray.Create;
  LJOAutoCommit.AddPair('log_type', LJAAutoCommitLogType);

  if not(FAutoCommit = []) then
    for I := Low(TLoggerType) to High(TLoggerType) do
      if TLoggerType(I) in FAutoCommit then
        LJAAutoCommitLogType.Add(TLoggerType(I).ToString);

  LJOAutoCommit.AddPair('type_commit', GetEnumName(TypeInfo(TLoggerTypeAutoCommit), Integer(FTypeAutoCommit)));
end;

procedure TDataLoggerProvider<T>.Lock;
begin
  FCriticalSection.Acquire;
end;

procedure TDataLoggerProvider<T>.UnLock;
begin
  FCriticalSection.Release;
end;

end.
