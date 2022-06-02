{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider;

interface

uses
  DataLogger.Types,
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections;

type
  TDataLoggerProvider = class(TThread)
  private
    FCriticalSection: TCriticalSection;
    FEvent: TEvent;
    FListLoggerItem: TList<TLoggerItem>;
    FLogFormat: string;
    FFormatTimestamp: string;
    FLogLevel: TLoggerType;
    FDisableLogType: TLoggerTypes;
    FOnlyLogType: TLoggerTypes;
    FLogException: TOnLogException;
    FMaxRetry: Integer;
    FUseTransaction: Boolean;
    FInTransaction: Boolean;
    FInitialMessage: string;
    FFinalMessage: string;

    function ExtractCache: TArray<TLoggerItem>;
  protected
    procedure Execute; override;
    procedure Save(const ACache: TArray<TLoggerItem>); virtual; abstract;

    function ValidationBeforeSave(const ALogItem: TLoggerItem): Boolean;
    function GetLogFormat: string;
    function GetFormatTimestamp: string;
    function GetLogLevel: TLoggerType;
    function GetDisableLevel: TLoggerTypes;
    function GetOnlyLogType: TLoggerTypes;
    function GetLogException: TOnLogException;
    function GetMaxRetry: Integer;
    procedure Lock;
    procedure UnLock;

    property LogException: TOnLogException read GetLogException;
  public
    function SetLogFormat(const ALogFormat: string): TDataLoggerProvider;
    function SetFormatTimestamp(const AFormatTimestamp: string): TDataLoggerProvider;
    function SetLogLevel(const ALogLevel: TLoggerType): TDataLoggerProvider;
    function SetDisableLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
    function SetOnlyLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
    function SetLogException(const AException: TOnLogException): TDataLoggerProvider;
    function SetMaxRetry(const AMaxRetry: Integer): TDataLoggerProvider;
    function SetInitialMessage(const AMessage: string): TDataLoggerProvider;
    function SetFinalMessage(const AMessage: string): TDataLoggerProvider;

    function UseTransaction(const AUseTransaction: Boolean): TDataLoggerProvider;
    function StartTransaction: TDataLoggerProvider;
    function CommitTransaction: TDataLoggerProvider;
    function RollbackTransaction: TDataLoggerProvider;
    function InTransaction: Boolean;

    function Clear: TDataLoggerProvider;
    function CountLogInCache: Int64;

    function AddCache(const AValues: TArray<TLoggerItem>): TDataLoggerProvider; overload;
    function AddCache(const AValue: TLoggerItem): TDataLoggerProvider; overload;
    function NotifyEvent: TDataLoggerProvider;

    constructor Create;
    procedure AfterConstruction; override; final;
    procedure BeforeDestruction; override; final;
  end;

implementation

{ TDataLoggerProvider }

constructor TDataLoggerProvider.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
end;

procedure TDataLoggerProvider.AfterConstruction;
begin
  inherited;

  FCriticalSection := TCriticalSection.Create;
  FEvent := TEvent.Create;
  FListLoggerItem := TList<TLoggerItem>.Create;

  SetLogFormat(TLoggerFormat.DEFAULT_LOG_FORMAT);
  SetFormatTimestamp('yyyy-mm-dd hh:nn:ss:zzz');
  SetLogLevel(TLoggerType.All);
  SetDisableLogType([]);
  SetOnlyLogType([TLoggerType.All]);
  SetLogException(nil);
  SetMaxRetry(5);
  UseTransaction(False);

  Start;
end;

procedure TDataLoggerProvider.BeforeDestruction;
begin
  Terminate;
  FEvent.SetEvent;
  WaitFor;

  FListLoggerItem.Free;
  FEvent.Free;
  FCriticalSection.Free;

  inherited;
end;

procedure TDataLoggerProvider.Execute;
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

function TDataLoggerProvider.SetLogFormat(const ALogFormat: string): TDataLoggerProvider;
begin
  Result := Self;
  FLogFormat := ALogFormat;
end;

function TDataLoggerProvider.SetFormatTimestamp(const AFormatTimestamp: string): TDataLoggerProvider;
begin
  Result := Self;
  FFormatTimestamp := AFormatTimestamp;
end;

function TDataLoggerProvider.SetLogLevel(const ALogLevel: TLoggerType): TDataLoggerProvider;
begin
  Result := Self;
  FLogLevel := ALogLevel;
end;

function TDataLoggerProvider.SetDisableLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
begin
  Result := Self;
  FDisableLogType := ALogType;
end;

function TDataLoggerProvider.SetOnlyLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
begin
  Result := Self;
  FOnlyLogType := ALogType;
end;

function TDataLoggerProvider.SetLogException(const AException: TOnLogException): TDataLoggerProvider;
begin
  Result := Self;
  FLogException := AException;
end;

function TDataLoggerProvider.SetMaxRetry(const AMaxRetry: Integer): TDataLoggerProvider;
begin
  Result := Self;
  FMaxRetry := AMaxRetry;
end;

function TDataLoggerProvider.SetInitialMessage(const AMessage: string): TDataLoggerProvider;
begin
  Result := Self;
  FInitialMessage := AMessage;
end;

function TDataLoggerProvider.SetFinalMessage(const AMessage: string): TDataLoggerProvider;
begin
  Result := Self;
  FFinalMessage := AMessage;
end;

function TDataLoggerProvider.Clear: TDataLoggerProvider;
begin
  Result := Self;

  Lock;
  try
    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider.UseTransaction(const AUseTransaction: Boolean): TDataLoggerProvider;
begin
  Result := Self;
  FUseTransaction := AUseTransaction;
end;

function TDataLoggerProvider.StartTransaction: TDataLoggerProvider;
begin
  Result := Self;

  if not FUseTransaction then
    Exit;

  Lock;
  try
    FEvent.SetEvent;
    FInTransaction := True;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider.CommitTransaction: TDataLoggerProvider;
begin
  Result := Self;

  if not FUseTransaction then
    Exit;

  Lock;
  try
    FInTransaction := False;
    FEvent.SetEvent;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider.RollbackTransaction: TDataLoggerProvider;
begin
  Result := Self;

  if not FUseTransaction then
    Exit;

  Lock;
  try
    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;

    FInTransaction := False;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider.InTransaction: Boolean;
begin
  Lock;
  try
    Result := FInTransaction;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider.CountLogInCache: Int64;
begin
  Lock;
  try
    Result := FListLoggerItem.Count;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider.NotifyEvent: TDataLoggerProvider;
begin
  Result := Self;

  Lock;
  try
    FEvent.SetEvent;
  finally
    UnLock;
  end;
end;

function TDataLoggerProvider.ValidationBeforeSave(const ALogItem: TLoggerItem): Boolean;
begin
  Result := False;

  if (TLoggerType.All in GetDisableLevel) or (ALogItem.&Type in GetDisableLevel) then
    Exit;

  if not(TLoggerType.All in GetOnlyLogType) and not(ALogItem.&Type in GetOnlyLogType) then
    Exit;

  if not(ALogItem.&Type in GetOnlyLogType) then
    if Ord(GetLogLevel) > Ord(ALogItem.&Type) then
      Exit;

  Result := True;
end;

function TDataLoggerProvider.GetLogFormat: string;
begin
  Result := FLogFormat;
end;

function TDataLoggerProvider.GetFormatTimestamp: string;
begin
  Result := FFormatTimestamp;
end;

function TDataLoggerProvider.GetLogLevel: TLoggerType;
begin
  Result := FLogLevel;
end;

function TDataLoggerProvider.GetDisableLevel: TLoggerTypes;
begin
  Result := FDisableLogType;
end;

function TDataLoggerProvider.GetOnlyLogType: TLoggerTypes;
begin
  Result := FOnlyLogType;
end;

function TDataLoggerProvider.GetLogException: TOnLogException;
begin
  Result := FLogException;
end;

function TDataLoggerProvider.GetMaxRetry: Integer;
begin
  Result := FMaxRetry;
end;

procedure TDataLoggerProvider.Lock;
begin
  FCriticalSection.Acquire;
end;

procedure TDataLoggerProvider.UnLock;
begin
  FCriticalSection.Release;
end;

function TDataLoggerProvider.AddCache(const AValues: TArray<TLoggerItem>): TDataLoggerProvider;
var
  LItems: TArray<TLoggerItem>;
  LItem: TLoggerItem;
  LMessage: string;
  I: Integer;
begin
  Result := Self;

  Lock;
  try
    LItems := AValues;

    for I := Low(AValues) to High(AValues) do
    begin
      LItem := AValues[I];

      LMessage := LItem.Message;

      if not FInitialMessage.Trim.IsEmpty then
        LMessage := FInitialMessage + LMessage;

      if not FFinalMessage.Trim.IsEmpty then
        LMessage := LMessage + FFinalMessage;

      LItem.Message := LMessage;

      FListLoggerItem.Add(LItem);
    end;
  finally
    if not FUseTransaction then
      FEvent.SetEvent
    else
      if not FInTransaction then
        FEvent.SetEvent;

    UnLock;
  end;
end;

function TDataLoggerProvider.AddCache(const AValue: TLoggerItem): TDataLoggerProvider;
begin
  Result := AddCache([AValue]);
end;

function TDataLoggerProvider.ExtractCache: TArray<TLoggerItem>;
var
  LCache: TArray<TLoggerItem>;
begin
  Lock;
  try
    LCache := FListLoggerItem.ToArray;

    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;
  finally
    UnLock;
  end;

  Result := LCache;
end;

end.
