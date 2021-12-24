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
    FEvent: TEvent;
    FListLoggerItem: TList<TLoggerItem>;
    FLogFormat: string;
    FFormatTimestamp: string;
    FLogLevel: TLoggerType;
    FDisableLogType: TLoggerTypes;
    FOnlyLogType: TLoggerTypes;
    FLogException: TOnLogException;
    FMaxRetry: Integer;
    FCriticalSection: TCriticalSection;
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
    function CriticalSection: TCriticalSection;

    property LogException: TOnLogException read GetLogException;

    constructor Create;
  public
    function SetLogFormat(const ALogFormat: string): TDataLoggerProvider;
    function SetFormatTimestamp(const AFormatTimestamp: string): TDataLoggerProvider;
    function SetLogLevel(const ALogLevel: TLoggerType): TDataLoggerProvider;
    function SetDisableLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
    function SetOnlyLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
    function SetLogException(const AException: TOnLogException): TDataLoggerProvider;
    function SetMaxRetry(const AMaxRetry: Integer): TDataLoggerProvider;
    function Clear: TDataLoggerProvider;
    function NotifyEvent: TDataLoggerProvider;

    function AddCache(const AValues: TArray<TLoggerItem>): TDataLoggerProvider; overload;
    function AddCache(const AValue: TLoggerItem): TDataLoggerProvider; overload;

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
end;

procedure TDataLoggerProvider.Execute;
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
      LCache := ExtractCache;

      if Length(LCache) = 0 then
        Continue;

      Save(LCache);
    end;
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

function TDataLoggerProvider.Clear: TDataLoggerProvider;
begin
  Result := Self;

  FCriticalSection.Acquire;
  try
    FListLoggerItem.Clear;
    FListLoggerItem.TrimExcess;
  finally
    FCriticalSection.Release;
  end;
end;

function TDataLoggerProvider.NotifyEvent: TDataLoggerProvider;
begin
  Result := Self;

  FCriticalSection.Acquire;
  try
    FEvent.SetEvent;
  finally
    FCriticalSection.Release;
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

function TDataLoggerProvider.CriticalSection: TCriticalSection;
begin
  Result := FCriticalSection;
end;

function TDataLoggerProvider.AddCache(const AValues: TArray<TLoggerItem>): TDataLoggerProvider;
var
  LItems: TArray<TLoggerItem>;
  LItem: TLoggerItem;
begin
  Result := Self;

  FCriticalSection.Acquire;
  try
    LItems := AValues;

    for LItem in LItems do
      FListLoggerItem.Add(LItem);
  finally
    FEvent.SetEvent;
    FCriticalSection.Release;
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

end.
