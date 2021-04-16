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
    FList: TList<TLoggerItem>;
    FLogFormat: string;
    FFormatSettings: string;
    FLogLevel: TLoggerType;
    FDisableLogType: TLoggerTypes;
    FOnlyLogType: TLoggerTypes;
    FLogException: TOnLogException;
    FMaxRetry: Integer;

    function ExtractCache: TArray<TLoggerItem>;
  protected
    FCriticalSection: TCriticalSection;

    procedure Execute; override;
    procedure Save(const ACache: TArray<TLoggerItem>); virtual; abstract;

    function ValidationBeforeSave(const ALogItem: TLoggerItem): Boolean;
    function GetLogFormat: string;
    function GetFormatSettings: string;
    function GetLogLevel: TLoggerType;
    function GetDisableLevel: TLoggerTypes;
    function GetOnlyLogType: TLoggerTypes;
    function GetLogException: TOnLogException;
    function GetMaxRetry: Integer;

    property LogException: TOnLogException read GetLogException;

    constructor Create;
  public
    function SetLogFormat(const ALogFormat: string): TDataLoggerProvider;
    function SetFormatSettings(const AFormatSettings: string): TDataLoggerProvider;
    function SetLogLevel(const ALogLevel: TLoggerType): TDataLoggerProvider;
    function SetDisableLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
    function SetOnlyLogType(const ALogType: TLoggerTypes): TDataLoggerProvider;
    function SetLogException(const AException: TOnLogException): TDataLoggerProvider;
    function SetMaxRetry(const AMaxRetry: Integer): TDataLoggerProvider;

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

  SetLogFormat(DEFAULT_LOG_FORMAT);
  SetMaxRetry(3);
end;

procedure TDataLoggerProvider.AfterConstruction;
begin
  FCriticalSection := TCriticalSection.Create;
  FEvent := TEvent.Create;
  FList := TList<TLoggerItem>.Create;

  FLogLevel := TLoggerType.All;
  FDisableLogType := [];
  FOnlyLogType := [TLoggerType.All];

  FFormatSettings := 'yyyy-mm-dd hh:nn:ss:zzz';

  FreeOnTerminate := False;
  Start;
end;

procedure TDataLoggerProvider.BeforeDestruction;
begin
  Terminate;
  FEvent.SetEvent;
  WaitFor;

  FList.DisposeOf;
  FEvent.DisposeOf;
  FCriticalSection.DisposeOf;
end;

procedure TDataLoggerProvider.Execute;
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
            Save(LCache);
          finally
            FCriticalSection.Leave;
          end;
        end
    else
      Continue;
    end;
  end;
end;

function TDataLoggerProvider.SetLogFormat(const ALogFormat: string): TDataLoggerProvider;
begin
  Result := Self;
  FLogFormat := ALogFormat;
end;

function TDataLoggerProvider.SetFormatSettings(const AFormatSettings: string): TDataLoggerProvider;
begin
  Result := Self;

  FFormatSettings := AFormatSettings;
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

function TDataLoggerProvider.GetFormatSettings: string;
begin
  Result := FFormatSettings;
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

function TDataLoggerProvider.AddCache(const AValues: TArray<TLoggerItem>): TDataLoggerProvider;
var
  LItems: TArray<TLoggerItem>;
  LItem: TLoggerItem;
begin
  Result := Self;

  LItems := AValues;

  FCriticalSection.Enter;
  try
    for LItem in LItems do
      FList.Add(LItem);
  finally
    FCriticalSection.Leave;
  end;

  FEvent.SetEvent;
end;

function TDataLoggerProvider.AddCache(const AValue: TLoggerItem): TDataLoggerProvider;
begin
  Result := AddCache([AValue]);
end;

function TDataLoggerProvider.ExtractCache: TArray<TLoggerItem>;
var
  LCache: TArray<TLoggerItem>;
begin
  FCriticalSection.Enter;
  try
    LCache := FList.ToArray;
    FList.Clear;
    FList.TrimExcess;
  finally
    FCriticalSection.Leave;
  end;

  Result := LCache;
end;

end.
