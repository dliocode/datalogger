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
  TLoggerTypeAutoCommit = DataLogger.Types.TLoggerTypeAutoCommit;
  TOnLogException = DataLogger.Types.TOnLogException;
  TDataLoggerProvider = DataLogger.Provider.TDataLoggerProvider;
  TLoggerFormat = DataLogger.Types.TLoggerFormat;
  EDataLoggerException = DataLogger.Types.EDataLoggerException;
  Exception = DataLogger.Types.Exception;

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
    FName: string;
    FIsRequiredTagName: Boolean;

    constructor Create;
    procedure Start;

    function AddCache(const AType: TLoggerType; const AMessageString: string; const AMessageJSON: string; const ATagName: string; const ACustomType: string; const ATypeSlineBreak: Boolean): TDataLogger; overload;
    function AddCache(const AType: TLoggerType; const AMessage: string; const ATagName: string): TDataLogger; overload;
    function AddCache(const AType: TLoggerType; const AMessage: TJSONObject; const ATagName: string): TDataLogger; overload;
    function ExtractCache: TArray<TLoggerItem>;
    procedure SaveForced;
    procedure CloseProvider;
    function GetProviders: TArray<TDataLoggerProvider>;
    procedure Lock;
    procedure UnLock;
  protected
    procedure Execute; override;
  public
    function AddProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger; overload;
    function AddProvider(const AProvider: TDataLoggerProvider): TDataLogger; overload;
    function RemoveProvider(const AProvider: TDataLoggerProvider): TDataLogger;
    function SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;

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

    function CustomType(const AType: string; const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function CustomType(const AType: string; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function CustomType(const AType: string; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function Log(const AType: TLoggerType; const AMessage: string; const ATagName: string = ''): TDataLogger; overload;
    function Log(const AType: TLoggerType; const AMessage: string; const AArgs: array of const; const ATagName: string = ''): TDataLogger; overload;
    function Log(const AType: TLoggerType; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger; overload;

    function SlineBreak: TDataLogger;

    function StartTransaction: TDataLogger;
    function CommitTransaction: TDataLogger;
    function RollbackTransaction: TDataLogger;
    function InTransaction: Boolean;

    function SetLogFormat(const ALogFormat: string): TDataLogger;
    function SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
    function SetLogLevel(const ALogLevel: TLoggerType): TDataLogger;
    function SetDisableLogType(const ALogType: TLoggerTypes): TDataLogger;
    function SetOnlyLogType(const ALogType: TLoggerTypes): TDataLogger;
    function SetLogException(const AException: TOnLogException): TDataLogger;
    function SetMaxRetries(const AMaxRetries: Integer): TDataLogger;
    function SetInitialMessage(const AMessage: string): TDataLogger;
    function SetFinalMessage(const AMessage: string): TDataLogger;
    function SetName(const AName: string): TDataLogger;
    function SetIsRequiredTagName(const AIsRequiredTagName: Boolean): TDataLogger;

    function Clear: TDataLogger;
    function CountLogInCache: Int64;

    procedure LoadFromJSON(const AJSON: string);
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
end;

procedure TDataLogger.AfterConstruction;
begin
  inherited;

  FCriticalSection := TCriticalSection.Create;

  FEvent := TEvent.Create;
  FListLoggerItem := TList<TLoggerItem>.Create;
  FListProviders := TObjectList<TDataLoggerProvider>.Create(True);

  SetLogLevel(TLoggerType.All);
  SetDisableLogType([]);
  SetOnlyLogType([TLoggerType.All]);
  SetName('');
  SetIsRequiredTagName(False);

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
  LProviders: TArray<TDataLoggerProvider>;
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
      LProviders[I].AddCache(LCache);
  end;
end;

function TDataLogger.AddProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FListProviders.AddRange(AProviders);
  finally
    UnLock;
  end;
end;

function TDataLogger.AddProvider(const AProvider: TDataLoggerProvider): TDataLogger;
begin
  Result := AddProvider([AProvider]);
end;

function TDataLogger.RemoveProvider(const AProvider: TDataLoggerProvider): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FListProviders.Remove(AProvider);
  finally
    UnLock;
  end;
end;

function TDataLogger.SetProvider(const AProviders: TArray<TDataLoggerProvider>): TDataLogger;
var
  LItem: TDataLoggerProvider;
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
  Result := AddCache(TLoggerType.Trace, AMessage, ATagName);
end;

function TDataLogger.Trace(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Trace, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Trace(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Trace, AMessage, ATagName);
end;

function TDataLogger.Debug(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, AMessage, ATagName);
end;

function TDataLogger.Debug(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Debug(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Debug, AMessage, ATagName);
end;

function TDataLogger.Info(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, AMessage, ATagName);
end;

function TDataLogger.Info(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Info(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Info, AMessage, ATagName);
end;

function TDataLogger.Success(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, AMessage, ATagName);
end;

function TDataLogger.Success(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Success(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Success, AMessage, ATagName);
end;

function TDataLogger.Warn(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, AMessage, ATagName);
end;

function TDataLogger.Warn(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Warn(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Warn, AMessage, ATagName);
end;

function TDataLogger.Error(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, AMessage, ATagName);
end;

function TDataLogger.Error(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Error(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Error, AMessage, ATagName);
end;

function TDataLogger.Fatal(const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Fatal, AMessage, ATagName);
end;

function TDataLogger.Fatal(const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Fatal, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Fatal(const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Fatal, AMessage, ATagName);
end;

function TDataLogger.CustomType(const AType: string; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Custom, AMessage, '', ATagName, AType, False);
end;

function TDataLogger.CustomType(const AType: string; const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(TLoggerType.Custom, Format(AMessage, AArgs), '', ATagName, AType, False);
end;

function TDataLogger.CustomType(const AType: string; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(TLoggerType.Custom, '', AMessage.ToString, ATagName, AType, False);
end;

function TDataLogger.Log(const AType: TLoggerType; const AMessage: string; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(AType, AMessage, ATagName);
end;

function TDataLogger.Log(const AType: TLoggerType; const AMessage: string; const AArgs: array of const; const ATagName: string): TDataLogger;
begin
  Result := AddCache(AType, Format(AMessage, AArgs), ATagName);
end;

function TDataLogger.Log(const AType: TLoggerType; const AMessage: TJSONObject; const ATagName: string = ''): TDataLogger;
begin
  Result := AddCache(AType, AMessage, ATagName);
end;

function TDataLogger.SlineBreak: TDataLogger;
begin
  Result := AddCache(TLoggerType.All, '', '', '', '', True);
end;

function TDataLogger.StartTransaction: TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].StartTransaction(LID);
end;

function TDataLogger.CommitTransaction: TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].CommitTransaction(LID);
end;

function TDataLogger.RollbackTransaction: TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  LID: string;
  I: Integer;
begin
  Result := Self;

  SaveForced;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].RollbackTransaction(LID);
end;

function TDataLogger.InTransaction: Boolean;
var
  LProviders: TArray<TDataLoggerProvider>;
  LProvider: TDataLoggerProvider;
  LID: string;
begin
  Result := False;

  LID := TThread.Current.ThreadID.ToString;

  LProviders := GetProviders;

  for LProvider in LProviders do
  begin
    Result := LProvider.InTransaction(LID);
    if Result then
      Break;
  end;
end;

function TDataLogger.SetLogFormat(const ALogFormat: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].SetLogFormat(ALogFormat);
end;

function TDataLogger.SetFormatTimestamp(const AFormatTimestamp: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].SetFormatTimestamp(AFormatTimestamp);
end;

function TDataLogger.SetLogLevel(const ALogLevel: TLoggerType): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FLogLevel := ALogLevel;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetDisableLogType(const ALogType: TLoggerTypes): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FDisableLogType := ALogType;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetOnlyLogType(const ALogType: TLoggerTypes): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FOnlyLogType := ALogType;
  finally
    UnLock;
  end;
end;

function TDataLogger.SetLogException(const AException: TOnLogException): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].SetLogException(AException);
end;

function TDataLogger.SetMaxRetries(const AMaxRetries: Integer): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].SetMaxRetries(AMaxRetries);
end;

function TDataLogger.SetInitialMessage(const AMessage: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].SetInitialMessage(AMessage);
end;

function TDataLogger.SetFinalMessage(const AMessage: string): TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
  I: Integer;
begin
  Result := Self;

  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].SetFinalMessage(AMessage);
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

function TDataLogger.SetIsRequiredTagName(const AIsRequiredTagName: Boolean): TDataLogger;
begin
  Result := Self;

  Lock;
  try
    FIsRequiredTagName := AIsRequiredTagName;
  finally
    UnLock;
  end;
end;

function TDataLogger.Clear: TDataLogger;
var
  LProviders: TArray<TDataLoggerProvider>;
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
    LProviders[I].Clear;
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

procedure TDataLogger.LoadFromJSON(const AJSON: string);
var
  LProviders: TArray<TDataLoggerProvider>;
  LJSON: string;
  LJO: TJSONObject;
  LJA: TJSONArray;
  LJAName: string;
  LProvider: TDataLoggerProvider;
  I: Integer;
  J: Integer;
begin
  if AJSON.Trim.IsEmpty then
    Exit;

  LJSON := AJSON.Replace(#$D#$A, '');

  try
    LJO := TJSONObject.ParseJSONValue(LJSON) as TJSONObject;
  except
    on E: Exception do
      raise EDataLoggerException.Create('JSON invalid in LoadFromJSON!');
  end;

  if Assigned(LJO) then
  begin
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
            LProvider.LoadFromJSON(LJA.Items[0].ToString);
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
            LProvider := TLoggerRTTI.CreateObject(LJAName) as TDataLoggerProvider;

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
end;

function TDataLogger.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
  LJA: TJSONArray;
  LProviders: TArray<TDataLoggerProvider>;
  LProvider: TDataLoggerProvider;
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

function TDataLogger.AddCache(const AType: TLoggerType; const AMessageString: string; const AMessageJSON: string; const ATagName: string; const ACustomType: string; const ATypeSlineBreak: Boolean): TDataLogger;
var
  LLogItem: TLoggerItem;
begin
  Result := Self;

  if Terminated then
    Exit;

  if FIsRequiredTagName and not(AType = TLoggerType.All) then
    if ATagName.Trim.IsEmpty then
      raise EDataLoggerException.CreateFmt('DataLogger -> %s -> Tag name is empty!', [AType.ToString]);

  Lock;
  try
    if not ATypeSlineBreak then
    begin
      if (TLoggerType.All in FDisableLogType) or (AType in FDisableLogType) then
        Exit;

      if not(TLoggerType.All in FOnlyLogType) and not(AType in FOnlyLogType) then
        Exit;

      if not(AType in FOnlyLogType) then
        if Ord(FLogLevel) > Ord(AType) then
          Exit;

      if not(AType = TLoggerType.All) then
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
    LLogItem.&Type := AType;

    LLogItem.TypeString := ACustomType;
    if LLogItem.TypeString.Trim.IsEmpty then
      LLogItem.TypeString := AType.ToString;

    LLogItem.TypeLevel := Ord(AType);

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

    LLogItem.InternalItem.TypeSlineBreak := ATypeSlineBreak;
    LLogItem.InternalItem.TransactionID := TThread.Current.ThreadID.ToString;

    FListLoggerItem.Add(LLogItem);
  finally
    FEvent.SetEvent;
    UnLock;
  end;
end;

function TDataLogger.AddCache(const AType: TLoggerType; const AMessage: string; const ATagName: string): TDataLogger;
begin
  Result := AddCache(AType, AMessage, '', ATagName, '', False);
end;

function TDataLogger.AddCache(const AType: TLoggerType; const AMessage: TJSONObject; const ATagName: string): TDataLogger;
begin
  Result := AddCache(AType, '', AMessage.ToString, ATagName, '', False);
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
  LProviders: TArray<TDataLoggerProvider>;
  I: Integer;
begin
  LProviders := GetProviders;

  for I := Low(LProviders) to High(LProviders) do
    LProviders[I].NotifyEvent;
end;

function TDataLogger.GetProviders: TArray<TDataLoggerProvider>;
var
  LProviders: TArray<TDataLoggerProvider>;
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
