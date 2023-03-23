unit DataLogger.SerializeItem;

interface

uses
  DataLogger.Types,
  System.StrUtils, System.SysUtils, System.JSON, System.Classes, System.Generics.Collections;

type
  ILoggerSerializeItem = interface
    ['{B1ED6A14-5FD4-4D97-944A-552A31B94595}']
    function LogFormat(const AValue: string): ILoggerSerializeItem;
    function IgnoreLogFormat(const AValue: Boolean): ILoggerSerializeItem;
    function IgnoreLogFormatSeparator(const AValue: string): ILoggerSerializeItem;
    function IgnoreLogFormatIncludeKey(const AValue: Boolean): ILoggerSerializeItem;
    function IgnoreLogFormatIncludeKeySeparator(const AValue: string): ILoggerSerializeItem;
    function LogItem(const AValue: TLoggerItem): ILoggerSerializeItem;
    function FormatTimestamp(const AValue: string): ILoggerSerializeItem;
    function Prefix(const AValue: string): ILoggerSerializeItem;

    function ToJSONObject: TJSONObject;
    function ToJSON: string;
    function ToJSONStream: TStream;
    function ToString: string;
    function ToStream: TStream;
    function ToKeys: TArray<string>;
    function ToValues: TArray<string>;
    function ToListTAG(const ALog: string; const ATag: TArray<string>): TDictionary<string, string>;
  end;

  TLoggerSerializeItem = class(TInterfacedObject, ILoggerSerializeItem)
  private
    FLogFormat: string;
    FIgnoreLogFormat: Boolean;
    FIgnoreLogFormatSeparator: string;
    FIgnoreLogFormatIncludeKey: Boolean;
    FIgnoreLogFormatIncludeKeySeparator: string;
    FLogItem: TLoggerItem;
    FFormatTimestamp: string;
    FPrefix: string;

    constructor Create;
  public
    function LogFormat(const AValue: string): ILoggerSerializeItem;
    function IgnoreLogFormat(const AValue: Boolean): ILoggerSerializeItem;
    function IgnoreLogFormatSeparator(const AValue: string): ILoggerSerializeItem;
    function IgnoreLogFormatIncludeKey(const AValue: Boolean): ILoggerSerializeItem;
    function IgnoreLogFormatIncludeKeySeparator(const AValue: string): ILoggerSerializeItem;
    function LogItem(const AValue: TLoggerItem): ILoggerSerializeItem;
    function FormatTimestamp(const AValue: string): ILoggerSerializeItem;
    function Prefix(const AValue: string): ILoggerSerializeItem;

    function ToJSONObject: TJSONObject;
    function ToJSON: string;
    function ToJSONStream: TStream;
    function ToString: string; reintroduce;
    function ToStream: TStream;
    function ToKeys: TArray<string>;
    function ToValues: TArray<string>;
    function ToListTAG(const ALog: string; const ATag: TArray<string>): TDictionary<string, string>;

    class function New: ILoggerSerializeItem;
  end;

implementation

type
  TLoggerLetter = (tlNone, tlUpper, tlLower, tlFirstUp);

const
  TLoggerLetterKeyString: array [TLoggerLetter] of string = ((''), ('_upper'), ('_lower'), ('_first_up'));

{ TLoggerSerializeItem }

class function TLoggerSerializeItem.New: ILoggerSerializeItem;
begin
  Result := TLoggerSerializeItem.Create;
end;

constructor TLoggerSerializeItem.Create;
begin
  FLogFormat := '';
  FIgnoreLogFormat := False;
  FIgnoreLogFormatSeparator := ' ';
  FIgnoreLogFormatIncludeKey := False;
  FIgnoreLogFormatIncludeKeySeparator := ' -> ';
  FLogItem := Default (TLoggerItem);
  FFormatTimestamp := '';
  FPrefix := 'log_';
end;

function TLoggerSerializeItem.LogFormat(const AValue: string): ILoggerSerializeItem;
begin
  Result := Self;
  FLogFormat := AValue;
end;

function TLoggerSerializeItem.IgnoreLogFormat(const AValue: Boolean): ILoggerSerializeItem;
begin
  Result := Self;
  FIgnoreLogFormat := AValue;
end;

function TLoggerSerializeItem.IgnoreLogFormatSeparator(const AValue: string): ILoggerSerializeItem;
begin
  Result := Self;
  FIgnoreLogFormatSeparator := AValue;
end;

function TLoggerSerializeItem.IgnoreLogFormatIncludeKey(const AValue: Boolean): ILoggerSerializeItem;
begin
  Result := Self;
  FIgnoreLogFormatIncludeKey := AValue;
end;

function TLoggerSerializeItem.IgnoreLogFormatIncludeKeySeparator(const AValue: string): ILoggerSerializeItem;
begin
  Result := Self;
  FIgnoreLogFormatIncludeKeySeparator := AValue;
end;

function TLoggerSerializeItem.LogItem(const AValue: TLoggerItem): ILoggerSerializeItem;
begin
  Result := Self;
  FLogItem := AValue;
end;

function TLoggerSerializeItem.FormatTimestamp(const AValue: string): ILoggerSerializeItem;
begin
  Result := Self;
  FFormatTimestamp := AValue;
end;

function TLoggerSerializeItem.Prefix(const AValue: string): ILoggerSerializeItem;
begin
  Result := Self;
  FPrefix := AValue;
end;


function TLoggerSerializeItem.ToJSONObject: TJSONObject;
  procedure _Add(const ALogKey: string; const AIncludePrefix: Boolean; const AJSONValue: TJSONValue);
  var
    LJSONKey: string;
    LLetter: TLoggerLetter;
    LKeyLetter: string;
    LValue: string;
  begin
    LJSONKey := Copy(ALogKey, 3, (Length(ALogKey) - 3));

    if FIgnoreLogFormat then
    begin
      if AIncludePrefix then
        LJSONKey := FPrefix + LJSONKey;

      Result.AddPair(LJSONKey, AJSONValue);
      Exit;
    end;

    for LLetter := Low(TLoggerLetter) to High(TLoggerLetter) do
    begin
      LKeyLetter := Format('${%s}', [LJSONKey + TLoggerLetterKeyString[LLetter]]);
      if not FLogFormat.Contains(LKeyLetter) then
        Continue;

      if AIncludePrefix then
        LJSONKey := FPrefix + LJSONKey;

      if AJSONValue is TJSONString then
      begin
        LValue := AJSONValue.Value;

        case LLetter of
          tlNone:
            ;

          tlUpper:
            LValue := UpperCase(LValue);

          tlLower:
            LValue := LowerCase(LValue);

          tlFirstUp:
            begin
              LValue := LowerCase(LValue);
              LValue[1] := UpCase(LValue[1]);
            end;
        end;

        Result.AddPair(LJSONKey, TJSONString.Create(LValue));
        Break;
      end
      else
        Result.AddPair(LJSONKey, AJSONValue);

      Exit;
    end;

    AJSONValue.Free;
  end;

var
  LJO: TJSONObject;
  I: Integer;
  LKey: string;
  LValue: TJSONValue;
begin
  Result := TJSONObject.Create;

  try
    _Add(TLoggerFormat.LOG_TIMESTAMP, True, TJSONString.Create(FormatDateTime(FFormatTimestamp, FLogItem.TimeStamp)));
    _Add(TLoggerFormat.LOG_TIMESTAMP_ISO8601, True, TJSONString.Create(FLogItem.TimeStampISO8601));
    _Add(TLoggerFormat.LOG_TIMESTAMP_UNIX, True, TJSONNumber.Create(FLogItem.TimeStampUNIX));
    _Add(TLoggerFormat.LOG_NAME, True, TJSONString.Create(FLogItem.Name));
    _Add(TLoggerFormat.LOG_SEQUENCE, True, TJSONNumber.Create(FLogItem.Sequence));
    _Add(TLoggerFormat.LOG_THREADID, True, TJSONNumber.Create(FLogItem.ThreadID));
    _Add(TLoggerFormat.LOG_LEVEL, True, TJSONString.Create(FLogItem.LevelString));
    _Add(TLoggerFormat.LOG_LEVEL_VALUE, True, TJSONNumber.Create(FLogItem.LevelValue));
    _Add(TLoggerFormat.LOG_TAG, True, TJSONString.Create(FLogItem.Tag));

    if not FLogItem.MessageJSON.IsEmpty then
      _Add(TLoggerFormat.LOG_MESSAGE, True, TJSONString.Create(FLogItem.MessageJSON.Trim))
    else
      _Add(TLoggerFormat.LOG_MESSAGE, True, TJSONString.Create(FLogItem.Message.Trim));

    _Add(TLoggerFormat.LOG_APPNAME, True, TJSONString.Create(FLogItem.AppName));
    _Add(TLoggerFormat.LOG_APPVERSION, True, TJSONString.Create(FLogItem.AppVersion.FileVersion));
    _Add(TLoggerFormat.LOG_APPPATH, True, TJSONString.Create(FLogItem.AppPath));
    _Add(TLoggerFormat.LOG_APPSIZE, True, TJSONString.Create(FormatFloat('#,##0.00 MB', FLogItem.AppSize / 1024)));

    _Add(TLoggerFormat.LOG_COMPUTERNAME, True, TJSONString.Create(FLogItem.ComputerName));
    _Add(TLoggerFormat.LOG_USERNAME, True, TJSONString.Create(FLogItem.Username));
    _Add(TLoggerFormat.LOG_OSVERSION, True, TJSONString.Create(FLogItem.OSVersion));
    _Add(TLoggerFormat.LOG_PROCESSID, True, TJSONString.Create(FLogItem.ProcessId));

    _Add(TLoggerFormat.LOG_IP_LOCAL, True, TJSONString.Create(FLogItem.IPLocal));
    _Add(TLoggerFormat.LOG_MAC_ADDRESS, True, TJSONString.Create(FLogItem.MACAddress));

    if not FLogItem.MessageJSON.IsEmpty then
      try
        LJO := TJSONObject.ParseJSONValue(FLogItem.MessageJSON) as TJSONObject;

        if Assigned(LJO) then
          try
            for I := 0 to Pred(LJO.Count) do
            begin
              LKey := LJO.Pairs[I].JsonString.Value;
              LValue := LJO.Pairs[I].JsonValue.Clone as TJSONValue;

              _Add(Format('${%s}', [LKey]), False, LValue);
            end;
          finally
            LJO.Free;
          end;
      except
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TLoggerSerializeItem.ToJSON: string;
var
  LJO: TJSONObject;
begin
  LJO := ToJSONObject;
  try
    Result := LJO.ToString;
  finally
    LJO.Free;
  end;
end;

function TLoggerSerializeItem.ToJSONStream: TStream;
begin
  Result := TStringStream.Create(ToJSON, TEncoding.UTF8);
end;

function TLoggerSerializeItem.ToString: string;
var
  LLog: string;
  LJO: TJSONObject;
  I: Integer;
  LKey: string;
  LValue: string;
  LLetter: TLoggerLetter;
  LKeyLetter: string;
begin
  Prefix('');

  LLog := '';
  if not FIgnoreLogFormat then
    LLog := FLogFormat;

  LJO := ToJSONObject;
  try
    for I := 0 to Pred(LJO.Count) do
    begin
      LKey := LJO.Pairs[I].JsonString.Value;
      LValue := LJO.Pairs[I].JsonValue.Value;

      if not FIgnoreLogFormat then
      begin
        for LLetter := Low(TLoggerLetter) to High(TLoggerLetter) do
        begin
          LKeyLetter := Format('${%s}', [LKey + TLoggerLetterKeyString[LLetter]]);
          if not FLogFormat.Contains(LKeyLetter) then
            Continue;

          LLog := LLog.Replace(LKeyLetter, LValue);
        end;
      end
      else
      begin
        if FIgnoreLogFormatIncludeKey then
          LLog := LLog + LKey + FIgnoreLogFormatIncludeKeySeparator + LValue + FIgnoreLogFormatSeparator
        else
          LLog := LLog + LValue + FIgnoreLogFormatSeparator;
      end;
    end;

    if FIgnoreLogFormat then
      LLog := Copy(LLog, 1, Length(LLog) - Length(FIgnoreLogFormatSeparator));
  finally
    LJO.Free;
  end;

  Result := LLog;
end;

function TLoggerSerializeItem.ToStream: TStream;
begin
  Result := TStringStream.Create(ToString, TEncoding.UTF8);
end;

function TLoggerSerializeItem.ToKeys: TArray<string>;
var
  LJO: TJSONObject;
  I: Integer;
begin
  Prefix('');

  Result := [];

  LJO := ToJSONObject;
  try
    for I := 0 to Pred(LJO.Count) do
      Result := Result + [LJO.Pairs[I].JsonString.Value];
  finally
    LJO.Free;
  end;
end;

function TLoggerSerializeItem.ToValues: TArray<string>;
var
  LJO: TJSONObject;
  I: Integer;
begin
  Result := [];

  LJO := ToJSONObject;
  try
    for I := 0 to Pred(LJO.Count) do
      Result := Result + [LJO.Pairs[I].JsonValue.Value];
  finally
    LJO.Free;
  end;
end;

function TLoggerSerializeItem.ToListTAG(const ALog: string; const ATag: TArray<string>): TDictionary<string, string>;
var
  LJO: TJSONObject;
  LLogFormatBase: TArray<string>;
  LTag: string;
  I: Integer;
  LLetter: TLoggerLetter;
  LKey: string;
  LValue: string;
begin
  Prefix('');
  IgnoreLogFormat(True);

  Result := TDictionary<string, string>.Create;

  LJO := ToJSONObject;
  try
    LLogFormatBase := [];

    for LTag in ATag do
      for I := 0 to Pred(LJO.Count) do
      begin
        LValue := LJO.Pairs[I].JsonValue.Value;

        for LLetter := Low(TLoggerLetter) to High(TLoggerLetter) do
        begin
          LKey := LJO.Pairs[I].JsonString.Value + TLoggerLetterKeyString[LLetter] + LTag;
          if not ALog.Contains(Format('{%s}', [LKey])) then
            Continue;

          Result.Add(LKey, LValue);
        end;
      end;
  finally
    LJO.Free;
  end;
end;

end.
