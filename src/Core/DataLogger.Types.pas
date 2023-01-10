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

unit DataLogger.Types;

interface

uses
  DataLogger.Utils,
  System.TypInfo, System.JSON, System.DateUtils, System.SysUtils, System.Generics.Collections, System.Classes;

type
  EDataLoggerException = class(Exception)
  end;

{$SCOPEDENUMS ON}

  TLoggerLevel = (All, Trace, Debug, Info, Success, Warn, Error, Fatal, Custom);
  TLoggerLevels = set of TLoggerLevel;

  TLoggerTransactionTypeCommit = (tcAll, tcBlock);
{$SCOPEDENUMS OFF}

  TLoggerLevelHelper = record helper for TLoggerLevel
  public
    procedure SetLevelName(const AName: string);
    function ToString: string;
  end;

  TLoggerInternalItem = record
    IsSlinebreak: Boolean;
    TransactionID: string;
  end;

  TLoggerItem = record
    Name: string;
    Sequence: Int64;
    TimeStamp: TDateTime;
    TimeStampISO8601: string;
    TimeStampUNIX: Int64;
    ThreadID: Int64;
    Level: TLoggerLevel;
    LevelString: string;
    LevelValue: Integer;
    Tag: string;
    Message: string;
    MessageJSON: string;

    AppName: string;
    AppPath: string;
    AppVersion: TLoggerUtils.TAppVersion;
    AppSize: Double;

    ComputerName: string;
    Username: string;
    OSVersion: string;
    ProcessId: string;
    IPLocal: string;
    MACAddress: string;

    InternalItem: TLoggerInternalItem;
  end;

  TDataLoggerListItem = TList<TLoggerItem>;
  TDataLoggerListItemTransaction = TObjectDictionary<Integer, TDataLoggerListItem>;

  TLoggerSerializeItem = class
  private
    class function AsBase(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False; const APrefix: string = 'log_'): TJSONObject;
  public
    class function AsHeader(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TArray<string>;
    class function AsValues(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TArray<string>;

    class function AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TJSONObject;
    class function AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): string;
    class function AsString(
      const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
      const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): string;
    class function AsStream(
      const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
      const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): TStream;
    class function AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TStream;
    class function ListTAG(const ALog: string; const ATag: TArray<string>; const AItem: TLoggerItem; const AFormatTimestamp: string): TDictionary<string, string>;
  end;

  TLoggerOnException = reference to procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetriesCount: Integer);

  TLoggerFormat = record
  const
    LOG_NAME = '${name}';
    LOG_SEQUENCE = '${sequence}';
    LOG_TIMESTAMP = '${timestamp}';
    LOG_TIMESTAMP_ISO8601 = '${timestamp_iso8601}';
    LOG_TIMESTAMP_UNIX = '${timestamp_unix}';
    LOG_THREADID = '${thread_id}';
    LOG_PROCESSID = '${process_id}';
    LOG_LEVEL = '${level}';
    LOG_LEVEL_VALUE = '${level_value}';
    LOG_TAG = '${tag}';
    LOG_MESSAGE = '${message}';

    LOG_APPNAME = '${app_name}';
    LOG_APPPATH = '${app_path}';
    LOG_APPVERSION = '${app_version}';
    LOG_APPSIZE = '${app_size}';

    LOG_COMPUTERNAME = '${computer_name}';
    LOG_USERNAME = '${username}';
    LOG_OSVERSION = '${os_version}';
    LOG_IP_LOCAL = '${ip_local}';
    LOG_MAC_ADDRESS = '${mac_address}';

    DEFAULT_LOG_FORMAT = LOG_TIMESTAMP + ' [TID ' + LOG_THREADID + '] [PID ' + LOG_PROCESSID + '] [SEQ ' + LOG_SEQUENCE + '] [' + LOG_LEVEL + '] [' + LOG_TAG + '] ' + LOG_MESSAGE;
  end;

  TLoggerConst = record
  const
    TRANSACTION_ID = 'DATALOGGER_TRANSACTION';
    BASE_FORMAT: array [0 .. 19] of string = ('timestamp', 'timestamp_iso8601', 'timestamp_unix', 'name', 'sequence', 'thread_id', 'level', 'level_value', 'tag', 'message', 'app_name', 'app_version', 'app_path', 'app_size', 'computer_name', 'username', 'os_version', 'process_id', 'ip_local', 'mac_address');
    BASE_FORMAT_NAME: array [0 .. 19] of string = ('Timestamp', 'Timestamp ISO8601', 'Timestamp Unix', 'Name', 'Sequence', 'Thread ID', 'Level', 'Level Value', 'Tag', 'Message', 'App Name', 'App Version', 'App Path', 'App Size', 'Computer Name', 'Username', 'OS Version', 'Process ID', 'IP Local', 'Mac Address');
  end;

implementation

type
  TLoggerLetter = (tlNone, tlUpper, tlLower, tlFirstUp);

const
  TLoggerLetterKeyString: array [TLoggerLetter] of string = ((''), ('_upper'), ('_lower'), ('_first_up'));

  { TLoggerLevelHelper }

procedure TLoggerLevelHelper.SetLevelName(const AName: string);
begin
  Self := TLoggerLevel(GetEnumValue(TypeInfo(TLoggerLevel), AName));
end;

function TLoggerLevelHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TLoggerLevel), Integer(Self)).ToUpper;
end;

{ TLoggerSerializeItem }

class function TLoggerSerializeItem.AsBase(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False; const APrefix: string = 'log_'): TJSONObject;
  procedure _Add(const ALogKey: string; const AJSONKey: string; const AJSONValue: TJSONValue);
  begin
    if ALogFormat.Contains(ALogKey) or AIgnoreLogFormat then
      Result.AddPair(AJSONKey, AJSONValue)
    else
      AJSONValue.Free;
  end;

var
  LJO: TJSONObject;
  I: Integer;
  LKey: string;
  LValue: TJSONValue;
begin
  Result := TJSONObject.Create;

  _Add(TLoggerFormat.LOG_TIMESTAMP, APrefix + 'timestamp', TJSONString.Create(FormatDateTime(AFormatTimestamp, AItem.TimeStamp)));
  _Add(TLoggerFormat.LOG_TIMESTAMP_ISO8601, APrefix + 'timestamp_iso8601', TJSONString.Create(AItem.TimeStampISO8601));
  _Add(TLoggerFormat.LOG_TIMESTAMP_UNIX, APrefix + 'timestamp_unix', TJSONNumber.Create(AItem.TimeStampUNIX));
  _Add(TLoggerFormat.LOG_NAME, APrefix + 'name', TJSONString.Create(AItem.Name));
  _Add(TLoggerFormat.LOG_SEQUENCE, APrefix + 'sequence', TJSONNumber.Create(AItem.Sequence));
  _Add(TLoggerFormat.LOG_THREADID, APrefix + 'thread_id', TJSONNumber.Create(AItem.ThreadID));
  _Add(TLoggerFormat.LOG_LEVEL, APrefix + 'level', TJSONString.Create(AItem.LevelString));
  _Add(TLoggerFormat.LOG_LEVEL_VALUE, APrefix + 'level_value', TJSONNumber.Create(AItem.LevelValue));
  _Add(TLoggerFormat.LOG_TAG, APrefix + 'tag', TJSONString.Create(AItem.Tag));

  if not AItem.MessageJSON.IsEmpty then
    _Add(TLoggerFormat.LOG_MESSAGE, APrefix + 'message', TJSONString.Create(AItem.MessageJSON.Trim))
  else
    _Add(TLoggerFormat.LOG_MESSAGE, APrefix + 'message', TJSONString.Create(AItem.Message.Trim));

  _Add(TLoggerFormat.LOG_APPNAME, APrefix + 'app_name', TJSONString.Create(AItem.AppName));
  _Add(TLoggerFormat.LOG_APPVERSION, APrefix + 'app_version', TJSONString.Create(AItem.AppVersion.FileVersion));
  _Add(TLoggerFormat.LOG_APPPATH, APrefix + 'app_path', TJSONString.Create(AItem.AppPath));
  _Add(TLoggerFormat.LOG_APPSIZE, APrefix + 'app_size', TJSONString.Create(FormatFloat('#,##0.00 MB', AItem.AppSize / 1024)));

  _Add(TLoggerFormat.LOG_COMPUTERNAME, APrefix + 'computer_name', TJSONString.Create(AItem.ComputerName));
  _Add(TLoggerFormat.LOG_USERNAME, APrefix + 'username', TJSONString.Create(AItem.Username));
  _Add(TLoggerFormat.LOG_OSVERSION, APrefix + 'os_version', TJSONString.Create(AItem.OSVersion));
  _Add(TLoggerFormat.LOG_PROCESSID, APrefix + 'process_id', TJSONString.Create(AItem.ProcessId));

  _Add(TLoggerFormat.LOG_IP_LOCAL, APrefix + 'ip_local', TJSONString.Create(AItem.IPLocal));
  _Add(TLoggerFormat.LOG_MAC_ADDRESS, APrefix + 'mac_address', TJSONString.Create(AItem.MACAddress));

  if not AItem.MessageJSON.IsEmpty then
    try
      LJO := TJSONObject.ParseJSONValue(AItem.MessageJSON) as TJSONObject;

      if Assigned(LJO) then
        try
          for I := 0 to Pred(LJO.Count) do
          begin
            LKey := LJO.Pairs[I].JsonString.Value;
            LValue := LJO.Pairs[I].JsonValue.Clone as TJSONValue;

            _Add(Format('${%s}', [LKey]), LKey, LValue);
          end;
        finally
          LJO.Free;
        end;
    except
    end;
end;

class function TLoggerSerializeItem.AsHeader(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TArray<string>;
var
  LJO: TJSONObject;
  I: Integer;
  LKey: string;
  LLetter: TLoggerLetter;
  LKeyLetter: string;
begin
  Result := [];

  LJO := AsBase(ALogFormat, AItem, AFormatTimestamp, True, '');
  try
    for I := 0 to Pred(LJO.Count) do
    begin
      LKey := LJO.Pairs[I].JsonString.Value;

      for LLetter := Low(TLoggerLetter) to High(TLoggerLetter) do
      begin
        if not AIgnoreLogFormat then
        begin
          LKeyLetter := Format('${%s}', [LKey + TLoggerLetterKeyString[LLetter]]);
          if not ALogFormat.Contains(LKeyLetter) then
            Continue;
        end;

        Result := Result + [LKey];

        if AIgnoreLogFormat then
          Break;
      end;
    end;
  finally
    LJO.Free;
  end;
end;

class function TLoggerSerializeItem.AsValues(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TArray<string>;
var
  LJO: TJSONObject;
  I: Integer;
  LKey: string;
  LLetter: TLoggerLetter;
  LValue: string;
  LKeyLetter: string;
begin
  Result := [];

  LJO := AsBase(ALogFormat, AItem, AFormatTimestamp, True, '');
  try
    for I := 0 to Pred(LJO.Count) do
    begin
      LKey := LJO.Pairs[I].JsonString.Value;
      LValue := LJO.Pairs[I].JsonValue.Value;

      for LLetter := Low(TLoggerLetter) to High(TLoggerLetter) do
      begin
        if not AIgnoreLogFormat then
        begin
          LKeyLetter := Format('${%s}', [LKey + TLoggerLetterKeyString[LLetter]]);
          if not ALogFormat.Contains(LKeyLetter) then
            Continue;
        end;

        if not LValue.Trim.IsEmpty then
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

        Result := Result + [LValue];

        if AIgnoreLogFormat then
          Break;
      end;
    end;
  finally
    LJO.Free;
  end;
end;

class function TLoggerSerializeItem.AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TJSONObject;
begin
  Result := AsBase(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat, 'log_');
end;

class function TLoggerSerializeItem.AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): string;
var
  LJO: TJSONObject;
begin
  LJO := AsJsonObject(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat);
  try
{$IF CompilerVersion > 32} // 32 = Delphi Tokyo (10.2)
    Result := LJO.ToString;
{$ELSE}
    Result := LJO.ToJSON;
{$ENDIF}
  finally
    LJO.Free;
  end;
end;

class function TLoggerSerializeItem.AsString(
  const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
  const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): string;
var
  LLog: string;
  LJO: TJSONObject;
  I: Integer;
  LKey: string;
  LValue: string;
  LKeyLetter: string;
  LLetter: TLoggerLetter;
begin
  if not AIgnoreLogFormat then
    LLog := ALogFormat
  else
    LLog := '';

  LJO := AsBase(ALogFormat, AItem, AFormatTimestamp, True, '');
  try
    for I := 0 to Pred(LJO.Count) do
    begin
      LKey := LJO.Pairs[I].JsonString.Value;
      LValue := LJO.Pairs[I].JsonValue.Value;

      if not AIgnoreLogFormat then
      begin
        for LLetter := Low(TLoggerLetter) to High(TLoggerLetter) do
        begin
          LKeyLetter := Format('${%s}', [LKey + TLoggerLetterKeyString[LLetter]]);
          if not LLog.Contains(LKeyLetter) then
            Continue;

          if not LValue.Trim.IsEmpty then
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

          LLog := LLog.Replace(LKeyLetter, LValue);
        end;
      end
      else
      begin
        if AIgnoreLogFormatIncludeKey then
          LLog := LLog + LKey + AIgnoreLogFormatIncludeKeySeparator + LValue + AIgnoreLogFormatSeparator
        else
          LLog := LLog + LValue + AIgnoreLogFormatSeparator;
      end;
    end;

    if AIgnoreLogFormat then
      LLog := Copy(LLog, 1, Length(LLog) - Length(AIgnoreLogFormatSeparator));
  finally
    LJO.Free;
  end;

  Result := LLog;
end;

class function TLoggerSerializeItem.AsStream(
  const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
  const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): TStream;
var
  LLog: string;
begin
  LLog := AsString(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat, AIgnoreLogFormatSeparator, AIgnoreLogFormatIncludeKey, AIgnoreLogFormatIncludeKeySeparator);

  Result := TStringStream.Create(LLog, TEncoding.UTF8);
  Result.Seek(0, soFromBeginning);
end;

class function TLoggerSerializeItem.AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TStream;
var
  LLog: string;
begin
  LLog := AsJsonObjectToString(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat);

  Result := TStringStream.Create(LLog, TEncoding.UTF8);
  Result.Seek(0, soFromBeginning);
end;

class function TLoggerSerializeItem.ListTAG(const ALog: string; const ATag: TArray<string>; const AItem: TLoggerItem; const AFormatTimestamp: string): TDictionary<string, string>;
var
  LJO: TJSONObject;
  LLogFormatBase: TArray<string>;
  LTag: string;
  I: Integer;
  LLetter: TLoggerLetter;
  LKey: string;
  LValue: string;
begin
  Result := TDictionary<string, string>.Create;

  LJO := TLoggerSerializeItem.AsBase('', AItem, AFormatTimestamp, True, '');
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

          if not LValue.Trim.IsEmpty then
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

          Result.Add(LKey, LValue);
        end;
      end;
  finally
    LJO.Free;
  end;
end;

end.
