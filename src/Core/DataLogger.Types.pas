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
    procedure SetName(const AName: string);
    function ToString: string;
  end;

  TLoggerInternalItem = record
    LevelSlineBreak: Boolean;
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

    InternalItem: TLoggerInternalItem;
  end;

  TDataLoggerListItem = TList<TLoggerItem>;
  TDataLoggerListItemTransaction = TObjectDictionary<Integer, TDataLoggerListItem>;

  TLoggerLogFormat = class
  private
    class function AsBase(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False; const APrefix: string = 'log_'): TJSONObject;
  public
    class function AsCSV(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False; const ASeparator: Char = ','; const AOnlyHeader: Boolean = False): string;
    class function AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TJSONObject;
    class function AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): string;
    class function AsString(
      const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
      const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): string;
    class function AsStream(
      const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
      const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): TStream;
    class function AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TStream;
  end;

  TOnLogException = reference to procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetriesCount: Integer);

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

    DEFAULT_LOG_FORMAT = LOG_TIMESTAMP + ' [TID ' + LOG_THREADID + '] [PID ' + LOG_PROCESSID + '] [SEQ ' + LOG_SEQUENCE + '] [' + LOG_LEVEL + '] [' + LOG_TAG + '] ' + LOG_MESSAGE;
  end;

implementation

{ TLoggerLevelHelper }

procedure TLoggerLevelHelper.SetName(const AName: string);
begin
  Self := TLoggerLevel(GetEnumValue(TypeInfo(TLoggerLevel), AName));
end;

function TLoggerLevelHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TLoggerLevel), Integer(Self));
end;

{ TLoggerLogFormat }

class function TLoggerLogFormat.AsBase(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False; const APrefix: string = 'log_'): TJSONObject;
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
  begin
    _Add(TLoggerFormat.LOG_MESSAGE, APrefix + 'message', TJSONString.Create(AItem.MessageJSON.Trim));

    try
      LJO := TJSONObject.ParseJSONValue(AItem.MessageJSON) as TJSONObject;

      if Assigned(LJO) then
        try
          for I := 0 to Pred(LJO.Count) do
          begin
            LKey := Format('${%s}', [LJO.Pairs[I].JsonString.Value]);

            if ALogFormat.Contains(LKey) or AIgnoreLogFormat then
            begin
              LValue := LJO.Pairs[I].JsonValue.Clone as TJSONValue;
              Result.AddPair(LJO.Pairs[I].JsonString.Value, LValue);
            end;
          end;
        finally
          LJO.Free;
        end;
    except
    end;
  end
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
end;

class function TLoggerLogFormat.AsCSV(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False; const ASeparator: Char = ','; const AOnlyHeader: Boolean = False): string;
var
  LLog: string;
  LJO: TJSONObject;
  LKey: string;
  I: Integer;
begin
  LLog := '';

  LJO := AsBase(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat, '');
  try
    for I := 0 to Pred(LJO.Count) do
    begin
      if AOnlyHeader then
      begin
        LKey := LJO.Pairs[I].JsonString.Value;
        LLog := LLog + LKey + ASeparator
      end
      else
        LLog := LLog + LJO.Pairs[I].JsonValue.Value + ASeparator;
    end;

    Result := LLog.Trim([ASeparator]);
  finally
    LJO.Free;
  end;
end;

class function TLoggerLogFormat.AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TJSONObject;
begin
  Result := AsBase(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat, 'log_');
end;

class function TLoggerLogFormat.AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): string;
var
  LJO: TJSONObject;
begin
  LJO := AsJsonObject(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat);
  try
{$IF RTLVersion > 32} // 32 = Delphi Tokyo (10.2)
    Result := LJO.ToString;
{$ELSE}
    Result := LJO.ToJSON;
{$ENDIF}
  finally
    LJO.Free;
  end;
end;

class function TLoggerLogFormat.AsString(
  const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
  const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): string;
var
  LLog: string;
  LJO: TJSONObject;
  I: Integer;
  LKey: string;
  LValue: string;
begin
  if not AIgnoreLogFormat then
    LLog := ALogFormat
  else
    LLog := '';

  LJO := AsBase(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat, '');
  try
    for I := 0 to Pred(LJO.Count) do
    begin
      LKey := LJO.Pairs[I].JsonString.Value;
      LValue := LJO.Pairs[I].JsonValue.Value;

      if not AIgnoreLogFormat then
      begin
        LLog := LLog.Replace(Format('${%s}', [LKey]), LValue);
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

class function TLoggerLogFormat.AsStream(
  const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False;
  const AIgnoreLogFormatSeparator: string = ' '; const AIgnoreLogFormatIncludeKey: Boolean = False; const AIgnoreLogFormatIncludeKeySeparator: string = ' -> '): TStream;
var
  LLog: string;
begin
  LLog := AsString(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat, AIgnoreLogFormatSeparator, AIgnoreLogFormatIncludeKey, AIgnoreLogFormatIncludeKeySeparator);

  Result := TStringStream.Create(LLog, TEncoding.UTF8);
  Result.Seek(0, soFromBeginning);
end;

class function TLoggerLogFormat.AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string; const AIgnoreLogFormat: Boolean = False): TStream;
var
  LLog: string;
begin
  LLog := AsJsonObjectToString(ALogFormat, AItem, AFormatTimestamp, AIgnoreLogFormat);

  Result := TStringStream.Create(LLog, TEncoding.UTF8);
  Result.Seek(0, soFromBeginning);
end;

end.
