{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Types;

interface

uses
  DataLogger.Utils,
  System.TypInfo, System.JSON, System.DateUtils, System.SysUtils, System.Generics.Collections, System.Classes;

type
  EDataLoggerException = class(Exception)
  end;

  TLoggerType = (All, Trace, Debug, Info, Success, Warn, Error, Fatal);
  TLoggerTypes = set of TLoggerType;

  TLoggerTypeHelper = record helper for TLoggerType
  public
    function ToString: string;
  end;

  TLoggerItem = record
    Sequence: UInt64;
    TimeStamp: TDateTime;
    ThreadID: Integer;
    &Type: TLoggerType;
    Tag: string;
    Message: string;
    MessageJSON: string;

    AppName: string;
    AppPath: string;
    AppVersion: TLoggerUtils.TAppVersion;
    ComputerName: string;
    Username: string;
    OSVersion: string;
    ProcessId: string;
  end;

  TLoggerLogFormat = class
    class function AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem): TJsonObject;
    class function AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem): string;
    class function AsString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): string;
    class function AsStream(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): TStream;
    class function AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem): TStream;
  end;

  TOnLogException = reference to procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetryCount: Integer);

  TLoggerFormat = record
  const
    LOG_SEQUENCE = '${sequence}';
    LOG_TIMESTAMP = '${timestamp}';
    LOG_THREADID = '${threadid}';
    LOG_PROCESSID = '${processid}';
    LOG_TYPE = '${type}';
    LOG_TAG = '${tag}';
    LOG_MESSAGE = '${message}';

    LOG_APPNAME = '${appname}';
    LOG_APPVERSION = '${appversion}';
    LOG_APPPATH = '${apppath}';
    LOG_COMPUTERNAME = '${computername}';
    LOG_USERNAME = '${username}';
    LOG_OSVERSION = '${osversion}';

    DEFAULT_LOG_FORMAT = LOG_TIMESTAMP + ' [TID ' + LOG_THREADID + '] [PID ' + LOG_PROCESSID + '] [SEQ ' + LOG_SEQUENCE + '] [' + LOG_TYPE + '] [' + LOG_TAG + '] ' + LOG_MESSAGE;
  end;

implementation

{ TLoggerTypeHelper }

function TLoggerTypeHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TLoggerType), Integer(Self));
end;

{ TLoggerLogFormat }

class function TLoggerLogFormat.AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem): TJsonObject;
  procedure _Add(const ALogKey: string; const AJSONKey: string; const AJSONValue: TJSONValue);
  begin
    if ALogFormat.Contains(ALogKey) then
      Result.AddPair(AJSONKey, AJSONValue)
    else
      AJSONValue.Free;
  end;

var
  I: Integer;
  LJO: TJsonObject;
  LKey: string;
  LValue: TJsonValue;
begin
  Result := TJsonObject.Create;

  Result.AddPair('timestamp', TJSONString.Create(DateToISO8601(AItem.TimeStamp, False)));

  _Add(TLoggerFormat.LOG_THREADID, 'log_sequence', TJSONNumber.Create(AItem.Sequence));
  _Add(TLoggerFormat.LOG_TIMESTAMP, 'log_datetime', TJSONString.Create(DateToISO8601(AItem.TimeStamp, False)));
  _Add(TLoggerFormat.LOG_THREADID, 'log_threadid', TJSONNumber.Create(AItem.ThreadID));
  _Add(TLoggerFormat.LOG_PROCESSID, 'log_processid', TJSONString.Create(AItem.ProcessId));
  _Add(TLoggerFormat.LOG_TYPE, 'log_type', TJSONString.Create(AItem.&Type.ToString));
  _Add(TLoggerFormat.LOG_TAG, 'log_tag', TJSONString.Create(AItem.Tag));

  if not AItem.MessageJSON.Trim.IsEmpty then
  begin
    _Add(TLoggerFormat.LOG_MESSAGE, 'log_message', TJSONString.Create(AItem.MessageJSON.Trim));

    try
      LJO := TJsonObject.ParseJSONValue(AItem.MessageJSON.Trim) as TJsonObject;

      if Assigned(LJO) then
        try
          for I := 0 to Pred(LJO.Count) do
          begin
            LKey := Format('${%s}',[LJO.Pairs[I].JsonString.Value]);

            if ALogFormat.Contains(LKey) then
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
    _Add(TLoggerFormat.LOG_MESSAGE, 'log_message', TJSONString.Create(AItem.Message.Trim));

  _Add(TLoggerFormat.LOG_APPNAME, 'log_appname', TJSONString.Create(AItem.AppName));
  _Add(TLoggerFormat.LOG_APPPATH, 'log_appversion', TJSONString.Create(AItem.AppVersion.FileVersion));
  _Add(TLoggerFormat.LOG_APPPATH, 'log_apppath', TJSONString.Create(AItem.AppPath));
  _Add(TLoggerFormat.LOG_COMPUTERNAME, 'log_computername', TJSONString.Create(AItem.ComputerName));
  _Add(TLoggerFormat.LOG_USERNAME, 'log_username', TJSONString.Create(AItem.Username));
  _Add(TLoggerFormat.LOG_OSVERSION, 'log_osversion', TJSONString.Create(AItem.OSVersion));
end;

class function TLoggerLogFormat.AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem): string;
var
  LJO: TJsonObject;
begin
  LJO := AsJsonObject(ALogFormat, AItem);
  try
    Result := LJO.ToString;
  finally
    LJO.Free;
  end;
end;

class function TLoggerLogFormat.AsString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): string;
var
  LLog: string;
  LJO: TJsonObject;
  I: Integer;

  function _Add(const AKey: string; const AValue: string): string;
  begin
    Result := LLog.Replace(AKey, AValue);
  end;

begin
  LLog := ALogFormat;

  LLog := _Add(TLoggerFormat.LOG_SEQUENCE, AItem.Sequence.ToString);
  LLog := _Add(TLoggerFormat.LOG_TIMESTAMP, FormatDateTime(AFormatTimestamp, AItem.TimeStamp));
  LLog := _Add(TLoggerFormat.LOG_THREADID, AItem.ThreadID.ToString);
  LLog := _Add(TLoggerFormat.LOG_PROCESSID, AItem.ProcessId);
  LLog := _Add(TLoggerFormat.LOG_TYPE, AItem.&Type.ToString);
  LLog := _Add(TLoggerFormat.LOG_TAG, AItem.Tag.Trim);

  if not AItem.MessageJSON.Trim.IsEmpty then
  begin
    LLog := _Add(TLoggerFormat.LOG_MESSAGE, AItem.MessageJSON.Trim);

    try
      LJO := TJsonObject.ParseJSONValue(AItem.MessageJSON.Trim) as TJsonObject;

      if Assigned(LJO) then
        try
          for I := 0 to Pred(LJO.Count) do
            LLog := _Add(Format('${%s}', [LJO.Pairs[I].JsonString.Value]), LJO.Pairs[I].JsonValue.Value);
        finally
          LJO.Free;
        end;
    except
    end;
  end
  else
    LLog := _Add(TLoggerFormat.LOG_MESSAGE, AItem.Message.Trim);

  LLog := _Add(TLoggerFormat.LOG_APPNAME, AItem.AppName);
  LLog := _Add(TLoggerFormat.LOG_APPPATH, AItem.AppPath);
  LLog := _Add(TLoggerFormat.LOG_APPVERSION, AItem.AppVersion.FileVersion);
  LLog := _Add(TLoggerFormat.LOG_COMPUTERNAME, AItem.ComputerName);
  LLog := _Add(TLoggerFormat.LOG_USERNAME, AItem.Username);
  LLog := _Add(TLoggerFormat.LOG_OSVERSION, AItem.OSVersion);

  Result := LLog;
end;

class function TLoggerLogFormat.AsStream(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): TStream;
begin
  Result := TStringStream.Create(AsString(ALogFormat, AItem, AFormatTimestamp), TEncoding.UTF8);
  Result.Seek(0, soFromBeginning);
end;

class function TLoggerLogFormat.AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem): TStream;
begin
  Result := TStringStream.Create(AsJsonObjectToString(ALogFormat, AItem), TEncoding.UTF8);
end;

end.
