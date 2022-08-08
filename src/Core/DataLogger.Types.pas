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

  Exception = System.SysUtils.Exception;

{$SCOPEDENUMS ON}

  TLoggerType = (All, Trace, Debug, Info, Success, Warn, Error, Fatal, Custom);
  TLoggerTypes = set of TLoggerType;

  TLoggerTypeAutoCommit = (tcAll, tcBlock);
{$SCOPEDENUMS OFF}

  TLoggerTypeHelper = record helper for TLoggerType
  public
    procedure SetName(const AName: string);
    function ToString: string;
  end;

  TLoggerInternalItem = record
    TypeSlineBreak: Boolean;
  end;

  TLoggerItem = record
    Name: string;
    Sequence: Int64;
    TimeStamp: TDateTime;
    ThreadID: Integer;
    &Type: TLoggerType;
    TypeString: string;
    TypeLevel: Integer;
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

  TLoggerLogFormat = class
    class function AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AIgnoreLogFormat: Boolean = False): TJSONObject;
    class function AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem; const AIgnoreLogFormat: Boolean = False): string;
    class function AsString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): string;
    class function AsStream(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): TStream;
    class function AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AIgnoreLogFormat: Boolean = False): TStream;
  end;

  TOnLogException = reference to procedure(const Sender: TObject; const LogItem: TLoggerItem; const E: Exception; var RetriesCount: Integer);

  TLoggerFormat = record
  const
    LOG_NAME = '${name}';
    LOG_SEQUENCE = '${sequence}';
    LOG_TIMESTAMP = '${timestamp}';
    LOG_THREADID = '${thread_id}';
    LOG_PROCESSID = '${process_id}';
    LOG_TYPE = '${type}';
    LOG_TYPE_LEVEL = '${type_level}';
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

    DEFAULT_LOG_FORMAT = LOG_TIMESTAMP + ' [TID ' + LOG_THREADID + '] [PID ' + LOG_PROCESSID + '] [SEQ ' + LOG_SEQUENCE + '] [' + LOG_TYPE + '] [' + LOG_TAG + '] ' + LOG_MESSAGE;
  end;

implementation

{ TLoggerTypeHelper }

procedure TLoggerTypeHelper.SetName(const AName: string);
begin
  Self := TLoggerType(GetEnumValue(TypeInfo(TLoggerType), AName));
end;

function TLoggerTypeHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TLoggerType), Integer(Self));
end;

{ TLoggerLogFormat }

class function TLoggerLogFormat.AsJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AIgnoreLogFormat: Boolean = False): TJSONObject;
  procedure _Add(const ALogKey: string; const AJSONKey: string; const AJSONValue: TJSONValue);
  begin
    if ALogFormat.Contains(ALogKey) or AIgnoreLogFormat then
      Result.AddPair(AJSONKey, AJSONValue)
    else
      AJSONValue.Free;
  end;

var
  I: Integer;
  LJO: TJSONObject;
  LKey: string;
  LValue: TJSONValue;
begin
  Result := TJSONObject.Create;

  _Add(TLoggerFormat.LOG_TIMESTAMP, 'log_timestamp', TJSONString.Create(DateToISO8601(AItem.TimeStamp, False)));
  _Add(TLoggerFormat.LOG_NAME, 'log_name', TJSONString.Create(AItem.Name));
  _Add(TLoggerFormat.LOG_SEQUENCE, 'log_sequence', TJSONNumber.Create(AItem.Sequence));
  _Add(TLoggerFormat.LOG_THREADID, 'log_thread_id', TJSONNumber.Create(AItem.ThreadID));
  _Add(TLoggerFormat.LOG_TYPE, 'log_type', TJSONString.Create(AItem.TypeString));
  _Add(TLoggerFormat.LOG_TYPE_LEVEL, 'log_type_level', TJSONNumber.Create(AItem.TypeLevel));
  _Add(TLoggerFormat.LOG_TAG, 'log_tag', TJSONString.Create(AItem.Tag));

  if not AItem.MessageJSON.Trim.IsEmpty then
  begin
    _Add(TLoggerFormat.LOG_MESSAGE, 'log_message', TJSONString.Create(AItem.MessageJSON.Trim));

    try
      LJO := TJSONObject.ParseJSONValue(AItem.MessageJSON.Trim) as TJSONObject;

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
    _Add(TLoggerFormat.LOG_MESSAGE, 'log_message', TJSONString.Create(AItem.Message.Trim));

  _Add(TLoggerFormat.LOG_APPNAME, 'log_app_name', TJSONString.Create(AItem.AppName));
  _Add(TLoggerFormat.LOG_APPVERSION, 'log_app_version', TJSONString.Create(AItem.AppVersion.FileVersion));
  _Add(TLoggerFormat.LOG_APPPATH, 'log_app_path', TJSONString.Create(AItem.AppPath));
  _Add(TLoggerFormat.LOG_APPSIZE, 'log_app_size', TJSONString.Create(FormatFloat('#,##0.00 MB', AItem.AppSize / 1024)));

  _Add(TLoggerFormat.LOG_COMPUTERNAME, 'log_computer_name', TJSONString.Create(AItem.ComputerName));
  _Add(TLoggerFormat.LOG_USERNAME, 'log_username', TJSONString.Create(AItem.Username));
  _Add(TLoggerFormat.LOG_OSVERSION, 'log_os_version', TJSONString.Create(AItem.OSVersion));
  _Add(TLoggerFormat.LOG_PROCESSID, 'log_process_id', TJSONString.Create(AItem.ProcessId));

  _Add(TLoggerFormat.LOG_IP_LOCAL, 'log_ip_local', TJSONString.Create(AItem.IPLocal));
end;

class function TLoggerLogFormat.AsJsonObjectToString(const ALogFormat: string; const AItem: TLoggerItem; const AIgnoreLogFormat: Boolean = False): string;
var
  LJO: TJSONObject;
begin
  LJO := AsJsonObject(ALogFormat, AItem, AIgnoreLogFormat);
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

class function TLoggerLogFormat.AsString(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): string;
var
  LLog: string;
  LJO: TJSONObject;
  I: Integer;

  function _Add(const AKey: string; const AValue: string): string;
  begin
    Result := LLog.Replace(AKey, AValue);
  end;

begin
  LLog := ALogFormat;

  LLog := _Add(TLoggerFormat.LOG_NAME, AItem.Name);
  LLog := _Add(TLoggerFormat.LOG_SEQUENCE, AItem.Sequence.ToString);
  LLog := _Add(TLoggerFormat.LOG_TIMESTAMP, FormatDateTime(AFormatTimestamp, AItem.TimeStamp));
  LLog := _Add(TLoggerFormat.LOG_THREADID, AItem.ThreadID.ToString);
  LLog := _Add(TLoggerFormat.LOG_TYPE, AItem.TypeString);
  LLog := _Add(TLoggerFormat.LOG_TYPE_LEVEL, AItem.TypeLevel.ToString);
  LLog := _Add(TLoggerFormat.LOG_TAG, AItem.Tag.Trim);

  if not AItem.MessageJSON.Trim.IsEmpty then
  begin
    LLog := _Add(TLoggerFormat.LOG_MESSAGE, AItem.MessageJSON.Trim);

    try
      LJO := TJSONObject.ParseJSONValue(AItem.MessageJSON.Trim) as TJSONObject;

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
  LLog := _Add(TLoggerFormat.LOG_APPSIZE, FormatFloat('#,##0.00 MB', AItem.AppSize / 1024));

  LLog := _Add(TLoggerFormat.LOG_COMPUTERNAME, AItem.ComputerName);
  LLog := _Add(TLoggerFormat.LOG_USERNAME, AItem.Username);
  LLog := _Add(TLoggerFormat.LOG_OSVERSION, AItem.OSVersion);
  LLog := _Add(TLoggerFormat.LOG_PROCESSID, AItem.ProcessId);
  LLog := _Add(TLoggerFormat.LOG_IP_LOCAL, AItem.IPLocal);

  Result := LLog;
end;

class function TLoggerLogFormat.AsStream(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string): TStream;
var
  LLog: string;
begin
  LLog := AsString(ALogFormat, AItem, AFormatTimestamp);

  Result := TStringStream.Create(LLog, TEncoding.UTF8);
  Result.Seek(0, soFromBeginning);
end;

class function TLoggerLogFormat.AsStreamJsonObject(const ALogFormat: string; const AItem: TLoggerItem; const AIgnoreLogFormat: Boolean = False): TStream;
var
  LLog: string;
begin
  LLog := AsJsonObjectToString(ALogFormat, AItem, AIgnoreLogFormat);

  Result := TStringStream.Create(LLog, TEncoding.UTF8);
  Result.Seek(0, soFromBeginning);
end;

end.
