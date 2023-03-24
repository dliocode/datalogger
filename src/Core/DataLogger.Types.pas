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
    IsUndoLastLine: Boolean;
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

{ TLoggerLevelHelper }

procedure TLoggerLevelHelper.SetLevelName(const AName: string);
begin
  Self := TLoggerLevel(GetEnumValue(TypeInfo(TLoggerLevel), AName));
end;

function TLoggerLevelHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TLoggerLevel), Integer(Self)).ToUpper;
end;

end.
