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

// https://man7.org/linux/man-pages/man3/syslog.3.html

unit DataLogger.Provider.SysLog;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(LINUX)}
  Posix.Base,
{$ENDIF}
  System.SysUtils, System.JSON;

type
  TProviderSysLog = class(TDataLoggerProvider<TProviderSysLog>)
  private
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;
  end;

implementation

{$IF DEFINED(LINUX)}
procedure OpenLog(ident: MarshaledAString; option: LongInt; facility: LongInt); cdecl; external libc name _PU + 'openlog';
procedure SysLog(priority: LongInt; _format: MarshaledAString; args: array of const); cdecl; external libc name _PU + 'syslog';
procedure CloseLog; cdecl; external libc name _PU + 'closelog';
{$ENDIF}

{ TProviderSysLog }

procedure TProviderSysLog.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
begin
  if AJSON.Trim.IsEmpty then
    Exit;

  try
    LJO := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  except
    on E: Exception do
      Exit;
  end;

  if not Assigned(LJO) then
    Exit;

  try
    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSysLog.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSysLog.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(LINUX)}
const
  LOG_OPTION_PID = $01;
  LOG_OPTION_NDELAY = $08;
  LOG_FACILITY_USER = 1 shl 3;
  LOG_LEVEL_CRITICAL = 2;
  LOG_LEVEL_ERROR = 3;
  LOG_LEVEL_WARNING = 4;
  LOG_LEVEL_NOTICE = 5;
  LOG_LEVEL_INFO = 6;
  LOG_LEVEL_DEBUG = 7;

var
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LPriority: LongInt;
  LM: TMarshaller;
{$ENDIF}
begin
{$IF DEFINED(LINUX)}
  if (Length(ACache) = 0) then
    Exit;

  OpenLog(nil, LOG_OPTION_PID or LOG_OPTION_NDELAY, LOG_FACILITY_USER);
  try
    for LItem in ACache do
    begin
      if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLast then
        Continue;

      LLog := SerializeItem.LogItem(LItem).ToString;

      case LItem.Level of
        TLoggerLevel.Trace, TLoggerLevel.Debug:
          LPriority := LOG_LEVEL_DEBUG;

        TLoggerLevel.Info, TLoggerLevel.Success:
          LPriority := LOG_LEVEL_INFO;

        TLoggerLevel.Warn:
          LPriority := LOG_LEVEL_WARNING;

        TLoggerLevel.Error:
          LPriority := LOG_LEVEL_ERROR;

        TLoggerLevel.Fatal:
          LPriority := LOG_LEVEL_CRITICAL;

        TLoggerLevel.Custom:
          LPriority := LOG_LEVEL_NOTICE;
      else
        LPriority := LOG_LEVEL_INFO;
      end;

      LRetriesCount := 0;

      while True do
        try
          SysLog(LPriority, LM.AsAnsi(LLog, CP_UTF8).ToPointer, []);
          Break;
        except
          on E: Exception do
          begin
            Inc(LRetriesCount);

            Sleep(50);

            if Assigned(FLogException) then
              FLogException(Self, LItem, E, LRetriesCount);

            if Self.Terminated then
              Exit;

            if (LRetriesCount <= 0) then
              Break;

            if (LRetriesCount >= FMaxRetries) then
              Break;
          end;
        end;
    end;
  finally
    CloseLog();
  end;
{$ENDIF}
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSysLog);

end.
