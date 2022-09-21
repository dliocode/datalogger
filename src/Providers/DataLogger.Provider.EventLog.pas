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

// C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Administrative Tools\Event Viewer.lnk

unit DataLogger.Provider.EventLog;

interface

uses
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils,
{$IF DEFINED(MSWINDOWS)}
  Vcl.SvcMgr, Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.Types, System.JSON;

type
  TProviderEventLog = class(TDataLoggerProvider<TProviderEventLog>)
  private
    FName: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Name(const AValue: string): TProviderEventLog;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

{ TProviderEventLog }

constructor TProviderEventLog.Create;
begin
  inherited Create;

  Name(TLoggerUtils.AppName);
end;

function TProviderEventLog.Name(const AValue: string): TProviderEventLog;
begin
  Result := Self;

  if AValue.Trim.IsEmpty then
    FName := TLoggerUtils.AppName
  else
    FName := AValue;
end;

procedure TProviderEventLog.LoadFromJSON(const AJSON: string);
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
    Name(LJO.GetValue<string>('name', FName));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderEventLog.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('name', FName);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderEventLog.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(MSWINDOWS)}

var
  LEventLogger: TEventLogger;
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LEventType: DWord;
begin
  if Length(ACache) = 0 then
    Exit;

  LEventLogger := TEventLogger.Create(FName);
  try
    for LItem in ACache do
    begin
      if LItem.InternalItem.LevelSlineBreak then
        Continue;

      LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

      case LItem.Level of
        TLoggerLevel.Info:
          LEventType := EVENTLOG_INFORMATION_TYPE;
        TLoggerLevel.Warn:
          LEventType := EVENTLOG_WARNING_TYPE;
        TLoggerLevel.Error:
          LEventType := EVENTLOG_ERROR_TYPE;
        TLoggerLevel.Success:
          LEventType := EVENTLOG_SUCCESS;
      else
        LEventType := EVENTLOG_INFORMATION_TYPE;
      end;

      LRetriesCount := 0;

      while True do
        try
          LEventLogger.LogMessage(LLog, LEventType, 0, 0);
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

            if LRetriesCount <= 0 then
              Break;

            if LRetriesCount >= FMaxRetries then
              Break;
          end;
        end;
    end;
  finally
    LEventLogger.Free;
  end;
end;

{$ELSE}

begin
end;

{$ENDIF}

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderEventLog);

end.
