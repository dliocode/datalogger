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

unit DataLogger.Provider.SysLog.Indy;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  IdSysLog, IdSysLogMessage,
  System.SysUtils, System.JSON;

type
  TProviderSysLogIndy = class(TDataLoggerProvider<TProviderSysLogIndy>)
  private
    FSysLog: TIdSysLog;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Host(const AValue: string): TProviderSysLogIndy;
    function Port(const AValue: Integer): TProviderSysLogIndy;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSysLogIndy }

constructor TProviderSysLogIndy.Create;
begin
  inherited Create;

  FSysLog := TIdSysLog.Create(nil);

  Host('127.0.0.1');
  Port(514);
end;

destructor TProviderSysLogIndy.Destroy;
begin
  FSysLog.Free;
  inherited;
end;

function TProviderSysLogIndy.Host(const AValue: string): TProviderSysLogIndy;
begin
  Result := Self;
  FSysLog.Host := AValue;
end;

function TProviderSysLogIndy.Port(const AValue: Integer): TProviderSysLogIndy;
begin
  Result := Self;
  FSysLog.Port := AValue;
end;

procedure TProviderSysLogIndy.LoadFromJSON(const AJSON: string);
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
    Host(LJO.GetValue<string>('host', FSysLog.Host));
    Port(LJO.GetValue<Integer>('port', FSysLog.Port));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSysLogIndy.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('host', TJSONString.Create(FSysLog.Host));
    LJO.AddPair('port', TJSONNumber.Create(FSysLog.Port));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSysLogIndy.Save(const ACache: TArray<TLoggerItem>);
var
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LSysLogMessage: TIdSysLogMessage;
begin
  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLast then
      Continue;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LSysLogMessage := TIdSysLogMessage.Create(nil);
    try
      LSysLogMessage.TimeStamp := LItem.TimeStamp;
      LSysLogMessage.Hostname := LItem.ComputerName;
      LSysLogMessage.Facility := TIdSyslogFacility.sfUserLevel;;
      LSysLogMessage.Msg.Process := LItem.ProcessId;
      LSysLogMessage.Msg.PID := LItem.ProcessId.ToInteger;

      case LItem.Level of
        TLoggerLevel.Trace:
          LSysLogMessage.Severity := TIdSyslogSeverity.slInformational;

        TLoggerLevel.Debug:
          LSysLogMessage.Severity := TIdSyslogSeverity.slDebug;

        TLoggerLevel.Info:
          LSysLogMessage.Severity := TIdSyslogSeverity.slInformational;

        TLoggerLevel.Warn:
          LSysLogMessage.Severity := TIdSyslogSeverity.slWarning;

        TLoggerLevel.Error:
          LSysLogMessage.Severity := TIdSyslogSeverity.slError;

        TLoggerLevel.Success:
          LSysLogMessage.Severity := TIdSyslogSeverity.slNotice;

        TLoggerLevel.Fatal:
          LSysLogMessage.Severity := TIdSyslogSeverity.slCritical;

        TLoggerLevel.Custom:
          LSysLogMessage.Severity := TIdSyslogSeverity.slInformational;
      end;

      LSysLogMessage.Msg.Text := LItem.Message;
      if LSysLogMessage.Msg.Text.Trim.IsEmpty then
        LSysLogMessage.Msg.Text := LItem.MessageJSON;

      if LSysLogMessage.Msg.Text.Trim.IsEmpty then
        Exit;

      LRetriesCount := 0;

      while True do
        try
          if not FSysLog.Connected then
            FSysLog.Connect;

          FSysLog.SendLogMessage(LSysLogMessage, False);

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
    finally
      LSysLogMessage.Free;
    end;
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSysLogIndy);

end.
