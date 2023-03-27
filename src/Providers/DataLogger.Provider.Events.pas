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

unit DataLogger.Provider.Events;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  System.SysUtils, System.JSON;

type
  TLoggerItem = DataLogger.Types.TLoggerItem;

  TExecuteEvents = reference to procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string);
  TExecuteEventsJSON = reference to procedure(const AItem: TJSONObject);

  TProviderEvents = class(TDataLoggerProvider<TProviderEvents>)
  private
    FOnAny: TExecuteEvents;
    FOnAnyJSON: TExecuteEventsJSON;
    FOnTrace: TExecuteEvents;
    FOnTraceJSON: TExecuteEventsJSON;
    FOnDebug: TExecuteEvents;
    FOnDebugJSON: TExecuteEventsJSON;
    FOnInfo: TExecuteEvents;
    FOnInfoJSON: TExecuteEventsJSON;
    FOnSuccess: TExecuteEvents;
    FOnSuccessJSON: TExecuteEventsJSON;
    FOnWarn: TExecuteEvents;
    FOnWarnJSON: TExecuteEventsJSON;
    FOnError: TExecuteEvents;
    FOnErrorJSON: TExecuteEventsJSON;
    FOnFatal: TExecuteEvents;
    FOnFatalJSON: TExecuteEventsJSON;
    FOnCustom: TExecuteEvents;
    FOnCustomJSON: TExecuteEventsJSON;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function OnAny(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnAny(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnTrace(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnTrace(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnDebug(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnDebug(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnInfo(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnInfo(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnSuccess(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnSuccess(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnWarn(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnWarn(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnError(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnError(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnFatal(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnFatal(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;
    function OnCustom(const AEvent: TExecuteEvents): TProviderEvents; overload;
    function OnCustom(const AEvent: TExecuteEventsJSON): TProviderEvents; overload;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
  end;

implementation

{ TProviderEvents }

constructor TProviderEvents.Create;
begin
  inherited Create;

  FOnAny := nil;
  FOnAnyJSON := nil;
  FOnTrace := nil;
  FOnTraceJSON := nil;
  FOnDebug := nil;
  FOnDebugJSON := nil;
  FOnInfo := nil;
  FOnInfoJSON := nil;
  FOnWarn := nil;
  FOnWarnJSON := nil;
  FOnError := nil;
  FOnErrorJSON := nil;
  FOnSuccess := nil;
  FOnSuccessJSON := nil;
  FOnFatal := nil;
  FOnFatalJSON := nil;
end;

procedure TProviderEvents.AfterConstruction;
begin
  inherited;
  SetIgnoreLogFormat(True);
end;

function TProviderEvents.OnAny(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnAny := AEvent;
end;

function TProviderEvents.OnAny(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnAnyJSON := AEvent;
end;

function TProviderEvents.OnTrace(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnTrace := AEvent;
end;

function TProviderEvents.OnTrace(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnTraceJSON := AEvent;
end;

function TProviderEvents.OnDebug(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnDebug := AEvent;
end;

function TProviderEvents.OnDebug(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnDebugJSON := AEvent;
end;

function TProviderEvents.OnInfo(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnInfo := AEvent;
end;

function TProviderEvents.OnInfo(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnInfoJSON := AEvent;
end;

function TProviderEvents.OnWarn(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnWarn := AEvent;
end;

function TProviderEvents.OnWarn(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnWarnJSON := AEvent;
end;

function TProviderEvents.OnError(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnError := AEvent;
end;

function TProviderEvents.OnError(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnErrorJSON := AEvent;
end;

function TProviderEvents.OnSuccess(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnSuccess := AEvent;
end;

function TProviderEvents.OnSuccess(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnSuccessJSON := AEvent;
end;

function TProviderEvents.OnFatal(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnFatal := AEvent;
end;

function TProviderEvents.OnFatal(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnFatalJSON := AEvent;
end;

function TProviderEvents.OnCustom(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnFatal := AEvent;
end;

function TProviderEvents.OnCustom(const AEvent: TExecuteEventsJSON): TProviderEvents;
begin
  Result := Self;
  FOnFatalJSON := AEvent;
end;

procedure TProviderEvents.LoadFromJSON(const AJSON: string);
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

function TProviderEvents.ToJSON(const AFormat: Boolean): string;
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

procedure TProviderEvents.Save(const ACache: TArray<TLoggerItem>);
  procedure _Execute(const AItem: TLoggerItem; const AEvent: TExecuteEvents; const AEventJSON: TExecuteEventsJSON);
  var
    LRetriesCount: Integer;
    LJO: TJSONObject;
  begin
    LRetriesCount := 0;

    while True do
      try
        if Assigned(AEvent) then
          AEvent(FLogFormat, AItem, FFormatTimestamp);

        if Assigned(AEventJSON) then
        begin
          LJO := SerializeItem.LogItem(AItem).ToJSONObject;
          try
            AEventJSON(LJO);
          finally
            LJO.Free;
          end;
        end;

        Break;
      except
        on E: Exception do
        begin
          Inc(LRetriesCount);

          Sleep(50);

          if Assigned(FLogException) then
            FLogException(Self, AItem, E, LRetriesCount);

          if Self.Terminated then
            Exit;

          if (LRetriesCount <= 0) then
            Break;

          if (LRetriesCount >= FMaxRetries) then
            Break;
        end;
      end;
  end;

var
  LItem: TLoggerItem;
begin
  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak or LItem.InternalItem.IsUndoLastLine then
      Continue;

    case LItem.Level of
      TLoggerLevel.Trace:
        _Execute(LItem, FOnTrace, FOnTraceJSON);

      TLoggerLevel.Debug:
        _Execute(LItem, FOnDebug, FOnDebugJSON);

      TLoggerLevel.Info:
        _Execute(LItem, FOnInfo, FOnInfoJSON);

      TLoggerLevel.Warn:
        _Execute(LItem, FOnWarn, FOnWarnJSON);

      TLoggerLevel.Error:
        _Execute(LItem, FOnError, FOnErrorJSON);

      TLoggerLevel.Success:
        _Execute(LItem, FOnSuccess, FOnSuccessJSON);

      TLoggerLevel.Fatal:
        _Execute(LItem, FOnFatal, FOnFatalJSON);

      TLoggerLevel.Custom:
        _Execute(LItem, FOnCustom, FOnCustomJSON);
    end;

    _Execute(LItem, FOnAny, FOnAnyJSON);
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderEvents);

end.
