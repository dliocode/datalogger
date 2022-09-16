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

unit DataLogger.Provider.Events;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  System.SysUtils, System.JSON;

type
  TLoggerItem = DataLogger.Types.TLoggerItem;

  TExecuteEvents = reference to procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string);

  TProviderEvents = class(TDataLoggerProvider<TProviderEvents>)
  private
    FOnAny: TExecuteEvents;
    FOnTrace: TExecuteEvents;
    FOnDebug: TExecuteEvents;
    FOnInfo: TExecuteEvents;
    FOnSuccess: TExecuteEvents;
    FOnWarn: TExecuteEvents;
    FOnError: TExecuteEvents;
    FOnFatal: TExecuteEvents;
    FOnCustom: TExecuteEvents;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function OnAny(const AEvent: TExecuteEvents): TProviderEvents;
    function OnTrace(const AEvent: TExecuteEvents): TProviderEvents;
    function OnDebug(const AEvent: TExecuteEvents): TProviderEvents;
    function OnInfo(const AEvent: TExecuteEvents): TProviderEvents;
    function OnSuccess(const AEvent: TExecuteEvents): TProviderEvents;
    function OnWarn(const AEvent: TExecuteEvents): TProviderEvents;
    function OnError(const AEvent: TExecuteEvents): TProviderEvents;
    function OnFatal(const AEvent: TExecuteEvents): TProviderEvents;
    function OnCustom(const AEvent: TExecuteEvents): TProviderEvents;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderEvents }

constructor TProviderEvents.Create;
begin
  inherited Create;

  FOnAny := nil;
  FOnTrace := nil;
  FOnDebug := nil;
  FOnInfo := nil;
  FOnWarn := nil;
  FOnError := nil;
  FOnSuccess := nil;
  FOnFatal := nil;
end;

function TProviderEvents.OnAny(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnAny := AEvent;
end;

function TProviderEvents.OnTrace(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnTrace := AEvent;
end;

function TProviderEvents.OnDebug(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnDebug := AEvent;
end;

function TProviderEvents.OnInfo(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnInfo := AEvent;
end;

function TProviderEvents.OnWarn(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnWarn := AEvent;
end;

function TProviderEvents.OnError(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnError := AEvent;
end;

function TProviderEvents.OnSuccess(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnSuccess := AEvent;
end;

function TProviderEvents.OnFatal(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnFatal := AEvent;
end;

function TProviderEvents.OnCustom(const AEvent: TExecuteEvents): TProviderEvents;
begin
  Result := Self;
  FOnFatal := AEvent;
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
  procedure _Execute(const AEvent: TExecuteEvents; const AItem: TLoggerItem);
  var
    LRetriesCount: Integer;
  begin
    LRetriesCount := 0;

    while True do
      try
        if Assigned(AEvent) then
          AEvent(FLogFormat, AItem, FFormatTimestamp);

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

          if LRetriesCount <= 0 then
            Break;

          if LRetriesCount >= FMaxRetries then
            Break;
        end;
      end;
  end;

var
  LItem: TLoggerItem;
begin
  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.LevelSlineBreak then
      Continue;

    case LItem.Level of
      TLoggerLevel.Trace:
        _Execute(FOnTrace, LItem);

      TLoggerLevel.Debug:
        _Execute(FOnDebug, LItem);

      TLoggerLevel.Info:
        _Execute(FOnInfo, LItem);

      TLoggerLevel.Warn:
        _Execute(FOnWarn, LItem);

      TLoggerLevel.Error:
        _Execute(FOnError, LItem);

      TLoggerLevel.Success:
        _Execute(FOnSuccess, LItem);

      TLoggerLevel.Fatal:
        _Execute(FOnFatal, LItem);

      TLoggerLevel.Custom:
        _Execute(FOnCustom, LItem);
    end;

    _Execute(FOnAny, LItem);
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderEvents);

end.
