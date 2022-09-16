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

unit DataLogger.Provider.Notification;

interface

uses
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils,
  System.SysUtils, System.Notification, System.JSON, System.Classes;

type
  TProviderNotification = class(TDataLoggerProvider<TProviderNotification>)
  private
    FNotificationCenter: TNotificationCenter;
    FTitle: string;
    FIncludeLogLevelInTitle: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Title(const AValue: string; const AIncludeLogLevelInTitle: Boolean = True): TProviderNotification;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderNotification }

constructor TProviderNotification.Create;
begin
  inherited Create;

  FNotificationCenter := TNotificationCenter.Create(nil);

  Title('DataLogger - Notification');
end;

function TProviderNotification.Title(const AValue: string; const AIncludeLogLevelInTitle: Boolean = True): TProviderNotification;
begin
  Result := Self;

  if AValue.Trim.IsEmpty then
    FTitle := 'DataLogger - Notification'
  else
    FTitle := AValue;

  FIncludeLogLevelInTitle := AIncludeLogLevelInTitle;
end;

destructor TProviderNotification.Destroy;
begin
  FNotificationCenter.Free;

  inherited;
end;

procedure TProviderNotification.LoadFromJSON(const AJSON: string);
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
    Title(LJO.GetValue<string>('title', FTitle), LJO.GetValue<Boolean>('include_log_level_in_title', FIncludeLogLevelInTitle));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderNotification.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('title', FTitle);
    LJO.AddPair('include_log_level_in_title', TJSONBool.Create(FIncludeLogLevelInTitle));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderNotification.Save(const ACache: TArray<TLoggerItem>);
var
  LName: string;
  LNotification: TNotification;
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.LevelSlineBreak then
      Continue;

    LName := FTitle;
    if FIncludeLogLevelInTitle then
      LName := LName + ' - ' + LItem.LevelString;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    LRetriesCount := 0;

    while True do
      try
        LNotification := FNotificationCenter.CreateNotification;
        try
          LNotification.Name := LName;
          LNotification.Title := LName;
          LNotification.AlertBody := LLog;

          TThread.Synchronize(nil,
            procedure
            begin
              FNotificationCenter.PresentNotification(LNotification);
            end);
        finally
          LNotification.Free;
        end;

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
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderNotification);

end.
