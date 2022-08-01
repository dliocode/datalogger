{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Notification;

interface

uses
  DataLogger.Provider, DataLogger.Types, DataLogger.Utils,
  System.SysUtils, System.Notification, System.JSON, System.Classes;

type
  TProviderNotification = class(TDataLoggerProvider)
  private
    FNotificationCenter: TNotificationCenter;
    FTitle: string;
    FIncludeLogTypeInTitle: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Title(const AValue: string; const AIncludeLogTypeInTitle: Boolean = True): TProviderNotification;

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

function TProviderNotification.Title(const AValue: string; const AIncludeLogTypeInTitle: Boolean = True): TProviderNotification;
begin
  Result := Self;

  if AValue.Trim.IsEmpty then
    FTitle := 'DataLogger - Notification'
  else
    FTitle := AValue;

  FIncludeLogTypeInTitle := AIncludeLogTypeInTitle;
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
    Title(LJO.GetValue<string>('title', FTitle), LJO.GetValue<Boolean>('include_log_type_in_title', FIncludeLogTypeInTitle));

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
    LJO.AddPair('include_log_type_in_title', TJSONBool.Create(FIncludeLogTypeInTitle));

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
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LName := FTitle;
    if FIncludeLogTypeInTitle then
      LName := LName + ' - ' + LItem.TypeString;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetriesCount := 0;

    while True do
      try
        LNotification := FNotificationCenter.CreateNotification;
        try
          LNotification.Name := LName;
          LNotification.Title := LName;
          LNotification.AlertBody := LLog;

          TThread.Synchronize(Self,
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
