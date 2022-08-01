{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.OutputDebugString;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_FMX)}
  FMX.Types,
{$ELSE}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.JSON;

type
  TProviderOutputDebugString = class(TDataLoggerProvider)
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;
  end;

implementation

{ TProviderOutputDebugString }

procedure TProviderOutputDebugString.LoadFromJSON(const AJSON: string);
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

function TProviderOutputDebugString.ToJSON(const AFormat: Boolean): string;
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

procedure TProviderOutputDebugString.Save(const ACache: TArray<TLoggerItem>);
var
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
{$IF DEFINED(LINUX)}
  Exit;
{$ENDIF}
  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetriesCount := 0;

    while True do
      try
{$IF DEFINED(DATALOGGER_FMX)}
        FMX.Types.Log.d(LLog);
{$ELSE}
        OutputDebugString(PChar(LLog));
{$ENDIF}
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

ForceReferenceToClass(TProviderOutputDebugString);

end.
