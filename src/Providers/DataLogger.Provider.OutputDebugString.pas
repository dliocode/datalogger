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
  System.SysUtils;

type
  TProviderOutputDebugString = class(TDataLoggerProvider)
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
  end;

implementation

{ TProviderOutputDebugString }

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
    if LItem.&Type = TLoggerType.All then
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

          if LRetriesCount = -1 then
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
