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
  LRetryCount: Integer;
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

    LRetryCount := 0;

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
          Inc(LRetryCount);

          Sleep(50);

          if Assigned(FLogException) then
            FLogException(Self, LItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= FMaxRetry then
            Break;
        end;
      end;
  end;
end;

end.
