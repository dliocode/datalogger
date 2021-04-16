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
{$IF DEFINED(MSWINDOWS)}
  Winapi.Windows,
{$ELSE}
  FMX.Types,
{$ENDIF}
  System.SysUtils;

type
  TMemoOutputDebugString = class(TDataLoggerProvider)
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

{ TMemoOutputDebugString }

constructor TMemoOutputDebugString.Create;
begin
  inherited Create;
end;

destructor TMemoOutputDebugString.Destroy;
begin
  inherited;
end;

procedure TMemoOutputDebugString.Save(const ACache: TArray<TLoggerItem>);
var
  LRetryCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      Continue;

    LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatSettings);

    LRetryCount := 0;

    repeat
      try
{$IF DEFINED(MSWINDOWS)}
        OutputDebugString(PChar(LLog));
{$ELSE}
        FMX.Types.Log.d(LLog);
{$ENDIF}
        Break;
      except
        on E: Exception do
        begin
          Inc(LRetryCount);

          if Assigned(LogException) then
            LogException(Self, LItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= GetMaxRetry then
            Break;
        end;
      end;
    until False;
  end;
end;

end.
