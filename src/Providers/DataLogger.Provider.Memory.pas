{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Memory;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  System.SysUtils, System.Classes;

type
  TProviderMemory = class(TDataLoggerProvider)
  private
    FStringList: TStringList;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function AsString: string;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderMemory }

constructor TProviderMemory.Create;
begin
  inherited Create;
  FStringList := TStringList.Create;
end;

destructor TProviderMemory.Destroy;
begin
  FStringList.Free;
  inherited;
end;

procedure TProviderMemory.Save(const ACache: TArray<TLoggerItem>);
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

    LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

    LRetryCount := 0;

    while True do
      try
        FStringList.Add(LLog);

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
  end;
end;

function TProviderMemory.AsString: string;
begin
  CriticalSection.Acquire;
  try
    Result := FStringList.Text;
  finally
    CriticalSection.Release;
  end;
end;

end.
