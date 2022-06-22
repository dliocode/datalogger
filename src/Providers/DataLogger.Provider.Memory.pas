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
    function Clear: TProviderMemory;
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
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
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
        FStringList.Add(LLog);

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

function TProviderMemory.Clear: TProviderMemory;
begin
  Result := Self;

  Lock;
  try
    FStringList.Clear;
  finally
    UnLock;
  end;
end;

function TProviderMemory.AsString: string;
begin
  Lock;
  try
    Result := FStringList.Text;
  finally
    UnLock;
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderMemory);

end.
