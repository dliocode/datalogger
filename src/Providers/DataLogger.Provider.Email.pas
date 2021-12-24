{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Email;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  IdSMTP, IdMessage,
  System.SysUtils, System.Classes;

type
  TProviderEmail = class(TDataLoggerProvider)
  private
    FIdSMTP: TIdSMTP;
    FIdMessage: TIdMessage;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AIdSMTP: TIdSMTP; const AFromAddress: string; const AToAddress: string; const ASubject: string = 'Logger');
    destructor Destroy; override;
  end;

implementation

{ TProviderEmail }

constructor TProviderEmail.Create(const AIdSMTP: TIdSMTP; const AFromAddress: string; const AToAddress: string; const ASubject: string = 'Logger');
begin
  inherited Create;

  FIdSMTP := AIdSMTP;
  FIdMessage := TIdMessage.Create;

  FIdMessage.From.Text := AFromAddress;
  FIdMessage.Recipients.Add.Text := AToAddress;
  FIdMessage.Subject := ASubject
end;

destructor TProviderEmail.Destroy;
begin
  FIdMessage.Free;
  inherited;
end;

procedure TProviderEmail.Save(const ACache: TArray<TLoggerItem>);
var
  LRetryCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LString: TStringList;
begin
  if Length(ACache) = 0 then
    Exit;

  LString := TStringList.Create;
  try
    for LItem in ACache do
    begin
      if not ValidationBeforeSave(LItem) then
        Continue;

      if LItem.&Type = TLoggerType.All then
        Continue;

      LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);
      LString.Add(LLog);
    end;

    FIdMessage.Body.Text := LString.Text;
  finally
    LString.Free;
  end;

  LRetryCount := 0;

  while True do
    try
      if not FIdSMTP.Connected then
        FIdSMTP.Connect;

      FIdSMTP.Send(FIdMessage);

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

  try
    if FIdSMTP.Connected then
      FIdSMTP.Disconnect(False);
  except
  end;
end;

end.
