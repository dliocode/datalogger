{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.SendEmail;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  SendEmail, // https://github.com/dliocode/sendemail
  System.SysUtils, System.Classes;

type
  TProviderSendEmail = class(TDataLoggerProvider)
  private
    FSendEmail: TSendEmail;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function SendEmail(const ASendEmail: TSendEmail): TProviderSendEmail; overload;
    function SendEmail: TSendEmail; overload;

    constructor Create;
  end;

implementation

{ TProviderSendEmail }

constructor TProviderSendEmail.Create;
begin
  inherited Create;

  FSendEmail := nil;
end;

function TProviderSendEmail.SendEmail(const ASendEmail: TSendEmail): TProviderSendEmail;
begin
  Result := Self;
  FSendEmail := ASendEmail;
end;

function TProviderSendEmail.SendEmail: TSendEmail;
begin
  Result := FSendEmail;
end;

procedure TProviderSendEmail.Save(const ACache: TArray<TLoggerItem>);
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
      if LItem.&Type = TLoggerType.All then
        Continue;

      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);
      LString.Add(LLog);
    end;

    FSendEmail.Message(LString.Text.Replace(sLineBreak, '<br />'));
  finally
    LString.Free;
  end;

  LRetryCount := 0;

  while True do
    try
      FSendEmail.Send;

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

end.
