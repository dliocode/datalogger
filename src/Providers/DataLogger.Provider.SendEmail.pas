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
    property SendEmail: TSendEmail read FSendEmail write FSendEmail;

    constructor Create(const ASendEmail: TSendEmail);
  end;

implementation

{ TProviderSendEmail }

constructor TProviderSendEmail.Create(const ASendEmail: TSendEmail);
begin
  inherited Create;

  FSendEmail := ASendEmail;
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
