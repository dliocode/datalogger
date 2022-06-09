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
    FFromAddress: string;
    FToAddress: string;
    FSubject: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    property IdSMTP: TIdSMTP read FIdSMTP write FIdSMTP;
    property FromAddress: string read FFromAddress write FFromAddress;
    property ToAddress: string read FToAddress write FToAddress;
    property Subject: string read FSubject write FSubject;

    constructor Create(const AIdSMTP: TIdSMTP; const AFromAddress: string; const AToAddress: string; const ASubject: string = 'Logger');
  end;

implementation

{ TProviderEmail }

constructor TProviderEmail.Create(const AIdSMTP: TIdSMTP; const AFromAddress: string; const AToAddress: string; const ASubject: string = 'Logger');
begin
  inherited Create;

  FIdSMTP := AIdSMTP;
  FFromAddress := AFromAddress;
  FToAddress := AToAddress;
  FSubject := ASubject;
end;

procedure TProviderEmail.Save(const ACache: TArray<TLoggerItem>);
var
  LIdMessage: TIdMessage;
  LToAddress: TArray<string>;
  LEmail: string;
  LRetryCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LString: TStringList;
begin
  if not Assigned(FIdSMTP) then
    raise EDataLoggerException.Create('IdSMTP not defined!');

  if Length(ACache) = 0 then
    Exit;

  LIdMessage := TIdMessage.Create;
  try
    LIdMessage.From.Text := FFromAddress;

    LToAddress := FToAddress.Trim.Split([';']);
    for LEmail in LToAddress do
      LIdMessage.Recipients.Add.Text := LEmail.Trim;

    LIdMessage.Subject := FSubject;

    LString := TStringList.Create;
    try
      for LItem in ACache do
      begin
        if LItem.&Type = TLoggerType.All then
          Continue;

        LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);
        LString.Add(LLog);
      end;

      LIdMessage.Body.Text := LString.Text;
    finally
      LString.Free;
    end;

    LRetryCount := 0;

    while True do
      try
        if not FIdSMTP.Connected then
          FIdSMTP.Connect;

        FIdSMTP.Send(LIdMessage);

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

    try
      if FIdSMTP.Connected then
        FIdSMTP.Disconnect(False);
    except
    end;
  finally
    LIdMessage.Free;
  end;
end;

end.
