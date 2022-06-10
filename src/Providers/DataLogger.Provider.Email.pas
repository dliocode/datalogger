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
    function IdSMTP(const AValue: TIdSMTP): TProviderEmail; overload;
    function IdSMTP: TIdSMTP; overload;
    function FromAddress(const AValue: string): TProviderEmail;
    function ToAddress(const AValue: string): TProviderEmail;
    function Subject(const AValue: string): TProviderEmail;

    constructor Create; overload;
    constructor Create(const AIdSMTP: TIdSMTP; const AFromAddress: string; const AToAddress: string; const ASubject: string = 'Logger'); overload; deprecated 'Use TProviderEmail.Create.IdSMTP(IdSMTP).FromAddress(''email@email.com'').ToAddress(''email@email.com'').Subject(''My Subject'') - This function will be removed in future versions';
  end;

implementation

{ TProviderEmail }

constructor TProviderEmail.Create;
begin
  inherited Create;

  IdSMTP(nil);
  FromAddress('');
  ToAddress('');
  Subject('');
end;

constructor TProviderEmail.Create(const AIdSMTP: TIdSMTP; const AFromAddress: string; const AToAddress: string; const ASubject: string = 'Logger');
begin
  Create;

  IdSMTP(AIdSMTP);
  FromAddress(AFromAddress);
  ToAddress(AToAddress);
  Subject(ASubject);
end;

function TProviderEmail.IdSMTP(const AValue: TIdSMTP): TProviderEmail;
begin
  Result := Self;
  FIdSMTP := AValue;
end;

function TProviderEmail.IdSMTP: TIdSMTP;
begin
  Result := FIdSMTP;
end;

function TProviderEmail.FromAddress(const AValue: string): TProviderEmail;
begin
  Result := Self;
  FFromAddress := AValue;
end;

function TProviderEmail.ToAddress(const AValue: string): TProviderEmail;
begin
  Result := Self;
  FToAddress := AValue;
end;

function TProviderEmail.Subject(const AValue: string): TProviderEmail;
begin
  Result := Self;
  FSubject := AValue;
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

          Sleep(50);

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
