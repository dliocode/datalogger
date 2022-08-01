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
  System.SysUtils, System.Classes, System.JSON;

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
    function FromAddress(const AValue: string): TProviderEmail;
    function ToAddress(const AValue: string): TProviderEmail;
    function Subject(const AValue: string): TProviderEmail;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
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

function TProviderEmail.IdSMTP(const AValue: TIdSMTP): TProviderEmail;
begin
  Result := Self;
  FIdSMTP := AValue;
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

procedure TProviderEmail.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
begin
  if AJSON.Trim.IsEmpty then
    Exit;

  try
    LJO := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  except
    on E: Exception do
      Exit;
  end;

  if not Assigned(LJO) then
    Exit;

  try
    FromAddress(LJO.GetValue<string>('from_address', FFromAddress));
    ToAddress(LJO.GetValue<string>('to_address', FToAddress));
    Subject(LJO.GetValue<string>('subject', FSubject));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderEmail.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('from_address', FFromAddress);
    LJO.AddPair('to_address', FToAddress);
    LJO.AddPair('subject', FSubject);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderEmail.Save(const ACache: TArray<TLoggerItem>);
var
  LIdMessage: TIdMessage;
  LToAddress: TArray<string>;
  LEmail: string;
  LRetriesCount: Integer;
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
        if LItem.InternalItem.TypeSlineBreak then
          Continue;

        LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);
        LString.Add(LLog);
      end;

      LIdMessage.Body.Text := LString.Text;
    finally
      LString.Free;
    end;

    LRetriesCount := 0;

    while True do
      try
        if not FIdSMTP.Connected then
          FIdSMTP.Connect;

        FIdSMTP.Send(LIdMessage);

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

          if LRetriesCount <= 0 then
            Break;

          if LRetriesCount >= FMaxRetries then
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

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderEmail);

end.
