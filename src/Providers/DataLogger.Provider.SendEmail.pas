{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

// https://github.com/dliocode/sendemail

unit DataLogger.Provider.SendEmail;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  SendEmail,
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSendEmail = class(TDataLoggerProvider)
  private
    FSendEmail: TSendEmail;
    FModeCustom: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function SendEmail(const ASendEmail: TSendEmail): TProviderSendEmail; overload;
    function SendEmail: TSendEmail; overload;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProviderSendEmail }

constructor TProviderSendEmail.Create;
begin
  inherited Create;

  SendEmail(nil);
  FModeCustom := False;
end;

destructor TProviderSendEmail.Destroy;
begin
  if not FModeCustom then
    if Assigned(FSendEmail) then
    begin
      FSendEmail.Free;
      FSendEmail := nil;
    end;

  inherited;
end;

function TProviderSendEmail.SendEmail(const ASendEmail: TSendEmail): TProviderSendEmail;
begin
  Result := Self;

  if not FModeCustom then
  begin
    if Assigned(FSendEmail) then
    begin
      FSendEmail.Free;
      FSendEmail := nil;
    end;

    FModeCustom := True;
  end;

  FSendEmail := ASendEmail;
end;

function TProviderSendEmail.SendEmail: TSendEmail;
begin
  if not Assigned(FSendEmail) then
    FSendEmail := TSendEmail.Create;

  Result := FSendEmail;
end;

procedure TProviderSendEmail.LoadFromJSON(const AJSON: string);
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
    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderSendEmail.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderSendEmail.Save(const ACache: TArray<TLoggerItem>);
var
  LRetriesCount: Integer;
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
      if LItem.InternalItem.TypeSlineBreak then
        Continue;

      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);
      LString.Add(LLog);
    end;

    FSendEmail.Message(LString.Text.Replace(sLineBreak, '<br />'));
  finally
    LString.Free;
  end;

  LRetriesCount := 0;

  while True do
    try
      FSendEmail.Send;

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
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderSendEmail);

end.
