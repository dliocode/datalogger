{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2023 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

// https://github.com/dliocode/sendemail

unit DataLogger.Provider.SendEmail;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  SendEmail,
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderSendEmail = class(TDataLoggerProvider<TProviderSendEmail>)
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
  if (Length(ACache) = 0) then
    Exit;

  LString := TStringList.Create;
  try
    for LItem in ACache do
    begin
      if LItem.InternalItem.IsSlinebreak then
        Continue;

      LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);
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

        if (LRetriesCount <= 0) then
          Break;

        if (LRetriesCount >= FMaxRetries) then
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
