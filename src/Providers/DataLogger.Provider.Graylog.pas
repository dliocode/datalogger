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

// https://www.graylog.org/
// https://go2docs.graylog.org/5-0/home.htm

unit DataLogger.Provider.Graylog;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_GRAYLOG_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_GRAYLOG_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderGraylog = class(TDataLoggerProvider<TProviderGraylog>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_GRAYLOG_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_GRAYLOG_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FPortInput: Integer;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function URL(const AValue: string): TProviderGraylog;
    function PortInput(const AValue: Integer): TProviderGraylog;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderGraylog }

constructor TProviderGraylog.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP
    .URL('http://localhost')
    .ContentType('application/json');

  PortInput(12201);
end;

procedure TProviderGraylog.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderGraylog.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderGraylog.URL(const AValue: string): TProviderGraylog;
begin
  Result := Self;
  FHTTP.URL(AValue);
end;

function TProviderGraylog.PortInput(const AValue: Integer): TProviderGraylog;
begin
  Result := Self;
  FPortInput := AValue;
end;

procedure TProviderGraylog.LoadFromJSON(const AJSON: string);
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
    URL(LJO.GetValue<string>('url', FHTTP.URL));
    PortInput(LJO.GetValue<Integer>('port_input', FPortInput));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderGraylog.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('url', TJSONString.Create(FHTTP.URL));
    LJO.AddPair('port_input', TJSONNumber.Create(FPortInput));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderGraylog.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LJO := SerializeItem.LogItem(LItem).ToJSONObject;
    try
      LJO
        .AddPair('timestamp', TJSONNumber.Create(LItem.TimeStampUNIX))
        .AddPair('host', TJSONString.Create(LItem.ComputerName));

      if LItem.Message.Trim.IsEmpty then
        LJO.AddPair('message', TJSONString.Create(LItem.MessageJSON))
      else
        LJO.AddPair('message', TJSONString.Create(LItem.Message));

      LLog := LJO.ToString;
    finally
      LJO.Free;
    end;

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := Format('%s:%d/gelf', [FHTTP.URL, FPortInput]);

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveASync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderGraylog);

end.
