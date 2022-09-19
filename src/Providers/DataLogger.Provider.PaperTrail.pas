{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2022 Danilo Lucas

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

// https://papertrailapp.com
// https://papertrailapp.com/account/destinations
// https://www.papertrail.com/help/log-destinations

unit DataLogger.Provider.PaperTrail;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_PAPERTRAIL_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_PAPERTRAIL_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderPaperTrail = class(TDataLoggerProvider<TProviderPaperTrail>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_PAPERTRAIL_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_PAPERTRAIL_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FToken: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Token(const AValue: string): TProviderPaperTrail;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderPaperTrail }

constructor TProviderPaperTrail.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
  FHTTP.URL('https://logs.collector.solarwinds.com/v1/log');
end;

procedure TProviderPaperTrail.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderPaperTrail.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderPaperTrail.Token(const AValue: string): TProviderPaperTrail;
begin
  Result := Self;

  FToken := AValue;
  FHTTP.BasicAuth('', AValue);
end;

procedure TProviderPaperTrail.LoadFromJSON(const AJSON: string);
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
    Token(LJO.GetValue<string>('token', FToken));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderPaperTrail.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('token', FToken);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderPaperTrail.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LLog: string;
  LLogItemREST: TLogItemREST;
begin
  LItemREST := [];

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.LevelSlineBreak then
      Continue;

    LLog := TLoggerLogFormat.AsJsonObjectToString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat);

    LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
    LLogItemREST.LogItem := LItem;
    LLogItemREST.URL := 'https://logs.collector.solarwinds.com/v1/log';

    LItemREST := Concat(LItemREST, [LLogItemREST]);
  end;

  FHTTP.InternalSave(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderPaperTrail);

end.
