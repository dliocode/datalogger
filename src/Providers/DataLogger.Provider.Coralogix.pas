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

// https://coralogix.com/
// https://coralogix.com/docs/coralogix-rest-api/

unit DataLogger.Provider.Coralogix;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_CORALOGIX_USE_INDY)}
  DataLogger.Provider.REST.Indy,
{$ELSEIF DEFINED(DATALOGGER_CORALOGIX_USE_NETHTTPCLIENT)}
  DataLogger.Provider.REST.NetHTTPClient,
{$ELSE}
  DataLogger.Provider.REST.HTTPClient,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON;

type
  TProviderCoralogix = class(TDataLoggerProvider<TProviderCoralogix>)
  private
    type
    TProviderHTTP = class(
{$IF DEFINED(DATALOGGER_CORALOGIX_USE_INDY)}
      TProviderRESTIndy
{$ELSEIF DEFINED(DATALOGGER_CORALOGIX_USE_NETHTTPCLIENT)}
      TProviderRESTNetHTTPClient
{$ELSE}
      TProviderRESTHTTPClient
{$ENDIF});

  private
    FHTTP: TProviderHTTP;
    FPrivateKey: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function PrivateKey(const AValue: string): TProviderCoralogix;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderCoralogix }

constructor TProviderCoralogix.Create;
begin
  inherited Create;

  FHTTP := TProviderHTTP.Create;
  FHTTP.ContentType('application/json');
end;

procedure TProviderCoralogix.AfterConstruction;
begin
  inherited;

  SetIgnoreLogFormat(True);
end;

destructor TProviderCoralogix.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TProviderCoralogix.PrivateKey(const AValue: string): TProviderCoralogix;
begin
  Result := Self;
  FPrivateKey := AValue;
end;

procedure TProviderCoralogix.LoadFromJSON(const AJSON: string);
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
    PrivateKey(LJO.GetValue<string>('private_key', FPrivateKey));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderCoralogix.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('private_key', TJSONString.Create(FPrivateKey));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderCoralogix.Save(const ACache: TArray<TLoggerItem>);
var
  LItemREST: TArray<TLogItemREST>;
  LItem: TLoggerItem;
  LJO: TJSONObject;
  LJA: TJSONArray;
  LLog: string;
  LSeverity: Integer;
  LLogItemREST: TLogItemREST;
begin
  if (Length(ACache) = 0) then
    Exit;

  LItemREST := [];

  LJO := TJSONObject.Create;
  try
    LJA := TJSONArray.Create;

    LJO
      .AddPair('privateKey', TJSONString.Create(FPrivateKey))
      .AddPair('applicationName', TJSONString.Create(ACache[0].AppName))
      .AddPair('subsystemName', TJSONString.Create(ACache[0].OSVersion))
      .AddPair('computerName', TJSONString.Create(ACache[0].ComputerName))
      .AddPair('logEntries', LJA)
      ;

    for LItem in ACache do
    begin
      if LItem.InternalItem.IsSlinebreak then
        Continue;

      LLog := SerializeItem.LogItem(LItem).ToJSON;

      LSeverity := 3;
      case LItem.Level of
        TLoggerLevel.Trace, TLoggerLevel.Debug:
          LSeverity := 1;
        TLoggerLevel.Success, TLoggerLevel.Custom:
          LSeverity := 2;
        TLoggerLevel.Info:
          LSeverity := 3;
        TLoggerLevel.Warn:
          LSeverity := 4;
        TLoggerLevel.Error:
          LSeverity := 5;
        TLoggerLevel.Fatal:
          LSeverity := 6;
      end;

      LJA.Add(
        TJSONObject.Create
        .AddPair('timestamp', TJSONNumber.Create(LItem.TimeStampUNIX * 1000))
        .AddPair('severity', TJSONNumber.Create(LSeverity))
        .AddPair('text', TJSONString.Create(LLog))
        );
    end;

    LLog := LJO.ToString;
  finally
    LJO.Free;
  end;

  LLogItemREST.Stream := TStringStream.Create(LLog, TEncoding.UTF8);
  LLogItemREST.LogItem := LItem;
  LLogItemREST.URL := 'https://api.coralogix.com/api/v1/logs';

  LItemREST := Concat(LItemREST, [LLogItemREST]);

  FHTTP
    .SetLogException(FLogException)
    .SetMaxRetries(FMaxRetries);

  FHTTP.InternalSaveAsync(TRESTMethod.tlmPost, LItemREST);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderCoralogix);

end.
