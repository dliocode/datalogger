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

unit DataLogger.Provider.OutputDebugString;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  FMX.Types,
{$IF DEFINED(ANDROID)}
  Androidapi.Log,
{$ELSEIF DEFINED(IOS)}
  FMX.Platform.Logger.iOS,
{$ELSEIF DEFINED(MACOS)}
  FMX.Platform.Logger.Mac,
{$ENDIF}
  System.SysUtils, System.JSON;

type
  TProviderOutputDebugString = class(TDataLoggerProvider<TProviderOutputDebugString>)
  private
    FTag: string;
    procedure WriteLog(const ALevel: TLoggerLevel; const ALog: string);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Tag(const ATag: string): TProviderOutputDebugString;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{$IF DEFINED(ANDROID)}
function __android_log_write(Priority: android_LogPriority; const Tag, Text: MarshaledAString): Integer; cdecl; external AndroidLogLib name '__android_log_write';
{$ENDIF}

{ TProviderOutputDebugString }

constructor TProviderOutputDebugString.Create;
begin
  inherited Create;
  Tag('DataLogger');
end;

destructor TProviderOutputDebugString.Destroy;
begin
  inherited;
end;

function TProviderOutputDebugString.Tag(const ATag: string): TProviderOutputDebugString;
begin
  Result := Self;
  FTag := ATag;
end;

procedure TProviderOutputDebugString.LoadFromJSON(const AJSON: string);
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
    Tag(LJO.GetValue<string>('tag', FTag));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderOutputDebugString.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('tag', TJSONString.Create(FTag));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderOutputDebugString.Save(const ACache: TArray<TLoggerItem>);
var
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
{$IF DEFINED(LINUX)}
  Exit;
{$ENDIF}
  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      Continue;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LRetriesCount := 0;

    while True do
      try
        WriteLog(LItem.Level, LLog);
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
end;

procedure TProviderOutputDebugString.WriteLog(const ALevel: TLoggerLevel; const ALog: string);
{$IF DEFINED(ANDROID)}
var
  LTag, LMessage: MarshaledAString;
  M: TMarshaller;
{$ENDIF}
begin
{$IF DEFINED(ANDROID)}
  if FTag.Trim.IsEmpty then
    FTag := 'DataLogger';

  LTag := M.AsUtf8(FTag).ToPointer;
  LMessage := M.AsUtf8(ALog).ToPointer;

  case ALevel of
    TLoggerLevel.Trace:
      __android_log_write(ANDROID_LOG_VERBOSE, LTag, LMessage);

    TLoggerLevel.Debug:
      __android_log_write(ANDROID_LOG_DEBUG, LTag, LMessage);

    TLoggerLevel.Info, TLoggerLevel.Success, TLoggerLevel.Custom:
      __android_log_write(ANDROID_LOG_INFO, LTag, LMessage);

    TLoggerLevel.Warn:
      __android_log_write(ANDROID_LOG_WARN, LTag, LMessage);

    TLoggerLevel.Error:
      __android_log_write(ANDROID_LOG_ERROR, LTag, LMessage);

    TLoggerLevel.Fatal:
      __android_log_write(ANDROID_LOG_FATAL, LTag, LMessage);
  end;
{$ELSE}
  if FTag.Trim.IsEmpty then
    Log.d(ALog)
  else
    Log.d(Format('%s: %s', [FTag, ALog]));
{$ENDIF}
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderOutputDebugString);

end.
