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

unit DataLogger.Provider.Console;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.JSON;

type
{$SCOPEDENUMS ON}
{$IF DEFINED(MSWINDOWS)}
  TColor = (Black, DarkBlue, DarkGreen, DarkCyan, DarkRed, DarkMagenta, DarkYellow, Gray, DarkGray, Blue, Green, Cyan, Red, Magenta, Yellow, White);
{$ELSE}
  TColor = (Black, DarkRed, DarkGreen, DarkYellow, DarkBlue, DarkMagenta, DarkCyan, Gray, DarkGray, Red, Green, Yellow, Blue, Magenta, Cyan, White);
{$ENDIF}
{$SCOPEDENUMS OFF}

  TProviderConsole = class(TDataLoggerProvider<TProviderConsole>)
  strict private
  type
    TColorConsole = record
      Background: TColor;
      Foreground: TColor;
    end;
  private
    FUseColorInConsole: Boolean;
    FUseColorOnlyInLevels: Boolean;
    FColorTrace: TColorConsole;
    FColorDebug: TColorConsole;
    FColorInfo: TColorConsole;
    FColorSuccess: TColorConsole;
    FColorWarn: TColorConsole;
    FColorError: TColorConsole;
    FColorFatal: TColorConsole;
    FColorCustom: TColorConsole;
    procedure WriteColor(const ALevel: TLoggerLevel; const ALog: string; const ASlinebreak: Boolean = True);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function UseColorInConsole(const AValue: Boolean): TProviderConsole;
    function UseColorOnlyInLevels(const AValue: Boolean): TProviderConsole;
    function ChangeColor(const ALogLevel: TLoggerLevel; const AColorBackground: TColor; const AColorForeground: TColor): TProviderConsole;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderConsole }

constructor TProviderConsole.Create;
begin
  inherited Create;

  UseColorInConsole(True);
  UseColorOnlyInLevels(False);

  ChangeColor(TLoggerLevel.Trace, TColor.Black, TColor.Magenta);
  ChangeColor(TLoggerLevel.Debug, TColor.Black, TColor.Cyan);
  ChangeColor(TLoggerLevel.Info, TColor.Black, TColor.White);
  ChangeColor(TLoggerLevel.Success, TColor.Black, TColor.Green);
  ChangeColor(TLoggerLevel.Warn, TColor.Black, TColor.Yellow);
  ChangeColor(TLoggerLevel.Error, TColor.Black, TColor.Red);
  ChangeColor(TLoggerLevel.Fatal, TColor.Black, TColor.DarkRed);
  ChangeColor(TLoggerLevel.Custom, TColor.Black, TColor.White);
end;

function TProviderConsole.UseColorInConsole(const AValue: Boolean): TProviderConsole;
begin
  Result := Self;
  FUseColorInConsole := AValue;
end;

function TProviderConsole.UseColorOnlyInLevels(const AValue: Boolean): TProviderConsole;
begin
  Result := Self;
  FUseColorOnlyInLevels := AValue;
end;

function TProviderConsole.ChangeColor(const ALogLevel: TLoggerLevel; const AColorBackground: TColor; const AColorForeground: TColor): TProviderConsole;
begin
  Result := Self;

  case ALogLevel of
    TLoggerLevel.Trace:
      begin
        FColorTrace.Background := AColorBackground;
        FColorTrace.Foreground := AColorForeground;
      end;

    TLoggerLevel.Debug:
      begin
        FColorDebug.Background := AColorBackground;
        FColorDebug.Foreground := AColorForeground;
      end;

    TLoggerLevel.Info:
      begin
        FColorInfo.Background := AColorBackground;
        FColorInfo.Foreground := AColorForeground;
      end;

    TLoggerLevel.Success:
      begin
        FColorSuccess.Background := AColorBackground;
        FColorSuccess.Foreground := AColorForeground;
      end;

    TLoggerLevel.Warn:
      begin
        FColorWarn.Background := AColorBackground;
        FColorWarn.Foreground := AColorForeground;
      end;

    TLoggerLevel.Error:
      begin
        FColorError.Background := AColorBackground;
        FColorError.Foreground := AColorForeground;
      end;

    TLoggerLevel.Fatal:
      begin
        FColorFatal.Background := AColorBackground;
        FColorFatal.Foreground := AColorForeground;
      end;

    TLoggerLevel.Custom:
      begin
        FColorCustom.Background := AColorBackground;
        FColorCustom.Foreground := AColorForeground;
      end;
  end;
end;

procedure TProviderConsole.LoadFromJSON(const AJSON: string);
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
    UseColorInConsole(LJO.GetValue<Boolean>('use_color_in_console', FUseColorInConsole));
    UseColorOnlyInLevels(LJO.GetValue<Boolean>('use_color_only_in_levels', FUseColorOnlyInLevels));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderConsole.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('use_color_in_console', TJSONBool.Create(FUseColorInConsole));
    LJO.AddPair('use_color_only_in_levels', TJSONBool.Create(FUseColorOnlyInLevels));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderConsole.Save(const ACache: TArray<TLoggerItem>);
var
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LLogFormat: TArray<string>;
  I: Integer;
begin
  if not IsConsole then
    Exit;

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.LevelSlineBreak then
    begin
      Writeln;
      Continue;
    end;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetriesCount := 0;

    while True do
      try
        if FUseColorInConsole then
        begin
          if FUseColorOnlyInLevels and FLogFormat.Contains(TLoggerFormat.LOG_LEVEL) then
          begin
            LLogFormat := FLogFormat.Split([TLoggerFormat.LOG_LEVEL]);

            for I := Low(LLogFormat) to High(LLogFormat) do
            begin
              LLog := TLoggerLogFormat.AsString(LLogFormat[I], LItem, FFormatTimestamp);

              Write(LLog);

              if I = 0 then
                WriteColor(LItem.Level, LItem.LevelString, False);
            end;

            Writeln;
          end
          else
            WriteColor(LItem.Level, LLog);
        end
        else
          Writeln(LLog);

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
end;

procedure TProviderConsole.WriteColor(const ALevel: TLoggerLevel; const ALog: string; const ASlinebreak: Boolean = True);
  function _Color(const AColor: TColorConsole): SmallInt;
  begin
    Result := SmallInt(Integer(AColor.Background) shl 4) or Integer(AColor.Foreground);
  end;

  function _ColorLevel: TColorConsole;
  begin
    case ALevel of
      TLoggerLevel.Trace:
        Result := FColorTrace;

      TLoggerLevel.Debug:
        Result := FColorDebug;

      TLoggerLevel.Info:
        Result := FColorInfo;

      TLoggerLevel.Success:
        Result := FColorSuccess;

      TLoggerLevel.Warn:
        Result := FColorWarn;

      TLoggerLevel.Error:
        Result := FColorError;

      TLoggerLevel.Fatal:
        Result := FColorFatal;

      TLoggerLevel.Custom:
        Result := FColorCustom;
    else
      Result := FColorInfo;
    end;
  end;

{$IF DEFINED(MSWINDOWS)}

var
  LHandleOutput: THandle;
  LBufferInfo: TConsoleScreenBufferInfo;
begin
  LHandleOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(LHandleOutput, LBufferInfo);

  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), _Color(_ColorLevel));

  if ASlinebreak then
    Writeln(ALog)
  else
    Write(ALog);

  SetConsoleTextAttribute(LHandleOutput, LBufferInfo.wAttributes);
end;

{$ELSEIF DEFINED(LINUX)}

var
  LColor: Integer;
begin
  LColor := Integer(_ColorLevel.Foreground) + 30;

  if LColor > 37 then
    LColor := LColor - 8;

  Write(#27'[1;' + LColor.ToString + 'm');

  if ASlinebreak then
    Writeln(ALog)
  else
    Write(ALog);

  Write(#27'[0m');
end;

{$ELSE}

begin
  Writeln(ALog);
end;

{$ENDIF}

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderConsole);

end.
