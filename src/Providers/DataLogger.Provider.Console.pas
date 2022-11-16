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
  System.SysUtils, System.JSON, System.Generics.Collections;

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
    FUseColorInConsoleByLogFormat: Boolean;

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
    function UseColorInConsoleByLogFormat(const AValue: Boolean): TProviderConsole;
    function ChangeColor(const ALevel: TLoggerLevel; const AColorBackground: TColor; const AColorForeground: TColor): TProviderConsole;

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
  UseColorInConsoleByLogFormat(False);

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

function TProviderConsole.UseColorInConsoleByLogFormat(const AValue: Boolean): TProviderConsole;
begin
  Result := Self;
  FUseColorInConsoleByLogFormat := AValue;
end;

function TProviderConsole.ChangeColor(const ALevel: TLoggerLevel; const AColorBackground: TColor; const AColorForeground: TColor): TProviderConsole;
begin
  Result := Self;

  case ALevel of
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
    UseColorInConsoleByLogFormat(LJO.GetValue<Boolean>('use_color_in_console_by_logformat', FUseColorInConsoleByLogFormat));

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
    LJO.AddPair('use_color_in_console_by_logformat', TJSONBool.Create(FUseColorInConsoleByLogFormat));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderConsole.Save(const ACache: TArray<TLoggerItem>);
const
  C_TAG = '_color';

var
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
  LTagsKeys: TArray<string>;
  LTagsValues: TArray<string>;
  LTagsLevel: TArray<TLoggerLevel>;
  LLevel: TLoggerLevel;  
  LTag: string;
  LTags: TDictionary<string, string>;
  LLogMessage: string;
  I: Integer;
  J: Integer;
  LLogFormatBase: TArray<string>;
begin
  if not IsConsole then
    Exit;

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
    begin
      Writeln;
      Continue;
    end;

    LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    LRetriesCount := 0;

    while True do
      try
        if FUseColorInConsole and not FUseColorInConsoleByLogFormat then
        begin
          WriteColor(LItem.Level, LLog);
          Break;
        end;

        if FUseColorInConsoleByLogFormat then
        begin
          LTagsKeys := [];        
          LTagsValues := [];
          LTagsLevel := [];

          for LLevel := Low(TLoggerLevel) to High(TLoggerLevel) do
          begin
            LTag := C_TAG;
            if not(LLevel = TLoggerLevel.All) then
              LTag := LTag + '_' + LLevel.ToString.ToLower;

            LTags := TLoggerSerializeItem.ListTAG(LLog, [LTag], LItem, FFormatTimestamp);
            try
              if LTags.Count = 0 then
                Continue;

              LTagsKeys := LTagsKeys + LTags.Keys.ToArray;
              LTagsValues := LTagsValues + LTags.Values.ToArray;

              for I := Low(LTagsKeys) to High(LTagsKeys) do
                if LLevel = TLoggerLevel.All then
                  LTagsLevel := LTagsLevel + [LItem.Level]
                else
                  LTagsLevel := LTagsLevel + [LLevel];
            finally
              LTags.Free;
            end;
          end;

          if Length(LTagsKeys) = 0 then
            Write(LLog)
          else
          begin
            LLogMessage := LLog;

            repeat
              for I := 0 to Pred(Length(LTagsKeys)) do
              begin
                if LLogMessage.Trim.IsEmpty then
                  Break;

                if not LLogMessage.Contains(LTagsKeys[I]) then
                  Continue;

                LLogFormatBase := LLogMessage.Split(['${' + LTagsKeys[I] + '}']);
                if Length(LLogFormatBase) > 1 then
                  if LLogFormatBase[0].Contains(C_TAG) then
                    Continue;

                LLogMessage := '';
                if Length(LLogFormatBase) > 1 then
                  for J := 1 to High(LLogFormatBase) do
                  begin
                    LLogMessage := LLogMessage + LLogFormatBase[J];

                    if J <> High(LLogFormatBase) then
                      LLogMessage := LLogMessage + '${' + LTagsKeys[I] + '}';
                  end;

                Write(LLogFormatBase[0]);
                WriteColor(LTagsLevel[I], LTagsValues[I], False);
              end;
            until not LLogMessage.Contains(C_TAG);
          end;

          if LLogMessage.Trim.IsEmpty then
            Writeln
          else
            Writeln(LLogMessage);

          Break;
        end;

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
