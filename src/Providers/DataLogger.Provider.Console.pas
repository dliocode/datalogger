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

unit DataLogger.Provider.Console;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.StrUtils, System.JSON, System.Generics.Collections, System.TypInfo;

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
    FUseColor: Boolean;
    FUseColorCustomTemplate: Boolean;

    FColorTrace: TColorConsole;
    FColorDebug: TColorConsole;
    FColorInfo: TColorConsole;
    FColorSuccess: TColorConsole;
    FColorWarn: TColorConsole;
    FColorError: TColorConsole;
    FColorFatal: TColorConsole;
    FColorCustom: TColorConsole;

    procedure WriteColor(const ALevel: TLoggerLevel; const ALog: string; const ASlinebreak: Boolean = True); overload;
    procedure WriteColor(const AColorBackground: TColor; const AColorForeground: TColor; const ALog: string; const ASlinebreak: Boolean = True); overload;
    procedure UndoLast;
    function ColorLevel(const ALevel: TLoggerLevel): TColorConsole;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function UseColor(const AValue: Boolean): TProviderConsole;
    function UseColorCustomTemplate(const AValue: Boolean): TProviderConsole;
    function ChangeColor(const ALevel: TLoggerLevel; const AColorBackground: TColor; const AColorForeground: TColor): TProviderConsole;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

type
  TColorHelper = record helper for TColor
  public
    function ToString: string;
  end;

{ TProviderConsole }

constructor TProviderConsole.Create;
begin
  inherited Create;

  UseColor(True);
  UseColorCustomTemplate(False);

  ChangeColor(TLoggerLevel.Trace, TColor.Black, TColor.Magenta);
  ChangeColor(TLoggerLevel.Debug, TColor.Black, TColor.Cyan);
  ChangeColor(TLoggerLevel.Info, TColor.Black, TColor.White);
  ChangeColor(TLoggerLevel.Success, TColor.Black, TColor.Green);
  ChangeColor(TLoggerLevel.Warn, TColor.Black, TColor.Yellow);
  ChangeColor(TLoggerLevel.Error, TColor.Black, TColor.Red);
  ChangeColor(TLoggerLevel.Fatal, TColor.Black, TColor.DarkRed);
  ChangeColor(TLoggerLevel.Custom, TColor.Black, TColor.White);
end;

function TProviderConsole.UseColor(const AValue: Boolean): TProviderConsole;
begin
  Result := Self;
  FUseColor := AValue;
end;

function TProviderConsole.UseColorCustomTemplate(const AValue: Boolean): TProviderConsole;
begin
  Result := Self;
  FUseColorCustomTemplate := AValue;
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
    UseColor(LJO.GetValue<Boolean>('use_color_in_console', FUseColor));
    UseColorCustomTemplate(LJO.GetValue<Boolean>('use_color_in_console_by_Template', FUseColorCustomTemplate));

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
    LJO.AddPair('use_color_in_console', TJSONBool.Create(FUseColor));
    LJO.AddPair('use_color_in_console_by_Template', TJSONBool.Create(FUseColorCustomTemplate));

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
  LTag: string;
  LTagsColorKeys: TArray<string>;
  LTagsColorValues: TArray<string>;
  LTagsColor: TArray<TColor>;
  LColor: TColor;
  LListTags: TDictionary<string, string>;
  LLogMessage: string;
  I: Integer;
  LTemplateBase: TArray<string>;
  J: Integer;
  LColorLevel: TColorConsole;
begin
  if not IsConsole then
    Exit;

  if (Length(ACache) = 0) then
    Exit;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
    begin
      Writeln;
      Continue;
    end;

    if LItem.InternalItem.IsUndoLast then
    begin
      UndoLast;
      Continue;
    end;

    LLog := SerializeItem.LogItem(LItem).ToString;

    LRetriesCount := 0;

    while True do
      try
        if FUseColor and not FUseColorCustomTemplate then
        begin
          WriteColor(LItem.Level, LLog);
          Break;
        end;

        if FUseColorCustomTemplate then
        begin
          // Color
          LTagsColorKeys := [];
          LTagsColorValues := [];
          LTagsColor := [];

          LTag := C_TAG;
          LListTags := SerializeItem.LogItem(LItem).ToListTAG(LLog, [LTag]);
          LColor := ColorLevel(LItem.Level).Foreground;
          try
            if (LListTags.Count > 0) then
            begin
              LTagsColorKeys := LTagsColorKeys + LListTags.Keys.ToArray;
              LTagsColorValues := LTagsColorValues + LListTags.Values.ToArray;

              for I := Low(LListTags.Keys.ToArray) to High(LListTags.Keys.ToArray) do
                LTagsColor := LTagsColor + [LColor];
            end;
          finally
            LListTags.Free;
          end;

          for LColor := Low(TColor) to High(TColor) do
          begin
            LTag := C_TAG + '_' + LColor.ToString.ToLower;

            LListTags := SerializeItem.LogItem(LItem).ToListTAG(LLog, [LTag]);
            try
              if (LListTags.Count = 0) then
                Continue;

              LTagsColorKeys := LTagsColorKeys + LListTags.Keys.ToArray;
              LTagsColorValues := LTagsColorValues + LListTags.Values.ToArray;

              for I := Low(LListTags.Keys.ToArray) to High(LListTags.Keys.ToArray) do
                LTagsColor := LTagsColor + [LColor];
            finally
              LListTags.Free;
            end;
          end;

          if (Length(LTagsColorKeys) = 0) then
          begin
            if FUseColor then
              WriteColor(LItem.Level, LLog, False)
            else
              Write(LLog);
          end
          else
          begin
            LLogMessage := LLog;

            repeat
              // Color
              for I := 0 to Pred(Length(LTagsColorKeys)) do
              begin
                if LLogMessage.Trim.IsEmpty then
                  Break;

                if not LLogMessage.Contains(LTagsColorKeys[I]) then
                  Continue;

                LTemplateBase := LLogMessage.Split(['${' + LTagsColorKeys[I] + '}']);
                if (Length(LTemplateBase) > 1) then
                  if
                    (LTemplateBase[0].Contains(C_TAG + '_black}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_darkblue}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_darkgreen}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_darkcyan}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_darkred}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_darkmagenta}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_darkyellow}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_gray}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_darkgray}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_blue}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_green}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_cyan}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_red}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_magenta}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_yellow}')) or
                    (LTemplateBase[0].Contains(C_TAG + '_white}')) or
                    (LTemplateBase[0].Contains(C_TAG + '}'))
                  then
                    Continue;

                LLogMessage := '';
                if (Length(LTemplateBase) > 1) then
                  for J := 1 to High(LTemplateBase) do
                  begin
                    LLogMessage := LLogMessage + LTemplateBase[J];

                    if (J <> High(LTemplateBase)) then
                      LLogMessage := LLogMessage + '${' + LTagsColorKeys[I] + '}';
                  end;

                if FUseColor then
                  WriteColor(LItem.Level, LTemplateBase[0], False)
                else
                  Write(LTemplateBase[0]);

                LColorLevel := ColorLevel(LItem.Level);
                WriteColor(LColorLevel.Background, LTagsColor[I], LTagsColorValues[I], False);
              end;
            until (not LLogMessage.Contains(C_TAG + '_black}')) and
                  (not LLogMessage.Contains(C_TAG + '_darkblue}')) and
                  (not LLogMessage.Contains(C_TAG + '_darkgreen}')) and
                  (not LLogMessage.Contains(C_TAG + '_darkcyan}')) and
                  (not LLogMessage.Contains(C_TAG + '_darkred}')) and
                  (not LLogMessage.Contains(C_TAG + '_darkmagenta}')) and
                  (not LLogMessage.Contains(C_TAG + '_darkyellow}')) and
                  (not LLogMessage.Contains(C_TAG + '_gray}')) and
                  (not LLogMessage.Contains(C_TAG + '_darkgray}')) and
                  (not LLogMessage.Contains(C_TAG + '_blue}')) and
                  (not LLogMessage.Contains(C_TAG + '_green}')) and
                  (not LLogMessage.Contains(C_TAG + '_cyan}')) and
                  (not LLogMessage.Contains(C_TAG + '_red}')) and
                  (not LLogMessage.Contains(C_TAG + '_magenta}')) and
                  (not LLogMessage.Contains(C_TAG + '_yellow}')) and
                  (not LLogMessage.Contains(C_TAG + '_white}')) and
                  (not LLogMessage.Contains(C_TAG + '}'))
          end;

          if LLogMessage.Trim.IsEmpty then
            Writeln('')
          else
            if FUseColor then
              WriteColor(LItem.Level, LLogMessage)
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

          if (LRetriesCount <= 0) then
            Break;

          if (LRetriesCount >= FMaxRetries) then
            Break;
        end;
      end;
  end;
end;

procedure TProviderConsole.WriteColor(const ALevel: TLoggerLevel; const ALog: string; const ASlinebreak: Boolean = True);
var
  LColorLevel: TColorConsole;
begin
  LColorLevel := ColorLevel(ALevel);
  WriteColor(LColorLevel.Background, LColorLevel.Foreground, ALog, ASlinebreak);
end;

procedure TProviderConsole.WriteColor(const AColorBackground: TColor; const AColorForeground: TColor; const ALog: string; const ASlinebreak: Boolean = True);
{$IF DEFINED(MSWINDOWS)}

var
  LColorLevel: SmallInt;
  LHandleOutput: THandle;
  LBufferInfo: TConsoleScreenBufferInfo;
begin
  LColorLevel := SmallInt(Integer(AColorBackground) shl 4) or Integer(AColorForeground);

  LHandleOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(LHandleOutput, LBufferInfo);
  SetConsoleTextAttribute(LHandleOutput, LColorLevel);

  if ASlinebreak then
    Writeln(ALog)
  else
    Write(ALog);

  SetConsoleTextAttribute(LHandleOutput, LBufferInfo.wAttributes);
end;

{$ELSEIF DEFINED(LINUX)}

var
  LColorBackground: Integer;
  LColorForeground: Integer;
begin
  LColorForeground := Integer(AColorForeground) + 30;
  if (LColorForeground > 37) then
    LColorForeground := Integer(AColorForeground) + 82;

  LColorBackground := Integer(AColorBackground) + 40;
  if (LColorBackground > 47) then
    LColorBackground := Integer(AColorBackground) + 92;

  Write(#27'[0;' + LColorForeground.ToString + ';' + LColorBackground.ToString + 'm');

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

function TProviderConsole.ColorLevel(const ALevel: TLoggerLevel): TColorConsole;
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

procedure TProviderConsole.UndoLast;
{$IF DEFINED(MSWINDOWS)}
var
  LHandleOutput: THandle;
  LBufferInfo: TConsoleScreenBufferInfo;
  LCoord: TCoord;
  LCount, LSize: DWord;
begin
  LHandleOutput := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(LHandleOutput, LBufferInfo);

  LCoord.X := 0;
  LCoord.Y := LBufferInfo.dwCursorPosition.Y - 1;

  // Clear line
  LSize := LBufferInfo.dwSize.X;
  FillConsoleOutputAttribute(LHandleOutput, LBufferInfo.wAttributes and $FF, LSize, LCoord, LCount);
  FillConsoleOutputCharacter(LHandleOutput, ' ', LSize, LCoord, LCount);

  // Set Position
  SetConsoleCursorPosition(LHandleOutput, LCoord);
end;

{$ELSEIF DEFINED(LINUX)}

begin
  Write(#27'[1F'); // Move Line Prior
  Write(#27'[K'); // Move End Cursor
  Write(#27'[2K'); // Clear Line
end;

{$ELSE}

begin

end;

{$ENDIF}

procedure ForceReferenceToClass(C: TClass);
begin
end;

{ TColorHelper }

function TColorHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TColor), Integer(Self)).ToLower;
end;

initialization

ForceReferenceToClass(TProviderConsole);

end.
