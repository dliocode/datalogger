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

unit DataLogger.Provider.RichEdit;

interface

{$IF NOT DEFINED(DATALOGGER_FMX) AND NOT DEFINED(FRAMEWORK_FMX) AND DEFINED(MSWINDOWS)}

uses
  DataLogger.Provider, DataLogger.Types,
  Winapi.Windows, Winapi.Messages,
  Vcl.ComCtrls, Vcl.Graphics,
  System.SysUtils, System.Classes, System.JSON, System.TypInfo, System.UITypes;

type
  TColor = System.UITypes.TColor;
  TColorRec = System.UITypes.TColorRec;
  TFontStyle = System.UITypes.TFontStyle;

{$SCOPEDENUMS ON}
  TRichEditModeInsert = (tmFirst, tmLast);
{$SCOPEDENUMS OFF}

  TProviderRichEdit = class(TDataLoggerProvider<TProviderRichEdit>)
  strict private
  type
    TAttributesRichEdit = record
      BackgroundColor: TColor;
      ForegroundColor: TColor;
      FontName: string;
      FontSize: Integer;
      FontStyle: TFontStyles;
    end;
  private
    FRichEdit: TCustomRichEdit;
    FUseColorInRichEdit: Boolean;

    FAttributesTrace: TAttributesRichEdit;
    FAttributesDebug: TAttributesRichEdit;
    FAttributesInfo: TAttributesRichEdit;
    FAttributesSuccess: TAttributesRichEdit;
    FAttributesWarn: TAttributesRichEdit;
    FAttributesError: TAttributesRichEdit;
    FAttributesFatal: TAttributesRichEdit;
    FAttributesCustom: TAttributesRichEdit;

    FMaxLogLines: Integer;
    FModeInsert: TRichEditModeInsert;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;

    function AttributesLevel(const ALevel: TLoggerLevel): TAttributesRichEdit;
    procedure SetColor(const ALevel: TLoggerLevel);
    procedure SetFont(const ALevel: TLoggerLevel);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function RichEdit(const AValue: TCustomRichEdit): TProviderRichEdit;
    function UseColorInRichEdit(const AValue: Boolean): TProviderRichEdit;
    function ChangeColor(const ALevel: TLoggerLevel; const ABackgroundColor: TColor; const AForegroundColor: TColor): TProviderRichEdit;
    function ChangeFont(const ALevel: TLoggerLevel; const AFontName: TFontName; const AFontSize: Integer; const AFontStyle: TFontStyles): TProviderRichEdit;
    function ModeInsert(const AValue: TRichEditModeInsert): TProviderRichEdit;
    function MaxLogLines(const AValue: Integer): TProviderRichEdit;
    function CleanOnStart(const AValue: Boolean): TProviderRichEdit;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

{$ENDIF}

implementation

{$IF NOT DEFINED(DATALOGGER_FMX) AND NOT DEFINED(FRAMEWORK_FMX) AND DEFINED(MSWINDOWS)}

  { TProviderRichEdit }

constructor TProviderRichEdit.Create;
begin
  inherited Create;

  RichEdit(nil);
  UseColorInRichEdit(True);

  ChangeColor(TLoggerLevel.Trace, $FFFFFF, $00A100AD);
  ChangeColor(TLoggerLevel.Debug, $FFFFFF, $00AEB600);
  ChangeColor(TLoggerLevel.Info, $FFFFFF, $000000);
  ChangeColor(TLoggerLevel.Success, $FFFFFF, $0009AC00);
  ChangeColor(TLoggerLevel.Warn, $FFFFFF, $0000A4D8);
  ChangeColor(TLoggerLevel.Error, $FFFFFF, $001D2AAA);
  ChangeColor(TLoggerLevel.Fatal, $FFFFFF, $001C0FD1);
  ChangeColor(TLoggerLevel.Custom, $FFFFFF, $000000);

  ChangeFont(TLoggerLevel.Trace, '', 0, []);
  ChangeFont(TLoggerLevel.Debug, '', 0, []);
  ChangeFont(TLoggerLevel.Info, '', 0, []);
  ChangeFont(TLoggerLevel.Success, '', 0, []);
  ChangeFont(TLoggerLevel.Warn, '', 0, []);
  ChangeFont(TLoggerLevel.Error, '', 0, []);
  ChangeFont(TLoggerLevel.Fatal, '', 0, []);
  ChangeFont(TLoggerLevel.Custom, '', 0, []);

  ModeInsert(TRichEditModeInsert.tmLast);
  MaxLogLines(0);
  CleanOnStart(False);
  FCleanOnRun := False;
end;

function TProviderRichEdit.RichEdit(const AValue: TCustomRichEdit): TProviderRichEdit;
begin
  Result := Self;
  FRichEdit := AValue;
end;

function TProviderRichEdit.UseColorInRichEdit(const AValue: Boolean): TProviderRichEdit;
begin
  Result := Self;
  FUseColorInRichEdit := AValue;
end;

function TProviderRichEdit.ChangeColor(const ALevel: TLoggerLevel; const ABackgroundColor: TColor; const AForegroundColor: TColor): TProviderRichEdit;
var
  LLogLevel: TLoggerLevel;
  LAttributes: TAttributesRichEdit;
begin
  Result := Self;

  case ALevel of
    TLoggerLevel.Trace:
      begin
        FAttributesTrace.BackgroundColor := ABackgroundColor;
        FAttributesTrace.ForegroundColor := AForegroundColor;
      end;

    TLoggerLevel.Debug:
      begin
        FAttributesDebug.BackgroundColor := ABackgroundColor;
        FAttributesDebug.ForegroundColor := AForegroundColor;
      end;

    TLoggerLevel.Info:
      begin
        FAttributesInfo.BackgroundColor := ABackgroundColor;
        FAttributesInfo.ForegroundColor := AForegroundColor;
      end;

    TLoggerLevel.Success:
      begin
        FAttributesSuccess.BackgroundColor := ABackgroundColor;
        FAttributesSuccess.ForegroundColor := AForegroundColor;
      end;

    TLoggerLevel.Warn:
      begin
        FAttributesWarn.BackgroundColor := ABackgroundColor;
        FAttributesWarn.ForegroundColor := AForegroundColor;
      end;

    TLoggerLevel.Error:
      begin
        FAttributesError.BackgroundColor := ABackgroundColor;
        FAttributesError.ForegroundColor := AForegroundColor;
      end;

    TLoggerLevel.Fatal:
      begin
        FAttributesFatal.BackgroundColor := ABackgroundColor;
        FAttributesFatal.ForegroundColor := AForegroundColor;
      end;

    TLoggerLevel.Custom:
      begin
        FAttributesCustom.BackgroundColor := ABackgroundColor;
        FAttributesCustom.ForegroundColor := AForegroundColor;
      end;
  else
    for LLogLevel := Low(TLoggerLevel) to High(TLoggerLevel) do
    begin
      if (LLogLevel = TLoggerLevel.All) then
        Continue;

      LAttributes := AttributesLevel(LLogLevel);

      if (ABackgroundColor <> TColorRec.SysNone) then
        LAttributes.BackgroundColor := ABackgroundColor;

      if (AForegroundColor <> TColorRec.SysNone) then
        LAttributes.ForegroundColor := AForegroundColor;

      ChangeColor(LLogLevel, LAttributes.BackgroundColor, LAttributes.ForegroundColor);
    end;
  end;
end;

function TProviderRichEdit.ChangeFont(const ALevel: TLoggerLevel; const AFontName: TFontName; const AFontSize: Integer; const AFontStyle: TFontStyles): TProviderRichEdit;
begin
  Result := Self;

  case ALevel of
    TLoggerLevel.Trace:
      begin
        FAttributesTrace.FontName := AFontName;
        FAttributesTrace.FontSize := AFontSize;
        FAttributesTrace.FontStyle := AFontStyle;
      end;

    TLoggerLevel.Debug:
      begin
        FAttributesDebug.FontName := AFontName;
        FAttributesDebug.FontSize := AFontSize;
        FAttributesDebug.FontStyle := AFontStyle;
      end;

    TLoggerLevel.Info:
      begin
        FAttributesInfo.FontName := AFontName;
        FAttributesInfo.FontSize := AFontSize;
        FAttributesInfo.FontStyle := AFontStyle;
      end;

    TLoggerLevel.Success:
      begin
        FAttributesSuccess.FontName := AFontName;
        FAttributesSuccess.FontSize := AFontSize;
        FAttributesSuccess.FontStyle := AFontStyle;
      end;

    TLoggerLevel.Warn:
      begin
        FAttributesWarn.FontName := AFontName;
        FAttributesWarn.FontSize := AFontSize;
        FAttributesWarn.FontStyle := AFontStyle;
      end;

    TLoggerLevel.Error:
      begin
        FAttributesError.FontName := AFontName;
        FAttributesError.FontSize := AFontSize;
        FAttributesError.FontStyle := AFontStyle;
      end;

    TLoggerLevel.Fatal:
      begin
        FAttributesFatal.FontName := AFontName;
        FAttributesFatal.FontSize := AFontSize;
        FAttributesFatal.FontStyle := AFontStyle;
      end;

    TLoggerLevel.Custom:
      begin
        FAttributesCustom.FontName := AFontName;
        FAttributesCustom.FontSize := AFontSize;
        FAttributesCustom.FontStyle := AFontStyle;
      end;
  end;
end;

function TProviderRichEdit.MaxLogLines(const AValue: Integer): TProviderRichEdit;
begin
  Result := Self;
  FMaxLogLines := AValue;
end;

function TProviderRichEdit.ModeInsert(const AValue: TRichEditModeInsert): TProviderRichEdit;
begin
  Result := Self;
  FModeInsert := AValue;
end;

function TProviderRichEdit.CleanOnStart(const AValue: Boolean): TProviderRichEdit;
begin
  Result := Self;
  FCleanOnStart := AValue;
end;

procedure TProviderRichEdit.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
  LValue: string;
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
    UseColorInRichEdit(LJO.GetValue<Boolean>('use_color_in_richedit', FUseColorInRichEdit));

    ChangeColor(TLoggerLevel.Trace,
      StringToColor(LJO.GetValue<string>('change_color_trace_background', ColorToString(FAttributesTrace.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_trace_foreground', ColorToString(FAttributesTrace.ForegroundColor))));

    ChangeColor(TLoggerLevel.Debug,
      StringToColor(LJO.GetValue<string>('change_color_debug_background', ColorToString(FAttributesDebug.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_debug_foreground', ColorToString(FAttributesDebug.ForegroundColor))));

    ChangeColor(TLoggerLevel.Info,
      StringToColor(LJO.GetValue<string>('change_color_info_background', ColorToString(FAttributesInfo.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_info_foreground', ColorToString(FAttributesInfo.ForegroundColor))));

    ChangeColor(TLoggerLevel.Success,
      StringToColor(LJO.GetValue<string>('change_color_success_background', ColorToString(FAttributesSuccess.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_success_foreground', ColorToString(FAttributesSuccess.ForegroundColor))));

    ChangeColor(TLoggerLevel.Warn,
      StringToColor(LJO.GetValue<string>('change_color_warn_background', ColorToString(FAttributesWarn.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_warn_foreground', ColorToString(FAttributesWarn.ForegroundColor))));

    ChangeColor(TLoggerLevel.Error,
      StringToColor(LJO.GetValue<string>('change_color_error_background', ColorToString(FAttributesError.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_error_foreground', ColorToString(FAttributesError.ForegroundColor))));

    ChangeColor(TLoggerLevel.Fatal,
      StringToColor(LJO.GetValue<string>('change_color_fatal_background', ColorToString(FAttributesFatal.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_fatal_foreground', ColorToString(FAttributesFatal.ForegroundColor))));

    ChangeColor(TLoggerLevel.Custom,
      StringToColor(LJO.GetValue<string>('change_color_custom_background', ColorToString(FAttributesCustom.BackgroundColor))),
      StringToColor(LJO.GetValue<string>('change_color_custom_foreground', ColorToString(FAttributesCustom.ForegroundColor))));

    MaxLogLines(LJO.GetValue<Integer>('max_log_lines', FMaxLogLines));

    LValue := GetEnumName(TypeInfo(TRichEditModeInsert), Integer(FModeInsert));
    FModeInsert := TRichEditModeInsert(GetEnumValue(TypeInfo(TRichEditModeInsert), LJO.GetValue<string>('mode_insert', LValue)));

    CleanOnStart(LJO.GetValue<Boolean>('clean_on_start', FCleanOnStart));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderRichEdit.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('use_color_in_richedit', TJSONBool.Create(FUseColorInRichEdit));

    LJO.AddPair('change_color_trace_background', TJSONString.Create(ColorToString(FAttributesTrace.BackgroundColor)));
    LJO.AddPair('change_color_trace_foreground', TJSONString.Create(ColorToString(FAttributesTrace.ForegroundColor)));

    LJO.AddPair('change_color_debug_background', TJSONString.Create(ColorToString(FAttributesDebug.BackgroundColor)));
    LJO.AddPair('change_color_debug_foreground', TJSONString.Create(ColorToString(FAttributesDebug.ForegroundColor)));

    LJO.AddPair('change_color_info_background', TJSONString.Create(ColorToString(FAttributesInfo.BackgroundColor)));
    LJO.AddPair('change_color_info_foreground', TJSONString.Create(ColorToString(FAttributesInfo.ForegroundColor)));

    LJO.AddPair('change_color_success_background', TJSONString.Create(ColorToString(FAttributesSuccess.BackgroundColor)));
    LJO.AddPair('change_color_success_foreground', TJSONString.Create(ColorToString(FAttributesSuccess.ForegroundColor)));

    LJO.AddPair('change_color_warn_background', TJSONString.Create(ColorToString(FAttributesWarn.BackgroundColor)));
    LJO.AddPair('change_color_warn_foreground', TJSONString.Create(ColorToString(FAttributesWarn.ForegroundColor)));

    LJO.AddPair('change_color_error_background', TJSONString.Create(ColorToString(FAttributesError.BackgroundColor)));
    LJO.AddPair('change_color_error_foreground', TJSONString.Create(ColorToString(FAttributesError.ForegroundColor)));

    LJO.AddPair('change_color_fatal_background', TJSONString.Create(ColorToString(FAttributesFatal.BackgroundColor)));
    LJO.AddPair('change_color_fatal_foreground', TJSONString.Create(ColorToString(FAttributesFatal.ForegroundColor)));

    LJO.AddPair('change_color_custom_background', TJSONString.Create(ColorToString(FAttributesCustom.BackgroundColor)));
    LJO.AddPair('change_color_custom_foreground', TJSONString.Create(ColorToString(FAttributesCustom.ForegroundColor)));

    LJO.AddPair('mode_insert', TJSONString.Create(GetEnumName(TypeInfo(TRichEditModeInsert), Integer(FModeInsert))));
    LJO.AddPair('max_log_lines', TJSONNumber.Create(FMaxLogLines));
    LJO.AddPair('clean_on_start', TJSONBool.Create(FCleanOnStart));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderRichEdit.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LLog: string;
  LFontName: TFontName;
  LFontSize: Integer;
  LFontStyle: TFontStyles;
  LRetriesCount: Integer;
  LLines: Integer;
begin
  if not Assigned(FRichEdit) then
    raise EDataLoggerException.Create('RichEdit not defined!');

  if (Length(ACache) = 0) then
    Exit;

  if not FCleanOnRun then
    if FCleanOnStart then
    begin
      FRichEdit.Lines.Clear;
      FCleanOnRun := True;
    end;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsSlinebreak then
      LLog := ''
    else
      LLog := TLoggerSerializeItem.AsString(FLogFormat, LItem, FFormatTimestamp, FIgnoreLogFormat, FIgnoreLogFormatSeparator, FIgnoreLogFormatIncludeKey, FIgnoreLogFormatIncludeKeySeparator);

    while True do
      try
        try
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FRichEdit.ComponentState) then
                Exit;

              if (FModeInsert = TRichEditModeInsert.tmLast) then
              begin
                SetFont(LItem.Level);
                if FUseColorInRichEdit then
                  SetColor(LItem.Level);
              end;

              FRichEdit.Lines.BeginUpdate;
              case FModeInsert of
                TRichEditModeInsert.tmFirst:
                  FRichEdit.Lines.Insert(0, LLog);

                TRichEditModeInsert.tmLast:
                  FRichEdit.Lines.Add(LLog);
              end;

              if (FModeInsert = TRichEditModeInsert.tmFirst) then
              begin
                FRichEdit.SelStart := 0;
                FRichEdit.SelLength := Length(LLog);

                LFontName := FRichEdit.SelAttributes.Name;
                LFontSize := FRichEdit.SelAttributes.Size;
                LFontStyle := FRichEdit.SelAttributes.Style;

                SetFont(LItem.Level);
                if FUseColorInRichEdit then
                  SetColor(LItem.Level);

                FRichEdit.SelStart := 0;
                FRichEdit.SelLength := 0;

                FRichEdit.SelAttributes.Name := LFontName;
                FRichEdit.SelAttributes.Size := LFontSize;
                FRichEdit.SelAttributes.Style := LFontStyle;
              end;
            end);

          if (FMaxLogLines > 0) then
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FRichEdit.ComponentState) then
                  Exit;

                LLines := FRichEdit.Lines.Count;
                while (LLines > FMaxLogLines) do
                begin
                  case FModeInsert of
                    TRichEditModeInsert.tmFirst:
                      FRichEdit.Lines.Delete(Pred(LLines));

                    TRichEditModeInsert.tmLast:
                      FRichEdit.Lines.Delete(0);
                  end;

                  LLines := FRichEdit.Lines.Count;
                end;
              end);
        finally
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FRichEdit.ComponentState) then
                Exit;

              FRichEdit.Lines.EndUpdate;

              case FModeInsert of
                TRichEditModeInsert.tmFirst:
                  begin
                  end;

                TRichEditModeInsert.tmLast:
                  SendMessage(FRichEdit.Handle, EM_LINESCROLL, 0, FRichEdit.Lines.Count);
              end;
            end);
        end;

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

function TProviderRichEdit.AttributesLevel(const ALevel: TLoggerLevel): TAttributesRichEdit;
begin
  case ALevel of
    TLoggerLevel.Trace:
      Result := FAttributesTrace;

    TLoggerLevel.Debug:
      Result := FAttributesDebug;

    TLoggerLevel.Info:
      Result := FAttributesInfo;

    TLoggerLevel.Success:
      Result := FAttributesSuccess;

    TLoggerLevel.Warn:
      Result := FAttributesWarn;

    TLoggerLevel.Error:
      Result := FAttributesError;

    TLoggerLevel.Fatal:
      Result := FAttributesFatal;

    TLoggerLevel.Custom:
      Result := FAttributesCustom;
  else
    Result := FAttributesInfo;
  end;
end;

procedure TProviderRichEdit.SetColor(const ALevel: TLoggerLevel);
var
{$IF CompilerVersion <= 34} // 34 = Delphi 10.4 Sydney
  LFormat: TCharFormat2;
{$ENDIF}
  LAttributes: TAttributesRichEdit;
begin
  LAttributes := AttributesLevel(ALevel);

  FRichEdit.SelAttributes.Color := LAttributes.ForegroundColor;

{$IF RTLVersion > 34} // 34 = Delphi 10.4 Sydney
  FRichEdit.SelAttributes.BackColor := LAttributes.BackgroundColor;
{$ELSE}
  FillChar(LFormat, SizeOf(LFormat), 0);
  LFormat.cbSize := SizeOf(LFormat);
  LFormat.dwMask := CFM_BACKCOLOR;
  LFormat.crBackColor := TColorRec.ColorToRGB(LAttributes.BackgroundColor);
  FRichEdit.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Longint(@LFormat));
{$ENDIF}
end;

procedure TProviderRichEdit.SetFont(const ALevel: TLoggerLevel);
var
  LAttributes: TAttributesRichEdit;
begin
  LAttributes := AttributesLevel(ALevel);

  if not LAttributes.FontName.Trim.IsEmpty then
    FRichEdit.SelAttributes.Name := LAttributes.FontName;

  if (LAttributes.FontSize > 0) then
    FRichEdit.SelAttributes.Size := LAttributes.FontSize;

  if (LAttributes.FontStyle <> []) then
    FRichEdit.SelAttributes.Style := LAttributes.FontStyle;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderRichEdit);

{$ENDIF}

end.
