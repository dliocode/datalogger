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

unit DataLogger.Provider.RichEdit;

interface

{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX) OR NOT(DEFINED(LINUX))}

uses
  DataLogger.Provider, DataLogger.Types,
  Vcl.ComCtrls, Vcl.Graphics, Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.JSON, System.TypInfo, System.UITypes;

type
  TColor = System.UITypes.TColor;
  TColorRec = System.UITypes.TColorRec;
{$SCOPEDENUMS ON}
  TRichEditModeInsert = (tmFirst, tmLast);
{$SCOPEDENUMS OFF}

  TProviderRichEdit = class(TDataLoggerProvider<TProviderRichEdit>)
  private
    FRichEdit: TCustomRichEdit;
    FUseColorInRichEdit: Boolean;
    FColorTrace: TColor;
    FColorDebug: TColor;
    FColorInfo: TColor;
    FColorSuccess: TColor;
    FColorWarn: TColor;
    FColorError: TColor;
    FColorFatal: TColor;
    FColorCustom: TColor;
    FMaxLogLines: Integer;
    FModeInsert: TRichEditModeInsert;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function RichEdit(const AValue: TCustomRichEdit): TProviderRichEdit;
    function UseColorInRichEdit(const AValue: Boolean): TProviderRichEdit;
    function ChangeColor(const ALevel: TLoggerLevel; const AColor: TColor): TProviderRichEdit;
    function MaxLogLines(const AValue: Integer): TProviderRichEdit;
    function ModeInsert(const AValue: TRichEditModeInsert): TProviderRichEdit;
    function CleanOnStart(const AValue: Boolean): TProviderRichEdit;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

{$ENDIF}

implementation

{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX) OR NOT(DEFINED(LINUX))}

type
  THackCustom = class(TCustomRichEdit);

  { TProviderRichEdit }

constructor TProviderRichEdit.Create;
begin
  inherited Create;

  RichEdit(nil);
  UseColorInRichEdit(True);

  ChangeColor(TLoggerLevel.Trace, $00A100AD);
  ChangeColor(TLoggerLevel.Debug, $00AEB600);
  ChangeColor(TLoggerLevel.Info, $000000);
  ChangeColor(TLoggerLevel.Success, $0009AC00);
  ChangeColor(TLoggerLevel.Warn, $0000A4D8);
  ChangeColor(TLoggerLevel.Error, $001D2AAA);
  ChangeColor(TLoggerLevel.Fatal, $001C0FD1);
  ChangeColor(TLoggerLevel.Custom, $000000);

  MaxLogLines(0);
  ModeInsert(TRichEditModeInsert.tmLast);
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

function TProviderRichEdit.ChangeColor(const ALevel: TLoggerLevel; const AColor: TColor): TProviderRichEdit;
begin
  Result := Self;

  case ALevel of
    TLoggerLevel.Trace:
      FColorTrace := AColor;

    TLoggerLevel.Debug:
      FColorDebug := AColor;

    TLoggerLevel.Info:
      FColorInfo := AColor;

    TLoggerLevel.Success:
      FColorSuccess := AColor;

    TLoggerLevel.Warn:
      FColorWarn := AColor;

    TLoggerLevel.Error:
      FColorError := AColor;

    TLoggerLevel.Fatal:
      FColorFatal := AColor;

    TLoggerLevel.Custom:
      FColorCustom := AColor;
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
    ChangeColor(TLoggerLevel.Trace, StringToColor(LJO.GetValue<string>('change_color_trace', ColorToString(FColorTrace))));
    ChangeColor(TLoggerLevel.Debug, StringToColor(LJO.GetValue<string>('change_color_debug', ColorToString(FColorDebug))));
    ChangeColor(TLoggerLevel.Info, StringToColor(LJO.GetValue<string>('change_color_info', ColorToString(FColorInfo))));
    ChangeColor(TLoggerLevel.Success, StringToColor(LJO.GetValue<string>('change_color_success', ColorToString(FColorSuccess))));
    ChangeColor(TLoggerLevel.Warn, StringToColor(LJO.GetValue<string>('change_color_warn', ColorToString(FColorWarn))));
    ChangeColor(TLoggerLevel.Error, StringToColor(LJO.GetValue<string>('change_color_error', ColorToString(FColorError))));
    ChangeColor(TLoggerLevel.Fatal, StringToColor(LJO.GetValue<string>('change_color_fatal', ColorToString(FColorFatal))));
    ChangeColor(TLoggerLevel.Custom, StringToColor(LJO.GetValue<string>('change_color_custom', ColorToString(FColorCustom))));
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
    LJO.AddPair('change_color_trace', TJSONString.Create(ColorToString(FColorTrace)));
    LJO.AddPair('change_color_debug', TJSONString.Create(ColorToString(FColorDebug)));
    LJO.AddPair('change_color_info', TJSONString.Create(ColorToString(FColorInfo)));
    LJO.AddPair('change_color_success', TJSONString.Create(ColorToString(FColorSuccess)));
    LJO.AddPair('change_color_warn', TJSONString.Create(ColorToString(FColorWarn)));
    LJO.AddPair('change_color_error', TJSONString.Create(ColorToString(FColorError)));
    LJO.AddPair('change_color_fatal', TJSONString.Create(ColorToString(FColorFatal)));
    LJO.AddPair('change_color_custom', TJSONString.Create(ColorToString(FColorCustom)));
    LJO.AddPair('max_log_lines', TJSONNumber.Create(FMaxLogLines));
    LJO.AddPair('mode_insert', TJSONString.Create(GetEnumName(TypeInfo(TRichEditModeInsert), Integer(FModeInsert))));
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
  LColor: TColor;
  LRetriesCount: Integer;
  LLines: Integer;
  LSelStart: Int64;
  LSelLength: Int64;
begin
  if not Assigned(FRichEdit) then
    raise EDataLoggerException.Create('RichEdit not defined!');

  if Length(ACache) = 0 then
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

    LColor := clBlack;

    TThread.Synchronize(nil,
      procedure
      begin
        if (csDestroying in FRichEdit.ComponentState) then
          Exit;

        if not(THackCustom(FRichEdit).Color = clWindow) or not(THackCustom(FRichEdit).Color = clWhite) then
          LColor := clWhite
      end);

    if FUseColorInRichEdit then
      case LItem.Level of
        TLoggerLevel.Trace:
          LColor := FColorTrace;

        TLoggerLevel.Debug:
          LColor := FColorDebug;

        TLoggerLevel.Info:
          LColor := FColorInfo;

        TLoggerLevel.Success:
          LColor := FColorSuccess;

        TLoggerLevel.Warn:
          LColor := FColorWarn;

        TLoggerLevel.Error:
          LColor := FColorError;

        TLoggerLevel.Fatal:
          LColor := FColorFatal;

        TLoggerLevel.Custom:
          LColor := FColorCustom;
      end;

    LRetriesCount := 0;

    while True do
      try
        try
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FRichEdit.ComponentState) then
                Exit;

              if FModeInsert = TRichEditModeInsert.tmLast then
                FRichEdit.SelAttributes.Color := LColor;

              FRichEdit.Lines.BeginUpdate;
              case FModeInsert of
                TRichEditModeInsert.tmFirst:
                  FRichEdit.Lines.Insert(0, LLog);

                TRichEditModeInsert.tmLast:
                  FRichEdit.Lines.Add(LLog);
              end;

              if FUseColorInRichEdit then
                if FModeInsert = TRichEditModeInsert.tmFirst then
                begin
                  LSelStart := FRichEdit.SelStart;
                  LSelLength := FRichEdit.SelLength;

                  FRichEdit.SelStart := 0;
                  FRichEdit.SelLength := Length(LLog);

                  FRichEdit.SelAttributes.Color := LColor;

                  FRichEdit.SelStart := LSelStart;
                  FRichEdit.SelLength := LSelLength;
                end;
            end);

          if FMaxLogLines > 0 then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FRichEdit.ComponentState) then
                  Exit;

                LLines := FRichEdit.Lines.Count;
                while LLines > FMaxLogLines do
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
          end;
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
{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
                    FRichEdit.VScrollBar.Value := FRichEdit.VScrollBar.Min;
{$ENDIF}
                  end;

                TRichEditModeInsert.tmLast:
                  begin
{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
                    FRichEdit.VScrollBar.Value := FRichEdit.VScrollBar.Max;
{$ELSE}
                    SendMessage(FRichEdit.Handle, EM_LINESCROLL, 0, FRichEdit.Lines.Count);
{$ENDIF}
                  end;
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

          if LRetriesCount <= 0 then
            Break;

          if LRetriesCount >= FMaxRetries then
            Break;
        end;
      end;
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderRichEdit);

{$ENDIF}

end.
