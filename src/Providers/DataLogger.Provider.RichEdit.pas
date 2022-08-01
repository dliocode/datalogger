{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.RichEdit;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  Vcl.ComCtrls, Vcl.Graphics, Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.JSON, System.TypInfo, System.UITypes;

type
  TColor = System.UITypes.TColor;
  TColorRec = System.UITypes.TColorRec;
  TRichEditModeInsert = (tmFirst, tmLast);

  TProviderRichEdit = class(TDataLoggerProvider)
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
    function ChangeColor(const ALogType: TLoggerType; const AColor: TColor): TProviderRichEdit;
    function MaxLogLines(const AValue: Integer): TProviderRichEdit;
    function ModeInsert(const AValue: TRichEditModeInsert): TProviderRichEdit;
    function CleanOnStart(const AValue: Boolean): TProviderRichEdit;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
  end;

implementation

type
  THackCustom = class(TCustomRichEdit);

  { TProviderRichEdit }

constructor TProviderRichEdit.Create;
begin
  inherited Create;

  RichEdit(nil);
  UseColorInRichEdit(True);

  ChangeColor(TLoggerType.Trace, $00A100AD);
  ChangeColor(TLoggerType.Debug, $00CCD249);
  ChangeColor(TLoggerType.Info, $000000);
  ChangeColor(TLoggerType.Success, $000CC516);
  ChangeColor(TLoggerType.Warn, $0074DFFF);
  ChangeColor(TLoggerType.Error, $001D2AAA);
  ChangeColor(TLoggerType.Fatal, $001C0FD1);
  ChangeColor(TLoggerType.Custom, $000000);

  MaxLogLines(0);
  ModeInsert(tmLast);
  CleanOnStart(False);
  FCleanOnRun := False;
end;

function TProviderRichEdit.RichEdit(const AValue: TCustomRichEdit): TProviderRichEdit;
begin
  Result := Self;
  FRichEdit := AValue;
end;

function TProviderRichEdit.ChangeColor(const ALogType: TLoggerType; const AColor: TColor): TProviderRichEdit;
begin
  Result := Self;

  case ALogType of
    TLoggerType.Trace:
      FColorTrace := AColor;
    TLoggerType.Debug:
      FColorDebug := AColor;
    TLoggerType.Info:
      FColorInfo := AColor;
    TLoggerType.Success:
      FColorSuccess := AColor;
    TLoggerType.Warn:
      FColorWarn := AColor;
    TLoggerType.Error:
      FColorError := AColor;
    TLoggerType.Fatal:
      FColorFatal := AColor;
    TLoggerType.Custom:
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
    ChangeColor(TLoggerType.Trace, StringToColor(LJO.GetValue<string>('change_color_trace', ColorToString(FColorTrace))));
    ChangeColor(TLoggerType.Debug, StringToColor(LJO.GetValue<string>('change_color_debug', ColorToString(FColorDebug))));
    ChangeColor(TLoggerType.Info, StringToColor(LJO.GetValue<string>('change_color_info', ColorToString(FColorInfo))));
    ChangeColor(TLoggerType.Success, StringToColor(LJO.GetValue<string>('change_color_success', ColorToString(FColorSuccess))));
    ChangeColor(TLoggerType.Warn, StringToColor(LJO.GetValue<string>('change_color_warn', ColorToString(FColorWarn))));
    ChangeColor(TLoggerType.Error, StringToColor(LJO.GetValue<string>('change_color_error', ColorToString(FColorError))));
    ChangeColor(TLoggerType.Fatal, StringToColor(LJO.GetValue<string>('change_color_fatal', ColorToString(FColorFatal))));
    ChangeColor(TLoggerType.Custom, StringToColor(LJO.GetValue<string>('change_color_custom', ColorToString(FColorCustom))));

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
    LJO.AddPair('change_color_trace', ColorToString(FColorTrace));
    LJO.AddPair('change_color_debug', ColorToString(FColorDebug));
    LJO.AddPair('change_color_info', ColorToString(FColorInfo));
    LJO.AddPair('change_color_success', ColorToString(FColorSuccess));
    LJO.AddPair('change_color_warn', ColorToString(FColorWarn));
    LJO.AddPair('change_color_error', ColorToString(FColorError));
    LJO.AddPair('change_color_fatal', ColorToString(FColorFatal));
    LJO.AddPair('change_color_custom', ColorToString(FColorCustom));
    LJO.AddPair('max_log_lines', TJSONNumber.Create(FMaxLogLines));
    LJO.AddPair('mode_insert', GetEnumName(TypeInfo(TRichEditModeInsert), Integer(FModeInsert)));
    LJO.AddPair('clean_on_start', TJSONBool.Create(FCleanOnStart));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

function TProviderRichEdit.UseColorInRichEdit(const AValue: Boolean): TProviderRichEdit;
begin
  Result := Self;
  FUseColorInRichEdit := AValue;
end;

procedure TProviderRichEdit.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LLog: string;
  LRetriesCount: Integer;
  LLines: Integer;
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
    if LItem.InternalItem.TypeSlineBreak then
      LLog := ''
    else
      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    TThread.Synchronize(nil,
      procedure
      begin
        if (csDestroying in FRichEdit.ComponentState) then
          Exit;
      end);

    if (THackCustom(FRichEdit).Color = clWindow) or (THackCustom(FRichEdit).Color = clWhite) then
      FRichEdit.SelAttributes.Color := clBlack
    else
      FRichEdit.SelAttributes.Color := clWhite;

    if FUseColorInRichEdit then
      case LItem.&Type of
        TLoggerType.Trace:
          FRichEdit.SelAttributes.Color := FColorTrace;

        TLoggerType.Debug:
          FRichEdit.SelAttributes.Color := FColorDebug;

        TLoggerType.Info:
          FRichEdit.SelAttributes.Color := FColorInfo;

        TLoggerType.Success:
          FRichEdit.SelAttributes.Color := FColorSuccess;

        TLoggerType.Warn:
          FRichEdit.SelAttributes.Color := FColorWarn;

        TLoggerType.Error:
          FRichEdit.SelAttributes.Color := FColorError;

        TLoggerType.Fatal:
          FRichEdit.SelAttributes.Color := FColorFatal;

        TLoggerType.Custom:
          FRichEdit.SelAttributes.Color := FColorCustom;
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

              FRichEdit.Lines.BeginUpdate;
              case FModeInsert of
                tmFirst:
                  FRichEdit.Lines.Insert(0, LLog);

                tmLast:
                  FRichEdit.Lines.Add(LLog);
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
                    tmFirst:
                      FRichEdit.Lines.Delete(Pred(LLines));

                    tmLast:
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
                tmFirst:
                  begin
{$IF DEFINED(DATALOGGER_FMX)}
                    FRichEdit.VScrollBar.Value := FRichEdit.VScrollBar.Min;
{$ENDIF}
                  end;

                tmLast:
                  begin
{$IF DEFINED(DATALOGGER_FMX)}
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

end.
