{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Memo;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_FMX)}
  FMX.Memo,
{$ELSE}
  Vcl.StdCtrls, Winapi.Windows, Winapi.Messages,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.TypInfo;

type
{$SCOPEDENUMS ON}
  TMemoModeInsert = (tmFirst, tmLast);
{$SCOPEDENUMS OFF}

  TProviderMemo = class(TDataLoggerProvider)
  private
    FMemo: TCustomMemo;
    FMaxLogLines: Integer;
    FModeInsert: TMemoModeInsert;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function Memo(const AValue: TCustomMemo): TProviderMemo;
    function MaxLogLines(const AValue: Integer): TProviderMemo;
    function ModeInsert(const AValue: TMemoModeInsert): TProviderMemo;
    function CleanOnStart(const AValue: Boolean): TProviderMemo;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
    constructor Create(const AMemo: TCustomMemo; const AMaxLogLines: Integer = 0; const AModeInsert: TMemoModeInsert = TMemoModeInsert.tmLast); overload; deprecated 'Use TProviderMemo.Create.Memo(Memo).MaxLogLines(0).ModeInsert(tmLast) - This function will be removed in future versions';
  end;

implementation

{ TProviderMemo }

constructor TProviderMemo.Create;
begin
  inherited Create;

  Memo(nil);
  MaxLogLines(0);
  ModeInsert(TMemoModeInsert.tmLast);
  CleanOnStart(False);
  FCleanOnRun := False;
end;

constructor TProviderMemo.Create(const AMemo: TCustomMemo; const AMaxLogLines: Integer = 0; const AModeInsert: TMemoModeInsert = TMemoModeInsert.tmLast);
begin
  Create;

  Memo(AMemo);
  MaxLogLines(AMaxLogLines);
  ModeInsert(AModeInsert);
end;

function TProviderMemo.Memo(const AValue: TCustomMemo): TProviderMemo;
begin
  Result := Self;
  FMemo := AValue;
end;

function TProviderMemo.MaxLogLines(const AValue: Integer): TProviderMemo;
begin
  Result := Self;
  FMaxLogLines := AValue;
end;

function TProviderMemo.ModeInsert(const AValue: TMemoModeInsert): TProviderMemo;
begin
  Result := Self;
  FModeInsert := AValue;
end;

function TProviderMemo.CleanOnStart(const AValue: Boolean): TProviderMemo;
begin
  Result := Self;
  FCleanOnStart := AValue;
end;

procedure TProviderMemo.LoadFromJSON(const AJSON: string);
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
    MaxLogLines(LJO.GetValue<Integer>('max_log_lines', FMaxLogLines));

    LValue := GetEnumName(TypeInfo(TMemoModeInsert), Integer(FModeInsert));
    FModeInsert := TMemoModeInsert(GetEnumValue(TypeInfo(TMemoModeInsert), LJO.GetValue<string>('mode_insert', LValue)));

    CleanOnStart(LJO.GetValue<Boolean>('clean_on_start', FCleanOnStart));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderMemo.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('max_log_lines', TJSONNumber.Create(FMaxLogLines));
    LJO.AddPair('mode_insert', GetEnumName(TypeInfo(TMemoModeInsert), Integer(FModeInsert)));
    LJO.AddPair('clean_on_start', TJSONBool.Create(FCleanOnStart));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderMemo.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LLog: string;
  LRetriesCount: Integer;
  LLines: Integer;
begin
  if not Assigned(FMemo) then
    raise EDataLoggerException.Create('Memo not defined!');

  if Length(ACache) = 0 then
    Exit;

  if not FCleanOnRun then
    if FCleanOnStart then
    begin
      FMemo.Lines.Clear;
      FCleanOnRun := True;
    end;

  for LItem in ACache do
  begin
    if LItem.InternalItem.TypeSlineBreak then
      LLog := ''
    else
      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetriesCount := 0;

    while True do
      try
        try
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FMemo.ComponentState) then
                Exit;

              FMemo.Lines.BeginUpdate;

              case FModeInsert of
                TMemoModeInsert.tmFirst:
                  FMemo.Lines.Insert(0, LLog);

                TMemoModeInsert.tmLast:
                  FMemo.Lines.Add(LLog);
              end;
            end);

          if FMaxLogLines > 0 then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FMemo.ComponentState) then
                  Exit;

                LLines := FMemo.Lines.Count;
                while LLines > FMaxLogLines do
                begin
                  case FModeInsert of
                    TMemoModeInsert.tmFirst:
                      FMemo.Lines.Delete(Pred(LLines));

                    TMemoModeInsert.tmLast:
                      FMemo.Lines.Delete(0);
                  end;

                  LLines := FMemo.Lines.Count;
                end;
              end);
          end;
        finally
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FMemo.ComponentState) then
                Exit;

              FMemo.Lines.EndUpdate;

              case FModeInsert of
                TMemoModeInsert.tmFirst:
                  begin
{$IF DEFINED(DATALOGGER_FMX)}
                    FMemo.VScrollBar.Value := FMemo.VScrollBar.Min;
{$ENDIF}
                  end;

                TMemoModeInsert.tmLast:
                  begin
{$IF DEFINED(DATALOGGER_FMX)}
                    FMemo.VScrollBar.Value := FMemo.VScrollBar.Max;
{$ELSE}
                    SendMessage(FMemo.Handle, EM_LINESCROLL, 0, FMemo.Lines.Count);
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

ForceReferenceToClass(TProviderMemo);

end.
