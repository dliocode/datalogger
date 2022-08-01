{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.ListBox;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_FMX)}
  FMX.ListBox,
{$ELSE}
  Vcl.StdCtrls,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.TypInfo;

type
{$SCOPEDENUMS ON}
  TListBoxModeInsert = (tmFirst, tmLast);
{$SCOPEDENUMS OFF}

  TProviderListBox = class(TDataLoggerProvider)
  private
    FListBox: TCustomListBox;
    FMaxLogLines: Integer;
    FModeInsert: TListBoxModeInsert;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ListBox(const AValue: TCustomListBox): TProviderListBox;
    function MaxLogLines(const AValue: Integer): TProviderListBox;
    function ModeInsert(const AValue: TListBoxModeInsert): TProviderListBox;
    function CleanOnStart(const AValue: Boolean): TProviderListBox;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderListBox }

constructor TProviderListBox.Create;
begin
  inherited Create;

  ListBox(nil);
  MaxLogLines(0);
  ModeInsert(TListBoxModeInsert.tmLast);
  CleanOnStart(False);
  FCleanOnRun := False;
end;

function TProviderListBox.ListBox(const AValue: TCustomListBox): TProviderListBox;
begin
  Result := Self;
  FListBox := AValue;
end;

function TProviderListBox.MaxLogLines(const AValue: Integer): TProviderListBox;
begin
  Result := Self;
  FMaxLogLines := AValue;
end;

function TProviderListBox.ModeInsert(const AValue: TListBoxModeInsert): TProviderListBox;
begin
  Result := Self;
  FModeInsert := AValue;
end;

function TProviderListBox.CleanOnStart(const AValue: Boolean): TProviderListBox;
begin
  Result := Self;
  FCleanOnStart := AValue;
end;

procedure TProviderListBox.LoadFromJSON(const AJSON: string);
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

    LValue := GetEnumName(TypeInfo(TListBoxModeInsert), Integer(FModeInsert));
    FModeInsert := TListBoxModeInsert(GetEnumValue(TypeInfo(TListBoxModeInsert), LJO.GetValue<string>('mode_insert', LValue)));

    CleanOnStart(LJO.GetValue<Boolean>('clean_on_start', FCleanOnStart));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderListBox.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('max_log_lines', TJSONNumber.Create(FMaxLogLines));
    LJO.AddPair('mode_insert', GetEnumName(TypeInfo(TListBoxModeInsert), Integer(FModeInsert)));
    LJO.AddPair('clean_on_start', TJSONBool.Create(FCleanOnStart));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderListBox.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LLog: string;
  LRetriesCount: Integer;
  LLines: Integer;
begin
  if not Assigned(FListBox) then
    raise EDataLoggerException.Create('ListBox not defined!');

  if Length(ACache) = 0 then
    Exit;

  if not FCleanOnRun then
    if FCleanOnStart then
    begin
      FListBox.Clear;
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
        if (csDestroying in FListBox.ComponentState) then
          Exit;

        try
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FListBox.ComponentState) then
                Exit;

              FListBox.Items.BeginUpdate;

              case FModeInsert of
                TListBoxModeInsert.tmFirst:
                  FListBox.Items.Insert(0, LLog);

                TListBoxModeInsert.tmLast:
                  FListBox.Items.Add(LLog);
              end;
            end);

          if FMaxLogLines > 0 then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListBox.ComponentState) then
                  Exit;

                LLines := FListBox.Items.Count;

                case FModeInsert of
                  TListBoxModeInsert.tmFirst:
                    begin
                      while LLines > FMaxLogLines do
                      begin
                        FListBox.Items.Delete(Pred(LLines));
                        LLines := FListBox.Items.Count;
                      end;
                    end;

                  TListBoxModeInsert.tmLast:
                    begin
                      while LLines > FMaxLogLines do
                      begin
                        FListBox.Items.Delete(0);
                        LLines := FListBox.Items.Count;
                      end;
                    end;
                end;
              end);
          end;
        finally
          if not(csDestroying in FListBox.ComponentState) then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListBox.ComponentState) then
                  Exit;

                FListBox.Items.EndUpdate;

                case FModeInsert of
                  TListBoxModeInsert.tmFirst:
                    FListBox.ItemIndex := 0;

                  TListBoxModeInsert.tmLast:
                    FListBox.ItemIndex := FListBox.Items.Count - 1;
                end;
              end);
          end;
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

ForceReferenceToClass(TProviderListBox);

end.
