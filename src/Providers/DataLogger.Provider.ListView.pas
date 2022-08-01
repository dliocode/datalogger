{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}
unit DataLogger.Provider.ListView;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_FMX)}
  FMX.ListView,
{$ELSE}
  Vcl.ComCtrls,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.TypInfo;

type
{$SCOPEDENUMS ON}
  TListViewModeInsert = (tmFirst, tmLast);
{$SCOPEDENUMS OFF}

  TProviderListView = class(TDataLoggerProvider)
  private
    FListView: TCustomListView;
    FMaxLogLines: Integer;
    FModeInsert: TListViewModeInsert;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ListView(const AValue: TCustomListView): TProviderListView;
    function MaxLogLines(const AValue: Integer): TProviderListView;
    function ModeInsert(const AValue: TListViewModeInsert): TProviderListView;
    function CleanOnStart(const AValue: Boolean): TProviderListView;
    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;
    constructor Create;
  end;

implementation

{ TProviderListView }

constructor TProviderListView.Create;
begin
  inherited Create;

  ListView(nil);
  MaxLogLines(0);
  ModeInsert(TListViewModeInsert.tmLast);
  CleanOnStart(False);
  FCleanOnRun := False;
end;

function TProviderListView.ListView(const AValue: TCustomListView): TProviderListView;
begin
  Result := Self;
  FListView := AValue;
end;

function TProviderListView.MaxLogLines(const AValue: Integer): TProviderListView;
begin
  Result := Self;
  FMaxLogLines := AValue;
end;

function TProviderListView.ModeInsert(const AValue: TListViewModeInsert): TProviderListView;
begin
  Result := Self;
  FModeInsert := AValue;
end;

function TProviderListView.CleanOnStart(const AValue: Boolean): TProviderListView;
begin
  Result := Self;
  FCleanOnStart := AValue;
end;

procedure TProviderListView.LoadFromJSON(const AJSON: string);
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
    LValue := GetEnumName(TypeInfo(TListViewModeInsert), Integer(FModeInsert));
    FModeInsert := TListViewModeInsert(GetEnumValue(TypeInfo(TListViewModeInsert), LJO.GetValue<string>('mode_insert', LValue)));
    CleanOnStart(LJO.GetValue<Boolean>('clean_on_start', FCleanOnStart));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderListView.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('max_log_lines', TJSONNumber.Create(FMaxLogLines));
    LJO.AddPair('mode_insert', GetEnumName(TypeInfo(TListViewModeInsert), Integer(FModeInsert)));
    LJO.AddPair('clean_on_start', TJSONBool.Create(FCleanOnStart));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderListView.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LLog: string;
  LRetriesCount: Integer;
  LLines: Integer;
begin
  if not Assigned(FListView) then
    raise EDataLoggerException.Create('ListView not defined!');

  if Length(ACache) = 0 then
    Exit;

  if not FCleanOnRun then
    if FCleanOnStart then
    begin
      FListView.Items.Clear;
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
        if (csDestroying in FListView.ComponentState) then
          Exit;

        try
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FListView.ComponentState) then
                Exit;

{$IF DEFINED(DATALOGGER_FMX)}
              FListView.BeginUpdate;
{$ELSE}
              FListView.Items.BeginUpdate;
{$ENDIF}

              case FModeInsert of
                TListViewModeInsert.tmFirst:
                  begin
{$IF DEFINED(DATALOGGER_FMX)}
                    FListView.Items.AddItem(0).IndexTitle := LLog;
{$ELSE}
                    FListView.Items.Insert(0).Caption := LLog;
{$ENDIF}
                  end;

                TListViewModeInsert.tmLast:
                  begin
{$IF DEFINED(DATALOGGER_FMX)}
                    FListView.Items.Add.IndexTitle := LLog;
{$ELSE}
                    FListView.Items.Add.Caption := LLog;
{$ENDIF}
                  end;
              end;
            end);

          if FMaxLogLines > 0 then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListView.ComponentState) then
                  Exit;

                LLines := FListView.Items.Count;

                case FModeInsert of
                  TListViewModeInsert.tmFirst:
                    begin
                      while LLines > FMaxLogLines do
                      begin
                        FListView.Items.Delete(Pred(FListView.Items.Count));
                        LLines := FListView.Items.Count;
                      end;
                    end;

                  TListViewModeInsert.tmLast:
                    begin
                      while LLines > FMaxLogLines do
                      begin
                        FListView.Items.Delete(0);
                        LLines := FListView.Items.Count;
                      end;
                    end;
                end;
              end);
          end;
        finally
          if not(csDestroying in FListView.ComponentState) then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListView.ComponentState) then
                  Exit;

{$IF DEFINED(DATALOGGER_FMX)}
                FListView.EndUpdate;
{$ELSE}
                FListView.Items.EndUpdate;
{$ENDIF}

                case FModeInsert of
                  TListViewModeInsert.tmFirst:
                    begin
{$IF DEFINED(DATALOGGER_FMX)}
                      FListView.ScrollTo(0);
{$ELSE}
                      FListView.Scroll(0, 0);
{$ENDIF}
                    end;

                  TListViewModeInsert.tmLast:
                    begin
{$IF DEFINED(DATALOGGER_FMX)}
                      FListView.ScrollTo(Pred(FListView.Items.Count));
{$ELSE}
                      FListView.Scroll(0, FListView.Items.Count);
{$ENDIF}
                    end;
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

ForceReferenceToClass(TProviderListView);

end.
