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

unit DataLogger.Provider.ListView;

interface

{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX) OR NOT(DEFINED(LINUX))}


uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
  FMX.ListView,
{$ELSE}
  Vcl.ComCtrls,
{$ENDIF}
  System.SysUtils, System.Classes, System.JSON, System.TypInfo;

type
{$SCOPEDENUMS ON}
  TListViewModeInsert = (tmFirst, tmLast);
{$SCOPEDENUMS OFF}

  TProviderListView = class(TDataLoggerProvider<TProviderListView>)
  private
    FListView: TCustomListView;
    FMaxLogLines: Integer;
    FModeInsert: TListViewModeInsert;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
    procedure UndoLast;
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

{$ENDIF}

implementation

{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX) OR NOT(DEFINED(LINUX))}

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
    LJO.AddPair('mode_insert', TJSONString.Create(GetEnumName(TypeInfo(TListViewModeInsert), Integer(FModeInsert))));
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

  if (Length(ACache) = 0) then
    Exit;

  if not FCleanOnRun then
    if FCleanOnStart then
    begin
      FListView.Items.Clear;
      FCleanOnRun := True;
    end;

  for LItem in ACache do
  begin
    if LItem.InternalItem.IsUndoLast then
    begin
      UndoLast;
      Continue;
    end;

    if LItem.InternalItem.IsSlinebreak then
      LLog := ''
    else
      LLog := SerializeItem.LogItem(LItem).ToString;

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

{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
              FListView.BeginUpdate;
{$ELSE}
              FListView.Items.BeginUpdate;
{$ENDIF}
              case FModeInsert of
                TListViewModeInsert.tmFirst:
                  begin
{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
                    FListView.Items.AddItem(0).IndexTitle := LLog;
{$ELSE}
                    FListView.Items.Insert(0).Caption := LLog;
{$ENDIF}
                  end;

                TListViewModeInsert.tmLast:
                  begin
{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
                    FListView.Items.Add.IndexTitle := LLog;
{$ELSE}
                    FListView.Items.Add.Caption := LLog;
{$ENDIF}
                  end;
              end;
            end);

          if (FMaxLogLines > 0) then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListView.ComponentState) then
                  Exit;

                LLines := FListView.Items.Count;
                while (LLines > FMaxLogLines) do
                begin
                  case FModeInsert of
                    TListViewModeInsert.tmFirst:
                      FListView.Items.Delete(Pred(FListView.Items.Count));

                    TListViewModeInsert.tmLast:
                      FListView.Items.Delete(0);
                  end;

                  LLines := FListView.Items.Count;
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

{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
                FListView.EndUpdate;
{$ELSE}
                FListView.Items.EndUpdate;
{$ENDIF}
                case FModeInsert of
                  TListViewModeInsert.tmFirst:
                    begin
{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
                      FListView.ScrollTo(0);
{$ELSE}
                      FListView.Scroll(0, 0);
{$ENDIF}
                    end;

                  TListViewModeInsert.tmLast:
                    begin
{$IF DEFINED(DATALOGGER_FMX) OR DEFINED(FRAMEWORK_FMX)}
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

          if (LRetriesCount <= 0) then
            Break;

          if (LRetriesCount >= FMaxRetries) then
            Break;
        end;
      end;
  end;
end;

procedure TProviderListView.UndoLast;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      if (csDestroying in FListView.ComponentState) then
        Exit;

      case FModeInsert of
        TListViewModeInsert.tmFirst:
          FListView.Items.Delete(0);

        TListViewModeInsert.tmLast:
          FListView.Items.Delete(Pred(FListView.Items.Count));
      end;
    end);
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderListView);

{$ENDIF}

end.
