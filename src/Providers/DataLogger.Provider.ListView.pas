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
  System.SysUtils, System.Classes;

type
  TProviderListViewModeInsert = (tmFirst, tmLast);

  TProviderListView = class(TDataLoggerProvider)
  private
    FListView: TCustomListView;
    FMaxLogLines: Integer;
    FModeInsert: TProviderListViewModeInsert;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ListView(const AValue: TCustomListView): TProviderListView;
    function MaxLogLines(const AValue: Integer): TProviderListView;
    function ModeInsert(const AValue: TProviderListViewModeInsert): TProviderListView;

    constructor Create; overload;
    constructor Create(const AListView: TCustomListView; const AMaxLogLines: Integer = 0); overload; deprecated 'Use TProviderListView.Create.ListView(ListView).MaxLogLines(0) - This function will be removed in future versions';
  end;

implementation

{ TProviderListView }

constructor TProviderListView.Create;
begin
  inherited Create;

  ListView(nil);
  MaxLogLines(0);
  ModeInsert(tmLast);
end;

constructor TProviderListView.Create(const AListView: TCustomListView; const AMaxLogLines: Integer = 0);
begin
  Create;

  ListView(AListView);
  MaxLogLines(AMaxLogLines);
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

function TProviderListView.ModeInsert(const AValue: TProviderListViewModeInsert): TProviderListView;
begin
  Result := Self;
  FModeInsert := AValue;
end;

procedure TProviderListView.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LLog: string;
  LRetryCount: Integer;
  LLines: Integer;
begin
  if not Assigned(FListView) then
    raise EDataLoggerException.Create('ListView not defined!');

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.&Type = TLoggerType.All then
      LLog := ''
    else
      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetryCount := 0;

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

              FListView.Items.BeginUpdate;

              case FModeInsert of
                tmFirst:
                  begin
{$IF DEFINED(DATALOGGER_FMX)}
                    FListView.Items.AddItem(0).IndexTitle := LLog;
{$ELSE}
                    FListView.Items.Insert(0).Caption := LLog;
{$ENDIF}
                  end;

                tmLast:
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
                while LLines > FMaxLogLines do
                begin
                  FListView.Items.Delete(0);
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

                FListView.Items.EndUpdate;

                case FModeInsert of
                  tmFirst:
                    begin
{$IF DEFINED(DATALOGGER_FMX)}
                      FListView.ScrollTo(0);
{$ELSE}
                      FListView.Scroll(0, 0);
{$ENDIF}
                    end;

                  tmLast:
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
          Inc(LRetryCount);

          Sleep(50);

          if Assigned(FLogException) then
            FLogException(Self, LItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= FMaxRetry then
            Break;
        end;
      end;
  end;
end;

end.
