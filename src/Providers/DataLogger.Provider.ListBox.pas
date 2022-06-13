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
  System.SysUtils, System.Classes;

type
  TListBoxModeInsert = (tmFirst, tmLast);

  TProviderListBox = class(TDataLoggerProvider)
  private
    FListBox: TCustomListBox;
    FMaxLogLines: Integer;
    FModeInsert: TListBoxModeInsert;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function ListBox(const AValue: TCustomListBox): TProviderListBox;
    function MaxLogLines(const AValue: Integer): TProviderListBox;
    function ModeInsert(const AValue: TListBoxModeInsert): TProviderListBox;

    constructor Create; overload;
    constructor Create(const AListBox: TCustomListBox; const AMaxLogLines: Integer = 0); overload; deprecated 'Use TProviderListBox.Create.ListBox(ListBox).MaxLogLines(0) - This function will be removed in future versions';
  end;

implementation

{ TProviderListBox }

constructor TProviderListBox.Create;
begin
  inherited Create;

  ListBox(nil);
  MaxLogLines(0);
  ModeInsert(tmLast);
end;

constructor TProviderListBox.Create(const AListBox: TCustomListBox; const AMaxLogLines: Integer = 0);
begin
  Create;

  ListBox(AListBox);
  MaxLogLines(AMaxLogLines);
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

procedure TProviderListBox.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LLog: string;
  LRetryCount: Integer;
  LLines: Integer;
begin
  if not Assigned(FListBox) then
    raise EDataLoggerException.Create('ListBox not defined!');

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
                tmFirst:
                  FListBox.Items.Insert(0, LLog);

                tmLast:
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
                  tmFirst:
                    begin
                      while LLines > FMaxLogLines do
                      begin
                        FListBox.Items.Delete(Pred(LLines));
                        LLines := FListBox.Items.Count;
                      end;
                    end;

                  tmLast:
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
                  tmFirst:
                    FListBox.ItemIndex := 0;

                  tmLast:
                    FListBox.ItemIndex := FListBox.Items.Count - 1;
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
