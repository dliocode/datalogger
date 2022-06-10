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
{$IF DEFINED(MSWINDOWS)}
  Vcl.StdCtrls,
{$ENDIF}
  System.SysUtils, System.Classes;

type
  TProviderListBox = class(TDataLoggerProvider)
  private
{$IF DEFINED(MSWINDOWS)}
    FListBox: TCustomListBox;
    FMaxLogLines: Integer;
{$ENDIF}
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
{$IF DEFINED(MSWINDOWS)}
    function ListBox(const AValue: TCustomListBox): TProviderListBox;
    function MaxLogLines(const AValue: Integer): TProviderListBox;

    constructor Create; overload;
    constructor Create(const AListBox: TCustomListBox; const AMaxLogLines: Integer = 0); overload; deprecated 'Use TProviderListBox.Create.ListBox(ListBox).MaxLogLines(0) - This function will be removed in future versions';
{$ENDIF}
  end;

implementation

{ TProviderListBox }

{$IF DEFINED(MSWINDOWS)}


constructor TProviderListBox.Create;
begin
  inherited Create;

  ListBox(nil);
  MaxLogLines(0);
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

{$ENDIF}


procedure TProviderListBox.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(MSWINDOWS)}
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
              FListBox.AddItem(LLog, nil);
            end);

          if FMaxLogLines > 0 then
          begin
            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListBox.ComponentState) then
                  Exit;

                LLines := FListBox.Items.Count;
                while LLines > FMaxLogLines do
                begin
                  FListBox.Items.Delete(0);
                  LLines := FListBox.Items.Count;
                end;
              end);
          end;
        finally
          if not(csDestroying in FListBox.ComponentState) then
          begin
            FListBox.Items.EndUpdate;

            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListBox.ComponentState) then
                  Exit;

                FListBox.ItemIndex := FListBox.Items.Count - 1;
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
{$ELSE}


begin
end;
{$ENDIF}


end.
