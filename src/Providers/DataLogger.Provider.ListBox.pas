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
    constructor Create(const AListBox: TCustomListBox; const AMaxLogLines: Integer = 0);
{$ENDIF}
    destructor Destroy; override;
  end;

implementation

{ TProviderListBox }

{$IF DEFINED(MSWINDOWS)}

constructor TProviderListBox.Create(const AListBox: TCustomListBox; const AMaxLogLines: Integer = 0);
begin
  inherited Create;

  FListBox := AListBox;
  FMaxLogLines := AMaxLogLines;
end;

{$ENDIF}

destructor TProviderListBox.Destroy;
begin
  inherited;
end;

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
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      LLog := ''
    else
      LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

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

          if Assigned(LogException) then
            LogException(Self, LItem, E, LRetryCount);

          if Self.Terminated then
            Exit;

          if LRetryCount >= GetMaxRetry then
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
