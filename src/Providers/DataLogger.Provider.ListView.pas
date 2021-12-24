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
{$IF DEFINED(MSWINDOWS)}
  Vcl.ComCtrls,
{$ENDIF}
  System.SysUtils, System.Classes;

type
  TProviderListView = class(TDataLoggerProvider)
  private
{$IF DEFINED(MSWINDOWS)}
    FListView: TCustomListView;
    FMaxLogLines: Integer;
{$ENDIF}
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
{$IF DEFINED(MSWINDOWS)}
    constructor Create(const AListView: TCustomListView; const AMaxLogLines: Integer = 0);
{$ENDIF}
    destructor Destroy; override;
  end;

implementation

{ TProviderListView }

{$IF DEFINED(MSWINDOWS)}

constructor TProviderListView.Create(const AListView: TCustomListView; const AMaxLogLines: Integer = 0);
begin
  inherited Create;

  FListView := AListView;
  FMaxLogLines := AMaxLogLines;
end;

{$ENDIF}

destructor TProviderListView.Destroy;
begin
  inherited;
end;

procedure TProviderListView.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(MSWINDOWS)}
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
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
      LLog := ''
    else
      LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

    LRetryCount := 0;

    while True do
      try
        if (csDestroying in FListView.ComponentState) then
          Exit;

        FListView.Items.BeginUpdate;
        try
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FListView.ComponentState) then
                Exit;

              FListView.Items.Add.Caption := LLog;
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
            FListView.Items.EndUpdate;

            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FListView.ComponentState) then
                  Exit;

                FListView.Scroll(0, FListView.Items.Count);
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
