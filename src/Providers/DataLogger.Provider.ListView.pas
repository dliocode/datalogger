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
    function ListView(const AValue: TCustomListView): TProviderListView;
    function MaxLogLines(const AValue: Integer): TProviderListView;

    constructor Create; overload;
    constructor Create(const AListView: TCustomListView; const AMaxLogLines: Integer = 0); overload; deprecated 'Use TProviderListView.Create.ListView(ListView).MaxLogLines(0) - This function will be removed in future versions';
{$ENDIF}
  end;

implementation

{ TProviderListView }

{$IF DEFINED(MSWINDOWS)}


constructor TProviderListView.Create;
begin
  inherited Create;

  ListView(nil);
  MaxLogLines(0);
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

{$ENDIF}


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
    if LItem.&Type = TLoggerType.All then
      LLog := ''
    else
      LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

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
