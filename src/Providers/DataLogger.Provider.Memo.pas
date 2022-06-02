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
{$IF DEFINED(MSWINDOWS)}
  Vcl.StdCtrls, Winapi.Windows, Winapi.Messages,
{$ENDIF}
  System.SysUtils, System.Classes;

type
  TModeInsert = (tmFirst, tmLast);

  TProviderMemo = class(TDataLoggerProvider)
  private
{$IF DEFINED(MSWINDOWS)}
    FMemo: TCustomMemo;
    FMaxLogLines: Integer;
    FModeInsert: TModeInsert;
{$ENDIF}
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
{$IF DEFINED(MSWINDOWS)}
    property Memo: TCustomMemo read FMemo write FMemo;
    property MaxLogLines: Integer read FMaxLogLines write FMaxLogLines;

    constructor Create(const AMemo: TCustomMemo; const AMaxLogLines: Integer = 0; const AModeInsert: TModeInsert = tmLast);
{$ENDIF}
  end;

implementation

{ TProviderMemo }

{$IF DEFINED(MSWINDOWS)}


constructor TProviderMemo.Create(const AMemo: TCustomMemo; const AMaxLogLines: Integer = 0; const AModeInsert: TModeInsert = tmLast);
begin
  inherited Create;

  FMemo := AMemo;
  FMaxLogLines := AMaxLogLines;
  FModeInsert := AModeInsert;
end;

{$ENDIF}


procedure TProviderMemo.Save(const ACache: TArray<TLoggerItem>);
{$IF DEFINED(MSWINDOWS)}
var
  LItem: TLoggerItem;
  LLog: string;
  LRetryCount: Integer;
  LLines: Integer;
begin
  if not Assigned(FMemo) then
    raise EDataLoggerException.Create('Memo not defined');

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
        if (csDestroying in FMemo.ComponentState) then
          Exit;

        FMemo.Lines.BeginUpdate;
        try
          TThread.Synchronize(nil,
            procedure
            begin
              if (csDestroying in FMemo.ComponentState) then
                Exit;

              case FModeInsert of
                tmFirst:
                  FMemo.Lines.Insert(0, LLog);
                tmLast:
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
                    tmFirst:
                      FMemo.Lines.Delete(Pred(LLines));
                    tmLast:
                      FMemo.Lines.Delete(0);
                  end;

                  LLines := FMemo.Lines.Count;
                end;
              end);
          end;
        finally
          if not(csDestroying in FMemo.ComponentState) then
          begin
            FMemo.Lines.EndUpdate;

            TThread.Synchronize(nil,
              procedure
              begin
                if (csDestroying in FMemo.ComponentState) then
                  Exit;

                if FModeInsert = tmLast then
                  SendMessage(FMemo.Handle, EM_LINESCROLL, 0, FMemo.Lines.Count);
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
