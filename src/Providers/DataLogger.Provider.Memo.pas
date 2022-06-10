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
    function Memo(const AValue: TCustomMemo): TProviderMemo;
    function MaxLogLines(const AValue: Integer): TProviderMemo;
    function ModeInsert(const AValue: TModeInsert): TProviderMemo;

    constructor Create; overload;
    constructor Create(const AMemo: TCustomMemo; const AMaxLogLines: Integer = 0; const AModeInsert: TModeInsert = tmLast); overload; deprecated 'Use TProviderMemo.Create.Memo(Memo).MaxLogLines(0).ModeInsert(tmLast) - This function will be removed in future versions';
{$ENDIF}
  end;

implementation

{ TProviderMemo }

{$IF DEFINED(MSWINDOWS)}


constructor TProviderMemo.Create;
begin
  inherited Create;

  Memo(nil);
  MaxLogLines(0);
  ModeInsert(tmLast);
end;

constructor TProviderMemo.Create(const AMemo: TCustomMemo; const AMaxLogLines: Integer = 0; const AModeInsert: TModeInsert = tmLast);
begin
  Create;

  Memo(AMemo);
  MaxLogLines(AMaxLogLines);
  ModeInsert(AModeInsert);
end;

function TProviderMemo.Memo(const AValue: TCustomMemo): TProviderMemo;
begin
  Result := Self;
  FMemo := AValue;
end;

function TProviderMemo.MaxLogLines(const AValue: Integer): TProviderMemo;
begin
  Result := Self;
  FMaxLogLines := AValue;
end;

function TProviderMemo.ModeInsert(const AValue: TModeInsert): TProviderMemo;
begin
  Result := Self;
  FModeInsert := AValue;
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
    raise EDataLoggerException.Create('Memo not defined!');

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
