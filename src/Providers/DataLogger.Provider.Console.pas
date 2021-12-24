{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.Console;

interface

uses
  DataLogger.Provider, DataLogger.Types,
{$IF DEFINED(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils;

type
{$IF DEFINED(MSWINDOWS)}
  TConsoleColor = (Black, DarkBlue, DarkGreen, DarkCyan, DarkRed, DarkMagenta, DarkYellow, Gray, DarkGray, Blue, Green, Cyan, Red, Magenta, Yellow, White);
{$ELSE}
  TConsoleColor = (Black, DarkRed, DarkGreen, DarkYellow, DarkBlue, DarkMagenta, DarkCyan, Gray, DarkGray, Red, Green, Yellow, Blue, Magenta, Cyan, White);
{$ENDIF}

  TProviderConsole = class(TDataLoggerProvider)
  private
    FUseColorInConsole: Boolean;
    procedure WriteColor(const AType: TLoggerType; const ALog: string);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const AUseColorInConsole: Boolean = True);
    destructor Destroy; override;
  end;

implementation

{ TProviderConsole }

constructor TProviderConsole.Create(const AUseColorInConsole: Boolean = True);
begin
  inherited Create;
  FUseColorInConsole := AUseColorInConsole;
end;

destructor TProviderConsole.Destroy;
begin
  inherited;
end;

procedure TProviderConsole.Save(const ACache: TArray<TLoggerItem>);
var
  LRetryCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
  if not IsConsole then
    Exit;

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if not ValidationBeforeSave(LItem) then
      Continue;

    if LItem.&Type = TLoggerType.All then
    begin
      Writeln;
      Continue;
    end;

    LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

    LRetryCount := 0;

    while True do
      try
        if not FUseColorInConsole then
          Writeln(LLog)
        else
          WriteColor(LItem.&Type, LLog);

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

procedure TProviderConsole.WriteColor(const AType: TLoggerType; const ALog: string);
type
  TColor = record
    Background: TConsoleColor;
    Foreground: TConsoleColor;
  end;

  function _Color(const ABackground: TConsoleColor; const AForeground: TConsoleColor): SmallInt;
  begin
    Result := SmallInt(Integer(ABackground) shl 4) or Integer(AForeground);
  end;

  function _ColorType: TColor;
  begin
    case AType of
      TLoggerType.Trace:
        begin
          Result.Background := TConsoleColor.Black;
          Result.Foreground := TConsoleColor.Magenta;
        end;

      TLoggerType.Debug:
        begin
          Result.Background := TConsoleColor.Black;
          Result.Foreground := TConsoleColor.Cyan;
        end;

      TLoggerType.Info:
        begin
          Result.Background := TConsoleColor.Black;
          Result.Foreground := TConsoleColor.White;
        end;

      TLoggerType.Warn:
        begin
          Result.Background := TConsoleColor.Black;
          Result.Foreground := TConsoleColor.Yellow;
        end;

      TLoggerType.Error:
        begin
          Result.Background := TConsoleColor.Black;
          Result.Foreground := TConsoleColor.Red;
        end;

      TLoggerType.Success:
        begin
          Result.Background := TConsoleColor.Black;
          Result.Foreground := TConsoleColor.Green;
        end;

      TLoggerType.Fatal:
        begin
          Result.Background := TConsoleColor.Black;
          Result.Foreground := TConsoleColor.DarkRed;
        end;
    end;
  end;

{$IF DEFINED(MSWINDOWS)}
var
  ConOut: THandle;
  BufInfo: TConsoleScreenBufferInfo;
begin
  ConOut := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(ConOut, BufInfo);

  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), _Color(_ColorType.Background, _ColorType.Foreground));
  Writeln(ALog);
  SetConsoleTextAttribute(ConOut, BufInfo.wAttributes);
end;
{$ELSEIF DEFINED(LINUX)}
var
  LColor: Integer;
begin
  LColor := Integer(_ColorType.Foreground) + 30;

  if LColor > 37 then
    LColor := LColor - 8;

  Write(#27'[1;' + LColor.ToString + 'm');
  Writeln(ALog);
  Write(#27'[0m');
end;
{$ELSE}
begin
  Writeln(ALog);
end;
{$ENDIF}

end.
