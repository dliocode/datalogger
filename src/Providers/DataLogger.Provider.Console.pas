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
  System.SysUtils, System.JSON;

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
    function UseColorInConsole(const AValue: Boolean): TProviderConsole;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
  end;

implementation

{ TProviderConsole }

constructor TProviderConsole.Create;
begin
  inherited Create;

  UseColorInConsole(True);
end;

function TProviderConsole.UseColorInConsole(const AValue: Boolean): TProviderConsole;
begin
  Result := Self;
  FUseColorInConsole := AValue;
end;

procedure TProviderConsole.LoadFromJSON(const AJSON: string);
var
  LJO: TJSONObject;
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
    UseColorInConsole(LJO.GetValue<Boolean>('use_color_in_console', FUseColorInConsole));

    inherited SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderConsole.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('use_color_in_console', FUseColorInConsole);

    inherited ToJSONInternal(LJO);

    if AFormat then
      Result := LJO.Format
    else
      Result := LJO.ToString;
  finally
    LJO.Free;
  end;
end;

procedure TProviderConsole.Save(const ACache: TArray<TLoggerItem>);
var
  LRetriesCount: Integer;
  LItem: TLoggerItem;
  LLog: string;
begin
  if not IsConsole then
    Exit;

  if Length(ACache) = 0 then
    Exit;

  for LItem in ACache do
  begin
    if LItem.&Type = TLoggerType.All then
    begin
      Writeln;
      Continue;
    end;

    LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

    LRetriesCount := 0;

    while True do
      try
        if FUseColorInConsole then
          WriteColor(LItem.&Type, LLog)
        else
          Writeln(LLog);

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

          if LRetriesCount = -1 then
            Break;

          if LRetriesCount >= FMaxRetries then
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

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderConsole);

end.
