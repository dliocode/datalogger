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
{$SCOPEDENUMS ON}
{$IF DEFINED(MSWINDOWS)}
  TColor = (Black, DarkBlue, DarkGreen, DarkCyan, DarkRed, DarkMagenta, DarkYellow, Gray, DarkGray, Blue, Green, Cyan, Red, Magenta, Yellow, White);
{$ELSE}
  TColor = (Black, DarkRed, DarkGreen, DarkYellow, DarkBlue, DarkMagenta, DarkCyan, Gray, DarkGray, Red, Green, Yellow, Blue, Magenta, Cyan, White);
{$ENDIF}
{$SCOPEDENUMS OFF}

  TProviderConsole = class(TDataLoggerProvider)
  strict private
  type
    TColorConsole = record
      Background: TColor;
      Foreground: TColor;
    end;
  private
    FUseColorInConsole: Boolean;
    FColorTrace: TColorConsole;
    FColorDebug: TColorConsole;
    FColorInfo: TColorConsole;
    FColorSuccess: TColorConsole;
    FColorWarn: TColorConsole;
    FColorError: TColorConsole;
    FColorFatal: TColorConsole;
    FColorCustom: TColorConsole;
    procedure WriteColor(const AType: TLoggerType; const ALog: string);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function UseColorInConsole(const AValue: Boolean): TProviderConsole;
    function ChangeColor(const ALogType: TLoggerType; const AColorBackground: TColor; const AColorForeground: TColor): TProviderConsole;

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
  ChangeColor(TLoggerType.Trace, TColor.Black, TColor.Magenta);
  ChangeColor(TLoggerType.Debug, TColor.Black, TColor.Cyan);
  ChangeColor(TLoggerType.Info, TColor.Black, TColor.White);
  ChangeColor(TLoggerType.Success, TColor.Black, TColor.Green);
  ChangeColor(TLoggerType.Warn, TColor.Black, TColor.Yellow);
  ChangeColor(TLoggerType.Error, TColor.Black, TColor.Red);
  ChangeColor(TLoggerType.Fatal, TColor.Black, TColor.DarkRed);
  ChangeColor(TLoggerType.Custom, TColor.Black, TColor.White);
end;

function TProviderConsole.UseColorInConsole(const AValue: Boolean): TProviderConsole;
begin
  Result := Self;
  FUseColorInConsole := AValue;
end;

function TProviderConsole.ChangeColor(const ALogType: TLoggerType; const AColorBackground: TColor; const AColorForeground: TColor): TProviderConsole;
begin
  Result := Self;

  case ALogType of
    TLoggerType.Trace:
      begin
        FColorTrace.Background := AColorBackground;
        FColorTrace.Foreground := AColorForeground;
      end;

    TLoggerType.Debug:
      begin
        FColorDebug.Background := AColorBackground;
        FColorDebug.Foreground := AColorForeground;
      end;

    TLoggerType.Info:
      begin
        FColorInfo.Background := AColorBackground;
        FColorInfo.Foreground := AColorForeground;
      end;

    TLoggerType.Success:
      begin
        FColorSuccess.Background := AColorBackground;
        FColorSuccess.Foreground := AColorForeground;
      end;

    TLoggerType.Warn:
      begin
        FColorWarn.Background := AColorBackground;
        FColorWarn.Foreground := AColorForeground;
      end;

    TLoggerType.Error:
      begin
        FColorError.Background := AColorBackground;
        FColorError.Foreground := AColorForeground;
      end;

    TLoggerType.Fatal:
      begin
        FColorFatal.Background := AColorBackground;
        FColorFatal.Foreground := AColorForeground;
      end;

    TLoggerType.Custom:
      begin
        FColorCustom.Background := AColorBackground;
        FColorCustom.Foreground := AColorForeground;
      end;
  end;
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

    SetJSONInternal(LJO);
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
    LJO.AddPair('use_color_in_console', TJSONBool.Create(FUseColorInConsole));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
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
    if LItem.InternalItem.TypeSlineBreak then
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

          if LRetriesCount <= 0 then
            Break;

          if LRetriesCount >= FMaxRetries then
            Break;
        end;
      end;
  end;
end;

procedure TProviderConsole.WriteColor(const AType: TLoggerType; const ALog: string);
  function _Color(const AColor: TColorConsole): SmallInt;
  begin
    Result := SmallInt(Integer(AColor.Background) shl 4) or Integer(AColor.Foreground);
  end;

  function _ColorType: TColorConsole;
  begin
    case AType of
      TLoggerType.Trace:
        Result := FColorTrace;

      TLoggerType.Debug:
        Result := FColorDebug;

      TLoggerType.Info:
        Result := FColorInfo;

      TLoggerType.Success:
        Result := FColorSuccess;

      TLoggerType.Warn:
        Result := FColorWarn;

      TLoggerType.Error:
        Result := FColorError;

      TLoggerType.Fatal:
        Result := FColorFatal;

      TLoggerType.Custom:
        Result := FColorCustom;
    else
      Result := FColorInfo;
    end;
  end;

{$IF DEFINED(MSWINDOWS)}


var
  ConOut: THandle;
  BufInfo: TConsoleScreenBufferInfo;
begin
  ConOut := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(ConOut, BufInfo);

  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), _Color(_ColorType));
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
