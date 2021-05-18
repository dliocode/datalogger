{
  *************************************
  Created by Danilo Lucas
  Github - https://github.com/dliocode
  *************************************
}

unit DataLogger.Provider.TextFile;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  System.IOUtils, System.SysUtils;

type
  TProviderTextFile = class(TDataLoggerProvider)
  private
    FLogDir: string;
    FPrefixFileName: string;
    FExtension: string;
    FCleanOnStart: Boolean;
    FCleanIsRun: Boolean;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const ALogDir: string = ''; const APrefixFileName: string = ''; const AExtension: string = 'txt'; const ACleanOnStart: Boolean = False);
    destructor Destroy; override;
  end;

implementation

{ TProviderTextFile }

constructor TProviderTextFile.Create(const ALogDir: string = ''; const APrefixFileName: string = ''; const AExtension: string = 'txt'; const ACleanOnStart: Boolean = False);
begin
  inherited Create;

  FLogDir := ALogDir.Replace('/', TPath.DirectorySeparatorChar).Replace('\', TPath.DirectorySeparatorChar);
  FPrefixFileName := APrefixFileName;
  FExtension := AExtension;
  FCleanOnStart := ACleanOnStart;
  FCleanIsRun := False;
end;

destructor TProviderTextFile.Destroy;
begin
  inherited;
end;

procedure TProviderTextFile.Save(const ACache: TArray<TLoggerItem>);
var
  LRetryCount: Integer;
  LFileName: string;
  LTextFile: TextFile;
  LItem: TLoggerItem;
  LLog: string;
begin
  if Length(ACache) = 0 then
    Exit;

  if not FLogDir.Trim.IsEmpty then
    if not TDirectory.Exists(FLogDir) then
      if not CreateDir(FLogDir) then
        TDirectory.CreateDirectory(FLogDir);

  LFileName := Format('%s.%s', [TPath.Combine(FLogDir, FPrefixFileName) + FormatDateTime('yyyy-mm-dd', Date), FExtension]);

  AssignFile(LTextFile, LFileName);

  if TFile.Exists(LFileName) then
  begin
    if not FCleanOnStart and FCleanIsRun then
      Append(LTextFile)
    else
    begin
      TFile.Delete(LFileName);
      FCleanIsRun := True;

      Rewrite(LTextFile);
    end;
  end
  else
    Rewrite(LTextFile);

  try
    for LItem in ACache do
    begin
      if not ValidationBeforeSave(LItem) then
        Continue;

      if LItem.&Type = TLoggerType.All then
        LLog := ''
      else
        LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatSettings);

      LRetryCount := 0;

      repeat
        try
          if not TFile.Exists(LFileName) then
            Rewrite(LTextFile);

          Writeln(LTextFile, LLog);

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
      until False;
    end;
  finally
    CloseFile(LTextFile);
  end;
end;

end.
