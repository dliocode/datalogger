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
  System.IOUtils, System.SysUtils, System.Classes;

type
  TProviderTextFile = class(TDataLoggerProvider)
  private
    FLogDir: string;
    FPrefixFileName: string;
    FExtension: string;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
    FFormatDateTime: string;
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    constructor Create(const ALogDir: string = ''; const APrefixFileName: string = ''; const AExtension: string = 'txt'; const ACleanOnStart: Boolean = False; const AFormatDateTime: string = 'yyyy-mm-dd');
    destructor Destroy; override;
  end;

implementation

{ TProviderTextFile }

constructor TProviderTextFile.Create(const ALogDir: string = ''; const APrefixFileName: string = ''; const AExtension: string = 'txt'; const ACleanOnStart: Boolean = False; const AFormatDateTime: string = 'yyyy-mm-dd');
begin
  inherited Create;

  FLogDir := ALogDir.Replace('/', TPath.DirectorySeparatorChar).Replace('\', TPath.DirectorySeparatorChar);
  FPrefixFileName := APrefixFileName;
  FExtension := AExtension;
  FCleanOnStart := ACleanOnStart;
  FCleanOnRun := False;
  FFormatDateTime := AFormatDateTime;
end;

destructor TProviderTextFile.Destroy;
begin
  inherited;
end;

procedure TProviderTextFile.Save(const ACache: TArray<TLoggerItem>);
var
  LRetryCount: Integer;
  LFileName: string;
  LFileNameExt: string;
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

  if Trim(FLogDir).IsEmpty then
    LFileName := TPath.Combine(FLogDir, FPrefixFileName)
  else
    LFileName := TPath.Combine(IncludeTrailingPathDelimiter(FLogDir), FPrefixFileName);

  if not Trim(FFormatDateTime).IsEmpty then
    LFileName := LFileName + FormatDateTime(FFormatDateTime, Now);

  if not Trim(FExtension).IsEmpty then
    LFileNameExt := Format('%s.%s', [LFileName, FExtension])
  else
    LFileNameExt := LFileName;

  AssignFile(LTextFile, LFileNameExt);

  if TFile.Exists(LFileNameExt) then
  begin
    if FCleanOnStart and not FCleanOnRun then
    begin
      TFile.Delete(LFileNameExt);
      FCleanOnRun := True;

      Rewrite(LTextFile);
    end
    else
      Append(LTextFile);
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
        LLog := TLoggerLogFormat.AsString(GetLogFormat, LItem, GetFormatTimestamp);

      LRetryCount := 0;

      while True do
        try
          if not TFile.Exists(LFileNameExt) then
            Rewrite(LTextFile);

          Writeln(LTextFile, UTF8Encode(LLog));

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
  finally
    CloseFile(LTextFile);
  end;
end;

end.
