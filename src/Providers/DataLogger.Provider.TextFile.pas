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
  System.IOUtils, System.SysUtils, System.Classes, System.Zip, System.JSON;

type
  TProviderTextFileExecuteCompress = reference to procedure(const ADirLogFileName: string; const AFileName: string; var ARemoveFile: Boolean);

  TProviderTextFile = class(TDataLoggerProvider)
  private
    FLogDir: string;
    FPrefixFileName: string;
    FExtension: string;
    FMaxFileSizeInKiloByte: Int64;
    FMaxBackupFileCount: Int64;
    FCompress: Boolean;
    FCompressCustom: TProviderTextFileExecuteCompress;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
    FFormatDateTime: string;
    FEncoding: TEncoding;

    FWriter: TStreamWriter;
    FRotateInternal: Int64;
    FOldFileName: string;

    function GetLogFileName(const AFileNumber: Int64): string;
    procedure CreateWriter;
    procedure FreeWriter;
    procedure InternalWriteLog(const AValue: string);
    procedure MoveFile(const ASourceFileName, ADestFileName: string);
    procedure RotateLog;
    procedure CreateZipFile(const ADirFileName: string; const AFileName: string);
    procedure ZipFile(const ADirFileName, AFileName: string);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function LogDir(const AValue: string): TProviderTextFile;
    function PrefixFileName(const AValue: string): TProviderTextFile;
    function Extension(const AValue: string): TProviderTextFile;
    function MaxFileSizeInKiloByte(const AValue: Int64): TProviderTextFile;
    function MaxBackupFileCount(const AValue: Int64): TProviderTextFile;
    function Compress(const AValue: Boolean): TProviderTextFile;
    function CompressCustom(const AValue: TProviderTextFileExecuteCompress): TProviderTextFile;
    function CleanOnStart(const AValue: Boolean): TProviderTextFile;
    function FormatDateTime(const AValue: string): TProviderTextFile;
    function Encoding(const AValue: TEncoding): TProviderTextFile;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create; overload;
    constructor Create(
      const ALogDir: string; const APrefixFileName: string = ''; const AExtension: string = '.txt';
      const ACleanOnStart: Boolean = False; const AFormatDateTime: string = 'yyyy-mm-dd'); overload; deprecated 'Use TProviderTextFile.Create.LogDir(''.'').PrefixFileName(''.'').Extension(''log'') - This function will be removed in future versions';

    destructor Destroy; override;
  end;

implementation

{ TProviderTextFile }

constructor TProviderTextFile.Create;
begin
  inherited Create;

  LogDir('.');
  PrefixFileName('');
  Extension('.txt');
  MaxFileSizeInKiloByte(0);
  MaxBackupFileCount(0);
  Compress(False);
  CompressCustom(nil);
  CleanOnStart(False);
  FCleanOnRun := False;
  FormatDateTime('yyyy-mm-dd');
  Encoding(TEncoding.UTF8);

  FWriter := nil;
  FRotateInternal := 0;
end;

constructor TProviderTextFile.Create(
  const ALogDir: string; const APrefixFileName: string = ''; const AExtension: string = '.txt';
  const ACleanOnStart: Boolean = False; const AFormatDateTime: string = 'yyyy-mm-dd');
begin
  Create;

  LogDir(ALogDir);
  PrefixFileName(APrefixFileName);
  Extension(AExtension);
  CleanOnStart(ACleanOnStart);
  FormatDateTime(AFormatDateTime);
end;

destructor TProviderTextFile.Destroy;
begin
  FreeWriter;

  inherited;
end;

function TProviderTextFile.LogDir(const AValue: string): TProviderTextFile;
begin
  Result := Self;
  FLogDir := AValue.Replace('/', TPath.DirectorySeparatorChar).Replace('\', TPath.DirectorySeparatorChar);
end;

function TProviderTextFile.PrefixFileName(const AValue: string): TProviderTextFile;
begin
  Result := Self;
  FPrefixFileName := AValue;
end;

function TProviderTextFile.Extension(const AValue: string): TProviderTextFile;
begin
  Result := Self;

  FExtension := AValue;
  if not AValue.StartsWith('.') then
    FExtension := '.' + AValue;
end;

function TProviderTextFile.MaxFileSizeInKiloByte(const AValue: Int64): TProviderTextFile;
begin
  Result := Self;
  FMaxFileSizeInKiloByte := AValue;
end;

function TProviderTextFile.MaxBackupFileCount(const AValue: Int64): TProviderTextFile;
begin
  Result := Self;
  FMaxBackupFileCount := AValue;
end;

function TProviderTextFile.Compress(const AValue: Boolean): TProviderTextFile;
begin
  Result := Self;
  FCompress := AValue;
end;

function TProviderTextFile.CompressCustom(const AValue: TProviderTextFileExecuteCompress): TProviderTextFile;
begin
  Result := Self;
  FCompressCustom := AValue;
end;

function TProviderTextFile.CleanOnStart(const AValue: Boolean): TProviderTextFile;
begin
  Result := Self;
  FCleanOnRun := AValue;
end;

function TProviderTextFile.FormatDateTime(const AValue: string): TProviderTextFile;
begin
  Result := Self;
  FFormatDateTime := AValue;
end;

function TProviderTextFile.Encoding(const AValue: TEncoding): TProviderTextFile;
begin
  Result := Self;
  FEncoding := AValue
end;

procedure TProviderTextFile.LoadFromJSON(const AJSON: string);
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
    LogDir(LJO.GetValue<string>('log_dir', FLogDir));
    PrefixFileName(LJO.GetValue<string>('prefix_filename', FPrefixFileName));
    Extension(LJO.GetValue<string>('extension', FExtension));
    MaxFileSizeInKiloByte(LJO.GetValue<Int64>('max_file_size_in_kilo_byte', FMaxFileSizeInKiloByte));
    MaxBackupFileCount(LJO.GetValue<Int64>('max_backup_file_count', FMaxBackupFileCount));
    Compress(LJO.GetValue<Boolean>('compress', FCompress));
    CleanOnStart(LJO.GetValue<Boolean>('clean_on_start', FCleanOnStart));
    FormatDateTime(LJO.GetValue<string>('format_datetime', FFormatDateTime));

    SetJSONInternal(LJO);
  finally
    LJO.Free;
  end;
end;

function TProviderTextFile.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('log_dir', FLogDir);
    LJO.AddPair('prefix_filename', FPrefixFileName);
    LJO.AddPair('extension', FExtension);
    LJO.AddPair('max_file_size_in_kilo_byte', TJSONNumber.Create(FMaxFileSizeInKiloByte));
    LJO.AddPair('max_backup_file_count', TJSONNumber.Create(FMaxBackupFileCount));
    LJO.AddPair('compress', TJSONBool(FCompress));
    LJO.AddPair('clean_on_start', TJSONBool.Create(FCleanOnStart));
    LJO.AddPair('format_datetime', FFormatDateTime);

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderTextFile.Save(const ACache: TArray<TLoggerItem>);
var
  LItem: TLoggerItem;
  LFileName: string;
  LRetriesCount: Integer;
  LLog: string;
begin
  LFileName := GetLogFileName(0);
  if FOldFileName.Trim.IsEmpty then
    FOldFileName := LFileName;

  if not LFileName.Equals(FOldFileName) then
  begin
    if FCompress then
      ZipFile(FOldFileName, '');

    FOldFileName := LFileName;
  end;

  if not FCleanOnRun then
    if FCleanOnStart then
    begin
      LRetriesCount := 0;

      while True do
        try
          if TFile.Exists(LFileName) then
            TFile.Delete(LFileName);

          Break;
        except
          Inc(LRetriesCount);

          Sleep(50);

          if LRetriesCount <= 0 then
            Break;

          if LRetriesCount >= FMaxRetries then
            raise;
        end;

      FCleanOnRun := True;
    end;

  CreateWriter;
  try
    for LItem in ACache do
    begin
      if LItem.InternalItem.TypeSlineBreak then
        LLog := ''
      else
        LLog := TLoggerLogFormat.AsString(FLogFormat, LItem, FFormatTimestamp);

      InternalWriteLog(LLog);

      if FMaxFileSizeInKiloByte > 0 then
        if FWriter.BaseStream.Size > FMaxFileSizeInKiloByte * 1024 then
          RotateLog;
    end;
  finally
    FreeWriter;
  end;
end;

function TProviderTextFile.GetLogFileName(const AFileNumber: Int64): string;
var
  LFileName: string;
begin
  if not FLogDir.Trim.IsEmpty then
    if not TDirectory.Exists(FLogDir) then
      if not CreateDir(FLogDir) then
        TDirectory.CreateDirectory(FLogDir);

  if Trim(FLogDir).IsEmpty then
    LFileName := TPath.Combine(IncludeTrailingPathDelimiter('.'), FPrefixFileName)
  else
    LFileName := TPath.Combine(IncludeTrailingPathDelimiter(FLogDir), FPrefixFileName);

  if not Trim(FFormatDateTime).IsEmpty then
    LFileName := LFileName + System.SysUtils.FormatDateTime(FFormatDateTime, Now);

  if AFileNumber > 0 then
    LFileName := LFileName + '_' + IntToStr(AFileNumber);

  if not Trim(FExtension).IsEmpty then
    Result := Format('%s%s', [LFileName, FExtension])
  else
    Result := LFileName;
end;

procedure TProviderTextFile.CreateWriter;
var
  LLogFileName: string;
  LFileStream: TFileStream;
  LFileAccessMode: Word;
  LRetriesCount: Integer;
begin
  LLogFileName := GetLogFileName(0);

  LFileAccessMode := fmOpenWrite or fmShareDenyNone;
  if not TFile.Exists(LLogFileName) then
    LFileAccessMode := LFileAccessMode or fmCreate;

  LRetriesCount := 0;

  while True do
    try
      LFileStream := TFileStream.Create(LLogFileName, LFileAccessMode);
      try
        LFileStream.Seek(0, TSeekOrigin.soEnd);

        FWriter := TStreamWriter.Create(LFileStream, FEncoding, 128);
        FWriter.AutoFlush := True;
        FWriter.OwnStream;

        Break;
      except
        LFileStream.Free;
        raise;
      end;
    except
      on E: Exception do
      begin
        Inc(LRetriesCount);

        Sleep(50);

        if LRetriesCount >= FMaxRetries then
          raise;
      end;
    end;
end;

procedure TProviderTextFile.FreeWriter;
begin
  if not Assigned(FWriter) then
    Exit;

  FWriter.Free;
  FWriter := nil;
end;

procedure TProviderTextFile.InternalWriteLog(const AValue: string);
begin
  FWriter.WriteLine(AValue);
  FWriter.Flush;
end;

procedure TProviderTextFile.MoveFile(const ASourceFileName: string; const ADestFileName: string);
var
  LRetriesCount: Integer;
begin
  LRetriesCount := 0;

  while True do
    try
      TFile.Move(ASourceFileName, ADestFileName);
      Break;
    except
      on Exception do
      begin
        Inc(LRetriesCount);

        Sleep(50);

        if LRetriesCount >= FMaxRetries then
          raise EDataLoggerException.CreateFmt('Cannot rename %s to %s', [ASourceFileName, ADestFileName]);
      end;
    end;
end;

procedure TProviderTextFile.RotateLog;
var
  LNow: TDateTime;
  LRotateDateTime: string;
  LZipFileName: string;

  LRotateInternal: Int64;
  LRenamedFileName: string;
  I: Integer;
  LCurrentFileName: string;
  LFileName: string;
begin
  LNow := Now;
  LRotateDateTime := System.SysUtils.FormatDateTime('c.zzz', LNow);

  InternalWriteLog('');
  InternalWriteLog('[ROTATE ' + LRotateDateTime + ']');
  FreeWriter;

  if FMaxBackupFileCount = 0 then
  begin
    Inc(FRotateInternal);
    LRotateInternal := FRotateInternal;
  end
  else
  begin
    FRotateInternal := 0;
    LRotateInternal := FMaxBackupFileCount;
  end;

  LRenamedFileName := GetLogFileName(LRotateInternal);
  if FCompress then
    LRenamedFileName := ChangeFileExt(LRenamedFileName, '.zip');

  if TFile.Exists(LRenamedFileName) then
    TFile.Delete(LRenamedFileName);

  for I := Pred(LRotateInternal) downto 1 do
  begin
    LCurrentFileName := GetLogFileName(I);
    if FCompress then
      LCurrentFileName := ChangeFileExt(LCurrentFileName, '.zip');

    LRenamedFileName := GetLogFileName(I + 1);
    if FCompress then
      LRenamedFileName := ChangeFileExt(LRenamedFileName, '.zip');

    if TFile.Exists(LCurrentFileName) then
      MoveFile(LCurrentFileName, LRenamedFileName);
  end;

  LFileName := GetLogFileName(0);
  LRenamedFileName := GetLogFileName(1);
  MoveFile(LFileName, LRenamedFileName);

  if FCompress then
  begin
    LZipFileName := System.SysUtils.FormatDateTime('yyyymmdd_hhnnsszzz', LNow);
    ZipFile(LRenamedFileName, Format('%s_RT%s%s', [TPath.GetFileNameWithoutExtension(LFileName), LZipFileName, FExtension]));
  end;

  CreateWriter;
  InternalWriteLog('[START ROTATE ' + LRotateDateTime + ']');
  InternalWriteLog('');
end;

procedure TProviderTextFile.ZipFile(const ADirFileName: string; const AFileName: string);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      LRemoveLogFileName: Boolean;
    begin
      LRemoveLogFileName := True;

      if Assigned(FCompressCustom) then
        FCompressCustom(ADirFileName, AFileName, LRemoveLogFileName)
      else
        CreateZipFile(ADirFileName, AFileName);

      if LRemoveLogFileName then
        if TFile.Exists(ADirFileName) then
          TFile.Delete(ADirFileName);
    end).Start;
end;

procedure TProviderTextFile.CreateZipFile(const ADirFileName: string; const AFileName: string);
var
  LZipFileName: string;
  LZipFile: TZipFile;
begin
  LZipFileName := ChangeFileExt(ADirFileName, '.zip');

  if TFile.Exists(LZipFileName) then
    TFile.Delete(LZipFileName);

  LZipFile := TZipFile.Create;
  try
    LZipFile.Open(LZipFileName, zmWrite);

    if TFile.Exists(ADirFileName) then
      LZipFile.Add(ADirFileName, AFileName);
  finally
    LZipFile.Close;
    LZipFile.Free;
  end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderTextFile);

end.
