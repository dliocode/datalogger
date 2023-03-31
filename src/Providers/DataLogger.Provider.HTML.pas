{
  ********************************************************************************

  Github - https://github.com/dliocode/datalogger

  ********************************************************************************

  MIT License

  Copyright (c) 2023 Danilo Lucas

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

  ********************************************************************************
}

// HYPERTEXT MARKUP LANGUAGE

unit DataLogger.Provider.HTML;

interface

uses
  DataLogger.Provider, DataLogger.Types,
  System.IOUtils, System.SysUtils, System.StrUtils, System.Classes, System.Zip, System.JSON;

type
  TProviderHTMLExecuteCompress = reference to procedure(const ADirLogFileName: string; const AFileName: string; var ARemoveFile: Boolean);

  TProviderHTML = class(TDataLoggerProvider<TProviderHTML>)
  private
    FLogDir: string;
    FPrefixFileName: string;
    FExtension: string;
    FMaxFileSizeInKiloByte: Int64;
    FMaxBackupFileCount: Int64;
    FCompress: Boolean;
    FCompressCustom: TProviderHTMLExecuteCompress;
    FCleanOnStart: Boolean;
    FCleanOnRun: Boolean;
    FFormatDateTime: string;
    FEncoding: TEncoding;
    FIncludeSearch: Boolean;
    FPageTitle: string;

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
    procedure WriteHeader;
    procedure WriteFooter;
    procedure UndoLast(const ANumLines: Integer);
  protected
    procedure Save(const ACache: TArray<TLoggerItem>); override;
  public
    function LogDir(const AValue: string): TProviderHTML;
    function PrefixFileName(const AValue: string): TProviderHTML;
    function Extension(const AValue: string): TProviderHTML;
    function MaxFileSizeInKiloByte(const AValue: Int64): TProviderHTML;
    function MaxBackupFileCount(const AValue: Int64): TProviderHTML;
    function Compress(const AValue: Boolean): TProviderHTML;
    function CompressCustom(const AValue: TProviderHTMLExecuteCompress): TProviderHTML;
    function CleanOnStart(const AValue: Boolean): TProviderHTML;
    function FormatDateTime(const AValue: string): TProviderHTML;
    function Encoding(const AValue: TEncoding): TProviderHTML;
    function IncludeSearch(const AValue: Boolean): TProviderHTML;
    function PageTitle(const AValue: string): TProviderHTML;

    procedure LoadFromJSON(const AJSON: string); override;
    function ToJSON(const AFormat: Boolean = False): string; override;

    constructor Create;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{ TProviderHTML }

constructor TProviderHTML.Create;
begin
  inherited Create;

  LogDir('.');
  PrefixFileName('');
  Extension('.html');
  MaxFileSizeInKiloByte(0);
  MaxBackupFileCount(0);
  Compress(False);
  CompressCustom(nil);
  CleanOnStart(False);
  FCleanOnRun := False;
  FormatDateTime('yyyy-mm-dd');
  Encoding(TEncoding.UTF8);
  IncludeSearch(True);
  PageTitle('DataLogger');

  FWriter := nil;
  FRotateInternal := 0;
end;

procedure TProviderHTML.AfterConstruction;
begin
  inherited;

  SetIgnoreTemplate(False);
end;

destructor TProviderHTML.Destroy;
var
  LFileName: string;
begin
  LFileName := GetLogFileName(0);

  if TFile.Exists(LFileName) then
  begin
    CreateWriter;
    WriteFooter;
  end;

  FreeWriter;

  inherited;
end;

function TProviderHTML.LogDir(const AValue: string): TProviderHTML;
begin
  Result := Self;
  FLogDir := AValue.Replace('/', TPath.DirectorySeparatorChar).Replace('\', TPath.DirectorySeparatorChar);
end;

function TProviderHTML.PrefixFileName(const AValue: string): TProviderHTML;
begin
  Result := Self;
  FPrefixFileName := AValue;
end;

function TProviderHTML.Extension(const AValue: string): TProviderHTML;
begin
  Result := Self;

  FExtension := AValue;
  if not AValue.StartsWith('.') then
    FExtension := '.' + AValue;
end;

function TProviderHTML.MaxFileSizeInKiloByte(const AValue: Int64): TProviderHTML;
begin
  Result := Self;
  FMaxFileSizeInKiloByte := AValue;
end;

function TProviderHTML.MaxBackupFileCount(const AValue: Int64): TProviderHTML;
begin
  Result := Self;
  FMaxBackupFileCount := AValue;
end;

function TProviderHTML.Compress(const AValue: Boolean): TProviderHTML;
begin
  Result := Self;
  FCompress := AValue;
end;

function TProviderHTML.CompressCustom(const AValue: TProviderHTMLExecuteCompress): TProviderHTML;
begin
  Result := Self;
  FCompressCustom := AValue;
end;

function TProviderHTML.CleanOnStart(const AValue: Boolean): TProviderHTML;
begin
  Result := Self;
  FCleanOnStart := AValue;
end;

function TProviderHTML.FormatDateTime(const AValue: string): TProviderHTML;
begin
  Result := Self;
  FFormatDateTime := AValue;
end;

function TProviderHTML.Encoding(const AValue: TEncoding): TProviderHTML;
begin
  Result := Self;
  FEncoding := AValue
end;

function TProviderHTML.IncludeSearch(const AValue: Boolean): TProviderHTML;
begin
  Result := Self;
  FIncludeSearch := AValue;
end;

function TProviderHTML.PageTitle(const AValue: string): TProviderHTML;
begin
  Result := Self;
  FPageTitle := AValue;
end;

procedure TProviderHTML.LoadFromJSON(const AJSON: string);
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

function TProviderHTML.ToJSON(const AFormat: Boolean): string;
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('log_dir', TJSONString.Create(FLogDir));
    LJO.AddPair('prefix_filename', TJSONString.Create(FPrefixFileName));
    LJO.AddPair('extension', TJSONString.Create(FExtension));
    LJO.AddPair('max_file_size_in_kilo_byte', TJSONNumber.Create(FMaxFileSizeInKiloByte));
    LJO.AddPair('max_backup_file_count', TJSONNumber.Create(FMaxBackupFileCount));
    LJO.AddPair('compress', TJSONBool(FCompress));
    LJO.AddPair('clean_on_start', TJSONBool.Create(FCleanOnStart));
    LJO.AddPair('format_datetime', TJSONString.Create(FFormatDateTime));

    ToJSONInternal(LJO);

    Result := TLoggerJSON.Format(LJO, AFormat);
  finally
    LJO.Free;
  end;
end;

procedure TProviderHTML.Save(const ACache: TArray<TLoggerItem>);
var
  LLog: string;
  LLogs: TArray<string>;
  I: Integer;
  LItem: TLoggerItem;
  LFileName: string;
  LRetriesCount: Integer;
  LFileExist: Boolean;
begin
  if (Length(ACache) = 0) then
    Exit;

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

          if (LRetriesCount <= 0) then
            Break;

          if (LRetriesCount >= FMaxRetries) then
            raise;
        end;

      FCleanOnRun := True;
    end;

  LFileExist := TFile.Exists(LFileName);

  CreateWriter;
  try
    if not LFileExist then
      WriteHeader;

    for LItem in ACache do
    begin
      if LItem.InternalItem.IsSlinebreak then
        Continue;

      if LItem.InternalItem.IsUndoLast then
      begin
        LLogs := SerializeItem.LogItem(LItem).ToValues;
        UndoLast(Length(LLogs) + 2);

        Continue;
      end;

      LLogs := SerializeItem.LogItem(LItem).ToValues;

      LLog := '        <tr>' + sLineBreak;
      for I := Low(LLogs) to High(LLogs) do
        LLog := LLog + Format('          <td>%s</td>' + sLineBreak, [LLogs[I]]);
      LLog := LLog + '        </tr>' + sLineBreak;

      InternalWriteLog(LLog);

      if (FMaxFileSizeInKiloByte > 0) then
        if (FWriter.BaseStream.Size > (FMaxFileSizeInKiloByte * 1024)) then
        begin
          RotateLog;

          LLog := '        <tr>' + sLineBreak;
          for I := Low(LLogs) to High(LLogs) do
            LLog := LLog + Format('          <td>%s</td>' + sLineBreak, [LLogs[I]]);
          LLog := LLog + '        </tr>' + sLineBreak;

          InternalWriteLog(LLog);
        end;
    end;
  finally
    FreeWriter;
  end;
end;

function TProviderHTML.GetLogFileName(const AFileNumber: Int64): string;
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

procedure TProviderHTML.CreateWriter;
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

        if (LRetriesCount >= FMaxRetries) then
          raise;
      end;
    end;
end;

procedure TProviderHTML.FreeWriter;
begin
  if not Assigned(FWriter) then
    Exit;

  FWriter.Free;
  FWriter := nil;
end;

procedure TProviderHTML.InternalWriteLog(const AValue: string);
begin
  FWriter.WriteLine(AValue);
  FWriter.Flush;
end;

procedure TProviderHTML.MoveFile(const ASourceFileName: string; const ADestFileName: string);
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

        if (LRetriesCount >= FMaxRetries) then
          raise EDataLoggerException.CreateFmt('Cannot rename %s to %s', [ASourceFileName, ADestFileName]);
      end;
    end;
end;

procedure TProviderHTML.RotateLog;
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

  WriteFooter;
  InternalWriteLog('');
  InternalWriteLog('<!-- [ROTATE ' + LRotateDateTime + '] -->');
  FreeWriter;

  if (FMaxBackupFileCount = 0) then
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
  InternalWriteLog('<!-- [START ROTATE ' + LRotateDateTime + '] -->');
  InternalWriteLog('');
  WriteHeader;
end;

procedure TProviderHTML.ZipFile(const ADirFileName: string; const AFileName: string);
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
    end)
    .Start;
end;

procedure TProviderHTML.CreateZipFile(const ADirFileName: string; const AFileName: string);
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

procedure TProviderHTML.WriteHeader;
var
  LLog: string;
  LItem: TLoggerItem;
  LLogs: TArray<string>;
  I: Integer;
  LIndexBase: Integer;
  LName: string;
  LStyle: string;
begin
  LLog :=
    '<!DOCTYPE html>' + sLineBreak +
    '<html>' + sLineBreak +
    '<head>' + sLineBreak +
    '  <meta charset="utf-8">' + sLineBreak +
    '  <meta name="viewport" content="width=device-width">' + sLineBreak;

  if FPageTitle.Trim.IsEmpty then
    LLog := LLog + '  <title>DataLogger</title>' + sLineBreak
  else
    LLog := LLog + Format('  <title>%s</title>' + sLineBreak, [FPageTitle]);

  LLog := LLog +
    '  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-rbsA2VBKQhggwzxH7pPCaAqO46MgnOM80zW1RWuH61DGLwZJEdK2Kadq2F9CUG65" crossorigin="anonymous">' + sLineBreak;

  if FIncludeSearch then
    LLog := LLog +
      '  ' + sLineBreak +
      '<script>' + sLineBreak +
      '   function search(){' + sLineBreak +
      '    const searchInput = document.getElementById(''search-input'');' + sLineBreak +
      '    const dataTable = document.getElementById(''data-table'');    ' + sLineBreak +
      '     ' + sLineBreak +
      '    const searchTerm = searchInput.value.trim().toLowerCase();' + sLineBreak +
      '     ' + sLineBreak +
      '    const rows = dataTable.querySelectorAll(''tbody tr'');' + sLineBreak +
      '    for (const row of rows) {     ' + sLineBreak +
      '      const cells = row.querySelectorAll(''td'');' + sLineBreak +
      '      let rowMatch = false;' + sLineBreak +
      '      for (const cell of cells) {' + sLineBreak +
      '        const cellText = (cell.textContent || cell.innerText).trim().toLowerCase();' + sLineBreak +
      '        if (cellText.includes(searchTerm)) {' + sLineBreak +
      '          rowMatch = true;' + sLineBreak +
      '          break;' + sLineBreak +
      '        }' + sLineBreak +
      '      }' + sLineBreak +
      '      if (rowMatch) {' + sLineBreak +
      '        row.style.display = '''';' + sLineBreak +
      '      } else {' + sLineBreak +
      '        row.style.display = ''none'';' + sLineBreak +
      '      }' + sLineBreak +
      '    }     ' + sLineBreak +
      '   }' + sLineBreak +
      '</script>    ' + sLineBreak;

  LLog := LLog +
    '</head>' + sLineBreak +
    '  ' + sLineBreak +
    '<body style="padding: 10px; background-color: #252627">' + sLineBreak +
    '    ' + sLineBreak;

  if FPageTitle.Trim.IsEmpty then
    LLog := LLog + '  <h3 class="text-center my-3 text-white">DataLogger</h3>' + sLineBreak
  else
    LLog := LLog + Format('  <h3 class="text-center my-3 text-white">%s</h3>' + sLineBreak, [FPageTitle]);

  if FIncludeSearch then
    LLog := LLog +
      '  ' + sLineBreak +
      '  <div class="input-group mb-3">' + sLineBreak +
      '    <input id="search-input" type="text" class="form-control" placeholder="Pesquisar...">' + sLineBreak +
      '    <button type="button" class="btn btn-primary" onClick="search()">Pesquisar</button>' + sLineBreak +
      '  </div>' + sLineBreak +
      '  ' + sLineBreak +
      '  <div class="bg-light table-responsive" style="max-height: 83vh">' + sLineBreak
  else
    LLog := LLog +
      '  ' + sLineBreak +
      '  <div class="bg-light table-responsive" style="max-height: 90vh">' + sLineBreak;

  LLog := LLog +
    '    <table id="data-table" class="table table-striped table-bordered">' + sLineBreak +
    '      <thead class="table-dark align-middle text-center">' + sLineBreak +
    '        <tr>' + sLineBreak;

  LLogs := SerializeItem.LogItem(LItem).ToKeys;

  for I := Low(LLogs) to High(LLogs) do
  begin
    LIndexBase := IndexStr(LLogs[I], TLoggerConst.BASE_FORMAT);

    case LIndexBase of
      0:
        LStyle := 'min-width: 192px; width: 192px;'; // timestamp
      1:
        LStyle := 'min-width: 242px; width: 242px;'; // timestamp_iso8601
      2:
        LStyle := 'min-width: 139px; width: 139px;'; // timestamp_unix
      3:
        LStyle := 'min-width: 130px; width: 130px;'; // name
      4:
        LStyle := 'min-width: 100px; width: 100px;'; // sequence
      5:
        LStyle := 'min-width: 100px; width: 100px;'; // thread_id
      6:
        LStyle := 'min-width: 150px; width: 150px;'; // level
      7:
        LStyle := 'min-width: 100px; width: 100px;'; // level_value
      8:
        LStyle := 'min-width: 130px; width: 130px;'; // tag
      9:
        LStyle := 'min-width: 600px;'; // message
      10:
        LStyle := 'min-width: 160px; width: 160px;'; // app_name
      11:
        LStyle := 'min-width: 110px; width: 110px;'; // app_version
      12:
        LStyle := 'min-width: 550px; width: 550px;'; // app_path
      13:
        LStyle := 'min-width: 110px; width: 110px;'; // app_size
      14:
        LStyle := 'min-width: 180px; width: 180px;'; // computer_name
      15:
        LStyle := 'min-width: 100px; width: 100px;'; // username
      16:
        LStyle := 'min-width: 500px; width: 500px;'; // os_version
      17:
        LStyle := 'min-width: 110px; width: 110px;'; // process_id
      18:
        LStyle := 'min-width: 110px; width: 110px;'; // ip_local
      19:
        LStyle := 'min-width: 150px; width: 150px;'; // mac_address
    end;

    LName := TLoggerConst.BASE_FORMAT_NAME[LIndexBase];

    LLog := LLog + Format('          <th style="position: sticky; top: 0; z-index: 1; %s">%s</th>' + sLineBreak, [LStyle, LName]);
  end;

  LLog := LLog +
    '        </tr>' + sLineBreak +
    '      <tbody>' + sLineBreak;

  InternalWriteLog(LLog);
end;

procedure TProviderHTML.WriteFooter;
var
  LLog: string;
begin
  LLog :=
    '      </tbody>   ' + sLineBreak +
    '    </table>  ' + sLineBreak +
    '  </div>  ' + sLineBreak +
    '</body>' + sLineBreak +
    '</html>';

  InternalWriteLog(LLog);
end;

procedure TProviderHTML.UndoLast(const ANumLines: Integer);
var
  LLogFileName: string;
  LRetriesCount: Integer;
  LFileStreamReader: TFileStream;
  LStreamReader: TStreamReader;
  LFileStreamWriter: TFileStream;
  LStreamWriter: TStreamWriter;
  LNumLines: Integer;
  LCountLines: Integer;
  LBOM: TBytes;
  LReadLine: string;
begin
  LLogFileName := GetLogFileName(0);

  if not TFile.Exists(LLogFileName) then
    Exit;

  LRetriesCount := 0;

  FreeWriter;

  while True do
    try
      LFileStreamReader := TFileStream.Create(LLogFileName, fmOpenRead or fmShareDenyNone);
      LFileStreamWriter := TFileStream.Create(LLogFileName + '.undo', fmCreate or fmShareDenyNone);

      LStreamReader := TStreamReader.Create(LFileStreamReader, FEncoding, True, 4096);
      LStreamWriter := TStreamWriter.Create(LFileStreamWriter, FEncoding, 128);
      try
        LStreamReader.OwnStream;
        LStreamWriter.OwnStream;
        LStreamWriter.AutoFlush := True;

        LNumLines := 0;
        while not LStreamReader.EndOfStream do
        begin
          LStreamReader.ReadLine;
          Inc(LNumLines)
        end;

        LNumLines := LNumLines - ANumLines;

        LCountLines := 0;
        LStreamReader.DiscardBufferedData;
        LStreamReader.BaseStream.Seek(0, soBeginning);

        SetLength(LBOM, 3);
        LFileStreamReader.ReadBuffer(LBOM, 3);
        if (LBOM[0] = $EF) and (LBOM[1] = $BB) and (LBOM[2] = $BF) then
          LStreamReader.BaseStream.Seek(3, soBeginning)
        else
          LStreamReader.BaseStream.Seek(0, soBeginning);

        while not LStreamReader.EndOfStream do
        begin
          Inc(LCountLines);

          if (LCountLines = LNumLines) then
            Break;

          LReadLine := LStreamReader.ReadLine;

          if not LStreamReader.EndOfStream then
            LStreamWriter.WriteLine(LReadLine);
        end;
      finally
        LStreamWriter.Free;
        LStreamReader.Free;
      end;

      TFile.Delete(LLogFileName);
      MoveFile(LLogFileName + '.undo', LLogFileName);

      CreateWriter;

      Break;
    except
      on E: Exception do
      begin
        Inc(LRetriesCount);

        Sleep(50);

        if (LRetriesCount >= FMaxRetries) then
          raise;
      end;
    end;
end;

procedure ForceReferenceToClass(C: TClass);
begin
end;

initialization

ForceReferenceToClass(TProviderHTML);

end.
