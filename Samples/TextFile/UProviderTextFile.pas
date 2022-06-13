unit UProviderTextFile;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Winapi.ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    pnlInfo: TPanel;
    Panel1: TPanel;
    btnMakeLog: TButton;
    btnShowExplorer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
    procedure btnShowExplorerClick(Sender: TObject);
    procedure btnMakeLogClick(Sender: TObject);
  private
    { Private declarations }
    FLogDir: string;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}


uses
  System.IOUtils,
  DataLogger, DataLogger.Provider.TextFile;

procedure TForm2.btnMakeLogClick(Sender: TObject);
begin
  Logger
    .Trace('My trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal');
end;

procedure TForm2.btnShowExplorerClick(Sender: TObject);
begin
  if not TDirectory.Exists(FLogDir) then
    btnMakeLog.Click;

  ShellExecute(0, 'open', PChar('explorer.exe'), PChar('/e,"' + FLogDir + '"'), nil, SW_SHOWNORMAL);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  FLogDir := TPath.GetDirectoryName(ParamStr(0));

  Logger.AddProvider(
    TProviderTextFile.Create
    .LogDir(FLogDir)
//    .PrefixFileName('my_log_')
//    .Extension('.txt')
//    .MaxFileSizeInKiloByte(10)
//    .MaxBackupFileCount(5)
//    .Compress(False) // Compress only with file name change or size change
//    .CompressCustom(nil)
//    .CleanOnStart(False)
//    .FormatDateTime('yyyy-mm-dd hh-nn') // New file per minute
//    .Encoding(TEncoding.UTF8)
    );

  // Log Format
  Logger.SetLogFormat(TLoggerFormat.LOG_TIMESTAMP + ' - ' + TLoggerFormat.LOG_MESSAGE);
end;

procedure TForm2.pnlInfoClick(Sender: TObject);
var
  LURL: string;
begin
  LURL := pnlInfo.Caption;
  LURL := LURL.Replace('GITHUB: ', '').Replace(' ', '');

  ShellExecute(0, 'open', PChar(LURL), nil, nil, SW_SHOWNORMAL);
end;

end.
