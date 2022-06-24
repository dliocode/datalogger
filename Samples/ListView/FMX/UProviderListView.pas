unit UProviderListView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Posix.Stdlib,
{$ENDIF POSIX}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnMakeLog: TButton;
    pnlInfo: TRectangle;
    Label1: TLabel;
    ListView1: TListView;
    procedure btnMakeLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  DataLogger, DataLogger.Provider.ListView;

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

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  Logger.AddProvider(
    TProviderListView.Create
    .ListView(ListView1)
//    .MaxLogLines(10)
//    .ModeInsert(tmFirst)
//    .CleanOnStart(False)
    );

  // Log Format
  Logger.SetLogFormat(TLoggerFormat.LOG_TIMESTAMP + ' - ' + TLoggerFOrmat.LOG_MESSAGE);
end;

procedure TForm2.Label1Click(Sender: TObject);
var
  LURL: string;
begin
  LURL := Label1.Text;
  LURL := LURL.Replace('GITHUB: ', '').Replace(' ', '');

{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(LURL), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(LURL)));
{$ENDIF POSIX}
end;

end.
