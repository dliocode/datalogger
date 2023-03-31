unit UProviderRichEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Winapi.ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnMakeLog: TButton;
    pnlInfo: TPanel;
    RichEdit1: TRichEdit;
    procedure pnlInfoClick(Sender: TObject);
    procedure btnMakeLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}


uses
  System.Threading,
  DataLogger, DataLogger.Provider.RichEdit;

procedure TForm2.btnMakeLogClick(Sender: TObject);
begin
  Logger
    .Trace('My Trace')
    .Debug('My Debug')
    .Info('My Info')
    .Warn('My Warn')
    .Error('My Error')
    .Success('My Success')
    .Fatal('My Fatal')
    .Custom('Custom Level', 'My Custom')
    ;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  Logger.AddProvider(
    TProviderRichEdit.Create
    .RichEdit(RichEdit1)
    .UseColor(True)
    // .ChangeColor(TLoggerLevel.Debug, TColorRec.Black, $00AEB600)
    // .ChangeFont(TLoggerLevel.Debug, '', 14, [TFontStyle.fsBold])
    // .ModeInsert(TRichEditModeInsert.tmFirst)
    // .MaxLogLines(10)
    // .CleanOnStart(False)
    );

  // Log Format
  Logger.SetTemplate(TLoggerTemplate.LOG_TIMESTAMP + ' - ' + TLoggerTemplate.LOG_MESSAGE);
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
