unit UProviderSendEmail;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Winapi.ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnMakeLog: TButton;
    pnlInfo: TPanel;
    Memo1: TMemo;
    procedure btnMakeLogClick(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
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
  DataLogger, DataLogger.Provider.SendEmail,
  SendEmail; // http://github.com/dliocode/sendemail

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
    TProviderSendEmail.Create
    .SendEmail(
      TSendEmail.New 
      .OnLog(
        procedure(ALog: string)
        begin 
          Memo1.Lines.Add(ALog)
        end
      , lmAll)
      
      .From('from@email.com')
      .AddTo('to@email.com')
      .Subject('My Log')

      // Configuration
      .Host('smtp.mail.com')
      .Port(465)
      .Auth(True)
      .Username('from@email.com')
      .Password('pass@email')
      .SSL(True)
      .TLS(True)
      )
    );

  // Log Format
  Logger.SetLogFormat(TLoggerFormat.LOG_TIMESTAMP + ' - ' + TLoggerFOrmat.LOG_MESSAGE);
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
