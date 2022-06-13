unit UProviderEvents;

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
    Panel2: TPanel;
    Label1: TLabel;
    MemoOnAny: TMemo;
    Panel3: TPanel;
    Label2: TLabel;
    MemoOnError: TMemo;
    procedure btnMakeLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
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
  DataLogger, DataLogger.Provider.Events;

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
    TProviderEvents.Create
    .Config(
      TEventsConfig.Create
        .OnAny(
        procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string)
        begin
          MemoOnAny.Lines.Add(DateTimeToStr(AItem.TimeStamp) + ' - ' + AItem.Message);
        end)

        .OnTrace(nil)
        .OnDebug(nil)
        .OnInfo(nil)
        .OnSuccess(nil)
        .OnWarn(nil)

        .OnError(
        procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string)
        begin
          MemoOnError.Lines.Add(DateTimeToStr(AItem.TimeStamp) + ' - ' + AItem.Message);
        end)

        .OnFatal(nil)
        )
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
