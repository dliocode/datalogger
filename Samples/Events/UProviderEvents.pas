unit UProviderEvents;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.JSON,
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
    TProviderEvents.Create
      .OnAny(
      procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string)
      begin
        if not Assigned(MemoOnAny) then
          Exit;

        if (csDestroying in MemoOnAny.ComponentState) then
          Exit;

        MemoOnAny.Lines.Add(DateTimeToStr(AItem.TimeStamp) + ' - ' + AItem.Message);
      end)

      .OnTrace(
      procedure(const AItem: TJSONObject)
      begin
      end)

      .OnDebug(
      procedure(const AItem: TJSONObject)
      begin
      end)

      .OnInfo(
      procedure(const AItem: TJSONObject)
      begin
      end)

      .OnSuccess(
      procedure(const AItem: TJSONObject)
      begin
      end)

      .OnWarn(
      procedure(const AItem: TJSONObject)
      begin
      end)

      .OnError(
      procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string)
      begin
        if not Assigned(MemoOnError) then
          Exit;

        if (csDestroying in MemoOnError.ComponentState) then
          Exit;

        MemoOnError.Lines.Add(DateTimeToStr(AItem.TimeStamp) + ' - ' + AItem.Message);
      end)

      .OnFatal(
      procedure(const AItem: TJSONObject)
      begin
      end)

      .OnCustom(
      procedure(const AItem: TJSONObject)
      begin
      end)
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
