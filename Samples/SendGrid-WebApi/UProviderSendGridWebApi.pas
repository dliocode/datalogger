unit UProviderSendGridWebApi;

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
    btnMakeLogCustom: TButton;
    procedure btnMakeLogClick(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMakeLogCustomClick(Sender: TObject);
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
  System.JSON,
  DataLogger, DataLogger.Provider.SendGridWebApi;

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

procedure TForm2.btnMakeLogCustomClick(Sender: TObject);
var
  LJO: TJSONObject;
begin
  LJO := TJSONObject.Create;
  try
    LJO.AddPair('fields_custom1', 'my_value 1');
    LJO.AddPair('fields_custom2', 'my_value 2');
    LJO.AddPair('fields_custom3', 'my_value 3');
    LJO.AddPair('fields_custom4', 'my_value 4');
    LJO.AddPair('fields_custom5', 'my_value 5');

    Logger.Debug(LJO);
  finally
    LJO.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  Logger.AddProvider(
    TProviderSendGridWebApi.Create
    .ApiKey('{API_KEY}')
    .EmailFrom('{EMAIL_REGISTER_SEND_GRID}')
    .EmailTo(['{EMAIL@EMAIL.COM}', '{EMAIL2@EMAIL2.COM}'])
    .Subject('{SUBJECT}')
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