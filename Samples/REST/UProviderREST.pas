unit UProviderREST;

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
    procedure pnlInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMakeLogClick(Sender: TObject);
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
  DataLogger,
  DataLogger.Provider.REST.HTTPClient,
  DataLogger.Provider.REST.Indy,
  DataLogger.Provider.REST.NetHTTPClient;

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
    TProviderRESTHTTPClient.Create
      .URL('http://localhost:9000/api/log1')
      .ContentType('application/json')
      .BearerToken('')
      .Method(DataLogger.Provider.REST.HTTPClient.TRESTMethod.tlmPost) // Post
    );

  Logger.AddProvider(
    TProviderRESTIndy.Create
      .URL('http://localhost:9000/api/log2')
      .ContentType('application/json')
      .BearerToken('')
      .Method(DataLogger.Provider.REST.Indy.TRESTMethod.tlmPost) // Post
    );

  Logger.AddProvider(
    TProviderRESTNetHTTPClient.Create
      .URL('http://localhost:9000/api/log3')
      .ContentType('application/json')
      .BearerToken('')
      .Method(DataLogger.Provider.REST.NetHTTPClient.TRESTMethod.tlmPost) // Post
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
