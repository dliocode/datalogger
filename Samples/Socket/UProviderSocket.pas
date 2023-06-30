unit UProviderSocket;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Winapi.ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Mask;

type
  TForm2 = class(TForm)
    pnlInfo: TPanel;
    Panel1: TPanel;
    btnMakeLog: TButton;
    btnStart: TButton;
    btnStop: TButton;
    EditCountClient: TLabeledEdit;
    TimerCountClients: TTimer;
    btnOpenPreview: TButton;
    Memo1: TMemo;
    btnDisconnectAll: TButton;
    procedure btnMakeLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
    procedure btnOpenPreviewClick(Sender: TObject);
    procedure TimerCountClientsTimer(Sender: TObject);
    procedure btnDisconnectAllClick(Sender: TObject);
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
  DataLogger, DataLogger.Provider.Socket;

var
  FSocket: TProviderSocket;

procedure TForm2.btnMakeLogClick(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      I: Integer;
    begin
      for I := 0 to 100 do
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
    end).Start;
end;

procedure TForm2.btnOpenPreviewClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('.\..\..\..\..\src\Providers\DataLogger.Provider.Socket.html'), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm2.btnStartClick(Sender: TObject);
begin
  FSocket.Start;

  btnStart.Enabled := not FSocket.IsActive;
  btnStop.Enabled := FSocket.IsActive;
end;

procedure TForm2.btnStopClick(Sender: TObject);
begin
  FSocket.Stop;

  btnStart.Enabled := not FSocket.IsActive;
  btnStop.Enabled := FSocket.IsActive;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  FSocket :=
    TProviderSocket.Create
    .OnConnection(
    procedure(const AConnectionID: string)
    begin
      if not Assigned(Memo1) then
        Exit;

      if (csDestroying in Memo1.ComponentState) then
        Exit;

      Memo1.Lines.Add('Connect: ' + AConnectionID);
    end)

    .OnDisconnect(
    procedure(const AConnectionID: string)
    begin
      if not Assigned(Memo1) then
        Exit;

      if (csDestroying in Memo1.ComponentState) then
        Exit;

      Memo1.Lines.Add('Disconnect: ' + AConnectionID);
    end)

    .InitSSL(nil)
    .Port(8080)
    .MaxConnection(0) // 0 - unlimited
    .AutoStart(True)
    .CustomMessage(nil) // Send log custom
    .LogCacheSize(0) // When connecting receive the latest logs
    ;

  Logger.AddProvider(FSocket);

  btnStop.Enabled := FSocket.IsActive;

  btnStart.Click;
end;

procedure TForm2.pnlInfoClick(Sender: TObject);
var
  LURL: string;
begin
  LURL := pnlInfo.Caption;
  LURL := LURL.Replace('GITHUB: ', '').Replace(' ', '');

  ShellExecute(0, 'open', PChar(LURL), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm2.TimerCountClientsTimer(Sender: TObject);
begin
  EditCountClient.Text := FSocket.CountConnections.ToString;
end;

procedure TForm2.btnDisconnectAllClick(Sender: TObject);
begin
  FSocket.DisconnectAll;
end;

end.
