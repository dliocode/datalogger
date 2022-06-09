unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    btn1: TButton;
    Panel1: TPanel;
    Memo1: TMemo;
    Memo2: TMemo;
    btnParar: TButton;
    btnLimpar: TButton;
    btnLimpar2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure btnLimpar2Click(Sender: TObject);
    procedure btnPararClick(Sender: TObject);
  private
    { Private declarations }
    LStop: Boolean;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  DataLogger,
  DataLogger.Provider.Memo;

var
  M1: TDataLoggerProvider;
  M2: TDataLoggerProvider;

{$R *.dfm}

procedure TForm2.btn1Click(Sender: TObject);
begin
  LStop := False;

  TThread.CreateAnonymousThread(
  procedure
  var
    I: Integer;
  begin
    for I := 1 to 100 do
    begin
      if LStop then
        Break;

      Sleep(100);

      if (I = 5) or (I = 70) then
      begin
        Logger.Warn('StartTransaction');
        Logger.StartTransaction; // 1
      end;

      try
        Logger.Info('Teste ' + I.ToString);
      finally
        if I = 50 then
        begin
          Logger.CommitTransaction; // 1
          Logger.Warn('CommitTransaction');
        end;

        if I = 90 then
        begin
          Logger.RollbackTransaction;
          Logger.Warn('RollbackTransaction');
        end;
      end;
    end;
  end).Start;
end;

procedure TForm2.btnLimpar2Click(Sender: TObject);
begin
 Memo2.Clear;
end;

procedure TForm2.btnLimparClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm2.btnPararClick(Sender: TObject);
begin
  LStop := True;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  M1 := TProviderMemo.Create(Memo2).UseTransaction(True);
  M2 := TProviderMemo.Create(Memo1);

  Logger.SetProvider([M1, M2]);
  Logger.SetLogFormat(TLoggerFormat.LOG_TIMESTAMP + ': ' + TLoggerFormat.LOG_MESSAGE);
end;

end.
