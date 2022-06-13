unit UProviderEventsWithSQLLite;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Winapi.ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.DApt;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnMakeLog: TButton;
    pnlInfo: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    DBGridOnAny: TDBGrid;
    DataSourceOnAny: TDataSource;
    FDConnection1: TFDConnection;
    FDQueryOnAny: TFDQuery;
    FDQueryOnAnyId: TFDAutoIncField;
    FDQueryOnAnySeq: TLargeintField;
    FDQueryOnAnyTimestamp: TDateTimeField;
    FDQueryOnAnyType: TIntegerField;
    FDQueryOnAnyMessage: TStringField;
    btnDelete: TButton;
    btnDeleteAll: TButton;
    procedure btnMakeLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDeleteAllClick(Sender: TObject);
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
  System.IOUtils,
  DataLogger, DataLogger.Provider.Events;

procedure TForm2.btnDeleteAllClick(Sender: TObject);
begin
  FDConnection1.ExecSQL('DELETE FROM datalogger');
  FDQueryOnAny.Close;
  FDQueryOnAny.Open;
end;

procedure TForm2.btnDeleteClick(Sender: TObject);
begin
  FDQueryOnAny.Delete;
end;

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

  FDConnection1.Params.Values['database'] := '.\..\..\datalogger.sqlite';
  FDConnection1.Connected := True;

  FDQueryOnAny.Close;
  FDQueryOnAny.Open;

  Logger.AddProvider(
    TProviderEvents.Create
    .Config(
      TEventsConfig.Create
        .OnAny(
        procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string)
        begin
          TThread.Synchronize(nil,
          procedure
          begin
            FDQueryOnAny.Append;
  //          FDQueryOnAnyId.AsInteger := 0;
            FDQueryOnAnySeq.AsInteger := AItem.Sequence;
            FDQueryOnAnyTimestamp.AsDateTime := AItem.TimeStamp;
            FDQueryOnAnyMessage.AsString := AItem.Message;
            FDQueryOnAnyType.AsInteger := Integer(AItem.&Type);
            FDQueryOnAny.Post;
          end)
        end)
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
