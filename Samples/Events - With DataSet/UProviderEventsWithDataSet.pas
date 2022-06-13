unit UProviderEventsWithDataSet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Winapi.ShellAPI,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Data.DB, Vcl.Grids, Vcl.DBGrids, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    btnMakeLog: TButton;
    pnlInfo: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Panel3: TPanel;
    Label2: TLabel;
    DBGridOnAny: TDBGrid;
    DBGridOnError: TDBGrid;
    DataSourceOnAny: TDataSource;
    DataSourceOnError: TDataSource;
    FDMemTableOnError: TFDMemTable;
    FDMemTableOnAny: TFDMemTable;
    FDMemTableOnAnySeq: TIntegerField;
    FDMemTableOnAnyTimeStamp: TDateTimeField;
    FDMemTableOnAnyMessage: TStringField;
    FDMemTableOnErrorSeq: TIntegerField;
    FDMemTableOnErrorTimeStamp: TDateTimeField;
    FDMemTableOnErrorMessage: TStringField;
    procedure btnMakeLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlInfoClick(Sender: TObject);
  private
    { Private declarations }
    procedure AdjustDataSet(const ADataSet: TFDMemTable);
    procedure ResetDataSet(const ADataSet: TFDMemTable);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}


uses
  DataLogger, DataLogger.Provider.Events;

procedure TForm2.AdjustDataSet(const ADataSet: TFDMemTable);
begin
  ADataSet.LogChanges := False;
  ADataSet.ResourceOptions.SilentMode := True;
  ADataSet.UpdateOptions.LockMode := lmNone;
  ADataSet.UpdateOptions.LockPoint := lpDeferred;
  ADataSet.UpdateOptions.FetchGeneratorsPoint := gpImmediate;
end;

procedure TForm2.ResetDataSet(const ADataSet: TFDMemTable);
begin
  ADataSet.Close;
  ADataSet.CreateDataSet;
  ADataSet.Open;
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

  AdjustDataSet(FDMemTableOnAny);
  ResetDataSet(FDMemTableOnAny);

  AdjustDataSet(FDMemTableOnError);
  ResetDataSet(FDMemTableOnError);

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
            FDMemTableOnAny.Append;
            FDMemTableOnAnySeq.AsInteger := AItem.Sequence;
            FDMemTableOnAnyTimeStamp.AsDateTime := AItem.TimeStamp;
            FDMemTableOnAnyMessage.AsString := AItem.Message;
            FDMemTableOnAny.Post;
          end);
        end)

        .OnTrace(nil)
        .OnDebug(nil)
        .OnInfo(nil)
        .OnSuccess(nil)
        .OnWarn(nil)

        .OnError(
        procedure(const ALogFormat: string; const AItem: TLoggerItem; const AFormatTimestamp: string)
        begin
          TThread.Synchronize(nil,
          procedure
          begin
            FDMemTableOnError.Append;
            FDMemTableOnErrorSeq.AsInteger := AItem.Sequence;
            FDMemTableOnErrorTimeStamp.AsDateTime := AItem.TimeStamp;
            FDMemTableOnErrorMessage.AsString := AItem.Message;
            FDMemTableOnError.Post;
          end)
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
