object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - Events - With SQLLite'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 21
    Width = 624
    Height = 41
    Align = alTop
    TabOrder = 0
    object btnMakeLog: TButton
      Left = 11
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Make Log'
      TabOrder = 0
      OnClick = btnMakeLogClick
    end
    object btnDelete: TButton
      Left = 92
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 1
      OnClick = btnDeleteClick
    end
    object btnDeleteAll: TButton
      Left = 173
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Delete All'
      TabOrder = 2
      OnClick = btnDeleteAllClick
    end
  end
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 21
    Cursor = crHandPoint
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '  GITHUB: https://github.com/dliocode/datalogger'
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    OnClick = pnlInfoClick
  end
  object Panel2: TPanel
    Left = 0
    Top = 62
    Width = 624
    Height = 379
    Align = alClient
    TabOrder = 2
    ExplicitHeight = 173
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 622
      Height = 15
      Align = alTop
      Caption = 'OnAny'
      ExplicitWidth = 37
    end
    object DBGridOnAny: TDBGrid
      Left = 1
      Top = 16
      Width = 622
      Height = 362
      Align = alClient
      DataSource = DataSourceOnAny
      ReadOnly = True
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'Id'
          Width = 26
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Seq'
          Width = 46
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Timestamp'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Type'
          Width = 32
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'Message'
          Width = 353
          Visible = True
        end>
    end
  end
  object DataSourceOnAny: TDataSource
    DataSet = FDQueryOnAny
    Left = 232
    Top = 240
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database='#39'.\..\..\datalogger.sqlite'
      'LockingMode=Normal'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 232
    Top = 144
  end
  object FDQueryOnAny: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT D.Id,'
      '       D.Seq,'
      '       D.Timestamp,'
      '       D.Message,'
      '       D.Type'
      'FROM datalogger D')
    Left = 232
    Top = 192
    object FDQueryOnAnyId: TFDAutoIncField
      FieldName = 'Id'
      Origin = 'Id'
      ProviderFlags = [pfInWhere, pfInKey]
    end
    object FDQueryOnAnySeq: TLargeintField
      FieldName = 'Seq'
      Origin = 'Seq'
    end
    object FDQueryOnAnyTimestamp: TDateTimeField
      FieldName = 'Timestamp'
      Origin = 'Timestamp'
    end
    object FDQueryOnAnyType: TIntegerField
      FieldName = 'Type'
      Origin = 'Type'
    end
    object FDQueryOnAnyMessage: TStringField
      FieldName = 'Message'
      Origin = 'Message'
      Size = 1000
    end
  end
end
