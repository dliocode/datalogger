object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - Events - With DataSet'
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
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Make Log'
      TabOrder = 0
      OnClick = btnMakeLogClick
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
    Height = 173
    Align = alTop
    TabOrder = 2
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
      Height = 156
      Align = alClient
      DataSource = DataSourceOnAny
      ReadOnly = True
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 235
    Width = 624
    Height = 206
    Align = alClient
    TabOrder = 3
    object Label2: TLabel
      Left = 1
      Top = 1
      Width = 622
      Height = 15
      Align = alTop
      Caption = 'OnError'
      ExplicitWidth = 41
    end
    object DBGridOnError: TDBGrid
      Left = 1
      Top = 16
      Width = 622
      Height = 189
      Align = alClient
      DataSource = DataSourceOnError
      ReadOnly = True
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
    end
  end
  object DataSourceOnAny: TDataSource
    DataSet = FDMemTableOnAny
    Left = 536
    Top = 88
  end
  object DataSourceOnError: TDataSource
    DataSet = FDMemTableOnError
    Left = 544
    Top = 256
  end
  object FDMemTableOnError: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 544
    Top = 312
    object FDMemTableOnErrorSeq: TIntegerField
      FieldName = 'Seq'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object FDMemTableOnErrorTimeStamp: TDateTimeField
      FieldName = 'TimeStamp'
    end
    object FDMemTableOnErrorMessage: TStringField
      FieldName = 'Message'
      Size = 1000
    end
  end
  object FDMemTableOnAny: TFDMemTable
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    Left = 536
    Top = 136
    object FDMemTableOnAnySeq: TIntegerField
      FieldName = 'Seq'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
    end
    object FDMemTableOnAnyTimeStamp: TDateTimeField
      FieldName = 'TimeStamp'
    end
    object FDMemTableOnAnyMessage: TStringField
      FieldName = 'Message'
      Size = 1000
    end
  end
end
