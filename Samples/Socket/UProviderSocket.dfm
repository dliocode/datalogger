object Form2: TForm2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'DataLogger - Socket'
  ClientHeight = 60
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
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
    TabOrder = 0
    OnClick = pnlInfoClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 21
    Width = 624
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnMakeLog: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Make Log'
      TabOrder = 0
      OnClick = btnMakeLogClick
    end
    object btnStart: TButton
      Left = 105
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Start Socket'
      TabOrder = 1
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 186
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Stop Socket'
      TabOrder = 2
      OnClick = btnStopClick
    end
    object EditCountClient: TLabeledEdit
      Left = 496
      Top = 6
      Width = 121
      Height = 23
      EditLabel.Width = 75
      EditLabel.Height = 15
      EditLabel.Caption = 'Count Clients '
      LabelPosition = lpLeft
      TabOrder = 3
      Text = ''
    end
    object btnOpenPreview: TButton
      Left = 267
      Top = 6
      Width = 110
      Height = 25
      Caption = 'Open Preview'
      TabOrder = 4
      OnClick = btnOpenPreviewClick
    end
  end
  object TimerCountClients: TTimer
    OnTimer = TimerCountClientsTimer
    Left = 392
    Top = 24
  end
end
