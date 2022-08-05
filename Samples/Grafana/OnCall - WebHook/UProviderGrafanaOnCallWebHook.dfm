object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - Grafana OnCall WebHook'
  ClientHeight = 61
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
      Left = 96
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Make Log'
      TabOrder = 0
      OnClick = btnMakeLogClick
    end
    object btnMakeLogCustom: TButton
      Left = 360
      Top = 6
      Width = 137
      Height = 25
      Caption = 'Make LogCustom'
      TabOrder = 1
      OnClick = btnMakeLogCustomClick
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
end
