object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - EventLog'
  ClientHeight = 60
  ClientWidth = 294
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object btnMakeLog: TButton
    Left = 8
    Top = 27
    Width = 75
    Height = 25
    Caption = 'Make Log'
    TabOrder = 0
    OnClick = btnMakeLogClick
  end
  object btnOpenEventLog: TButton
    Left = 89
    Top = 27
    Width = 104
    Height = 25
    Caption = 'Open EventLog'
    TabOrder = 1
    OnClick = btnOpenEventLogClick
  end
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 294
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
    TabOrder = 2
    OnClick = pnlInfoClick
    ExplicitLeft = 8
  end
end
