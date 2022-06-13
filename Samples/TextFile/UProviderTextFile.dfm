object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - TextFile'
  ClientHeight = 61
  ClientWidth = 282
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
    Width = 282
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
    Width = 282
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
  end
  object btnShowExplorer: TButton
    Left = 89
    Top = 27
    Width = 96
    Height = 25
    Caption = 'Show Explorer'
    TabOrder = 2
    OnClick = btnShowExplorerClick
  end
end
