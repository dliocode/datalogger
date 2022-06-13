object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - ListView'
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
    ExplicitLeft = 232
    ExplicitTop = 224
    ExplicitWidth = 185
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
    ExplicitLeft = 8
    ExplicitWidth = 294
  end
  object ListView1: TListView
    Left = 0
    Top = 62
    Width = 624
    Height = 379
    Align = alClient
    Columns = <
      item
        Caption = 'Log'
        Width = 590
      end>
    TabOrder = 2
    ViewStyle = vsReport
    ExplicitLeft = 192
    ExplicitTop = 168
    ExplicitWidth = 250
    ExplicitHeight = 150
  end
end
