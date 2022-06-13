object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - Memory'
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
    ExplicitLeft = 8
    ExplicitTop = 15
    object btnMakeLog: TButton
      Left = 8
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Make Log'
      TabOrder = 0
      OnClick = btnMakeLogClick
    end
    object btnMemoryRead: TButton
      Left = 104
      Top = 6
      Width = 97
      Height = 25
      Caption = 'Memory Read'
      TabOrder = 1
      OnClick = btnMemoryReadClick
    end
    object btnMemoryClear: TButton
      Left = 207
      Top = 6
      Width = 106
      Height = 25
      Caption = 'Memory Clear'
      TabOrder = 2
      OnClick = btnMemoryClearClick
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
  object Memo1: TMemo
    Left = 0
    Top = 62
    Width = 624
    Height = 379
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 232
    ExplicitTop = 200
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
end
