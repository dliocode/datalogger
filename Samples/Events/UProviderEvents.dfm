object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - Events'
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
    object MemoOnAny: TMemo
      Left = 1
      Top = 16
      Width = 622
      Height = 156
      Align = alClient
      TabOrder = 0
      ExplicitTop = 22
      ExplicitHeight = 202
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 235
    Width = 624
    Height = 206
    Align = alClient
    TabOrder = 3
    ExplicitTop = 70
    ExplicitHeight = 163
    object Label2: TLabel
      Left = 1
      Top = 1
      Width = 622
      Height = 15
      Align = alTop
      Caption = 'OnError'
      ExplicitWidth = 41
    end
    object MemoOnError: TMemo
      Left = 1
      Top = 16
      Width = 622
      Height = 189
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 146
    end
  end
end
