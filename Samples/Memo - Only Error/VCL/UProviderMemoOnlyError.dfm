object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - Memo'
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
  object Panel2: TPanel
    Left = 0
    Top = 62
    Width = 624
    Height = 379
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 2
    ExplicitTop = 60
    object PanelAll: TPanel
      Left = 1
      Top = 1
      Width = 304
      Height = 377
      Align = alLeft
      Caption = 'PanelAll'
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 296
        Height = 15
        Align = alTop
        Caption = 'Logger Type -> All'
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 97
      end
      object MemoAll: TMemo
        Left = 1
        Top = 22
        Width = 302
        Height = 354
        Align = alClient
        TabOrder = 0
        ExplicitLeft = 2
        ExplicitTop = 88
        ExplicitHeight = 288
      end
    end
    object PanelOnlyError: TPanel
      Left = 305
      Top = 1
      Width = 318
      Height = 377
      Align = alClient
      Caption = 'PanelAll'
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitWidth = 304
      object Label2: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 310
        Height = 15
        Align = alTop
        Caption = 'Logger Type -> OnlyError'
        ExplicitWidth = 133
      end
      object MemoOnlyError: TMemo
        Left = 1
        Top = 22
        Width = 316
        Height = 354
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 302
      end
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
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Make Log'
      TabOrder = 0
      OnClick = btnMakeLogClick
    end
  end
end
