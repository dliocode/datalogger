object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 443
  ClientWidth = 923
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  OnCreate = FormCreate
  TextHeight = 15
  object btn1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Iniciar'
    TabOrder = 0
    OnClick = btn1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 923
    Height = 402
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Panel1'
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitTop = 39
    ExplicitWidth = 624
    object Memo1: TMemo
      Left = 457
      Top = 1
      Width = 465
      Height = 400
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 311
      ExplicitWidth = 304
    end
    object Memo2: TMemo
      Left = 1
      Top = 1
      Width = 456
      Height = 400
      Align = alLeft
      TabOrder = 1
    end
  end
  object btnParar: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Parar'
    TabOrder = 2
    OnClick = btnPararClick
  end
  object btnLimpar: TButton
    Left = 378
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Limpar'
    TabOrder = 3
    OnClick = btnLimparClick
  end
  object btnLimpar2: TButton
    Left = 463
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Limpar'
    TabOrder = 4
    OnClick = btnLimpar2Click
  end
end
