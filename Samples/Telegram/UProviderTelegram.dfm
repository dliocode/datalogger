object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'DataLogger - Telegram'
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
  object Memo1: TMemo
    Left = 0
    Top = 62
    Width = 624
    Height = 379
    Align = alClient
    Lines.Strings = (
      'How can I create bot in telegram?'
      ''
      '1. Enter @Botfather in the search tab and choose this bot.'
      ''
      '2. Choose or type the /newbot command and send it.'
      ''
      
        '3. Choose a name for your bot '#8212' your subscribers will see it in ' +
        'the conversation.'
      ''
      '4. Go to the @BotFather bot and send the command /token .'
      ''
      
        '5. Choose the one you need a token for so you can connect with T' +
        'ProviderTelegram'
      ''
      '6. Copy the token value.'
      ''
      '7. Send the message to the bot.'
      ''
      
        '8. Access to get the Chat Id that will receive the messages - ht' +
        'tps://api.telegram.org/bot<TOKEN>/getUpdates'
      ''
      '9. Copy in JSON - chat > id'
      ''
      '10 - Configure TProviderTelegram with the informations.')
    TabOrder = 2
  end
end
