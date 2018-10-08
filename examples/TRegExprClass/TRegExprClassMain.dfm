object fmTRegExprClassMain: TfmTRegExprClassMain
  Left = 223
  Top = 107
  Width = 440
  Height = 332
  Caption = 'fmTRegExprClassMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblSearchPhoneIn: TLabel
    Left = 8
    Top = 128
    Width = 84
    Height = 13
    Caption = 'lblSearchPhoneIn'
    FocusControl = memSearchPhoneIn
  end
  object Bevel1: TBevel
    Left = 8
    Top = 112
    Width = 417
    Height = 9
    Shape = bsTopLine
  end
  object lblSubstituteTemplate: TLabel
    Left = 224
    Top = 128
    Width = 101
    Height = 13
    Caption = 'lblSubstituteTemplate'
    FocusControl = memSubstituteTemplate
  end
  object btnExtractEmails: TBitBtn
    Left = 184
    Top = 24
    Width = 121
    Height = 25
    Caption = 'btnExtractEmails >>'
    TabOrder = 0
    OnClick = btnExtractEmailsClick
  end
  object memExtractEmails: TMemo
    Left = 8
    Top = 8
    Width = 169
    Height = 89
    Lines.Strings = (
      'My e-mails is anso@mail.ru and '
      'anso@paycash.ru')
    TabOrder = 1
  end
  object lbxEMailesExtracted: TListBox
    Left = 312
    Top = 8
    Width = 113
    Height = 89
    ItemHeight = 13
    TabOrder = 2
  end
  object memSearchPhoneIn: TMemo
    Left = 8
    Top = 144
    Width = 201
    Height = 89
    Lines.Strings = (
      'Phone of AlkorSoft (project PayCash, '
      'www.cyphermint.com) in Russia is '
      '+7(812) 329-44-69')
    TabOrder = 3
  end
  object memSubstituteTemplate: TMemo
    Left = 224
    Top = 144
    Width = 201
    Height = 89
    Lines.Strings = (
      'Zone code $1, city code $2. Whole '
      'phone number is $&.')
    TabOrder = 4
  end
  object btnSubstitutePhone: TBitBtn
    Left = 16
    Top = 248
    Width = 129
    Height = 25
    Caption = 'btnSubstitutePhone'
    TabOrder = 5
    OnClick = btnSubstitutePhoneClick
  end
  object memSubstitutePhoneRes: TMemo
    Left = 168
    Top = 248
    Width = 257
    Height = 49
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'memSubstitutePhoneRes')
    ReadOnly = True
    TabOrder = 6
  end
end
