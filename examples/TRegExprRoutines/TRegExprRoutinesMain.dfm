object fmTRegExprRoutines: TfmTRegExprRoutines
  Left = 223
  Top = 107
  Width = 368
  Height = 347
  Caption = 'fmTRegExprRoutines'
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
  object grpSearchOrValidate: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 145
    Caption = 'grpSearchOrValidate'
    TabOrder = 0
    object lblPhone: TLabel
      Left = 8
      Top = 24
      Width = 41
      Height = 13
      Caption = 'lblPhone'
      FocusControl = edPhone
    end
    object lblValidatePhoneRes: TLabel
      Left = 24
      Top = 64
      Width = 98
      Height = 13
      Caption = 'lblValidatePhoneRes'
      Visible = False
    end
    object lblTextWithPhone: TLabel
      Left = 8
      Top = 88
      Width = 84
      Height = 13
      Caption = 'lblTextWithPhone'
      FocusControl = edTextWithPhone
    end
    object lblSearchPhoneRes: TLabel
      Left = 24
      Top = 128
      Width = 94
      Height = 13
      Caption = 'lblSearchPhoneRes'
      Visible = False
    end
    object edPhone: TComboBox
      Left = 24
      Top = 40
      Width = 185
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = '555-1234'
      Items.Strings = (
        '555-1234'
        'Phone: 555-1234')
    end
    object btnValidatePhone: TBitBtn
      Left = 224
      Top = 40
      Width = 105
      Height = 25
      Caption = 'btnValidatePhone'
      TabOrder = 1
      OnClick = btnValidatePhoneClick
    end
    object edTextWithPhone: TComboBox
      Left = 24
      Top = 104
      Width = 185
      Height = 21
      ItemHeight = 13
      TabOrder = 2
      Text = 'Phone: 555-1234'
      Items.Strings = (
        'Phone: 555-1234'
        'Phone: 555+1234')
    end
    object btnSearchPhone: TBitBtn
      Left = 224
      Top = 104
      Width = 105
      Height = 25
      Caption = 'btnSearchPhone'
      TabOrder = 3
      OnClick = btnSearchPhoneClick
    end
  end
  object grpReplace: TGroupBox
    Left = 8
    Top = 160
    Width = 345
    Height = 153
    Caption = 'grpReplace'
    TabOrder = 1
    object lblSearchIn: TLabel
      Left = 8
      Top = 16
      Width = 53
      Height = 13
      Caption = 'lblSearchIn'
      FocusControl = memSearchIn
    end
    object lblReplaceWith: TLabel
      Left = 208
      Top = 56
      Width = 72
      Height = 13
      Caption = 'lblReplaceWith'
      FocusControl = edReplaceWith
    end
    object lblSearchFor: TLabel
      Left = 208
      Top = 16
      Width = 59
      Height = 13
      Caption = 'lblSearchFor'
      FocusControl = edSearchFor
    end
    object memSearchIn: TMemo
      Left = 16
      Top = 32
      Width = 177
      Height = 65
      Lines.Strings = (
        'Take a look at product. product is '
        'the best !')
      TabOrder = 0
    end
    object btnReplace: TBitBtn
      Left = 16
      Top = 112
      Width = 105
      Height = 25
      Caption = 'btnReplace'
      TabOrder = 1
      OnClick = btnReplaceClick
    end
    object edReplaceWith: TEdit
      Left = 208
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 2
      Text = 'TRegExpr'
    end
    object edSearchFor: TEdit
      Left = 208
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 3
      Text = 'product'
    end
    object memReplaceRes: TMemo
      Left = 136
      Top = 104
      Width = 193
      Height = 41
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'memReplaceRes')
      ReadOnly = True
      TabOrder = 4
    end
  end
end
