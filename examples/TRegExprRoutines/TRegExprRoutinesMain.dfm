object fmTRegExprRoutines: TfmTRegExprRoutines
  Left = 543
  Height = 338
  Top = 163
  Width = 357
  Caption = 'fmTRegExprRoutines'
  ClientHeight = 338
  ClientWidth = 357
  Color = clBtnFace
  DesignTimePPI = 144
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  LCLVersion = '1.8.4.0'
  Scaled = False
  object grpSearchOrValidate: TGroupBox
    Left = 8
    Height = 145
    Top = 8
    Width = 345
    Caption = 'grpSearchOrValidate'
    ClientHeight = 127
    ClientWidth = 341
    TabOrder = 0
    object lblPhone: TLabel
      Left = 8
      Height = 13
      Top = 0
      Width = 41
      Caption = 'lblPhone'
      FocusControl = edPhone
      ParentColor = False
      OnClick = lblPhoneClick
    end
    object lblValidatePhoneRes: TLabel
      Left = 24
      Height = 13
      Top = 40
      Width = 98
      Caption = 'lblValidatePhoneRes'
      ParentColor = False
      Visible = False
    end
    object lblTextWithPhone: TLabel
      Left = 8
      Height = 13
      Top = 64
      Width = 84
      Caption = 'lblTextWithPhone'
      FocusControl = edTextWithPhone
      ParentColor = False
    end
    object edPhone: TComboBox
      Left = 24
      Height = 21
      Top = 16
      Width = 185
      ItemHeight = 13
      ItemIndex = 0
      Items.Strings = (
        '555-1234'
        'Phone: 555-1234'
      )
      TabOrder = 0
      Text = '555-1234'
    end
    object btnValidatePhone: TBitBtn
      Left = 224
      Height = 25
      Top = 16
      Width = 105
      Caption = 'btnValidatePhone'
      OnClick = btnValidatePhoneClick
      TabOrder = 1
    end
    object edTextWithPhone: TComboBox
      Left = 24
      Height = 21
      Top = 80
      Width = 185
      ItemHeight = 13
      ItemIndex = 0
      Items.Strings = (
        'Phone: 555-1234'
        'Phone: 555+1234'
      )
      TabOrder = 2
      Text = 'Phone: 555-1234'
    end
    object btnSearchPhone: TBitBtn
      Left = 224
      Height = 25
      Top = 80
      Width = 105
      Caption = 'btnSearchPhone'
      OnClick = btnSearchPhoneClick
      TabOrder = 3
    end
    object lblSearchPhoneRes: TLabel
      Left = 24
      Height = 13
      Top = 105
      Width = 94
      Caption = 'lblSearchPhoneRes'
      ParentColor = False
      Visible = False
      OnClick = lblSearchPhoneResClick
    end
  end
  object grpReplace: TGroupBox
    Left = 12
    Height = 176
    Top = 160
    Width = 345
    Caption = 'grpReplace'
    ClientHeight = 158
    ClientWidth = 341
    TabOrder = 1
    object lblSearchIn: TLabel
      Left = 8
      Height = 13
      Top = 16
      Width = 53
      Caption = 'lblSearchIn'
      FocusControl = memSearchIn
      ParentColor = False
    end
    object lblReplaceWith: TLabel
      Left = 208
      Height = 13
      Top = 56
      Width = 72
      Caption = 'lblReplaceWith'
      FocusControl = edReplaceWith
      ParentColor = False
    end
    object lblSearchFor: TLabel
      Left = 208
      Height = 13
      Top = 16
      Width = 59
      Caption = 'lblSearchFor'
      FocusControl = edSearchFor
      ParentColor = False
    end
    object memSearchIn: TMemo
      Left = 16
      Height = 65
      Top = 32
      Width = 177
      Lines.Strings = (
        'Take a look at product. product is '
        'the best !'
      )
      TabOrder = 0
    end
    object btnReplace: TBitBtn
      Left = 16
      Height = 25
      Top = 112
      Width = 105
      Caption = 'btnReplace'
      OnClick = btnReplaceClick
      TabOrder = 1
    end
    object edReplaceWith: TEdit
      Left = 208
      Height = 21
      Top = 72
      Width = 121
      TabOrder = 2
      Text = 'TRegExpr'
    end
    object edSearchFor: TEdit
      Left = 208
      Height = 21
      Top = 32
      Width = 121
      TabOrder = 3
      Text = 'product'
    end
    object memReplaceRes: TMemo
      Left = 136
      Height = 41
      Top = 104
      Width = 193
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'memReplaceRes'
      )
      ReadOnly = True
      TabOrder = 4
    end
  end
end
