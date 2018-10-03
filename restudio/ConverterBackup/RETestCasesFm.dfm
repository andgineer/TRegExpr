object fmTestCases: TfmTestCases
  Left = 156
  Top = 128
  Width = 738
  Height = 436
  Caption = 'fmTestCases'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 283
    Top = 0
    Width = 5
    Height = 361
    Align = alRight
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 361
    Width = 730
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      730
      41)
    object btnSelect: TBitBtn
      Left = 440
      Top = 8
      Width = 140
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Select and exit'
      TabOrder = 0
      Kind = bkYes
    end
    object BitBtn2: TBitBtn
      Left = 595
      Top = 8
      Width = 121
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Panel1: TPanel
    Left = 288
    Top = 0
    Width = 442
    Height = 361
    Align = alRight
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    DesignSize = (
      442
      361)
    object grbDetails: TGroupBox
      Left = 8
      Top = 8
      Width = 425
      Height = 345
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'grbDetails'
      TabOrder = 0
      DesignSize = (
        425
        345)
      object lblREName: TLabel
        Left = 8
        Top = 20
        Width = 28
        Height = 13
        Caption = 'Name'
        FocusControl = edREName
      end
      object lblRE: TLabel
        Left = 8
        Top = 88
        Width = 51
        Height = 13
        Caption = 'Expression'
        FocusControl = memRE
      end
      object lblDescription: TLabel
        Left = 8
        Top = 44
        Width = 53
        Height = 13
        Caption = 'Description'
        FocusControl = edDescription
      end
      object lblComment: TLabel
        Left = 8
        Top = 68
        Width = 44
        Height = 13
        Caption = 'Comment'
        FocusControl = edComment
      end
      object edREName: TEdit
        Left = 56
        Top = 16
        Width = 217
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'edREName'
        OnChange = edRENameChange
      end
      object memRE: TMemo
        Left = 8
        Top = 104
        Width = 409
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'memRE')
        TabOrder = 3
        OnChange = edRENameChange
      end
      object grbTestCases: TGroupBox
        Left = 8
        Top = 192
        Width = 409
        Height = 137
        Anchors = [akLeft, akTop, akRight]
        Caption = ' Test case '
        TabOrder = 4
        DesignSize = (
          409
          137)
        object lblModifiers: TLabel
          Left = 7
          Top = 20
          Width = 42
          Height = 13
          Caption = 'Modifiers'
          FocusControl = edModifiers
        end
        object lblSubject: TLabel
          Left = 8
          Top = 40
          Width = 52
          Height = 13
          Caption = 'Input string'
        end
        object edModifiers: TEdit
          Left = 72
          Top = 16
          Width = 97
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'edModifiers'
          OnChange = edRENameChange
        end
        object memSubject: TMemo
          Left = 8
          Top = 56
          Width = 393
          Height = 57
          Color = cl3DLight
          Lines.Strings = (
            'memSubject')
          ReadOnly = True
          TabOrder = 1
        end
      end
      object btnSave: TBitBtn
        Left = 296
        Top = 24
        Width = 113
        Height = 25
        Caption = 'Save'
        Default = True
        TabOrder = 5
        OnClick = btnSaveClick
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          0400000000006801000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333330000333333333333333333333333F33333333333
          00003333344333333333333333388F3333333333000033334224333333333333
          338338F3333333330000333422224333333333333833338F3333333300003342
          222224333333333383333338F3333333000034222A22224333333338F338F333
          8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
          33333338F83338F338F33333000033A33333A222433333338333338F338F3333
          0000333333333A222433333333333338F338F33300003333333333A222433333
          333333338F338F33000033333333333A222433333333333338F338F300003333
          33333333A222433333333333338F338F00003333333333333A22433333333333
          3338F38F000033333333333333A223333333333333338F830000333333333333
          333A333333333333333338330000333333333333333333333333333333333333
          0000}
        NumGlyphs = 2
      end
      object edDescription: TEdit
        Left = 72
        Top = 40
        Width = 201
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'edDescription'
        OnChange = edRENameChange
      end
      object edComment: TEdit
        Left = 72
        Top = 64
        Width = 345
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'edComment'
        OnChange = edRENameChange
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 283
    Height = 361
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 2
    DesignSize = (
      283
      361)
    object grbREList: TGroupBox
      Left = 8
      Top = 8
      Width = 265
      Height = 345
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = ' Expressions list '
      TabOrder = 0
      DesignSize = (
        265
        345)
      object tvCases: TTreeView
        Left = 8
        Top = 16
        Width = 249
        Height = 321
        Anchors = [akLeft, akTop, akRight, akBottom]
        Indent = 19
        TabOrder = 0
        OnChange = tvCasesChange
        OnChanging = tvCasesChanging
        OnDblClick = tvCasesDblClick
      end
    end
  end
end
