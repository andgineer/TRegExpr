object fmText2HTMLMain: TfmText2HTMLMain
  Left = 223
  Top = 107
  Width = 413
  Height = 201
  Caption = 'fmText2HTMLMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblSourceFileName: TLabel
    Left = 8
    Top = 8
    Width = 88
    Height = 13
    Caption = 'lblSourceFileName'
    FocusControl = edSourceFileName
  end
  object lblDestFileName: TLabel
    Left = 8
    Top = 56
    Width = 76
    Height = 13
    Caption = 'lblDestFileName'
    FocusControl = edDestFileName
  end
  object btnSourceFileName: TSpeedButton
    Left = 376
    Top = 24
    Width = 23
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '...'
    OnClick = btnSourceFileNameClick
  end
  object btnDestFileName: TSpeedButton
    Left = 376
    Top = 72
    Width = 23
    Height = 22
    Anchors = [akTop, akRight]
    Caption = '...'
  end
  object lblDestFileNameComment: TLabel
    Left = 16
    Top = 104
    Width = 358
    Height = 13
    Caption = 
      'If destination file name is empty, then uses source name with .h' +
      'tm extension.'
  end
  object pbrConvert: TProgressBar
    Left = 16
    Top = 140
    Width = 385
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 3
    Visible = False
  end
  object edSourceFileName: TEdit
    Left = 16
    Top = 24
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'edSourceFileName'
  end
  object edDestFileName: TEdit
    Left = 16
    Top = 72
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'edDestFileName'
  end
  object btnConvert: TBitBtn
    Left = 280
    Top = 136
    Width = 121
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'btnConvert'
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object dlgSourceFile: TOpenDialog
    Left = 208
    Top = 8
  end
end
