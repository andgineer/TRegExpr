object fmRETestCasesDlg: TfmRETestCasesDlg
  Left = 230
  Top = 65
  Width = 386
  Height = 362
  BorderStyle = bsSizeToolWin
  Caption = 'RETestCases'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlRight: TPanel
    Left = 272
    Top = 0
    Width = 106
    Height = 335
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object btnOk: TBitBtn
      Left = 3
      Top = 8
      Width = 97
      Height = 25
      Caption = '&Select'
      TabOrder = 0
      Kind = bkYes
      Spacing = -1
    end
    object btnCancel: TBitBtn
      Left = 3
      Top = 40
      Width = 97
      Height = 25
      TabOrder = 1
      Kind = bkCancel
      Spacing = -1
    end
    object btnLoad: TBitBtn
      Left = 3
      Top = 112
      Width = 97
      Height = 25
      Hint = 'Load another r.e. repository'
      Caption = 'Load'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnLoadClick
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        555555FFFFFFFFFF55555000000000055555577777777775FFFF00B8B8B8B8B0
        0000775F5555555777770B0B8B8B8B8B0FF07F75F555555575F70FB0B8B8B8B8
        B0F07F575FFFFFFFF7F70BFB0000000000F07F557777777777570FBFBF0FFFFF
        FFF07F55557F5FFFFFF70BFBFB0F000000F07F55557F777777570FBFBF0FFFFF
        FFF075F5557F5FFFFFF750FBFB0F000000F0575FFF7F777777575700000FFFFF
        FFF05577777F5FF55FF75555550F00FF00005555557F775577775555550FFFFF
        0F055555557F55557F755555550FFFFF00555555557FFFFF7755555555000000
        0555555555777777755555555555555555555555555555555555}
      NumGlyphs = 2
      Spacing = -1
    end
  end
  object pnlClient: TPanel
    Left = 0
    Top = 0
    Width = 272
    Height = 335
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 2
      Top = 249
      Width = 268
      Height = 5
      Cursor = crVSplit
      Align = alBottom
    end
    object pnlBottom: TPanel
      Left = 2
      Top = 254
      Width = 268
      Height = 79
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object memDetailes: TMemo
        Left = 0
        Top = 0
        Width = 268
        Height = 79
        TabStop = False
        Align = alClient
        BorderStyle = bsNone
        Color = clBtnFace
        Lines.Strings = (
          'memDetailes')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object grdREs: TDrawGrid
      Left = 2
      Top = 2
      Width = 268
      Height = 247
      Align = alClient
      ColCount = 2
      DefaultRowHeight = 16
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
      TabOrder = 1
      OnClick = grdREsClick
      OnDblClick = grdREsDblClick
      OnDrawCell = grdREsDrawCell
    end
  end
  object LoadDlg: TOpenDialog
    Filter = 'Text files|*.txt|All files|*.*'
    FilterIndex = 0
    Left = 328
    Top = 272
  end
end
