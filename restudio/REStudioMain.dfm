object fmREDebuggerMain: TfmREDebuggerMain
  Left = 207
  Height = 474
  Top = 33
  Width = 531
  BorderIcons = [biSystemMenu]
  Caption = 'fmREDebuggerMain'
  ClientHeight = 474
  ClientWidth = 531
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 350
  DesignTimePPI = 144
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '1.8.4.0'
  Scaled = False
  object Bevel1: TBevel
    Left = 0
    Height = 2
    Top = 464
    Width = 124
    Anchors = [akLeft, akBottom]
    Shape = bsTopLine
    Style = bsRaised
  end
  object lblWWW: TLabel
    Cursor = crHandPoint
    Left = 8
    Height = 13
    Hint = 'Go to TRegExpr web-page in Internet'
    Top = 448
    Width = 83
    Anchors = [akLeft, akBottom]
    Caption = ' TRegExpr home '
    Color = clBtnFace
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Transparent = False
    OnClick = lblWWWClick
  end
  object btnClose: TBitBtn
    Left = 414
    Height = 25
    Top = 441
    Width = 107
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Exit'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Glyph.Data = {
      76020000424D7602000000000000760000002800000040000000100000000100
      0400000000000002000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00330000000000
      033377777777777F3333330000000000033330000000000033333301BBBBBBBB
      03337F333333337F33333301BBBBBBBB0333301111111110333333011BBBBBBB
      03337F333333337F333333011BBBBBBB03333011111111103333330111BBBBBB
      03337F333333337F3333330111BBBBBB033330111111111033333301110BBBBB
      03337F333333337F33333301110BBBBB033330111111111033333301110BBBBB
      03337F333333337F33333301110BBBBB033330111111111033333301110BBBBB
      03337F3333333F7F33333301110BBBBB033330111111111033333301110BBBBB
      03337F333333737F33333301110BBBBB0333301111111B1033333301110BBBBB
      03337F333333337F33333301110BBBBB033330111111111033333301110BBBBB
      03337F33FFFFF37F33333301110BBBBB03333011111111103333330111B0BBBB
      03337F377777F37F3333330111B0BBBB03333011EEEEE11033333301110BBBBB
      03337F37FFF7F37F33333301110BBBBB03333011EEEEE11033333301110BBBBB
      03337F377777337F33333301110BBBBB03333011EEEEE11033333301E10BBBBB
      03337F333333337F33333301E10BBBBB033330111111111033333301EE0BBBBB
      03337FFFFFFFFF7F33333301EE0BBBBB03333011111111103333330000000000
      0333777777777773333333000000000003333000000000003333
    }
    NumGlyphs = 4
    OnClick = btnCloseClick
    ParentFont = False
    Spacing = -1
    TabOrder = 1
  end
  object grpRegExpr: TGroupBox
    Left = 2
    Height = 433
    Top = 0
    Width = 519
    Anchors = [akTop, akLeft, akRight, akBottom]
    ClientHeight = 415
    ClientWidth = 515
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 0
      Height = 383
      Top = 32
      Width = 515
      ActivePage = tabExpression
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tabExpression: TTabSheet
        Caption = ' &Expression '
        ClientHeight = 357
        ClientWidth = 507
        object Splitter3: TSplitter
          Cursor = crVSplit
          Left = 0
          Height = 5
          Top = 171
          Width = 507
          Align = alTop
          AutoSnap = False
          Beveled = True
          MinSize = 120
          ResizeAnchor = akTop
        end
        object pnlRegExpr: TPanel
          Left = 0
          Height = 171
          Top = 0
          Width = 507
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 171
          ClientWidth = 507
          TabOrder = 0
          object lblRegExpr: TLabel
            Left = 4
            Height = 13
            Top = 48
            Width = 90
            Caption = 'Regular expression'
            FocusControl = edRegExpr
            ParentColor = False
          end
          object lblRegExprUnbalancedBrackets: TLabel
            Left = 136
            Height = 13
            Top = 48
            Width = 180
            Caption = 'lblRegExprUnbalancedBrackets'
            Font.Color = clPurple
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            Layout = tlCenter
            ParentColor = False
            ParentFont = False
            OnDblClick = lblRegExprUnbalancedBracketsDblClick
          end
          object edSubExprs: TLabel
            Left = 4
            Height = 13
            Top = 150
            Width = 77
            Anchors = [akLeft, akBottom]
            Caption = 'Subexpressions:'
            FocusControl = cbSubExprs
            ParentColor = False
          end
          object btnViewPCode: TSpeedButton
            Left = 424
            Height = 26
            Hint = 'View compiled r.e. as "P-code"'#13#10'for TRegExpr debugging and'#13#10'internal engine undestanding'
            Top = 144
            Width = 82
            Anchors = [akRight, akBottom]
            Caption = 'P-code'
            Flat = True
            Glyph.Data = {
              76010000424D7601000000000000760000002800000020000000100000000100
              04000000000000010000120B0000120B00001000000000000000000000000000
              800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00370777033333
              3330337F3F7F33333F3787070003333707303F737773333373F7007703333330
              700077337F3333373777887007333337007733F773F333337733700070333333
              077037773733333F7F37703707333300080737F373333377737F003333333307
              78087733FFF3337FFF7F33300033330008073F3777F33F777F73073070370733
              078073F7F7FF73F37FF7700070007037007837773777F73377FF007777700730
              70007733FFF77F37377707700077033707307F37773F7FFF7337080777070003
              3330737F3F7F777F333778080707770333333F7F737F3F7F3333080787070003
              33337F73FF737773333307800077033333337337773373333333
            }
            NumGlyphs = 2
            Spacing = -1
            OnClick = btnViewPCodeClick
            ShowHint = True
            ParentShowHint = False
          end
          object gbModifiers: TGroupBox
            Left = 0
            Height = 44
            Top = 0
            Width = 504
            Anchors = [akTop, akLeft, akRight]
            Caption = ' Global modifiers '
            ClientHeight = 26
            ClientWidth = 500
            TabOrder = 0
            object chkModifierI: TCheckBox
              Left = 8
              Height = 29
              Hint = 'Case insensitive'
              Top = 0
              Width = 37
              Caption = '/i'
              OnClick = chkModifierIClick
              ParentShowHint = False
              ShowHint = True
              TabOrder = 0
              TabStop = False
            end
            object chkModifierR: TCheckBox
              Left = 256
              Height = 29
              Top = 0
              Width = 103
              Caption = 'Russian ranges'
              OnClick = chkModifierRClick
              TabOrder = 5
              TabStop = False
            end
            object chkModifierS: TCheckBox
              Left = 88
              Height = 29
              Hint = 'If on then . means any char'#13#10'If off then . doesnt include line separators'
              Top = 0
              Width = 40
              Caption = '/s'
              OnClick = chkModifierSClick
              ParentShowHint = False
              ShowHint = True
              TabOrder = 2
              TabStop = False
            end
            object chkModifierG: TCheckBox
              Left = 184
              Height = 29
              Hint = 'If Off then all operators '#13#10'work as non-greedy '#13#10'(* as *?, + as +? '#13#10'and so on)'
              Top = 0
              Width = 64
              Caption = 'Greedy'
              OnClick = chkModifierGClick
              ParentShowHint = False
              ShowHint = True
              TabOrder = 4
              TabStop = False
            end
            object chkModifierM: TCheckBox
              Left = 48
              Height = 29
              Hint = 'If ON then ^ / $ match'#13#10'every embedded line start / end,'#13#10'if OFF, then only beginning / end'#13#10'of whole text'
              Top = 0
              Width = 43
              Caption = '/m'
              OnClick = chkModifierMClick
              ParentShowHint = False
              ShowHint = True
              TabOrder = 1
              TabStop = False
            end
            object chkModifierX: TCheckBox
              Left = 128
              Height = 29
              Hint = 'If ON then eXtended comment syntax available'
              Top = 0
              Width = 40
              Caption = '/x'
              OnClick = chkModifierXClick
              ParentShowHint = False
              ShowHint = True
              TabOrder = 3
              TabStop = False
            end
          end
          object edRegExpr: TMemo
            Left = 0
            Height = 79
            Top = 64
            Width = 507
            Anchors = [akTop, akLeft, akRight, akBottom]
            Font.CharSet = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Lines.Strings = (
              'edRegExpr'
            )
            OnChange = edRegExprChange
            OnClick = edRegExprClick
            OnKeyDown = edRegExprKeyDown
            OnKeyUp = edRegExprKeyDown
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
          end
          object cbSubExprs: TComboBox
            Left = 106
            Height = 21
            Hint = 'Subexpressions'
            Top = 146
            Width = 315
            Anchors = [akLeft, akRight, akBottom]
            Color = clBtnFace
            ItemHeight = 13
            OnClick = cbSubExprsClick
            ParentShowHint = False
            ShowHint = True
            Style = csDropDownList
            TabOrder = 2
            TabStop = False
          end
        end
        object pnlInputStrings: TPanel
          Left = 0
          Height = 181
          Top = 176
          Width = 507
          Align = alClient
          BevelOuter = bvNone
          ClientHeight = 181
          ClientWidth = 507
          TabOrder = 1
          object lblInputString: TLabel
            Left = 4
            Height = 13
            Top = 4
            Width = 55
            Caption = 'Input string:'
            FocusControl = edInputString
            ParentColor = False
          end
          object lblInputStringPos: TLabel
            Left = 88
            Height = 13
            Top = 4
            Width = 82
            Caption = 'Current selection:'
            FocusControl = edInputStringPos
            ParentColor = False
          end
          object lblTestResult: TLabel
            Left = 256
            Height = 40
            Hint = 'Last Exec* result and'#13#10'positions of r.e. and'#13#10'subexpressions'#13#10'in input string'
            Top = 141
            Width = 252
            Anchors = [akLeft, akRight, akBottom]
            AutoSize = False
            Caption = 'String is not tested'
            Color = clBtnFace
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Transparent = False
            WordWrap = True
          end
          object lblStopWatch: TLabel
            Left = 248
            Height = 17
            Top = 4
            Width = 249
            Alignment = taRightJustify
            Anchors = [akTop, akLeft, akRight]
            AutoSize = False
            Caption = 'lblStopWatch'
            Font.Color = clPurple
            Font.Height = -10
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
            Visible = False
          end
          object edInputString: TMemo
            Left = 0
            Height = 115
            Top = 24
            Width = 507
            Anchors = [akTop, akLeft, akRight, akBottom]
            Lines.Strings = (
              'edInputString'
            )
            OnChange = edInputStringClick
            OnClick = edInputStringClick
            OnKeyDown = edInputStringKeyDown
            OnKeyUp = edInputStringKeyDown
            OnMouseDown = edInputStringMouseDown
            OnMouseMove = edInputStringMouseMove
            OnMouseUp = edInputStringMouseDown
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
          object edInputStringPos: TEdit
            Left = 192
            Height = 21
            Top = 0
            Width = 49
            Color = clBtnFace
            ReadOnly = True
            TabStop = False
            TabOrder = 1
            Text = 'edInputStringPos'
          end
          object btnTestString: TBitBtn
            Left = 0
            Height = 33
            Hint = 'Exec r.e. for input string'
            Top = 145
            Width = 73
            Anchors = [akLeft, akBottom]
            Caption = 'E&xec'
            Glyph.Data = {
              66010000424D6601000000000000760000002800000014000000140000000100
              040000000000F000000000000000000000001000000010000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
              3333333300003333333333333300333300003333333333333333333300003333
              3333333333003333000033033333333333003333000030003333333333300333
              00000F000333333330330033000030F000333333300330030000330F00000007
              3300003300003330F0078887033333330000333300788FF87033333300003333
              0788888F877333330000333308888888F80333330000333307888888F8033333
              00003333078FF8888803333300003333777FF888877333330000333330778888
              7033333300003333330777770333333300003333333700073333333300003333
              33333333333333330000
            }
            OnClick = btnTestStringClick
            ParentShowHint = False
            ShowHint = True
            Spacing = -1
            TabOrder = 2
          end
          object btnExecNext: TBitBtn
            Left = 73
            Height = 33
            Hint = 'Exec from last match'
            Top = 145
            Width = 98
            Anchors = [akLeft, akBottom]
            Caption = 'Exec&Next'
            Glyph.Data = {
              66010000424D6601000000000000760000002800000014000000140000000100
              040000000000F000000000000000000000001000000010000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
              3333333300003333333333333333333300003333333330033003300300003333
              3333300330033003000033303333333333333333000033000333333333333333
              000030F000333333333333330000330F000333333333333300003330F0000000
              73333333000033330F0078887033333300003333300788FF8703333300003333
              30788888F877333300003333308888888F80333300003333307888888F803333
              000033333078FF8888803333000033333777FF88887733330000333333077888
              8703333300003333333077777033333300003333333370007333333300003333
              33333333333333330000
            }
            OnClick = btnExecNextClick
            ParentShowHint = False
            ShowHint = True
            Spacing = -1
            TabOrder = 3
          end
          object btnFindRegExprInFile: TBitBtn
            Left = 176
            Height = 33
            Top = 145
            Width = 75
            Anchors = [akLeft, akBottom]
            Caption = '&File'
            Glyph.Data = {
              42010000424D4201000000000000760000002800000011000000110000000100
              040000000000CC00000000000000000000001000000010000000000000000000
              BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
              77777000000070000000007777777000000070FFFFFFF07777700000000070F7
              7777F07777000000000070F77777F07770007000000070F77780008700077000
              000070F7700FFF0000777000000070F708FFFF0807777000000070F80E000F07
              08777000000070F0EFEFEF0770777000000070F0F0000F077077700000007000
              EFEFFF0770777000000077780000000708777000000077770077777807777000
              0000777770077700777770000000777777800087777770000000777777777777
              777770000000
            }
            Spacing = -1
            TabOrder = 4
          end
          object cbSubStrs: TComboBox
            Left = 256
            Height = 21
            Top = 157
            Width = 251
            Anchors = [akLeft, akRight, akBottom]
            Color = clBtnFace
            ItemHeight = 13
            OnClick = cbSubStrsClick
            Style = csDropDownList
            TabOrder = 5
            Visible = False
          end
        end
      end
      object tabSubstitute: TTabSheet
        Caption = ' &Substitute '
        ClientHeight = 357
        ClientWidth = 507
        ImageIndex = 1
        object Splitter2: TSplitter
          Cursor = crVSplit
          Left = 0
          Height = 5
          Top = 129
          Width = 507
          Align = alTop
          AutoSnap = False
          Beveled = True
          MinSize = 50
          ResizeAnchor = akTop
        end
        object pnlSubstitutionComment: TPanel
          Left = 0
          Height = 39
          Top = 0
          Width = 507
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 39
          ClientWidth = 507
          Color = clBtnShadow
          ParentColor = False
          TabOrder = 0
          object lblSubstitutionComment: TLabel
            Left = 8
            Height = 30
            Top = 6
            Width = 402
            Anchors = [akTop, akLeft, akRight, akBottom]
            AutoSize = False
            Caption = 'Use $&& in template for whole r.e. substitution and $n or ${n} for substitute subexpression of r.e. number n.'
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            ParentColor = False
            ParentFont = False
            WordWrap = True
          end
        end
        object pnlSubstitutionTemplate: TPanel
          Left = 0
          Height = 90
          Top = 39
          Width = 507
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 90
          ClientWidth = 507
          TabOrder = 1
          object lblSubstitutionTemplate: TLabel
            Left = 4
            Height = 13
            Top = 2
            Width = 72
            Caption = 'Template string'
            FocusControl = memSubstitutionTemplate
            ParentColor = False
          end
          object memSubstitutionTemplate: TMemo
            Left = 1
            Height = 71
            Top = 18
            Width = 505
            Anchors = [akTop, akLeft, akRight, akBottom]
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object pnlSubstitutionResult: TPanel
          Left = 0
          Height = 223
          Top = 134
          Width = 507
          Align = alClient
          BevelOuter = bvNone
          ClientHeight = 223
          ClientWidth = 507
          TabOrder = 2
          object lblSubstitutionResult: TLabel
            Left = 4
            Height = 13
            Top = 2
            Width = 83
            Caption = 'Substitution result'
            FocusControl = memSubstitutionResult
            ParentColor = False
          end
          object memSubstitutionResult: TMemo
            Left = 0
            Height = 189
            Top = 19
            Width = 507
            Anchors = [akTop, akLeft, akRight, akBottom]
            Color = clBtnFace
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
            TabStop = False
          end
        end
      end
      object tabReplace: TTabSheet
        Caption = ' &Replace '
        ClientHeight = 357
        ClientWidth = 507
        ImageIndex = 2
        object Splitter1: TSplitter
          Cursor = crVSplit
          Left = 0
          Height = 5
          Top = 137
          Width = 507
          Align = alTop
          AutoSnap = False
          Beveled = True
          MinSize = 50
          ResizeAnchor = akTop
        end
        object pnlReplaceComment: TPanel
          Left = 0
          Height = 39
          Top = 0
          Width = 507
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 39
          ClientWidth = 507
          Color = clBtnShadow
          ParentColor = False
          TabOrder = 0
          object lblReplaceComment: TLabel
            Left = 8
            Height = 30
            Top = 6
            Width = 497
            Anchors = [akTop, akLeft, akRight, akBottom]
            AutoSize = False
            Caption = 'Replace all entrances of r.e. in input string with another string (it may be template for substitution).'#13#10'Note: Replace uses Exec* calls, so Match* properties will be undefined after it.'
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            ParentColor = False
            ParentFont = False
            WordWrap = True
          end
        end
        object pnlReplaceTemplate: TPanel
          Left = 0
          Height = 98
          Top = 39
          Width = 507
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 98
          ClientWidth = 507
          TabOrder = 1
          object lblReplaceString: TLabel
            Left = 4
            Height = 13
            Top = 2
            Width = 80
            Caption = 'String for replace'
            FocusControl = edReplaceString
            ParentColor = False
          end
          object edReplaceString: TMemo
            Left = 0
            Height = 80
            Top = 18
            Width = 507
            Anchors = [akTop, akLeft, akRight, akBottom]
            ScrollBars = ssVertical
            TabOrder = 0
          end
          object chkUseSubstitution: TCheckBox
            Left = 144
            Height = 29
            Top = 0
            Width = 162
            Caption = 'Use as substitution template'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
        end
        object pnlReplaceResult: TPanel
          Left = 0
          Height = 215
          Top = 142
          Width = 507
          Align = alClient
          BevelOuter = bvNone
          ClientHeight = 215
          ClientWidth = 507
          TabOrder = 2
          object lblReplaceResult: TLabel
            Left = 4
            Height = 13
            Top = 0
            Width = 68
            Caption = 'Replace result'
            FocusControl = memReplaceResult
            ParentColor = False
          end
          object memReplaceResult: TMemo
            Left = 0
            Height = 159
            Top = 16
            Width = 507
            Anchors = [akTop, akLeft, akRight, akBottom]
            Color = clBtnFace
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
            TabStop = False
          end
          object btnReplace: TBitBtn
            Left = 408
            Height = 33
            Hint = 'Exec r.e. for input string'
            Top = 179
            Width = 97
            Anchors = [akRight, akBottom]
            Caption = '&Replace'
            Glyph.Data = {
              06020000424D0602000000000000760000002800000028000000140000000100
              0400000000009001000000000000000000001000000010000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
              3333333333333333333333333333333333333333333333333333333333333333
              33333333388888888883333333333FFFFFFFFFF3333333330000000000833333
              33338888888888F3333333330FFFFFFFF083333333338F33333F38F333333333
              0FFFF00FF083333333338F33388FF8F3333333330FFF0000F083333333338F33
              8888F8F3333333330FF80000F083333333338F338888F8F3333333330F800000
              0083333333338F38888888F3333333330F000F000083333333338F88838888F3
              333333330F00FFF00083333333338F88333888F3333333330FFFFF0000833333
              33338F33338888FF333333330FFFFF0F0003333333338F33338F888F33333333
              0FFFFF003008333333338FFFFF88388F33333333000000033800333333338888
              88833388F333333333333333330083333333333333333388FF33333333333333
              3330033333333333333333388F33333333333333333303333333333333333333
              8333333333333333333333333333333333333333333333333333333333333333
              33333333333333333333
            }
            NumGlyphs = 2
            OnClick = btnReplaceClick
            ParentShowHint = False
            ShowHint = True
            Spacing = -1
            TabOrder = 1
          end
        end
      end
      object tabSplit: TTabSheet
        Caption = ' &Split '
        ClientHeight = 357
        ClientWidth = 507
        ImageIndex = 3
        object lblSplitResult: TLabel
          Left = 4
          Height = 13
          Top = 40
          Width = 48
          Caption = 'Split result'
          ParentColor = False
        end
        object btnSplit: TBitBtn
          Left = 407
          Height = 33
          Hint = 'Exec r.e. for input string'
          Top = 320
          Width = 97
          Anchors = [akRight, akBottom]
          Caption = '&Split'
          Default = True
          Glyph.Data = {
            06020000424D0602000000000000760000002800000028000000140000000100
            0400000000009001000000000000000000001000000010000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333333333333333333333333333
            33333333388888888883333333333FFFFFFFFFF3333333330000000000833333
            33338888888888F3333333330FFFFFFFF083333333338F33333F38F333333333
            0FFFF00FF083333333338F33388FF8F3333333330FFF0000F083333333338F33
            8888F8F3333333330FF80000F083333333338F338888F8F3333333330F800000
            0083333333338F38888888F3333333330F000F000083333333338F88838888F3
            333333330F00FFF00083333333338F88333888F3333333330FFFFF0000833333
            33338F33338888FF333333330FFFFF0F0003333333338F33338F888F33333333
            0FFFFF003008333333338FFFFF88388F33333333000000033800333333338888
            88833388F333333333333333330083333333333333333388FF33333333333333
            3330033333333333333333388F33333333333333333303333333333333333333
            8333333333333333333333333333333333333333333333333333333333333333
            33333333333333333333
          }
          NumGlyphs = 2
          OnClick = btnSplitClick
          ParentShowHint = False
          ShowHint = True
          Spacing = -1
          TabOrder = 0
        end
        object memSplitResult: TMemo
          Left = 0
          Height = 263
          Top = 56
          Width = 504
          Anchors = [akTop, akLeft, akRight, akBottom]
          Color = clBtnFace
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
          TabStop = False
        end
        object pnlSplitComment: TPanel
          Left = 0
          Height = 39
          Top = 0
          Width = 507
          Align = alTop
          BevelOuter = bvNone
          ClientHeight = 39
          ClientWidth = 507
          Color = clBtnShadow
          ParentColor = False
          TabOrder = 2
          object lblSplitComment: TLabel
            Left = 8
            Height = 30
            Top = 6
            Width = 402
            Anchors = [akTop, akLeft, akRight, akBottom]
            AutoSize = False
            Caption = 'Split input string by r.e. entrances.'#13#10'Note: Split uses Exec* calls, so Match* properties will be undefined after it.'
            Font.Color = clWhite
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            ParentColor = False
            ParentFont = False
            WordWrap = True
          end
        end
      end
    end
    object pnlTopExamples: TPanel
      Left = 0
      Height = 32
      Top = 0
      Width = 515
      Align = alTop
      BevelOuter = bvNone
      ClientHeight = 32
      ClientWidth = 515
      TabOrder = 1
      object btnGetRE: TSpeedButton
        Left = 8
        Height = 28
        Hint = 'Load example from "r.e. repository"'
        Top = 0
        Width = 121
        Caption = 'r.e. repository >>'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
          55555555FFFFFFFF5555555000000005555555577777777FF555550999999900
          55555575555555775F55509999999901055557F55555557F75F5001111111101
          105577FFFFFFFF7FF75F00000000000011057777777777775F755070FFFFFF0F
          01105777F555557F75F75500FFFFFF0FF0105577F555FF7F57575550FF700008
          8F0055575FF7777555775555000888888F005555777FFFFFFF77555550000000
          0F055555577777777F7F555550FFFFFF0F05555557F5FFF57F7F555550F000FF
          0005555557F777557775555550FFFFFF0555555557F555FF7F55555550FF7000
          05555555575FF777755555555500055555555555557775555555
        }
        NumGlyphs = 2
        Spacing = -1
        OnClick = btnGetREClick
        ShowHint = True
        ParentShowHint = False
      end
    end
    object pnlRepositoryHint: TPanel
      Left = 140
      Height = 32
      Top = 0
      Width = 369
      Anchors = [akTop, akLeft, akRight]
      BevelOuter = bvNone
      ClientHeight = 32
      ClientWidth = 369
      Color = clBtnShadow
      Font.Color = clWhite
      Font.Height = -10
      Font.Name = 'MS Sans Serif'
      ParentColor = False
      ParentFont = False
      TabOrder = 2
      object Label1: TLabel
        Left = 32
        Height = 28
        Top = 0
        Width = 338
        Anchors = [akTop, akLeft, akRight, akBottom]
        AutoSize = False
        Caption = 'You can select one of ready-to-use regular expressions from the repository'
        ParentColor = False
        WordWrap = True
      end
      object Image1: TImage
        Left = 8
        Height = 16
        Top = 8
        Width = 16
        AutoSize = True
        Picture.Data = {
          055449636F6E3E01000000000100010010101000000000002801000016000000
          2800000010000000200000000100040000000000C00000000000000000000000
          0000000000000000000000000000800000800000008080008000000080008000
          8080000080808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00
          FFFF0000FFFFFF00000000000000000000000000000000000000000800000000
          00B00B000B00B000000BBB0B0BBB0000000BBB0F0BBB000000BBB0F7F0BBB000
          00BB0FB7BF0BB000BBBB0BF7FB0BBBB000BB0FBFBF0BB00000BBB0FBF0BBB000
          000BBB000BBB0000000BBBBBBBBB000000B00BBBBB00B0000000000B00000000
          0000000B00000000FEFF0000FC7F0000FC7F0000D8370000E00F0000E00F0000
          C0070000C007000000010000C0070000C0070000E00F0000E00F0000D8370000
          FEFF0000FEFF0000
        }
      end
    end
  end
end
