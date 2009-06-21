object frmConversionReport: TfrmConversionReport
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Converting...'
  ClientHeight = 282
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 16
    Width = 290
    Height = 217
    TabOrder = 0
    object lblProgress: TLabel
      Left = 110
      Top = 43
      Width = 46
      Height = 13
      Caption = 'Progress:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lblSteps: TLabel
      Left = 110
      Top = 93
      Width = 58
      Height = 13
      Caption = 'Conversion:'
    end
    object lblStatusLabel: TLabel
      Left = 40
      Top = 16
      Width = 35
      Height = 13
      Caption = 'Status:'
    end
    object lblCurrentStatus: TLabel
      Left = 159
      Top = 16
      Width = 90
      Height = 13
      Alignment = taRightJustify
      Caption = 'Converting project'
    end
    object prgConversion: TProgressBar
      Left = 40
      Top = 62
      Width = 209
      Height = 25
      Max = 14
      Smooth = True
      Step = 1
      TabOrder = 0
    end
    object prgSteps: TProgressBar
      Left = 40
      Top = 112
      Width = 209
      Height = 25
      Max = 14
      Smooth = True
      Step = 1
      TabOrder = 1
    end
    object pnlHints: TPanel
      Left = 16
      Top = 168
      Width = 73
      Height = 25
      BevelOuter = bvLowered
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      TabOrder = 2
      object Label1: TLabel
        Left = 5
        Top = 5
        Width = 28
        Height = 13
        Align = alLeft
        Caption = 'Hints:'
      end
      object lblHintCount: TLabel
        Left = 62
        Top = 5
        Width = 6
        Height = 13
        Align = alRight
        Caption = '0'
      end
    end
    object pnlWarnings: TPanel
      Left = 105
      Top = 168
      Width = 81
      Height = 25
      BevelOuter = bvLowered
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      TabOrder = 3
      object Label2: TLabel
        Left = 5
        Top = 5
        Width = 32
        Height = 13
        Align = alLeft
        Caption = 'Notes:'
      end
      object lblWarningCount: TLabel
        Left = 70
        Top = 5
        Width = 6
        Height = 13
        Align = alRight
        Caption = '0'
      end
    end
    object pnlErrors: TPanel
      Left = 192
      Top = 167
      Width = 81
      Height = 25
      BevelOuter = bvLowered
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      TabOrder = 4
      object Label3: TLabel
        Left = 5
        Top = 5
        Width = 29
        Height = 13
        Align = alLeft
        Caption = 'Errors'
      end
      object lblErrorCount: TLabel
        Left = 70
        Top = 5
        Width = 6
        Height = 13
        Align = alRight
        Caption = '0'
      end
    end
  end
  object btnDone: TButton
    Left = 114
    Top = 248
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
