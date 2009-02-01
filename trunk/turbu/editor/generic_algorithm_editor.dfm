object frmAlgorithmEditor: TfrmAlgorithmEditor
  Left = 381
  Top = 76
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Algorithm editor'
  ClientHeight = 544
  ClientWidth = 530
  Color = clBtnFace
  Constraints.MinWidth = 464
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    530
    544)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 360
    Top = 511
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 447
    Top = 511
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnUnitView: TButton
    Left = 169
    Top = 511
    Width = 104
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&View full unit...'
    Enabled = False
    TabOrder = 0
  end
  object grpEditor: TGroupBox
    Left = 8
    Top = 8
    Width = 514
    Height = 496
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    DesignSize = (
      514
      496)
    object Label1: TLabel
      Left = 25
      Top = 90
      Width = 343
      Height = 24
      Caption = 'THIS UNIT IS CURRENTLY BROKEN'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object grpFuncNames: TGroupBox
      Left = 9
      Top = 14
      Width = 497
      Height = 42
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Function names:'
      TabOrder = 0
      DesignSize = (
        497
        42)
      object lblDesign: TLabel
        Left = 16
        Top = 16
        Width = 36
        Height = 13
        Caption = 'Design:'
      end
      object lblCode: TLabel
        Left = 256
        Top = 16
        Width = 28
        Height = 13
        Anchors = [akLeft, akRight]
        Caption = 'Code:'
      end
      object txtDesignName: TEdit
        Left = 58
        Top = 14
        Width = 192
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object txtCodeName: TMaskEdit
        Left = 288
        Top = 14
        Width = 201
        Height = 21
        EditMask = 'Laaaaaaaaaaaaaaaaaaaaaaaaaaaaa;0; '
        MaxLength = 30
        TabOrder = 1
      end
    end
    object txtHeader: TEdit
      Left = 9
      Top = 120
      Width = 497
      Height = 21
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 1
      TextHint = 'This is the function declaration.'
    end
    object Button1: TButton
      Left = 128
      Top = 62
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 2
      OnClick = Button1Click
    end
    object txtEditor: TJvHLEditor
      Left = 9
      Top = 152
      Width = 497
      Height = 336
      Cursor = crIBeam
      GutterWidth = 0
      RightMarginColor = clSilver
      Completion.ItemHeight = 13
      Completion.Interval = 800
      Completion.ListBoxStyle = lbStandard
      Completion.CaretChar = '|'
      Completion.CRLF = '/n'
      Completion.Separator = '='
      TabStops = '3 5'
      CursorBeyondEOF = True
      BracketHighlighting.StringEscape = #39#39
      SelForeColor = clHighlightText
      SelBackColor = clHighlight
      OnKeyDown = txtEditorKeyDown
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabStop = True
      Anchors = [akLeft, akTop, akRight, akBottom]
      UseDockManager = False
      Colors.Comment.Style = [fsItalic]
      Colors.Comment.ForeColor = clNavy
      Colors.Comment.BackColor = clWindow
      Colors.Number.ForeColor = clNavy
      Colors.Number.BackColor = clWindow
      Colors.Strings.ForeColor = clBlue
      Colors.Strings.BackColor = clWindow
      Colors.Symbol.ForeColor = clBlack
      Colors.Symbol.BackColor = clWindow
      Colors.Reserved.Style = [fsBold]
      Colors.Reserved.ForeColor = clBlack
      Colors.Reserved.BackColor = clWindow
      Colors.Identifier.ForeColor = clBlack
      Colors.Identifier.BackColor = clWindow
      Colors.Preproc.ForeColor = clGreen
      Colors.Preproc.BackColor = clWindow
      Colors.FunctionCall.ForeColor = clWindowText
      Colors.FunctionCall.BackColor = clWindow
      Colors.Declaration.ForeColor = clWindowText
      Colors.Declaration.BackColor = clWindow
      Colors.Statement.Style = [fsBold]
      Colors.Statement.ForeColor = clWindowText
      Colors.Statement.BackColor = clWindow
      Colors.PlainText.ForeColor = clWindowText
      Colors.PlainText.BackColor = clWindow
    end
  end
  object dsRanges: TDataSource
    DataSet = dmDatabase.scriptRange
    Left = 80
    Top = 504
  end
end
