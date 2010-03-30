object frmAlgorithmEditor: TfrmAlgorithmEditor
  Left = 381
  Top = 76
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Algorithm editor'
  ClientHeight = 670
  ClientWidth = 652
  Color = clBtnFace
  Constraints.MinWidth = 571
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    652
    670)
  PixelsPerInch = 120
  TextHeight = 16
  object btnOK: TButton
    Left = 443
    Top = 629
    Width = 90
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 550
    Top = 629
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnUnitView: TButton
    Left = 208
    Top = 629
    Width = 128
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = '&View full unit...'
    Enabled = False
    TabOrder = 0
  end
  object grpEditor: TGroupBox
    Left = 10
    Top = 10
    Width = 632
    Height = 610
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    DesignSize = (
      632
      610)
    object grpFuncNames: TGroupBox
      Left = 11
      Top = 17
      Width = 612
      Height = 52
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Function names:'
      TabOrder = 0
      DesignSize = (
        612
        52)
      object lblDesign: TLabel
        Left = 20
        Top = 20
        Width = 46
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Design:'
      end
      object lblCode: TLabel
        Left = 315
        Top = 20
        Width = 36
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akRight]
        Caption = 'Code:'
      end
      object txtDesignName: TEdit
        Left = 71
        Top = 17
        Width = 237
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object txtCodeName: TMaskEdit
        Left = 354
        Top = 17
        Width = 248
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        EditMask = 'Laaaaaaaaaaaaaaaaaaaaaaaaaaaaa;0; '
        MaxLength = 30
        TabOrder = 1
      end
    end
    object txtHeader: TEdit
      Left = 11
      Top = 148
      Width = 612
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 1
      TextHint = 'This is the function declaration.'
    end
    object Button1: TButton
      Left = 158
      Top = 76
      Width = 92
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Button1'
      TabOrder = 2
      OnClick = Button1Click
    end
    object txtEditor: TJvHLEditor
      Left = 11
      Top = 187
      Width = 612
      Height = 414
      Cursor = crIBeam
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Completion.ItemHeight = 13
      Completion.CRLF = '/n'
      Completion.Separator = '='
      TabStops = '3 5'
      CursorBeyondEOF = True
      BracketHighlighting.StringEscape = #39#39
      OnKeyDown = txtEditorKeyDown
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Courier New'
      Font.Style = []
      Anchors = [akLeft, akTop, akRight, akBottom]
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
