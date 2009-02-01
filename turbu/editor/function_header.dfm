object frmFuncHeader: TfrmFuncHeader
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Function Declaration'
  ClientHeight = 504
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormShow
  OnShow = FormShow
  DesignSize = (
    353
    504)
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 8
    Top = 11
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label1: TLabel
    Left = 102
    Top = 51
    Width = 64
    Height = 13
    Caption = 'Return Type:'
  end
  object cbxResult: TComboBox
    Left = 172
    Top = 48
    Width = 173
    Height = 21
    ItemHeight = 0
    TabOrder = 0
  end
  object cbxProcType: TComboBox
    Left = 8
    Top = 48
    Width = 81
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = 'Function'
    OnChange = cbxProcTypeChange
    Items.Strings = (
      'Function'
      'Procedure')
  end
  object grpParams: TGroupBox
    Left = 11
    Top = 88
    Width = 334
    Height = 411
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Parameters'
    TabOrder = 2
    DesignSize = (
      334
      411)
    inline frameParams: TframeParams
      Left = 8
      Top = 14
      Width = 323
      Height = 389
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Anchors = [akLeft, akTop, akBottom]
      AutoScroll = True
      TabOrder = 0
      ExplicitLeft = 8
      ExplicitTop = 14
      ExplicitWidth = 323
      ExplicitHeight = 389
    end
  end
  object txtCodeName: TJvValidateEdit
    Left = 45
    Top = 8
    Width = 297
    Height = 21
    Alignment = taLeftJustify
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    DisplayFormat = dfIdentifier
    TabOrder = 3
  end
end
