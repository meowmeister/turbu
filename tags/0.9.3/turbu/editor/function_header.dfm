object frmFuncHeader: TfrmFuncHeader
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Function Declaration'
  ClientHeight = 659
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormShow
  OnShow = FormShow
  DesignSize = (
    462
    659)
  PixelsPerInch = 120
  TextHeight = 17
  object lblName: TLabel
    Left = 10
    Top = 14
    Width = 40
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Name:'
  end
  object Label1: TLabel
    Left = 133
    Top = 67
    Width = 82
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Return Type:'
  end
  object cbxResult: TComboBox
    Left = 225
    Top = 63
    Width = 226
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
  end
  object cbxProcType: TComboBox
    Left = 10
    Top = 63
    Width = 106
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Style = csDropDownList
    Anchors = [akLeft, akTop, akBottom]
    ItemIndex = 0
    TabOrder = 1
    Text = 'Function'
    OnChange = cbxProcTypeChange
    Items.Strings = (
      'Function'
      'Procedure')
  end
  object grpParams: TGroupBox
    Left = 14
    Top = 115
    Width = 437
    Height = 538
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Parameters'
    TabOrder = 2
    DesignSize = (
      437
      538)
    inline frameParams: TframeParams
      Left = 10
      Top = 18
      Width = 423
      Height = 509
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      VertScrollBar.Smooth = True
      VertScrollBar.Tracking = True
      Anchors = [akLeft, akTop, akBottom]
      AutoScroll = True
      TabOrder = 0
      inherited lblNumber: TLabel
        Left = 333
        Top = 3
        Width = 54
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
      end
      inherited spnCount: TJvSpinEdit
        Left = 333
        Top = 27
        Width = 54
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
      end
      inherited GroupBox1: TGroupBox
        Width = 326
        Height = 95
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        inherited Label1: TLabel
          Left = 10
          Top = 22
          Width = 40
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited Label2: TLabel
          Left = 141
          Top = 58
          Width = 36
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited cbxTypes: TComboBox
          Left = 186
          Top = 54
          Width = 129
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited txtCodeName: TJvValidateEdit
          Left = 59
          Top = 18
          Width = 256
          Height = 25
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        inherited chkVar: TCheckBox
          Left = 7
          Top = 54
          Width = 57
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Font.Height = -15
        end
      end
    end
  end
  object txtCodeName: TJvValidateEdit
    Left = 59
    Top = 10
    Width = 388
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taLeftJustify
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    DisplayFormat = dfAlphaNumeric
    EditText = '0'
    TabOrder = 3
  end
end
