object frameParams: TframeParams
  Left = 0
  Top = 0
  Width = 297
  Height = 80
  VertScrollBar.Smooth = True
  VertScrollBar.Tracking = True
  AutoScroll = True
  TabOrder = 0
  object lblNumber: TLabel
    Left = 255
    Top = 2
    Width = 41
    Height = 13
    Caption = 'Number:'
  end
  object spnCount: TJvSpinEdit
    Left = 255
    Top = 21
    Width = 41
    Height = 21
    ButtonKind = bkClassic
    MaxValue = 7.000000000000000000
    Value = 1.000000000000000000
    TabOrder = 0
    OnChange = spnCountChange
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 249
    Height = 73
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 17
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object Label2: TLabel
      Left = 108
      Top = 44
      Width = 28
      Height = 13
      Caption = 'Type:'
    end
    object cbxTypes: TComboBox
      Left = 142
      Top = 41
      Width = 99
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      OnChange = headerChange
    end
    object txtCodeName: TJvValidateEdit
      Left = 45
      Top = 14
      Width = 196
      Height = 21
      Alignment = taLeftJustify
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      DisplayFormat = dfAlphaNumeric
      TabOrder = 0
      OnChange = headerChange
    end
    object chkVar: TCheckBox
      Left = 5
      Top = 41
      Width = 44
      Height = 17
      Caption = 'var'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = headerChange
    end
  end
end
