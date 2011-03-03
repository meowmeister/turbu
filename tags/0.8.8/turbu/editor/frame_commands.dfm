object frameHeroCommands: TframeHeroCommands
  Left = 0
  Top = 0
  Width = 158
  Height = 188
  VertScrollBar.Tracking = True
  AutoScroll = True
  TabOrder = 0
  TabStop = True
  object lblNumber: TLabel
    Left = 110
    Top = 3
    Width = 41
    Height = 13
    Caption = 'Number:'
  end
  object spnCount: TJvSpinEdit
    Left = 110
    Top = 22
    Width = 41
    Height = 21
    MaxValue = 7.000000000000000000
    Value = 1.000000000000000000
    TabOrder = 0
    OnChange = spnCountChange
  end
  object cbxCommand1: TDBLookupComboBox
    Left = 8
    Top = 6
    Width = 96
    Height = 21
    DataField = 'command[1]'
    DataSource = DataSource
    KeyField = 'id'
    ListField = 'name'
    ListSource = dsCommands
    TabOrder = 1
  end
  object DataSource: TDataSource
    Left = 120
    Top = 56
  end
  object dsCommands: TDataSource
    DataSet = dmDatabase.commands
    Left = 120
    Top = 104
  end
end
