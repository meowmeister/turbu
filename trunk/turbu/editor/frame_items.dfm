object frameItems: TframeItems
  Left = 0
  Top = 0
  Width = 785
  Height = 538
  TabOrder = 0
  object pnlItems: TPanel
    Left = 0
    Top = 0
    Width = 785
    Height = 459
    TabOrder = 0
    object DBGrid1: TDBGrid
      Left = 8
      Top = 56
      Width = 753
      Height = 353
      DataSource = dsWeapons
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object cboDatasets: TComboBox
      Left = 280
      Top = 415
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      Text = 'cboDatasets'
      OnChange = cboDatasetsChange
    end
  end
  object dsWeapons: TDataSource
    DataSet = dmDatabase.items_book
    Left = 104
    Top = 488
  end
end
