object frameVocab: TframeVocab
  Left = 0
  Top = 0
  Width = 1029
  Height = 588
  TabOrder = 0
  object pnlVocab: TPanel
    Left = 0
    Top = 0
    Width = 1029
    Height = 588
    Align = alClient
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 513
      Top = 1
      Height = 586
    end
    object pnlSysVocab: TPanel
      Left = 1
      Top = 1
      Width = 512
      Height = 586
      Align = alLeft
      TabOrder = 0
      object lstSysVocab: TRpgListGrid
        Left = 1
        Top = 32
        Width = 510
        Height = 553
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Constraints.MinWidth = 100
        DataSource = dsSysVocab
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Key'
            Title.Caption = 'Name'
            Width = 150
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Value'
            Width = 350
            Visible = True
          end>
      end
      object StaticText1: TStaticText
        Left = 8
        Top = 8
        Width = 118
        Height = 20
        Caption = 'System Vocabulary:'
        TabOrder = 1
      end
    end
    object pnlCustomVocab: TPanel
      Left = 516
      Top = 1
      Width = 512
      Height = 586
      Align = alClient
      TabOrder = 1
      object lstCustomVocab: TRpgListGrid
        Left = 1
        Top = 32
        Width = 510
        Height = 553
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Constraints.MinWidth = 100
        DataSource = dsCustomVocab
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'Key'
            Title.Caption = 'Name'
            Width = 150
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Value'
            Width = 350
            Visible = True
          end>
      end
      object StaticText2: TStaticText
        Left = 8
        Top = 8
        Width = 119
        Height = 20
        Caption = 'Custom Vocabulary:'
        TabOrder = 1
      end
      object StaticText3: TStaticText
        Left = 332
        Top = 8
        Width = 29
        Height = 20
        Caption = 'New'
        TabOrder = 2
      end
      object StaticText4: TStaticText
        Left = 432
        Top = 8
        Width = 40
        Height = 20
        Caption = 'Delete'
        TabOrder = 3
      end
    end
  end
  object DBNavigator1: TDBNavigator
    Left = 888
    Top = 4
    Width = 54
    Height = 25
    DataSource = dsCustomVocab
    VisibleButtons = [nbInsert, nbDelete]
    TabOrder = 1
  end
  object dsSysVocab: TDataSource
    DataSet = dmDatabase.Vocab
    Left = 152
    Top = 352
  end
  object dsCustomVocab: TDataSource
    DataSet = dmDatabase.CustomVocab
    Left = 640
    Top = 352
  end
end
