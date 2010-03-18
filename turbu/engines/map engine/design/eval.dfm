object frmMapProperties: TfrmMapProperties
  Left = 12
  Top = 66
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Map Properties'
  ClientHeight = 567
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object btnOK: TButton
    Left = 215
    Top = 523
    Width = 90
    Height = 35
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object pnlBack: TPanel
    Left = 0
    Top = 0
    Width = 521
    Height = 505
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 0
    Margins.Bottom = 4
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvSpace
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
    object grpMonsters: TGroupBox
      Left = 9
      Top = 208
      Width = 167
      Height = 208
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Monster Parties'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object grdMonsterParties: TRpgListGrid
        Left = 10
        Top = 26
        Width = 149
        Height = 173
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Enabled = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -15
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object grpChipset: TGroupBox
      Left = 9
      Top = 57
      Width = 167
      Height = 50
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Tileset'
      TabOrder = 1
      object cboTileset: TDBLookupComboBox
        Left = 10
        Top = 18
        Width = 149
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'tileset'
        DataSource = srcMap
        KeyField = 'id'
        ListField = 'name'
        ListSource = srcTileset
        TabOrder = 0
      end
    end
    object grpMapsize: TGroupBox
      Left = 9
      Top = 111
      Width = 167
      Height = 44
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Map Size'
      TabOrder = 2
      object lblX: TLabel
        Left = 76
        Top = 21
        Width = 8
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'X'
      end
      object spnWidth: TJvDBSpinEdit
        Left = 10
        Top = 16
        Width = 59
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ButtonKind = bkStandard
        TabOrder = 0
        DataField = 'size.x'
        DataSource = srcMap
      end
      object spnHeight: TJvDBSpinEdit
        Left = 96
        Top = 16
        Width = 62
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ButtonKind = bkStandard
        TabOrder = 1
        DataField = 'size.y'
        DataSource = srcMap
      end
    end
    object grpScrollType: TGroupBox
      Left = 9
      Top = 159
      Width = 167
      Height = 45
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Wraparound Style'
      TabOrder = 3
      object cboWraparoundType: TDBIndexComboBox
        Left = 10
        Top = 16
        Width = 149
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'wraparound'
        DataSource = srcMap
        Items.Strings = (
          'None'
          'Vertical Loop'
          'Horizontal Loop'
          'Both Loop')
        TabOrder = 0
      end
    end
    object grpName: TGroupBox
      Left = 9
      Top = 9
      Width = 167
      Height = 45
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Name'
      TabOrder = 4
      object txtName: TDBEdit
        Left = 10
        Top = 16
        Width = 149
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'name'
        DataSource = srcMap
        TabOrder = 0
      end
    end
    object grpPano: TGroupBox
      Left = 183
      Top = 9
      Width = 329
      Height = 220
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Background Image'
      TabOrder = 5
      object grpSelectGraphic: TGroupBox
        Left = 9
        Top = 36
        Width = 181
        Height = 177
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Select Graphic'
        TabOrder = 0
        object imgBG: TImage
          Left = 9
          Top = 23
          Width = 163
          Height = 115
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
        end
        object btnSet: TButton
          Left = 121
          Top = 145
          Width = 51
          Height = 26
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Set'
          Enabled = False
          TabOrder = 0
        end
        object txtBGName: TDBEdit
          Left = 9
          Top = 145
          Width = 104
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          DataField = 'bgName'
          DataSource = srcMap
          ReadOnly = True
          TabOrder = 1
        end
        object StaticText4: TStaticText
          Left = 13
          Top = 150
          Width = 98
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Not Working Yet'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
        end
      end
      object grpHPan: TGroupBox
        Left = 196
        Top = 16
        Width = 125
        Height = 92
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Horizontal panning'
        TabOrder = 1
        object lbHPanlSpeed: TLabel
          Left = 9
          Top = 64
          Width = 41
          Height = 16
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Speed'
        end
        object cboHPan: TDBIndexComboBox
          Left = 9
          Top = 22
          Width = 103
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          DataField = 'hscroll'
          DataSource = srcMap
          Items.Strings = (
            'None'
            'Follow'
            'Constant')
          TabOrder = 0
        end
        object spnHPanSpeed: TJvDBSpinEdit
          Left = 57
          Top = 60
          Width = 61
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ButtonKind = bkStandard
          TabOrder = 1
          DataField = 'scrollSpeed.x'
          DataSource = srcMap
        end
      end
      object chkUseBG: TDBCheckBox
        Left = 9
        Top = 16
        Width = 160
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Use Background Image'
        DataField = 'hasBG'
        DataSource = srcMap
        TabOrder = 2
        ValueChecked = 'True'
        ValueUnchecked = 'False'
      end
      object grpVPan: TGroupBox
        Left = 196
        Top = 121
        Width = 125
        Height = 92
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Vertical panning'
        TabOrder = 3
        object lblVPanSpeed: TLabel
          Left = 9
          Top = 64
          Width = 41
          Height = 16
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Speed'
        end
        object cboVPan: TDBIndexComboBox
          Left = 9
          Top = 22
          Width = 103
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          DataField = 'vscroll'
          DataSource = srcMap
          Items.Strings = (
            'None'
            'Follow'
            'Constant')
          TabOrder = 0
        end
        object spnVPanSpeed: TJvDBSpinEdit
          Left = 57
          Top = 60
          Width = 61
          Height = 24
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ButtonKind = bkStandard
          TabOrder = 1
          DataField = 'scrollSpeed.y'
          DataSource = srcMap
        end
      end
    end
    object grpBGM: TGroupBox
      Left = 183
      Top = 236
      Width = 160
      Height = 86
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Background Music'
      TabOrder = 6
      object btnSetBGMTo: TButton
        Left = 134
        Top = 53
        Width = 19
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object cboBGM: TDBIndexComboBox
        Left = 9
        Top = 20
        Width = 142
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'bgmState'
        DataSource = srcMetadata
        Items.Strings = (
          'Parent Map'
          'No Change'
          'Set To')
        TabOrder = 1
      end
      object txtBGM: TDBEdit
        Left = 9
        Top = 53
        Width = 119
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'BgmData.name'
        DataSource = srcMetadata
        ReadOnly = True
        TabOrder = 2
      end
      object StaticText2: TStaticText
        Left = 22
        Top = 58
        Width = 98
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Not Working Yet'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
      end
    end
    object grpTeleport: TGroupBox
      Left = 356
      Top = 236
      Width = 156
      Height = 53
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Teleport'
      TabOrder = 7
      object cboTeleport: TDBIndexComboBox
        Left = 9
        Top = 18
        Width = 140
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'canPort'
        DataSource = srcMetadata
        Items.Strings = (
          'Parent Map'
          'Enable'
          'Disable')
        TabOrder = 0
      end
    end
    object grpEscape: TGroupBox
      Left = 356
      Top = 299
      Width = 156
      Height = 53
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Escape'
      TabOrder = 8
      object cboEscape: TDBIndexComboBox
        Left = 10
        Top = 18
        Width = 140
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'canEscape'
        DataSource = srcMetadata
        Items.Strings = (
          'Parent Map'
          'Enable'
          'Disable')
        TabOrder = 0
      end
    end
    object grpSave: TGroupBox
      Left = 356
      Top = 362
      Width = 156
      Height = 53
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Save'
      TabOrder = 9
      object cboSave: TDBIndexComboBox
        Left = 10
        Top = 18
        Width = 140
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'canSave'
        DataSource = srcMetadata
        Items.Strings = (
          'Parent Map'
          'Enable'
          'Disable')
        TabOrder = 0
      end
    end
    object grpBattleBG: TGroupBox
      Left = 183
      Top = 329
      Width = 160
      Height = 86
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Battle Background'
      TabOrder = 10
      object btnSetBGTo: TButton
        Left = 134
        Top = 53
        Width = 19
        Height = 26
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object cboBattleBG: TDBIndexComboBox
        Left = 9
        Top = 20
        Width = 142
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'battleBgState'
        DataSource = srcMetadata
        Items.Strings = (
          'Parent Map'
          'By Terrain Tile'
          'Set To')
        TabOrder = 1
      end
      object txtBattleBG: TDBEdit
        Left = 9
        Top = 53
        Width = 119
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'battleBgName'
        DataSource = srcMetadata
        ReadOnly = True
        TabOrder = 2
      end
      object StaticText3: TStaticText
        Left = 22
        Top = 58
        Width = 98
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Not Working Yet'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
      end
    end
    object grpMapEncounters: TGroupBox
      Left = 9
      Top = 417
      Width = 503
      Height = 77
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Map Encounters'
      TabOrder = 11
      Visible = False
      object lblScript: TLabel
        Left = 10
        Top = 21
        Width = 34
        Height = 16
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Script'
      end
      object lblVar1: TDBText
        Left = 183
        Top = 20
        Width = 59
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataSource = srcScript
      end
      object lblVar2: TDBText
        Left = 263
        Top = 20
        Width = 59
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataSource = srcScript
      end
      object lblVar3: TDBText
        Left = 343
        Top = 20
        Width = 59
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataSource = srcScript
      end
      object lblVar4: TDBText
        Left = 423
        Top = 20
        Width = 59
        Height = 21
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataSource = srcScript
      end
      object spnVar1: TJvDBSpinEdit
        Left = 183
        Top = 41
        Width = 59
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ButtonKind = bkStandard
        TabOrder = 0
        DataField = 'encounters[1]'
        DataSource = srcMap
      end
      object spnVar2: TJvDBSpinEdit
        Left = 263
        Top = 41
        Width = 59
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ButtonKind = bkStandard
        TabOrder = 1
        DataField = 'encounters[2]'
        DataSource = srcMap
      end
      object spnVar3: TJvDBSpinEdit
        Left = 343
        Top = 41
        Width = 59
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ButtonKind = bkStandard
        TabOrder = 2
        DataField = 'encounters[3]'
        DataSource = srcMap
      end
      object spnVar4: TJvDBSpinEdit
        Left = 423
        Top = 41
        Width = 59
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ButtonKind = bkStandard
        TabOrder = 3
        DataField = 'encounters[4]'
        DataSource = srcMap
      end
      object cboEncounterScript: TDBLookupComboBox
        Left = 10
        Top = 41
        Width = 149
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DataField = 'encounterScript'
        DataSource = srcMap
        KeyField = 'name'
        ListField = 'designName'
        ListSource = srcScript
        TabOrder = 4
      end
    end
  end
  object btnCancel: TButton
    Left = 319
    Top = 523
    Width = 90
    Height = 35
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 417
    Top = 523
    Width = 90
    Height = 35
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Apply'
    TabOrder = 3
    Visible = False
    OnClick = btnApplyClick
  end
  object btnHelp: TButton
    Left = 9
    Top = 523
    Width = 89
    Height = 35
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Help'
    TabOrder = 4
    Visible = False
  end
  object StaticText1: TStaticText
    Left = 32
    Top = 302
    Width = 98
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Not Working Yet'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
  end
  object srcMetadata: TDataSource
    DataSet = dmDatabase.metadata
    Left = 61
    Top = 392
  end
  object dsMap: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 392
    Data = {
      E80100009619E0BD010000001800000016000000000003000000E80102696404
      00010004000000046E616D650100490000000100055749445448020002002000
      086D6F64696669656402000300000000000774696C6573657404000100000000
      000673697A652E7804000100000000000673697A652E79040001000000000005
      646570746801000200000000000A7772617061726F756E640100020000000000
      05686173424702000300000000000662674E616D650200490000000100055749
      44544802000200FF0007767363726F6C6C010002000000000007687363726F6C
      6C01000200000000000D7363726F6C6C53706565642E7804000100000000000D
      7363726F6C6C53706565642E7904000100000000000C736372697074466F726D
      617401000200000000000A73637269707446696C650200490000000100055749
      44544802000200FF000F656E636F756E74657253637269707402004900000001
      0005574944544802000200FF000D656E636F756E746572735B315D0400010000
      0000000D656E636F756E746572735B325D04000100000000000D656E636F756E
      746572735B335D04000100000000000D656E636F756E746572735B345D040001
      00000000000774696C654D617004004B00000001000753554254595045020049
      00070042696E617279000000}
    object dsMapid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object dsMapname: TStringField
      FieldName = 'name'
      Size = 32
    end
    object dsMapmodified: TBooleanField
      FieldName = 'modified'
    end
    object dsMaptileset: TIntegerField
      FieldName = 'tileset'
    end
    object dsMapsizex: TIntegerField
      FieldName = 'size.x'
    end
    object dsMapsizey: TIntegerField
      FieldName = 'size.y'
    end
    object dsMapdepth: TByteField
      FieldName = 'depth'
    end
    object dsMapwraparound: TByteField
      FieldName = 'wraparound'
    end
    object dsMaphasBG: TBooleanField
      FieldName = 'hasBG'
    end
    object dsMapbgName: TStringField
      FieldName = 'bgName'
      Size = 255
    end
    object dsMapvscroll: TByteField
      FieldName = 'vscroll'
    end
    object dsMaphscroll: TByteField
      FieldName = 'hscroll'
    end
    object dsMapscrollSpeedx: TIntegerField
      FieldName = 'scrollSpeed.x'
    end
    object dsMapscrollSpeedy: TIntegerField
      FieldName = 'scrollSpeed.y'
    end
    object dsMapscriptFormat: TByteField
      FieldName = 'scriptFormat'
    end
    object dsMapscriptFile: TStringField
      FieldName = 'scriptFile'
      Size = 255
    end
    object dsMapencounterScript: TStringField
      FieldName = 'encounterScript'
      Size = 255
    end
    object dsMapencounters1: TIntegerField
      FieldName = 'encounters[1]'
    end
    object dsMapencounters2: TIntegerField
      FieldName = 'encounters[2]'
    end
    object dsMapencounters3: TIntegerField
      FieldName = 'encounters[3]'
    end
    object dsMapencounters4: TIntegerField
      FieldName = 'encounters[4]'
    end
    object dsMaptileMap: TBlobField
      FieldName = 'tileMap'
    end
  end
  object srcMap: TDataSource
    DataSet = dsMap
    Left = 8
    Top = 392
  end
  object srcTileset: TDataSource
    DataSet = dmDatabase.tilesets
    Left = 154
    Top = 392
  end
  object srcScript: TDataSource
    DataSet = dmDatabase.scriptRange
    Left = 208
    Top = 392
  end
end
