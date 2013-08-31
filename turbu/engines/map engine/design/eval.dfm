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
        DataField = 'size_x'
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
        DataField = 'size_y'
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
          DataField = 'scrollSpeed_x'
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
          DataField = 'scrollSpeed_y'
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
        DataField = 'BgmData_name'
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
        DataField = 'encounters_1'
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
        DataField = 'encounters_2'
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
        DataField = 'encounters_3'
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
        DataField = 'encounters_4'
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
    DataSet = metadata
    Left = 61
    Top = 392
  end
  object dsMap: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'name'
        DataType = ftWideString
        Size = 32
      end
      item
        Name = 'modified'
        DataType = ftBoolean
      end
      item
        Name = 'tileset'
        DataType = ftInteger
      end
      item
        Name = 'size_x'
        DataType = ftInteger
      end
      item
        Name = 'size_y'
        DataType = ftInteger
      end
      item
        Name = 'depth'
        DataType = ftByte
      end
      item
        Name = 'wraparound'
        DataType = ftByte
      end
      item
        Name = 'hasBG'
        DataType = ftBoolean
      end
      item
        Name = 'bgName'
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'vscroll'
        DataType = ftByte
      end
      item
        Name = 'hscroll'
        DataType = ftByte
      end
      item
        Name = 'scrollSpeed_x'
        DataType = ftInteger
      end
      item
        Name = 'scrollSpeed_y'
        DataType = ftInteger
      end
      item
        Name = 'scriptFormat'
        DataType = ftByte
      end
      item
        Name = 'scriptFile'
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'encounterScript'
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'encounters_1'
        DataType = ftInteger
      end
      item
        Name = 'encounters_2'
        DataType = ftInteger
      end
      item
        Name = 'encounters_3'
        DataType = ftInteger
      end
      item
        Name = 'encounters_4'
        DataType = ftInteger
      end
      item
        Name = 'tileMap'
        DataType = ftBlob
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 168
    Top = 504
    Data = {
      E40100009619E0BD010000001800000016000000000003000000E40102696404
      00010004000000046E616D6501004A0000000100055749445448020002004000
      086D6F64696669656402000300000000000774696C6573657404000100000000
      000673697A655F7804000100000000000673697A655F79040001000000000005
      646570746801000200000000000A7772617061726F756E640100020000000000
      05686173424702000300000000000662674E616D6502004A0000000100055749
      44544802000200FE0107767363726F6C6C010002000000000007687363726F6C
      6C01000200000000000D7363726F6C6C53706565645F7804000100000000000D
      7363726F6C6C53706565645F7904000100000000000C736372697074466F726D
      617401000200000000000A73637269707446696C6502004A0000000100055749
      44544802000200FE010F656E636F756E74657253637269707402004A00000001
      0005574944544802000200FE010C656E636F756E746572735F31040001000000
      00000C656E636F756E746572735F3204000100000000000C656E636F756E7465
      72735F3304000100000000000C656E636F756E746572735F3404000100000000
      000774696C654D617004004B0000000100075355425459504502004900070042
      696E617279000000}
    object dsMapid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object dsMapname: TWideStringField
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
      FieldName = 'size_x'
    end
    object dsMapsizey: TIntegerField
      FieldName = 'size_y'
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
    object dsMapbgName: TWideStringField
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
      FieldName = 'scrollSpeed_x'
    end
    object dsMapscrollSpeedy: TIntegerField
      FieldName = 'scrollSpeed_y'
    end
    object dsMapscriptFormat: TByteField
      FieldName = 'scriptFormat'
    end
    object dsMapscriptFile: TWideStringField
      FieldName = 'scriptFile'
      Size = 255
    end
    object dsMapencounterScript: TWideStringField
      FieldName = 'encounterScript'
      Size = 255
    end
    object dsMapencounters1: TIntegerField
      FieldName = 'encounters_1'
    end
    object dsMapencounters2: TIntegerField
      FieldName = 'encounters_2'
    end
    object dsMapencounters3: TIntegerField
      FieldName = 'encounters_3'
    end
    object dsMapencounters4: TIntegerField
      FieldName = 'encounters_4'
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
  object metadata: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'id'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'name'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'parent'
        Attributes = [faRequired]
        DataType = ftSmallint
      end
      item
        Name = 'scrollPosition_x'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'scrollPosition_y'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'treeOpen'
        Attributes = [faRequired]
        DataType = ftBoolean
      end
      item
        Name = 'bgmState'
        Attributes = [faRequired]
        DataType = ftByte
      end
      item
        Name = 'BgmData_id'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'BgmData_name'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'BgmData_fadeIn'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'BgmData_tempo'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'BgmData_volume'
        Attributes = [faRequired]
        DataType = ftByte
      end
      item
        Name = 'BgmData_Balance'
        Attributes = [faRequired]
        DataType = ftByte
      end
      item
        Name = 'battleBgState'
        Attributes = [faRequired]
        DataType = ftByte
      end
      item
        Name = 'battleBgName'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'canPort'
        Attributes = [faRequired]
        DataType = ftByte
      end
      item
        Name = 'canEscape'
        Attributes = [faRequired]
        DataType = ftByte
      end
      item
        Name = 'canSave'
        Attributes = [faRequired]
        DataType = ftByte
      end
      item
        Name = 'internalFilename_Name'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'internalFilename_duplicates'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'mapEngine'
        Attributes = [faRequired]
        DataType = ftShortint
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 120
    Top = 520
    Data = {
      FA0100009619E0BD010000001800000015000000000003000000FA0102696404
      00010004000000046E616D6501004A0004000100055749445448020002002800
      06706172656E740200010004000000107363726F6C6C506F736974696F6E5F78
      0400010004000000107363726F6C6C506F736974696F6E5F7904000100040000
      0008747265654F70656E02000300040000000862676D53746174650100020004
      0000000A42676D446174615F696404000100040000000C42676D446174615F6E
      616D6502004A000400010005574944544802000200FE010E42676D446174615F
      66616465496E04000100040000000D42676D446174615F74656D706F04000100
      040000000E42676D446174615F766F6C756D6501000200040000000F42676D44
      6174615F42616C616E636501000200040000000D626174746C65426753746174
      6501000200040000000C626174746C6542674E616D6502004A00040001000557
      4944544802000200FE010763616E506F727401000200040000000963616E4573
      6361706501000200040000000763616E53617665010002000400000015696E74
      65726E616C46696C656E616D655F4E616D6502004A0004000100055749445448
      02000200FE011B696E7465726E616C46696C656E616D655F6475706C69636174
      65730400010004000000096D6170456E67696E6501000100040000000000}
    object metadataid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object metadataname: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object metadataparent: TSmallintField
      FieldName = 'parent'
      Required = True
    end
    object metadatascrollPosition_x: TIntegerField
      FieldName = 'scrollPosition_x'
      Required = True
    end
    object metadatascrollPosition_y: TIntegerField
      FieldName = 'scrollPosition_y'
      Required = True
    end
    object metadatatreeOpen: TBooleanField
      FieldName = 'treeOpen'
      Required = True
    end
    object metadatabgmState: TByteField
      FieldName = 'bgmState'
      Required = True
    end
    object metadataBgmData_id: TIntegerField
      FieldName = 'BgmData_id'
      Required = True
    end
    object metadataBgmData_name: TWideStringField
      FieldName = 'BgmData_name'
      Required = True
      Size = 255
    end
    object metadataBgmData_fadeIn: TIntegerField
      FieldName = 'BgmData_fadeIn'
      Required = True
    end
    object metadataBgmData_tempo: TIntegerField
      FieldName = 'BgmData_tempo'
      Required = True
    end
    object metadataBgmData_volume: TByteField
      FieldName = 'BgmData_volume'
      Required = True
    end
    object metadataBgmData_leftBalance: TByteField
      FieldName = 'BgmData_Balance'
      Required = True
    end
    object metadatabattleBgState: TByteField
      FieldName = 'battleBgState'
      Required = True
    end
    object metadatabattleBgName: TWideStringField
      FieldName = 'battleBgName'
      Required = True
      Size = 255
    end
    object metadatacanPort: TByteField
      FieldName = 'canPort'
      Required = True
    end
    object metadatacanEscape: TByteField
      FieldName = 'canEscape'
      Required = True
    end
    object metadatacanSave: TByteField
      FieldName = 'canSave'
      Required = True
    end
    object metadatainternalFilename_Name: TWideStringField
      FieldName = 'internalFilename_Name'
      Required = True
      Size = 255
    end
    object metadatainternalFilename_duplicates: TIntegerField
      FieldName = 'internalFilename_duplicates'
      Required = True
    end
    object metadatamapEngine: TShortintField
      FieldName = 'mapEngine'
      Required = True
    end
  end
  object metadata_regions: TClientDataSet
    Active = True
    Aggregates = <>
    Filtered = True
    FieldDefs = <
      item
        Name = 'master'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'id'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'name'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'bounds_left'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'bounds_right'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'bounds_top'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'bounds_bottom'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'encounterScript'
        Attributes = [faRequired]
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'encounters_1'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'encounters_2'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'encounters_3'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'encounters_4'
        Attributes = [faRequired]
        DataType = ftInteger
      end
      item
        Name = 'battles'
        DataType = ftBlob
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 88
    Top = 488
    Data = {
      3C0100009619E0BD01000000180000000D0000000000030000003C01066D6173
      74657204000100040000000269640400010004000000046E616D6501004A0004
      0001000557494454480200020028000B626F756E64735F6C6566740400010004
      0000000C626F756E64735F726967687404000100040000000A626F756E64735F
      746F7004000100040000000D626F756E64735F626F74746F6D04000100040000
      000F656E636F756E74657253637269707402004A000400010005574944544802
      000200FE010C656E636F756E746572735F3104000100040000000C656E636F75
      6E746572735F3204000100040000000C656E636F756E746572735F3304000100
      040000000C656E636F756E746572735F34040001000400000007626174746C65
      7304004B0000000100075355425459504502004900070042696E617279000000}
    object metadata_regionsMaster: TIntegerField
      FieldName = 'master'
      Required = True
    end
    object IntegerField29: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object WideStringField12: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object metadata_regionsbounds_left: TIntegerField
      FieldName = 'bounds_left'
      Required = True
    end
    object metadata_regionsbounds_right: TIntegerField
      FieldName = 'bounds_right'
      Required = True
    end
    object metadata_regionsbounds_top: TIntegerField
      FieldName = 'bounds_top'
      Required = True
    end
    object metadata_regionsbounds_bottom: TIntegerField
      FieldName = 'bounds_bottom'
      Required = True
    end
    object metadata_regionsencounterScript: TWideStringField
      FieldName = 'encounterScript'
      Required = True
      Size = 255
    end
    object metadata_regionsencounters_1: TIntegerField
      FieldName = 'encounters_1'
      Required = True
    end
    object metadata_regionsencounters_2: TIntegerField
      FieldName = 'encounters_2'
      Required = True
    end
    object metadata_regionsencounters_3: TIntegerField
      FieldName = 'encounters_3'
      Required = True
    end
    object metadata_regionsencounters_4: TIntegerField
      FieldName = 'encounters_4'
      Required = True
    end
    object metadata_regionsbattles: TBlobField
      FieldName = 'battles'
    end
  end
end
