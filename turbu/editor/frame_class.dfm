object frameClass: TframeClass
  Left = 0
  Top = 0
  Width = 785
  Height = 527
  TabOrder = 0
  TabStop = True
  object lblClasses: TLabel
    Left = 16
    Top = 3
    Width = 137
    Height = 28
    Alignment = taCenter
    AutoSize = False
    Caption = 'CLASSES'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object pnlClass: TPanel
    Left = 159
    Top = 16
    Width = 610
    Height = 425
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 0
    object grpClassName: TGroupBox
      Left = 8
      Top = 8
      Width = 185
      Height = 41
      Caption = 'Name'
      TabOrder = 0
      object dbTxtName: TDBEdit
        Left = 8
        Top = 16
        Width = 169
        Height = 21
        DataField = 'name'
        DataSource = dsCharClass
        TabOrder = 0
      end
    end
    object grpClassStats: TGroupBox
      Left = 199
      Top = 8
      Width = 185
      Height = 127
      Caption = 'Class Stats'
      TabOrder = 1
      object imgStats: TImage
        Left = 8
        Top = 14
        Width = 168
        Height = 80
      end
      object cbxBaseStats: TComboBox
        Left = 8
        Top = 100
        Width = 168
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Hit Points'
        OnChange = cbxBaseStatsChange
        Items.Strings = (
          'Hit Points'
          'Magic Points'
          'Strength'
          'Defense'
          'Intelligence'
          'Speed')
      end
    end
    object grpClassOptions: TGroupBox
      Left = 199
      Top = 141
      Width = 185
      Height = 144
      Caption = 'Special Options'
      TabOrder = 2
      object radWeaponStyle: TDBRadioGroup
        Left = 8
        Top = 14
        Width = 169
        Height = 84
        Caption = 'Weapon Style'
        DataField = 'dualWield'
        DataSource = dsCharClass
        Items.Strings = (
          'Single Weapon'
          'Weapon + Shield'
          'Dual weapon'
          'Any')
        ParentBackground = True
        TabOrder = 2
        Values.Strings = (
          '0'
          '1'
          '2'
          '3')
        OnClick = radWeaponStyleClick
      end
      object chkEqLocked: TDBCheckBox
        Left = 8
        Top = 102
        Width = 121
        Height = 17
        Caption = 'Equipment Locked'
        DataField = 'staticEq'
        DataSource = dsCharClass
        TabOrder = 0
        ValueChecked = 'True'
        ValueUnchecked = 'False'
      end
      object chkStrongDef: TDBCheckBox
        Left = 8
        Top = 121
        Width = 97
        Height = 17
        Caption = 'Strong Defense'
        DataField = 'strongDef'
        DataSource = dsCharClass
        TabOrder = 1
        ValueChecked = 'True'
        ValueUnchecked = 'False'
      end
    end
    object grpClassExp: TGroupBox
      Left = 8
      Top = 261
      Width = 185
      Height = 156
      Caption = 'Experience Level Requirements'
      TabOrder = 3
      object lblExpVal1: TLabel
        Left = 8
        Top = 40
        Width = 40
        Height = 13
        Caption = 'Primary:'
      end
      object lblExpVal2: TLabel
        Left = 96
        Top = 40
        Width = 51
        Height = 13
        Caption = 'Additional:'
      end
      object lblExpVal3: TLabel
        Left = 8
        Top = 80
        Width = 59
        Height = 13
        Caption = 'Adjustment:'
      end
      object lblExpVal4: TLabel
        Left = 101
        Top = 80
        Width = 22
        Height = 13
        Caption = 'N/A:'
      end
      object btnExpCurveEditor: TButton
        Left = 31
        Top = 123
        Width = 106
        Height = 25
        Caption = 'C&urve Editor'
        TabOrder = 0
      end
      object spnExpVal1: TJvDBSpinEdit
        Left = 8
        Top = 56
        Width = 73
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 2
        DataField = 'expVars[0]'
        DataSource = dsCharClass
      end
      object spnExpVal2: TJvDBSpinEdit
        Left = 96
        Top = 56
        Width = 73
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 3
        DataField = 'expVars[1]'
        DataSource = dsCharClass
      end
      object spnExpVal3: TJvDBSpinEdit
        Left = 8
        Top = 96
        Width = 73
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 4
        DataField = 'expVars[2]'
        DataSource = dsCharClass
      end
      object spnExpVal4: TJvDBSpinEdit
        Left = 96
        Top = 96
        Width = 73
        Height = 21
        ButtonKind = bkClassic
        TabOrder = 5
        DataField = 'expVars[3]'
        DataSource = dsCharClass
      end
      object cbxExpAlgorithm: TDBLookupComboBox
        Left = 8
        Top = 14
        Width = 161
        Height = 21
        DataField = 'expFuncDesignName'
        DataSource = dsCharClass
        TabOrder = 1
      end
    end
    object grpGraphics: TGroupBox
      Left = 8
      Top = 55
      Width = 185
      Height = 194
      Caption = 'Graphics'
      TabOrder = 4
      object tabGraphics: TTabControl
        Left = 4
        Top = 24
        Width = 173
        Height = 161
        TabOrder = 0
        Tabs.Strings = (
          'Portrait'
          'Sprite'
          'Battle Sprite')
        TabIndex = 0
        OnChange = tabGraphicsChange
        object imgMapSprite: TImage
          Left = 8
          Top = 24
          Width = 96
          Height = 96
        end
        object btnSetGfx: TButton
          Left = 16
          Top = 126
          Width = 75
          Height = 25
          Caption = '&Set'
          TabOrder = 0
        end
      end
    end
    object grpRepertoire: TGroupBox
      Left = 390
      Top = 55
      Width = 211
      Height = 208
      Caption = 'Class Repertoire'
      TabOrder = 5
      DesignSize = (
        211
        208)
      object pageRepertoire: TPageControl
        Left = 8
        Top = 16
        Width = 196
        Height = 186
        ActivePage = tshEquipment
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object tshEquipment: TTabSheet
          Caption = 'Equipment'
          object lblEquip1: TLabel
            Left = 8
            Top = 8
            Width = 44
            Height = 13
            Caption = 'Weapon:'
          end
          object lblEquip2: TLabel
            Left = 8
            Top = 38
            Width = 32
            Height = 13
            Caption = 'Shield:'
          end
          object lblEquip3: TLabel
            Left = 8
            Top = 64
            Width = 33
            Height = 13
            Caption = 'Armor:'
          end
          object lblEquip4: TLabel
            Left = 8
            Top = 92
            Width = 37
            Height = 13
            Caption = 'Helmet:'
          end
          object lblEquip5: TLabel
            Left = 8
            Top = 122
            Width = 53
            Height = 13
            Caption = 'Accessory:'
          end
          object cbxEquip1: TDBLookupComboBox
            Left = 64
            Top = 5
            Width = 121
            Height = 21
            DataField = 'weaponName'
            DataSource = dsCharClass
            TabOrder = 0
            OnClick = checkFor2Handed
          end
          object cbxEquip4: TDBLookupComboBox
            Left = 64
            Top = 89
            Width = 121
            Height = 21
            DataField = 'helmetName'
            DataSource = dsCharClass
            KeyField = 'id'
            ListField = 'name'
            TabOrder = 2
          end
          object cbxEquip3: TDBLookupComboBox
            Left = 64
            Top = 62
            Width = 121
            Height = 21
            DataField = 'armorName'
            DataSource = dsCharClass
            KeyField = 'id'
            ListField = 'name'
            TabOrder = 3
          end
          object cbxEquip5: TDBLookupComboBox
            Left = 64
            Top = 122
            Width = 121
            Height = 21
            DataField = 'accessoryName'
            DataSource = dsCharClass
            KeyField = 'id'
            ListField = 'name'
            TabOrder = 4
          end
          object cbxEquip2: TDBLookupComboBox
            Left = 64
            Top = 35
            Width = 121
            Height = 21
            DataSource = dsCharClass
            KeyField = 'id'
            ListField = 'name'
            TabOrder = 1
          end
        end
        object tshSkills: TTabSheet
          Caption = 'Commands'
          ImageIndex = 1
          inline frameHeroCommands: TframeHeroCommands
            Left = 0
            Top = 0
            Width = 185
            Height = 156
            VertScrollBar.Tracking = True
            AutoScroll = True
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 185
            ExplicitHeight = 156
            inherited spnCount: TJvSpinEdit
              ButtonKind = bkClassic
            end
          end
        end
        object tshRepertoire: TTabSheet
          Caption = 'Skills'
          ImageIndex = 2
          object lstSkills: TRpgListGrid
            Left = 4
            Top = 4
            Width = 181
            Height = 151
            DataSource = dsSkillset
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            OnDblClick = lstSkillsDblClick
            Columns = <
              item
                Expanded = False
                FieldName = 'id'
                Title.Alignment = taCenter
                Title.Caption = 'ID'
                Width = 35
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'name'
                Title.Caption = 'Name'
                Width = 142
                Visible = True
              end>
          end
        end
      end
    end
    object grpScriptEvents: TGroupBox
      Left = 199
      Top = 291
      Width = 185
      Height = 126
      Caption = 'Script Events'
      TabOrder = 6
      object lstScripts: TListView
        Left = 8
        Top = 16
        Width = 164
        Height = 100
        Columns = <
          item
            Caption = 'Event'
            Width = 70
          end
          item
            Caption = 'Script'
            Width = 73
          end>
        Items.ItemData = {
          03760100000900000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
          00084F006E0043007200650061007400650000000000FFFFFFFFFFFFFFFF0000
          0000FFFFFFFF00000000064F006E004A006F0069006E0000000000FFFFFFFFFF
          FFFFFF00000000FFFFFFFF00000000074F006E004C0065006100760065000000
          0000FFFFFFFFFFFFFFFF00000000FFFFFFFF00000000054F006E004400690065
          0000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00000000084F006E005200
          6500760069007600650000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
          0000094F006E004700610069006E0045007800700000000000FFFFFFFFFFFFFF
          FF00000000FFFFFFFF000000000B4F006E004700610069006E004C0065007600
          65006C0000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00000000074F006E
          004500710075006900700000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF00
          000000094F006E0055006E0065007100750069007000}
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object grpUnarmed: TGroupBox
      Left = 390
      Top = 8
      Width = 211
      Height = 41
      Caption = 'Unarmed Attack Animation'
      TabOrder = 7
      object cbxUnarmedAnim: TDBLookupComboBox
        Left = 8
        Top = 16
        Width = 193
        Height = 21
        DataField = 'animName'
        DataSource = dsCharClass
        TabOrder = 0
      end
    end
    object grpResistVuln: TGroupBox
      Left = 390
      Top = 269
      Width = 211
      Height = 148
      Caption = 'Resists/Vulns'
      TabOrder = 8
      object pageResists: TPageControl
        Left = 8
        Top = 16
        Width = 196
        Height = 126
        ActivePage = tshAttributes
        TabOrder = 0
        object tshAttributes: TTabSheet
          Caption = 'Attributes'
          OnShow = linkNav
          object lstAttributes: TRpgListGrid
            Left = 3
            Top = 3
            Width = 142
            Height = 90
            DataSource = dsResist
            Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            Columns = <
              item
                Expanded = False
                FieldName = 'name'
                Title.Caption = '    Name'
                Width = 90
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'y'
                Title.Alignment = taCenter
                Title.Caption = '%'
                Width = 30
                Visible = True
              end>
          end
          object btnEditAttributes: TButton
            Left = 151
            Top = 67
            Width = 34
            Height = 25
            Caption = '&Edit'
            TabOrder = 1
            OnClick = btnEditAttributesClick
          end
        end
        object tshConditions: TTabSheet
          Caption = 'Conditions'
          ImageIndex = 1
          object RpgListGrid1: TRpgListGrid
            Left = 3
            Top = 3
            Width = 142
            Height = 90
            DataSource = dsCondition
            Options = [dgEditing, dgTitles, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            Columns = <
              item
                Expanded = False
                FieldName = 'name'
                Title.Caption = '    Name'
                Width = 90
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'y'
                Title.Alignment = taCenter
                Title.Caption = '%'
                Width = 30
                Visible = True
              end>
          end
        end
      end
    end
  end
  object grdClasses: TRpgListGrid
    Left = 16
    Top = 37
    Width = 137
    Height = 404
    DataSource = dsCharClass
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnRowEnter = grdClassesRowEnter
    Columns = <
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'id'
        ReadOnly = True
        Title.Alignment = taCenter
        Title.Caption = 'ID'
        Width = 31
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'name'
        Title.Caption = 'Name'
        Width = 99
        Visible = True
      end>
  end
  object navAdd: TDBNavigator
    Left = 712
    Top = 330
    Width = 34
    Height = 25
    DataSource = dsResist
    VisibleButtons = [nbInsert]
    TabOrder = 2
  end
  object navDel: TDBNavigator
    Left = 712
    Top = 361
    Width = 34
    Height = 25
    DataSource = dsResist
    VisibleButtons = [nbDelete]
    TabOrder = 3
  end
  object tmrAnim: TTimer
    Interval = 250
    OnTimer = tmrAnimTimer
    Left = 288
    Top = 152
  end
  object dsCharClass: TDataSource
    DataSet = dmDatabase.charClasses
    Left = 96
    Top = 456
  end
  object dsSkillset: TDataSource
    DataSet = dmDatabase.charClasses_skillset
    Left = 168
    Top = 456
  end
  object dsResist: TDataSource
    DataSet = dmDatabase.charClasses_Resist
    Left = 232
    Top = 456
  end
  object dsCondition: TDataSource
    DataSet = dmDatabase.charClasses_Condition
    Left = 304
    Top = 456
  end
end
