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
        Height = 24
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
        Height = 24
        Style = csDropDownList
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
        Width = 49
        Height = 16
        Caption = 'Primary:'
      end
      object lblExpVal2: TLabel
        Left = 96
        Top = 40
        Width = 61
        Height = 16
        Caption = 'Additional:'
      end
      object lblExpVal3: TLabel
        Left = 8
        Top = 80
        Width = 70
        Height = 16
        Caption = 'Adjustment:'
      end
      object lblExpVal4: TLabel
        Left = 101
        Top = 80
        Width = 26
        Height = 16
        Caption = 'N/A:'
      end
      object btnExpCurveEditor: TButton
        Left = 31
        Top = 123
        Width = 106
        Height = 25
        Caption = 'C&urve Editor'
        Enabled = False
        TabOrder = 0
      end
      object spnExpVal1: TJvDBSpinEdit
        Left = 8
        Top = 56
        Width = 73
        Height = 24
        ButtonKind = bkClassic
        TabOrder = 2
        DataField = 'expVars[1]'
        DataSource = dsCharClass
      end
      object spnExpVal2: TJvDBSpinEdit
        Left = 96
        Top = 56
        Width = 73
        Height = 24
        ButtonKind = bkClassic
        TabOrder = 3
        DataField = 'expVars[2]'
        DataSource = dsCharClass
      end
      object spnExpVal3: TJvDBSpinEdit
        Left = 8
        Top = 96
        Width = 73
        Height = 24
        ButtonKind = bkClassic
        TabOrder = 4
        DataField = 'expVars[3]'
        DataSource = dsCharClass
      end
      object spnExpVal4: TJvDBSpinEdit
        Left = 96
        Top = 96
        Width = 73
        Height = 24
        ButtonKind = bkClassic
        TabOrder = 5
        DataField = 'expVars[4]'
        DataSource = dsCharClass
      end
      object cbxExpAlgorithm: TDBLookupComboBox
        Left = 8
        Top = 14
        Width = 161
        Height = 24
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
        Left = 8
        Top = 25
        Width = 173
        Height = 161
        TabOrder = 0
        Tabs.Strings = (
          'Portrait'
          'Sprite'
          'Battle Sprite')
        TabIndex = 0
        OnChange = tabGraphicsChange
        object btnSetGfx: TButton
          Left = 16
          Top = 126
          Width = 75
          Height = 25
          Caption = '&Set'
          TabOrder = 0
          OnClick = btnSetGfxClick
        end
        object imgMapSprite: TSdlFrame
          Left = 8
          Top = 24
          Width = 96
          Height = 96
          Framerate = 16
          Active = False
          OnAvailable = imgMapSpriteAvailable
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
        ActivePage = tshRepertoire
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        object tshEquipment: TTabSheet
          Caption = 'Equipment'
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object lblEquip1: TLabel
            Left = 8
            Top = 8
            Width = 52
            Height = 16
            Caption = 'Weapon:'
          end
          object lblEquip2: TLabel
            Left = 8
            Top = 38
            Width = 40
            Height = 16
            Caption = 'Shield:'
          end
          object lblEquip3: TLabel
            Left = 8
            Top = 64
            Width = 41
            Height = 16
            Caption = 'Armor:'
          end
          object lblEquip4: TLabel
            Left = 8
            Top = 92
            Width = 45
            Height = 16
            Caption = 'Helmet:'
          end
          object lblEquip5: TLabel
            Left = 8
            Top = 122
            Width = 62
            Height = 16
            Caption = 'Accessory:'
          end
          object cbxEquip1: TDBLookupComboBox
            Left = 64
            Top = 5
            Width = 121
            Height = 24
            DataField = 'weaponName'
            DataSource = dsCharClass
            TabOrder = 0
            OnClick = checkFor2Handed
          end
          object cbxEquip4: TDBLookupComboBox
            Left = 64
            Top = 89
            Width = 121
            Height = 24
            DataField = 'helmetName'
            DataSource = dsCharClass
            ListField = 'name'
            TabOrder = 2
          end
          object cbxEquip3: TDBLookupComboBox
            Left = 64
            Top = 62
            Width = 121
            Height = 24
            DataField = 'armorName'
            DataSource = dsCharClass
            ListField = 'name'
            TabOrder = 3
          end
          object cbxEquip5: TDBLookupComboBox
            Left = 64
            Top = 122
            Width = 121
            Height = 24
            DataField = 'accessoryName'
            DataSource = dsCharClass
            ListField = 'name'
            TabOrder = 4
          end
          object cbxEquip2: TDBLookupComboBox
            Left = 64
            Top = 35
            Width = 121
            Height = 24
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
              Height = 24
              ButtonKind = bkClassic
              ExplicitHeight = 24
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
            TitleFont.Height = -13
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
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lstScriptsDblClick
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
        Height = 24
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
            TitleFont.Height = -13
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
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
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
            TitleFont.Height = -13
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
    TitleFont.Height = -13
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
    DataSet = dmDatabase.charClasses_Resists
    Left = 232
    Top = 456
  end
  object dsCondition: TDataSource
    DataSet = dmDatabase.charClasses_Conditions
    Left = 304
    Top = 456
  end
end
