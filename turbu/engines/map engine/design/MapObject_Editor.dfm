object frmObjectEditor: TfrmObjectEditor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Map Object Editor'
  ClientHeight = 886
  ClientWidth = 1038
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object pnlBackground: TPanel
    Left = 9
    Top = 10
    Width = 1015
    Height = 803
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    BevelWidth = 3
    TabOrder = 0
    object grpName: TGroupBox
      Left = 10
      Top = 10
      Width = 199
      Height = 53
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Name'
      TabOrder = 0
      object txtName: TEdit
        Left = 10
        Top = 21
        Width = 178
        Height = 24
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        TabOrder = 0
      end
    end
    object btnNew: TButton
      Left = 217
      Top = 17
      Width = 191
      Height = 46
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&New Page'
      TabOrder = 1
      OnClick = btnNewClick
    end
    object btnCopy: TButton
      Left = 416
      Top = 17
      Width = 191
      Height = 46
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Copy Page'
      TabOrder = 2
      OnClick = btnCopyClick
    end
    object btnPaste: TButton
      Left = 615
      Top = 17
      Width = 191
      Height = 46
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Paste Page'
      Enabled = False
      TabOrder = 3
      OnClick = btnPasteClick
    end
    object btnDelete: TButton
      Left = 813
      Top = 17
      Width = 191
      Height = 46
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = '&Delete Page'
      Enabled = False
      TabOrder = 4
      OnClick = btnDeleteClick
    end
    object tabEventPages: TTabControl
      Left = 10
      Top = 72
      Width = 994
      Height = 713
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 5
      OnChange = tabEventPagesChange
      object gbxEventText: TGroupBox
        Left = 431
        Top = 38
        Width = 552
        Height = 665
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Event Text'
        TabOrder = 0
        object trvEvents: TEBTreeView
          Left = 2
          Top = 18
          Width = 548
          Height = 645
          Align = alClient
          Indent = 19
          TabOrder = 0
          Context = dsContext
        end
      end
      object Panel1: TPanel
        Left = 10
        Top = 446
        Width = 408
        Height = 257
        BevelOuter = bvNone
        TabOrder = 1
        object gbxGraphic: TGroupBox
          Left = 10
          Top = 0
          Width = 181
          Height = 118
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Select Graphic'
          TabOrder = 0
          object imgEventSprite: TSdlFrame
            Left = 13
            Top = 22
            Width = 69
            Height = 77
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Framerate = 0
            Active = False
            LogicalWidth = 69
            LogicalHeight = 77
            OnAvailable = imgEventSpriteAvailable
            OnKeyDown = imgEventSpriteKeyDown
          end
          object chkTransparent: TDBCheckBox
            Left = 90
            Top = 21
            Width = 79
            Height = 22
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Transp.'
            DataField = 'Transparent'
            DataSource = srcPages
            TabOrder = 0
            ValueChecked = 'True'
            ValueUnchecked = 'False'
          end
          object btnSetImage: TButton
            Left = 90
            Top = 72
            Width = 70
            Height = 33
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = '&Set...'
            TabOrder = 1
            OnClick = btnSetImageClick
          end
        end
        object gbxEventTrigger: TGroupBox
          Left = 189
          Top = 0
          Width = 219
          Height = 55
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Event Trigger'
          TabOrder = 1
          object cbxEventTrigger: TDBIndexComboBox
            Left = 10
            Top = 20
            Width = 202
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            DataField = 'StartCondition'
            DataSource = srcPages
            Items.Strings = (
              'Action button'
              'Hero tags event'
              'Event tags hero'
              'Auto (cutscene)'
              'Auto (background)')
            TabOrder = 0
          end
        end
        object gbxPosition: TGroupBox
          Left = 189
          Top = 63
          Width = 219
          Height = 76
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Position'
          TabOrder = 2
          object cbxZOrder: TDBIndexComboBox
            Left = 10
            Top = 20
            Width = 202
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            DataField = 'EventHeight'
            DataSource = srcPages
            Items.Strings = (
              'Below hero'
              'Same level as hero'
              'Above hero')
            TabOrder = 0
          end
          object chkSolidEvent: TDBCheckBox
            Left = 10
            Top = 50
            Width = 180
            Height = 22
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Solid event (no overlap)'
            DataField = 'NoOverlap'
            DataSource = srcPages
            TabOrder = 1
            ValueChecked = 'True'
            ValueUnchecked = 'False'
          end
        end
        object gbxAnimationStyle: TGroupBox
          Left = 189
          Top = 140
          Width = 219
          Height = 55
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Animation Style'
          TabOrder = 3
          object cbxAnimStyle: TDBIndexComboBox
            Left = 10
            Top = 20
            Width = 202
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            DataField = 'AnimType'
            DataSource = srcPages
            Items.Strings = (
              'Standing (normal)'
              'Walking in place'
              'Standing (fixed direction)'
              'Walking (fixed direction)'
              'Statue'
              'Spin (right)'
              'Spin (left)')
            TabOrder = 0
          end
        end
        object gbxMoveSpeed: TGroupBox
          Left = 189
          Top = 199
          Width = 219
          Height = 55
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Movement Speed'
          TabOrder = 4
          object cbxMoveSpeed: TDBIndexComboBox
            Left = 10
            Top = 20
            Width = 202
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            DataField = 'MoveSpeed'
            DataSource = srcPages
            Items.Strings = (
              '1: 8x slower'
              '2: 4x slower'
              '3: 2x slower'
              '4: Normal'
              '5: 2x faster'
              '6: 4x faster')
            TabOrder = 0
          end
        end
        object gbxMoveData: TGroupBox
          Left = 10
          Top = 115
          Width = 181
          Height = 139
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Movement Information'
          TabOrder = 5
          object lblMoveFreq: TLabel
            Left = 10
            Top = 67
            Width = 64
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = 'Frequency:'
          end
          object cbxMoveType: TDBIndexComboBox
            Left = 10
            Top = 31
            Width = 157
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            DataField = 'MoveType'
            DataSource = srcPages
            Items.Strings = (
              'Stay still'
              'Random movement'
              'Cycle up-down'
              'Cycle left-right'
              'Chase hero'
              'Flee from hero'
              'Fixed route')
            TabOrder = 0
          end
          object cbxMoveFreq: TDBIndexComboBox
            Left = 90
            Top = 63
            Width = 79
            Height = 24
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            DataField = 'MoveFrequency'
            DataSource = srcPages
            Items.Strings = (
              '1'
              '2'
              '3'
              '4'
              '5'
              '6'
              '7'
              '8')
            TabOrder = 1
          end
          object btnEditRoute: TButton
            Left = 10
            Top = 98
            Width = 157
            Height = 33
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = '&Edit Route'
            Enabled = False
            TabOrder = 2
          end
        end
      end
      inline frameConditions: TframeConditions
        Left = 19
        Top = 38
        Width = 409
        Height = 401
        TabOrder = 2
        ExplicitLeft = 19
        ExplicitTop = 38
        inherited gbxConditions: TGroupBox
          inherited lblMinutes2: TLabel
            Transparent = True
          end
        end
        inherited dsConditions: TClientDataSet
          Active = True
          MasterFields = 'Id'
          MasterSource = srcPages
          PacketRecords = 0
          AfterPost = SetDirty
          Data = {
            CC0100009619E0BD01000000180000001A000000000003000000CC01064D6173
            7465720400010000000000077377697463683104000100000000000773776974
            6368320400010000000000097661726961626C65310400010000000000097661
            726961626C6532040001000000000006566172314F7001000200000000000656
            6172324F7001000200000000000956617256616C756531040001000000000009
            56617256616C7565320400010000000000044974656D04000100000000000448
            65726F04000100000000000A436C6F636B314D696E7304000100000000000A43
            6C6F636B3153656373040001000000000008436C6F636B314F70010002000000
            00000A436C6F636B324D696E7304000100000000000A436C6F636B3253656373
            040001000000000008436C6F636B324F70010002000000000006536372697074
            02004A000000010005574944544802000200FE01086253776974636831020003
            0000000000086253776974636832020003000000000005625661723102000300
            00000000056256617232020003000000000005624974656D0200030000000000
            05624865726F0200030000000000076254696D65723102000300000000000762
            54696D65723202000300000000000000}
        end
      end
    end
  end
  object btnOK: TButton
    Left = 566
    Top = 824
    Width = 110
    Height = 39
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 684
    Top = 824
    Width = 106
    Height = 39
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 798
    Top = 824
    Width = 103
    Height = 39
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Apply'
    Enabled = False
    TabOrder = 3
    OnClick = btnApplyClick
  end
  object btnHelp: TButton
    Left = 918
    Top = 824
    Width = 105
    Height = 39
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Help'
    Enabled = False
    TabOrder = 4
  end
  object Button1: TButton
    Left = 298
    Top = 831
    Width = 75
    Height = 25
    Caption = '&View'
    TabOrder = 5
    OnClick = Button1Click
  end
  object btnScript: TButton
    Left = 383
    Top = 831
    Width = 90
    Height = 25
    Caption = 'View Script'
    TabOrder = 6
    OnClick = btnScriptClick
  end
  object dsPages: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'Id'
        DataType = ftInteger
      end
      item
        Name = 'Name'
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'Modified'
        DataType = ftBoolean
      end
      item
        Name = 'frame'
        DataType = ftWord
      end
      item
        Name = 'Transparent'
        DataType = ftBoolean
      end
      item
        Name = 'Direction'
        DataType = ftByte
      end
      item
        Name = 'MoveType'
        DataType = ftByte
      end
      item
        Name = 'MoveFrequency'
        DataType = ftByte
      end
      item
        Name = 'StartCondition'
        DataType = ftByte
      end
      item
        Name = 'EventHeight'
        DataType = ftByte
      end
      item
        Name = 'NoOverlap'
        DataType = ftBoolean
      end
      item
        Name = 'AnimType'
        DataType = ftByte
      end
      item
        Name = 'MoveSpeed'
        DataType = ftByte
      end
      item
        Name = 'MoveIgnore'
        DataType = ftBoolean
      end
      item
        Name = 'EventText'
        DataType = ftWideMemo
      end
      item
        Name = 'Matrix'
        DataType = ftWord
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    IndexFieldNames = 'Id'
    Params = <>
    StoreDefs = True
    AfterPost = SetDirty
    Left = 32
    Top = 816
    Data = {
      6B0100009619E0BD0100000018000000100000000000030000006B0102496404
      00010000000000044E616D6502004A000000010005574944544802000200FE01
      084D6F6469666965640200030000000000056672616D6502000200000000000B
      5472616E73706172656E74020003000000000009446972656374696F6E010002
      0000000000084D6F76655479706501000200000000000D4D6F76654672657175
      656E637901000200000000000E5374617274436F6E646974696F6E0100020000
      0000000B4576656E744865696768740100020000000000094E6F4F7665726C61
      70020003000000000008416E696D547970650100020000000000094D6F766553
      7065656401000200000000000A4D6F766549676E6F7265020003000000000009
      4576656E745465787404004B0000000100075355425459504502004900090057
      6964655465787400064D6174726978020002000000000001000D44454641554C
      545F4F524445520200820000000000}
    object dsPagesId: TIntegerField
      FieldName = 'Id'
    end
    object dsPagesName: TWideStringField
      FieldName = 'Name'
      Size = 255
    end
    object dsPagesModified: TBooleanField
      FieldName = 'Modified'
    end
    object dsPagesframe: TWordField
      FieldName = 'frame'
    end
    object dsPagesTransparent: TBooleanField
      FieldName = 'Transparent'
    end
    object dsPagesDirection: TByteField
      FieldName = 'Direction'
    end
    object dsPagesMoveType: TByteField
      FieldName = 'MoveType'
    end
    object dsPagesMoveFrequency: TByteField
      FieldName = 'MoveFrequency'
    end
    object dsPagesStartCondition: TByteField
      FieldName = 'StartCondition'
    end
    object dsPagesEventHeight: TByteField
      FieldName = 'EventHeight'
    end
    object dsPagesNoOverlap: TBooleanField
      FieldName = 'NoOverlap'
    end
    object dsPagesAnimType: TByteField
      FieldName = 'AnimType'
    end
    object dsPagesMoveSpeed: TByteField
      FieldName = 'MoveSpeed'
    end
    object dsPagesMoveIgnore: TBooleanField
      FieldName = 'MoveIgnore'
    end
    object dsPagesEventText: TWideMemoField
      FieldName = 'EventText'
      BlobType = ftWideMemo
    end
    object dsPagesMatrix: TWordField
      FieldName = 'Matrix'
    end
  end
  object srcPages: TDataSource
    DataSet = dsPages
    Left = 88
    Top = 816
  end
  object dsContext: TClientDataSet
    Active = True
    Aggregates = <>
    Filtered = True
    FieldDefs = <
      item
        Name = 'name'
        DataType = ftWideString
        Size = 32
      end
      item
        Name = 'DisplayName'
        DataType = ftWideString
        Size = 32
      end
      item
        Name = 'type'
        DataType = ftWideString
        Size = 255
      end
      item
        Name = 'id'
        DataType = ftAutoInc
      end>
    IndexDefs = <
      item
        Name = 'idxFiltered'
        CaseInsFields = 'name'
        Fields = 'type;name'
        Options = [ixCaseInsensitive]
      end
      item
        Name = 'idxFull'
        CaseInsFields = 'name'
        Fields = 'name'
        Options = [ixPrimary, ixCaseInsensitive]
      end>
    IndexName = 'idxFull'
    ObjectView = False
    Params = <>
    StoreDefs = True
    Left = 248
    Top = 824
    Data = {
      A20000009619E0BD010000001800000004000000000003000000A200046E616D
      6501004A00000001000557494454480200020040000B446973706C61794E616D
      6501004A0000000100055749445448020002004000047479706502004A000000
      010005574944544802000200FE01026964040001000000010007535542545950
      450200490008004175746F696E630001000C4155544F494E4356414C55450400
      010001000000}
    object dsContextName: TWideStringField
      FieldName = 'name'
      Size = 32
    end
    object dsContextDisplayName: TWideStringField
      FieldName = 'DisplayName'
      Size = 32
    end
    object dsContextType: TWideStringField
      DisplayWidth = 32
      FieldName = 'type'
      Size = 255
    end
    object dsContextid: TAutoIncField
      FieldName = 'id'
    end
  end
end
