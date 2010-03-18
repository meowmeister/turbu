object frmDatabase: TfrmDatabase
  Left = 0
  Top = 0
  Caption = 'Database Viewer'
  ClientHeight = 558
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 393
    Top = 500
    Width = 84
    Height = 30
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 483
    Top = 500
    Width = 81
    Height = 30
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnApply: TButton
    Left = 570
    Top = 500
    Width = 79
    Height = 30
    Caption = '&Apply'
    Enabled = False
    TabOrder = 2
    OnClick = applyChanges
  end
  object btnHelp: TButton
    Left = 686
    Top = 500
    Width = 80
    Height = 30
    Caption = '&Help'
    Enabled = False
    TabOrder = 3
  end
  object tabPages: TPageControl
    Left = 8
    Top = 7
    Width = 793
    Height = 487
    ActivePage = tshClass
    TabOrder = 4
    object tshClass: TTabSheet
      Caption = '&Class'
      OnShow = tshClassShow
      inline frmClass: TframeClass
        Left = 0
        Top = 0
        Width = 785
        Height = 459
        TabOrder = 0
        TabStop = True
        ExplicitHeight = 459
        inherited navAdd: TDBNavigator
          Hints.Strings = ()
        end
        inherited navDel: TDBNavigator
          Hints.Strings = ()
        end
      end
    end
    object tshHero: TTabSheet
      Caption = '&Hero'
      ImageIndex = 1
    end
    object tshItems: TTabSheet
      Caption = 'Raw &Data Viewer'
      ImageIndex = 3
      inline frameItems1: TframeItems
        Left = -3
        Top = 0
        Width = 785
        Height = 538
        TabOrder = 0
        ExplicitLeft = -3
        inherited pnlItems: TPanel
          Left = -3
          ExplicitLeft = -3
          inherited DBGrid1: TDBGrid
            Left = 7
            Top = 3
            Height = 417
          end
        end
        inherited dsWeapons: TDataSource
          DataSet = dmDatabase.skillGainRecords
        end
      end
    end
    object tshGlobalEvents: TTabSheet
      Caption = '&Global Events'
      ImageIndex = 1
      object lblGlobalEvents: TLabel
        Left = 16
        Top = 16
        Width = 191
        Height = 28
        Alignment = taCenter
        AutoSize = False
        Caption = 'GLOBAL EVENTS'
        Color = clBlack
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -21
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
      object lstEvents: TListBox
        Left = 16
        Top = 39
        Width = 191
        Height = 409
        ExtendedSelect = False
        ItemHeight = 13
        TabOrder = 0
        OnClick = lstEventsClick
      end
      object pnlEvents: TPanel
        Left = 232
        Top = 0
        Width = 553
        Height = 457
        BevelInner = bvLowered
        BevelOuter = bvLowered
        TabOrder = 1
        object grpName: TGroupBox
          Left = 16
          Top = 8
          Width = 169
          Height = 41
          Caption = 'Name'
          TabOrder = 0
          object txtName: TEdit
            Left = 8
            Top = 13
            Width = 153
            Height = 21
            TabOrder = 0
          end
        end
        object grpStartCondition: TGroupBox
          Left = 191
          Top = 8
          Width = 185
          Height = 41
          Caption = 'Event Start Condition'
          TabOrder = 1
          object cbxStartCondition: TComboBox
            Left = 8
            Top = 13
            Width = 169
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            Items.Strings = (
              'Call'
              'Auto Start'
              'Parallel Process')
          end
        end
        object grpConditionSwitch: TGroupBox
          Left = 382
          Top = 8
          Width = 171
          Height = 41
          Caption = 'Condition Switch'
          TabOrder = 2
          object chkHasSwitch: TCheckBox
            Left = 3
            Top = 15
            Width = 22
            Height = 17
            TabOrder = 0
            OnClick = chkHasSwitchClick
          end
          object txtCondSwitch: TEdit
            Left = 24
            Top = 14
            Width = 140
            Height = 21
            Enabled = False
            ReadOnly = True
            TabOrder = 1
          end
          object btnCondSwitch: TButton
            Left = 146
            Top = 16
            Width = 17
            Height = 17
            Caption = '...'
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
          end
        end
        object grpEventCommands: TGroupBox
          Left = 16
          Top = 55
          Width = 537
          Height = 362
          Caption = 'Event Commands'
          TabOrder = 3
          DesignSize = (
            537
            362)
          object txtEventScript: TMemo
            Left = 8
            Top = 16
            Width = 521
            Height = 337
            Anchors = [akLeft, akTop, akRight, akBottom]
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
        end
        object btnCodeView: TButton
          Left = 24
          Top = 423
          Width = 98
          Height = 29
          Caption = '&Switch Code View'
          TabOrder = 4
          OnClick = btnCodeViewClick
        end
      end
    end
  end
end
