object frmRmConverter: TfrmRmConverter
  Left = 256
  Top = 230
  Caption = 'RPG Maker Project Importer'
  ClientHeight = 250
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgress: TLabel
    Left = 8
    Top = 222
    Width = 58
    Height = 13
    Caption = 'Conversion:'
    Visible = False
  end
  object pagOptions: TPageControl
    Left = 0
    Top = 0
    Width = 500
    Height = 211
    ActivePage = tshBasic
    TabOrder = 0
    object tshBasic: TTabSheet
      Caption = 'Basic'
      object lblLocation: TLabel
        Left = 185
        Top = 27
        Width = 117
        Height = 13
        Caption = 'Original project location:'
      end
      object lblConvertLocation: TLabel
        Left = 170
        Top = 54
        Width = 132
        Height = 13
        Caption = 'Converted project location:'
      end
      object dirProjectLocation: TJvDirectoryEdit
        Left = 308
        Top = 24
        Width = 163
        Height = 21
        DialogKind = dkWin32
        DirectInput = False
        TabOrder = 0
        OnClick = dirProjectLocationClick
      end
      object dirOutput: TJvDirectoryEdit
        Left = 308
        Top = 51
        Width = 163
        Height = 21
        DialogKind = dkWin32
        DirectInput = False
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        ShowButton = False
        TabOrder = 1
      end
      object grpFormat: TRadioGroup
        Left = 3
        Top = 13
        Width = 150
        Height = 54
        Caption = 'Project &Format'
        ItemIndex = 0
        Items.Strings = (
          'RPG Maker 2000'
          'RPG Maker 2003')
        TabOrder = 2
      end
    end
    object tshEvents: TTabSheet
      Caption = 'Events'
      ImageIndex = 1
      object grpEventFormat: TRadioGroup
        Left = 3
        Top = 3
        Width = 134
        Height = 62
        Caption = 'Convert events to'
        ItemIndex = 0
        Items.Strings = (
          '&Event Builder format'
          '&RPG Script')
        TabOrder = 0
      end
    end
    object tshCull: TTabSheet
      Caption = 'Culling'
      ImageIndex = 2
      object lblCullSelect: TLabel
        Left = 16
        Top = 100
        Width = 33
        Height = 13
        Caption = 'Select:'
      end
      object lstCull: TListView
        Left = 4
        Top = 4
        Width = 170
        Height = 90
        Checkboxes = True
        Columns = <
          item
            Caption = 'Cull Unused'
            Width = 149
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnCullSelectDB: TButton
        Left = 3
        Top = 119
        Width = 75
        Height = 26
        Caption = 'All &DB entries'
        TabOrder = 1
      end
      object btnCullClear: TButton
        Left = 56
        Top = 151
        Width = 75
        Height = 26
        Caption = '&None'
        TabOrder = 2
      end
      object btnCullSelectGraphics: TButton
        Left = 107
        Top = 119
        Width = 75
        Height = 26
        Caption = 'All &Graphics'
        TabOrder = 3
      end
    end
  end
  object btnCancel: TButton
    Left = 400
    Top = 218
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnConvert: TButton
    Left = 304
    Top = 217
    Width = 75
    Height = 25
    Caption = 'C&onvert'
    Enabled = False
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object prgConversion: TProgressBar
    Left = 80
    Top = 217
    Width = 209
    Height = 25
    Max = 13
    Smooth = True
    Step = 1
    TabOrder = 3
    Visible = False
  end
  object tmrValidate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrValidateTimer
    Left = 432
  end
end
