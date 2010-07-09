object frmRmConverter: TfrmRmConverter
  Left = 256
  Top = 230
  Caption = 'RPG Maker Project Importer'
  ClientHeight = 327
  ClientWidth = 654
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object pagOptions: TPageControl
    Left = 0
    Top = 0
    Width = 654
    Height = 276
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = tshBasic
    TabOrder = 0
    object tshBasic: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Basic'
      object lblLocation: TLabel
        Left = 222
        Top = 17
        Width = 148
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Original project location:'
      end
      object lblConvertLocation: TLabel
        Left = 222
        Top = 78
        Width = 169
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Converted project location:'
      end
      object dirProjectLocation: TJvDirectoryEdit
        Left = 222
        Top = 41
        Width = 394
        Height = 25
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DialogKind = dkWin32
        DirectInput = False
        ButtonWidth = 27
        TabOrder = 0
        OnClick = dirProjectLocationClick
      end
      object dirOutput: TJvDirectoryEdit
        Left = 222
        Top = 103
        Width = 394
        Height = 25
        Hint = 
          'Output folder selection is unavailable.  The project folder'#39's na' +
          'me and location'#13#10'will be generated automatically based on the or' +
          'iginal project folder'#39's name.'
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        DialogKind = dkWin32
        DirectInput = False
        ParentShowHint = False
        ReadOnly = True
        ShowHint = True
        ShowButton = False
        TabOrder = 1
      end
      object grpFormat: TRadioGroup
        Left = 4
        Top = 17
        Width = 196
        Height = 71
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Project &Format'
        ItemIndex = 0
        Items.Strings = (
          'RPG Maker 2000'
          'RPG Maker 2003')
        TabOrder = 2
      end
    end
    object tshEvents: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Events'
      Enabled = False
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grpEventFormat: TRadioGroup
        Left = 4
        Top = 4
        Width = 175
        Height = 81
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Convert events to'
        ItemIndex = 0
        Items.Strings = (
          '&Event Builder format'
          '&RPG Script')
        TabOrder = 0
      end
    end
    object tshCull: TTabSheet
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Culling'
      Enabled = False
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblCullSelect: TLabel
        Left = 21
        Top = 131
        Width = 41
        Height = 17
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Select:'
      end
      object lstCull: TListView
        Left = 5
        Top = 5
        Width = 223
        Height = 118
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Checkboxes = True
        Columns = <
          item
            Caption = 'Cull Unused'
            Width = 195
          end>
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnCullSelectDB: TButton
        Left = 4
        Top = 156
        Width = 98
        Height = 34
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'All &DB entries'
        TabOrder = 1
      end
      object btnCullClear: TButton
        Left = 73
        Top = 197
        Width = 98
        Height = 34
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = '&None'
        TabOrder = 2
      end
      object btnCullSelectGraphics: TButton
        Left = 140
        Top = 156
        Width = 98
        Height = 34
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'All &Graphics'
        TabOrder = 3
      end
    end
  end
  object btnCancel: TButton
    Left = 523
    Top = 285
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnConvert: TButton
    Left = 398
    Top = 284
    Width = 98
    Height = 32
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'C&onvert'
    Enabled = False
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object tmrValidate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrValidateTimer
    Left = 432
  end
end
