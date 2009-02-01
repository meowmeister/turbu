object frmConsole: TfrmConsole
  Left = 37
  Top = 38
  Caption = 'Console'
  ClientHeight = 278
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 176
    Width = 128
    Height = 13
    Caption = 'Enter your command here:'
  end
  object lblMemAllocated: TLabel
    Left = 16
    Top = 240
    Width = 3
    Height = 13
  end
  object txtScriptLine: TEdit
    Left = 8
    Top = 200
    Width = 321
    Height = 21
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 116
    Top = 240
    Width = 93
    Height = 25
    Caption = '&Execute Script'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object pclDisplay: TPageControl
    Left = 8
    Top = 8
    Width = 323
    Height = 162
    ActivePage = tcpSwitches
    TabOrder = 2
    object tcpSwitches: TTabSheet
      Caption = '&Switches'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object vleSwitches: TValueListEditor
        Left = 3
        Top = 0
        Width = 306
        Height = 100
        Strings.Strings = (
          '')
        TabOrder = 0
        TitleCaptions.Strings = (
          'Switch Name'
          'Value')
        ColWidths = (
          150
          150)
      end
      object btnSwitchRescan: TButton
        Left = 104
        Top = 106
        Width = 75
        Height = 25
        Caption = '&Rescan'
        TabOrder = 1
        OnClick = btnSwitchRescanClick
      end
    end
    object tcpVariables: TTabSheet
      Caption = '&Variables'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object vleVariables: TValueListEditor
        Left = 3
        Top = 0
        Width = 306
        Height = 100
        Strings.Strings = (
          '')
        TabOrder = 0
        TitleCaptions.Strings = (
          'Variable Name'
          'Value')
        ColWidths = (
          150
          150)
      end
      object btnVariableRescan: TButton
        Left = 112
        Top = 109
        Width = 75
        Height = 25
        Caption = '&Rescan'
        TabOrder = 1
        OnClick = btnVariableRescanClick
      end
    end
  end
end
