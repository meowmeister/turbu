object frmSpriteSelector: TfrmSpriteSelector
  Left = 0
  Top = 0
  Caption = 'Select Sprite'
  ClientHeight = 353
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 418
    Height = 297
    TabOrder = 0
    object lstFilenames: TListBox
      Left = 8
      Top = 12
      Width = 177
      Height = 260
      IntegralHeight = True
      TabOrder = 0
      OnClick = lstFilenamesClick
    end
    object Panel2: TPanel
      Left = 194
      Top = 12
      Width = 223
      Height = 276
      BevelOuter = bvNone
      TabOrder = 1
      object imgSelector: TSdlFrame
        Left = 0
        Top = 0
        Width = 196
        Height = 132
        Framerate = 0
        Active = False
        OnAvailable = imgSelectorAvailable
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 160
        Width = 113
        Height = 108
        Caption = 'Facing'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object radUp: TRadioButton
          Left = 32
          Top = 23
          Width = 41
          Height = 17
          Caption = 'Up'
          TabOrder = 0
        end
        object radRight: TRadioButton
          Left = 55
          Top = 51
          Width = 56
          Height = 19
          Caption = 'Right'
          TabOrder = 1
        end
        object radDown: TRadioButton
          Left = 32
          Top = 80
          Width = 55
          Height = 16
          Caption = 'Down'
          Checked = True
          TabOrder = 2
          TabStop = True
        end
        object radLeft: TRadioButton
          Left = 6
          Top = 51
          Width = 41
          Height = 19
          Caption = 'Left'
          TabOrder = 3
        end
      end
      object grpFrame: TRadioGroup
        Left = 120
        Top = 160
        Width = 76
        Height = 108
        Caption = 'Step'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Items.Strings = (
          'Left'
          'Center'
          'Right')
        ParentFont = False
        TabOrder = 2
      end
    end
  end
  object btnOK: TButton
    Left = 224
    Top = 316
    Width = 89
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 320
    Top = 316
    Width = 89
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
