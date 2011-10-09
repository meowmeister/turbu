object frmMusicSelector: TfrmMusicSelector
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Music Selector'
  ClientHeight = 381
  ClientWidth = 411
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
  DesignSize = (
    411
    381)
  PixelsPerInch = 120
  TextHeight = 16
  object lstFilename: TListBox
    Left = 208
    Top = 8
    Width = 195
    Height = 319
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    OnDblClick = btnPlayClick
  end
  object btnPlay: TButton
    Left = 8
    Top = 301
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Play'
    TabOrder = 1
    OnClick = btnPlayClick
  end
  object btnStop: TButton
    Left = 104
    Top = 301
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Stop'
    TabOrder = 2
    OnClick = btnStopClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 194
    Height = 287
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 3
    object GroupBox1: TGroupBox
      Left = 9
      Top = 8
      Width = 176
      Height = 61
      Caption = 'Volume'
      TabOrder = 0
      object sldVolume: TJvxSlider
        Left = 2
        Top = 18
        Width = 172
        Height = 26
        Align = alTop
        Options = [soShowPoints, soSmooth]
        TabOrder = 0
        Value = 100
        OnChange = sldVolumeChange
      end
    end
    object GroupBox2: TGroupBox
      Left = 9
      Top = 79
      Width = 176
      Height = 61
      Caption = 'Panning'
      TabOrder = 1
      object sldPanning: TJvxSlider
        Left = 2
        Top = 18
        Width = 172
        Height = 26
        Align = alTop
        Increment = 25
        MaxValue = 255
        Options = [soShowPoints, soSmooth]
        TabOrder = 0
        Value = 127
        OnChange = sldPanningChange
      end
    end
    object GroupBox3: TGroupBox
      Left = 9
      Top = 146
      Width = 176
      Height = 61
      Caption = 'Tempo'
      TabOrder = 2
      object sldTempo: TJvxSlider
        Left = 2
        Top = 18
        Width = 172
        Height = 26
        Align = alTop
        MinValue = 50
        MaxValue = 150
        Options = [soShowPoints, soSmooth]
        TabOrder = 0
        Value = 100
        OnChange = sldTempoChange
      end
    end
    object grpFadeIn: TGroupBox
      Left = 9
      Top = 213
      Width = 176
      Height = 61
      Caption = 'Fade In'
      TabOrder = 3
      object sldFadeIn: TJvxSlider
        Left = 2
        Top = 18
        Width = 172
        Height = 26
        Align = alTop
        Increment = 25
        MaxValue = 500
        Options = [soShowPoints, soSmooth]
        TabOrder = 0
        OnChange = sldFadeInChange
      end
    end
  end
  object btnClose: TButton
    Left = 315
    Top = 338
    Width = 88
    Height = 35
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    ModalResult = 2
    TabOrder = 4
    OnClick = btnCloseClick
  end
  object btnSelect: TButton
    Left = 208
    Top = 338
    Width = 88
    Height = 35
    Anchors = [akRight, akBottom]
    Caption = '&Select'
    Default = True
    ModalResult = 1
    TabOrder = 5
    Visible = False
  end
end
