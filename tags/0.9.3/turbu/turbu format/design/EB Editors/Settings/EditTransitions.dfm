inherited frmEditTransitions: TfrmEditTransitions
  Caption = 'Change Default Transitions'
  ClientHeight = 105
  ClientWidth = 326
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 326
    Height = 52
    object GroupBox2: TGroupBox
      Left = 167
      Top = 0
      Width = 151
      Height = 51
      Caption = 'Transition'
      TabOrder = 0
      object cboTransition: TComboBox
        Left = 6
        Top = 19
        Width = 135
        Height = 24
        ItemIndex = 0
        TabOrder = 0
        Text = 'Fade'
        Items.Strings = (
          'Fade'
          'Blocks'
          'Wipe Down'
          'Wipe Up'
          'Venetian Blinds'
          'Vertical Blinds'
          'Horizontal Blinds'
          'Square Iris In'
          'Square Iris Out'
          'Slide Up'
          'Slide Down'
          'Slide Left'
          'Slide Right'
          'Divide Vertical'
          'Divide Horizontal'
          'Divide Quad'
          'Zoom'
          'Mosaic'
          'Ripple'
          'Instant'
          'None')
      end
    end
  end
  inherited btnOK: TButton
    Left = 43
    Top = 62
  end
  inherited btnCancel: TButton
    Left = 137
    Top = 62
  end
  inherited btnHelp: TButton
    Left = 230
    Top = 62
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 151
    Height = 51
    Caption = 'Event'
    TabOrder = 4
    object cboEvent: TComboBox
      Left = 6
      Top = 19
      Width = 135
      Height = 24
      TabOrder = 0
      Text = 'Teleport/Erase'
      Items.Strings = (
        'Map Exit'
        'Map Enter'
        'Battle Start/Erase'
        'Battle Start/Show'
        'Battle End/Erase'
        'Battle End/Show')
    end
  end
end
