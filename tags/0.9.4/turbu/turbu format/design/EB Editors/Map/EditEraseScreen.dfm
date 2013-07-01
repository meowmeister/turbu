inherited frmEBEditShowTransition: TfrmEBEditShowTransition
  Caption = 'Erase Screen'
  ClientHeight = 123
  ClientWidth = 288
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 288
    Height = 70
  end
  inherited btnOK: TButton
    Left = 5
    Top = 80
  end
  inherited btnCancel: TButton
    Left = 99
    Top = 80
  end
  inherited btnHelp: TButton
    Left = 192
    Top = 80
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 280
    Height = 51
    Caption = 'Transition'
    TabOrder = 4
    DesignSize = (
      280
      51)
    object cboTransition: TComboBox
      Left = 6
      Top = 19
      Width = 264
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Fade'
      Items.Strings = (
        'Default Transition'
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
        'Instant')
    end
  end
end
