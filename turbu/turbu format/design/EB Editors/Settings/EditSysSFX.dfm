inherited frmEditSysSFX: TfrmEditSysSFX
  Caption = 'Change System SFX'
  ClientHeight = 118
  ClientWidth = 290
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 290
    Height = 65
    ExplicitWidth = 290
    ExplicitHeight = 65
    object StaticText1: TStaticText
      Left = 7
      Top = 8
      Width = 40
      Height = 20
      Caption = 'Event:'
      TabOrder = 0
    end
    object StaticText2: TStaticText
      Left = 7
      Top = 34
      Width = 38
      Height = 20
      Caption = 'Song:'
      TabOrder = 1
    end
    object cboWhich: TComboBox
      Left = 72
      Top = 6
      Width = 210
      Height = 24
      Style = csDropDownList
      TabOrder = 2
      OnChange = cboWhichChange
      Items.Strings = (
        'Cursor'
        'Accept'
        'Cancel'
        'Error'
        'Battle Start'
        'Escape'
        'Enemy Attack'
        'Enemy Damage'
        'Ally Damage'
        'Evade'
        'Enemy Death'
        'Item Used')
    end
    object selSFX: TSoundEdit
      Left = 72
      Top = 36
      Width = 210
      Height = 24
    end
  end
  inherited btnOK: TButton
    Left = 7
    Top = 75
    ExplicitLeft = 7
    ExplicitTop = 75
  end
  inherited btnCancel: TButton
    Left = 101
    Top = 75
    ExplicitLeft = 101
    ExplicitTop = 75
  end
  inherited btnHelp: TButton
    Left = 194
    Top = 75
    ExplicitLeft = 194
    ExplicitTop = 75
  end
end
