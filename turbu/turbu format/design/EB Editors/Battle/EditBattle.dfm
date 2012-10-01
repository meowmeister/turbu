inherited frmBattle: TfrmBattle
  Caption = 'Enter Battle'
  ClientHeight = 400
  ClientWidth = 378
  OnCreate = FormCreate
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 378
    Height = 347
    ExplicitWidth = 378
    ExplicitHeight = 347
  end
  inherited btnOK: TButton
    Left = 95
    Top = 357
    ExplicitLeft = 95
    ExplicitTop = 357
  end
  inherited btnCancel: TButton
    Left = 189
    Top = 357
    ExplicitLeft = 189
    ExplicitTop = 357
  end
  inherited btnHelp: TButton
    Left = 282
    Top = 357
    ExplicitLeft = 282
    ExplicitTop = 357
  end
  object grpMParty: TGroupBox
    Left = 8
    Top = 9
    Width = 362
    Height = 104
    Caption = 'Character'
    TabOrder = 4
    object cboMpartyID: TIDLookupCombo
      Left = 104
      Top = 28
      Width = 252
      Height = 24
      KeyField = 'id'
      ListField = 'name'
      ListSource = dmDatabaseAux.srcMPartyNames
      TabOrder = 0
    end
    object radSpecificParty: TRadioButton
      Left = 7
      Top = 31
      Width = 72
      Height = 17
      Caption = 'Specific:'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = RadioButtonClick
    end
    object radMPartyPtr: TRadioButton
      Left = 7
      Top = 66
      Width = 91
      Height = 17
      Caption = 'ID Value of:'
      TabOrder = 2
      OnClick = RadioButtonClick
    end
    object selMPartyID: TIntSelector
      Left = 104
      Top = 62
      Width = 252
      Height = 24
    end
  end
  object Background: TGroupBox
    Left = 8
    Top = 119
    Width = 362
    Height = 96
    Caption = 'Background'
    TabOrder = 5
    object radDefault: TRadioButton
      Left = 7
      Top = 31
      Width = 136
      Height = 17
      Caption = 'Map/Terrain Default'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButtonClick
    end
    object radFromFile: TRadioButton
      Left = 7
      Top = 66
      Width = 83
      Height = 17
      Caption = 'From File:'
      TabOrder = 1
      OnClick = RadioButtonClick
    end
    object selFilename: TImageEdit
      Left = 114
      Top = 62
      Width = 242
      Height = 24
    end
  end
  object grpOutcomes: TGroupBox
    Left = 8
    Top = 285
    Width = 362
    Height = 54
    Caption = 'Outcomes'
    TabOrder = 6
    object chkAllowEscape: TCheckBox
      Left = 7
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Allow Escape'
      TabOrder = 0
    end
    object chkDefeat: TCheckBox
      Left = 181
      Top = 24
      Width = 164
      Height = 17
      Caption = 'Custom event on defeat'
      TabOrder = 1
    end
  end
  object grpFormation: TGroupBox
    Left = 8
    Top = 221
    Width = 362
    Height = 57
    Caption = 'Start Conditions'
    TabOrder = 7
    object cboConditions: TComboBox
      Left = 7
      Top = 22
      Width = 338
      Height = 24
      TabOrder = 0
      Text = 'Normal'
      Items.Strings = (
        'Normal'
        'First Strike'
        'Surprised')
    end
  end
end
