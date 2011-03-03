inherited frmTimerEdit: TfrmTimerEdit
  Caption = 'Set Timer'
  ClientHeight = 305
  ClientWidth = 387
  ExplicitWidth = 393
  ExplicitHeight = 340
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 387
    Height = 252
    ExplicitWidth = 387
    ExplicitHeight = 252
    object radWhichTimer: TRadioGroup
      Left = 6
      Top = 3
      Width = 373
      Height = 49
      Caption = 'Which Timer'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Timer 1'
        'Timer 2')
      TabOrder = 0
    end
    object radOperation: TRadioGroup
      Left = 6
      Top = 58
      Width = 373
      Height = 49
      Caption = 'Action'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        'Set Timer'
        'Start'
        'Stop')
      TabOrder = 1
      OnClick = radOperationClick
    end
    object grpOptions: TGroupBox
      Left = 6
      Top = 195
      Width = 373
      Height = 49
      Caption = 'Timer Options'
      TabOrder = 2
      object chkVisible: TCheckBox
        Left = 8
        Top = 21
        Width = 97
        Height = 17
        Caption = 'Show Timer'
        Enabled = False
        TabOrder = 0
      end
      object chkBattle: TCheckBox
        Left = 182
        Top = 21
        Width = 139
        Height = 17
        Caption = 'Active during battles'
        Enabled = False
        TabOrder = 1
      end
    end
    object grpDuration: TGroupBox
      Left = 6
      Top = 109
      Width = 373
      Height = 80
      Caption = 'Duration'
      TabOrder = 3
      object lblMinutes: TLabel
        Left = 132
        Top = 25
        Width = 49
        Height = 16
        Caption = 'minutes,'
        Enabled = False
      end
      object lblSeconds: TLabel
        Left = 275
        Top = 25
        Width = 46
        Height = 16
        Caption = 'seconds'
        Enabled = False
      end
      object radFixed: TRadioButton
        Left = 8
        Top = 24
        Width = 65
        Height = 17
        Caption = 'Set to:'
        Checked = True
        Enabled = False
        TabOrder = 0
        TabStop = True
        OnClick = radFixedClick
      end
      object spnMinutes: TJvSpinEdit
        Left = 79
        Top = 20
        Width = 47
        Height = 24
        ButtonKind = bkStandard
        MaxValue = 100.000000000000000000
        Enabled = False
        TabOrder = 1
      end
      object spnSeconds: TJvSpinEdit
        Left = 222
        Top = 20
        Width = 47
        Height = 24
        ButtonKind = bkStandard
        MaxValue = 59.000000000000000000
        Enabled = False
        TabOrder = 2
      end
      object radVariable: TRadioButton
        Left = 8
        Top = 53
        Width = 140
        Height = 17
        Caption = 'Set to integer value:'
        Enabled = False
        TabOrder = 3
        OnClick = radFixedClick
      end
      object selSeconds: TIntSelector
        Left = 154
        Top = 50
        Width = 215
        Height = 24
        Enabled = False
      end
    end
  end
  inherited btnOK: TButton
    Left = 104
    Top = 262
    ExplicitLeft = 104
    ExplicitTop = 262
  end
  inherited btnCancel: TButton
    Left = 198
    Top = 262
    ExplicitLeft = 198
    ExplicitTop = 262
  end
  inherited btnHelp: TButton
    Left = 291
    Top = 262
    ExplicitLeft = 291
    ExplicitTop = 262
  end
end
