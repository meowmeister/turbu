inherited frmShowChoice: TfrmShowChoice
  Caption = 'Show Choice'
  ClientHeight = 381
  ClientWidth = 528
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 528
    Height = 328
    object GroupBox2: TGroupBox
      Left = 8
      Top = 146
      Width = 305
      Height = 57
      Caption = 'Choice 2'
      TabOrder = 1
      object txtChoice2: TEdit
        Left = 10
        Top = 16
        Width = 283
        Height = 24
        TabOrder = 0
        Text = 'No'
      end
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 205
      Width = 305
      Height = 57
      Caption = 'Choice 3'
      TabOrder = 2
      object txtChoice3: TEdit
        Left = 10
        Top = 16
        Width = 283
        Height = 24
        TabOrder = 0
      end
    end
    object GroupBox4: TGroupBox
      Left = 8
      Top = 264
      Width = 305
      Height = 57
      Caption = 'Choice 4'
      TabOrder = 3
      object txtChoice4: TEdit
        Left = 10
        Top = 16
        Width = 283
        Height = 24
        TabOrder = 0
      end
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 88
      Width = 305
      Height = 57
      Caption = 'Choice 1'
      TabOrder = 0
      object txtChoice1: TEdit
        Left = 10
        Top = 16
        Width = 283
        Height = 24
        TabOrder = 0
        Text = 'Yes'
      end
    end
    object grpCancel: TRadioGroup
      Left = 319
      Top = 88
      Width = 201
      Height = 233
      Caption = 'Cancel handler'
      ItemIndex = 2
      Items.Strings = (
        'No cancel'
        'Choice 1'
        'Choice 2'
        'Choice 3'
        'Choice 4'
        'Custom case')
      TabOrder = 4
    end
    object GroupBox5: TGroupBox
      Left = 8
      Top = 0
      Width = 512
      Height = 81
      Caption = 'Message (Optional)'
      TabOrder = 5
      DesignSize = (
        512
        81)
      object txtLines: TMemo
        Left = 10
        Top = 22
        Width = 487
        Height = 48
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  inherited btnOK: TButton
    Left = 245
    Top = 338
  end
  inherited btnCancel: TButton
    Left = 339
    Top = 338
  end
  inherited btnHelp: TButton
    Left = 432
    Top = 338
  end
end
