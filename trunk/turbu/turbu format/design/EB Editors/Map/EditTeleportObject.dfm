inherited frmEditTeleportObject: TfrmEditTeleportObject
  Caption = 'Teleport Map Object'
  ClientHeight = 347
  ClientWidth = 332
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 332
    Height = 294
    object GroupBox2: TGroupBox
      Left = 8
      Top = 79
      Width = 316
      Height = 209
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Teleport Location'
      TabOrder = 0
      object txtPosition: TRpgButtonEdit
        Left = 107
        Top = 29
        Width = 198
        Height = 24
        OnButtonClick = txtPositionButtonClick
      end
      object grpVariables: TGroupBox
        Left = 8
        Top = 89
        Width = 297
        Height = 104
        TabOrder = 1
        object Label3: TLabel
          Left = 16
          Top = 63
          Width = 78
          Height = 16
          Caption = 'Y Coordinate:'
        end
        object Label2: TLabel
          Left = 16
          Top = 23
          Width = 79
          Height = 16
          Caption = 'X Coordinate:'
        end
        object cboXVar: TIntSelector
          Left = 112
          Top = 20
          Width = 176
          Height = 24
        end
        object cboYVar: TIntSelector
          Left = 112
          Top = 60
          Width = 176
          Height = 24
        end
      end
      object radPosition: TRadioButton
        Left = 8
        Top = 33
        Width = 68
        Height = 17
        Caption = 'Position'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = RadioButtonClick
      end
      object radPtr: TRadioButton
        Left = 8
        Top = 65
        Width = 119
        Height = 17
        Caption = 'From Variables'
        TabOrder = 3
        OnClick = RadioButtonClick
      end
    end
  end
  inherited btnOK: TButton
    Left = 49
    Top = 304
  end
  inherited btnCancel: TButton
    Left = 143
    Top = 304
  end
  inherited btnHelp: TButton
    Left = 236
    Top = 304
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 316
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Object'
    TabOrder = 4
    DesignSize = (
      316
      65)
    object cboObject: TComboBox
      Left = 8
      Top = 24
      Width = 296
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = 'This Object'
      Items.Strings = (
        'This Object')
    end
  end
end
