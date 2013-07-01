inherited frmTeleportVehicle: TfrmTeleportVehicle
  Caption = 'Teleport Vehicle'
  ClientHeight = 371
  ClientWidth = 331
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 331
    Height = 318
    object GroupBox1: TGroupBox
      Left = 10
      Top = 10
      Width = 316
      Height = 63
      Align = alTop
      Caption = 'Vehicle'
      TabOrder = 0
      object cboVehicle: TIDLookupCombo
        Left = 8
        Top = 24
        Width = 297
        Height = 24
        KeyField = 'id'
        ListField = 'name'
        ListSource = srcVehicles
        TabOrder = 0
      end
    end
    object GroupBox2: TGroupBox
      Left = 10
      Top = 79
      Width = 316
      Height = 228
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Teleport Location'
      TabOrder = 1
      object txtPosition: TRpgButtonEdit
        Left = 85
        Top = 29
        Width = 220
        Height = 24
        OnButtonClick = txtPositionButtonClick
      end
      object grpVariables: TGroupBox
        Left = 8
        Top = 89
        Width = 297
        Height = 128
        TabOrder = 1
        object Label1: TLabel
          Left = 16
          Top = 15
          Width = 45
          Height = 16
          Caption = 'Map ID:'
        end
        object Label2: TLabel
          Left = 16
          Top = 55
          Width = 79
          Height = 16
          Caption = 'X Coordinate:'
        end
        object Label3: TLabel
          Left = 16
          Top = 95
          Width = 78
          Height = 16
          Caption = 'Y Coordinate:'
        end
        object cboMapID: TIntSelector
          Left = 112
          Top = 12
          Width = 176
          Height = 24
        end
        object cboXVar: TIntSelector
          Left = 112
          Top = 52
          Width = 176
          Height = 24
        end
        object cboYVar: TIntSelector
          Left = 112
          Top = 92
          Width = 176
          Height = 24
        end
      end
    end
  end
  inherited btnOK: TButton
    Left = 48
    Top = 328
  end
  inherited btnCancel: TButton
    Left = 142
    Top = 328
  end
  inherited btnHelp: TButton
    Left = 235
    Top = 328
  end
  object radPosition: TRadioButton
    Left = 18
    Top = 112
    Width = 68
    Height = 17
    Caption = 'Position'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = RadioButtonClick
  end
  object radPtr: TRadioButton
    Left = 18
    Top = 144
    Width = 119
    Height = 17
    Caption = 'From Variables'
    TabOrder = 5
    OnClick = RadioButtonClick
  end
  object srcVehicles: TDataSource
    DataSet = dmDatabase.vehicles
    Left = 8
    Top = 440
  end
end
