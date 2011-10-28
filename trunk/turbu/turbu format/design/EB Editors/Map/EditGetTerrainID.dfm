inherited frmGetID: TfrmGetID
  Caption = 'Get Terrain ID'
  ClientHeight = 360
  ClientWidth = 334
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 334
    Height = 307
    object GroupBox2: TGroupBox
      Left = 10
      Top = 79
      Width = 316
      Height = 207
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Tile location'
      TabOrder = 0
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
        Height = 104
        TabOrder = 1
        object Label2: TLabel
          Left = 16
          Top = 23
          Width = 79
          Height = 16
          Caption = 'X Coordinate:'
        end
        object Label3: TLabel
          Left = 16
          Top = 63
          Width = 78
          Height = 16
          Caption = 'Y Coordinate:'
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
      end
      object radPtr: TRadioButton
        Left = 8
        Top = 64
        Width = 114
        Height = 17
        Caption = 'From Variables'
        TabOrder = 3
      end
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 10
      Width = 316
      Height = 63
      Caption = 'Destination variable'
      TabOrder = 1
      object selLValue: TIntSelector
        Left = 8
        Top = 24
        Width = 297
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Left = 51
    Top = 317
  end
  inherited btnCancel: TButton
    Left = 145
    Top = 317
  end
  inherited btnHelp: TButton
    Left = 238
    Top = 317
  end
end
