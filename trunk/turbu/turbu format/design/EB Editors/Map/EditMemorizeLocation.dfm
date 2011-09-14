inherited frmMemorizeLocation: TfrmMemorizeLocation
  Caption = 'Memorize Location'
  ClientHeight = 214
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Height = 161
    object GroupBox1: TGroupBox
      Left = 10
      Top = 10
      Width = 334
      Height = 146
      Align = alClient
      Caption = 'Variable locations'
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 32
        Width = 45
        Height = 16
        Caption = 'Map ID:'
      end
      object Label2: TLabel
        Left = 16
        Top = 69
        Width = 79
        Height = 16
        Caption = 'X Coordinate:'
      end
      object Label3: TLabel
        Left = 16
        Top = 109
        Width = 78
        Height = 16
        Caption = 'Y Coordinate:'
      end
      object cboMapID: TIntSelector
        Left = 112
        Top = 24
        Width = 211
        Height = 24
      end
      object cboXVar: TIntSelector
        Left = 112
        Top = 64
        Width = 211
        Height = 24
      end
      object cboYVar: TIntSelector
        Left = 112
        Top = 104
        Width = 211
        Height = 24
      end
    end
  end
  inherited btnOK: TButton
    Top = 171
  end
  inherited btnCancel: TButton
    Top = 171
  end
  inherited btnHelp: TButton
    Top = 171
  end
end
