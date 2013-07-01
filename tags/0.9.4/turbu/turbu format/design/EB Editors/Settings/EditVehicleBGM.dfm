inherited frmEditVehicleBGM: TfrmEditVehicleBGM
  Caption = 'Change Vehicle BGM'
  ClientHeight = 118
  ClientWidth = 290
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 290
    Height = 65
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
    end
    object selMusic: TSoundEdit
      Left = 72
      Top = 36
      Width = 210
      Height = 24
    end
  end
  inherited btnOK: TButton
    Left = 7
    Top = 75
  end
  inherited btnCancel: TButton
    Left = 101
    Top = 75
  end
  inherited btnHelp: TButton
    Left = 194
    Top = 75
  end
  object cdsVehicles: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'id'
    Params = <>
    Left = 40
    Top = 112
    object cdsVehiclesid: TIntegerField
      FieldName = 'id'
      Required = True
    end
    object cdsVehiclesname: TWideStringField
      FieldName = 'name'
      Required = True
    end
    object cdsVehiclesmusic_id: TIntegerField
      FieldName = 'music_id'
      Required = True
    end
    object cdsVehiclesmusic_FadeIn: TIntegerField
      FieldName = 'music_FadeIn'
      Required = True
    end
    object cdsVehiclesmusic_tempo: TIntegerField
      FieldName = 'music_tempo'
      Required = True
    end
    object cdsVehiclesmusic_volume: TIntegerField
      FieldName = 'music_volume'
      Required = True
    end
    object cdsVehiclesmusic_balance: TIntegerField
      FieldName = 'music_balance'
      Required = True
    end
    object cdsVehiclesmusic_name: TWideStringField
      FieldName = 'music_name'
      Required = True
      Size = 255
    end
  end
end
