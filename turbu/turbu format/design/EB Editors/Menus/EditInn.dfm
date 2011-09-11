inherited frmEBEditInn: TfrmEBEditInn
  Caption = 'Call Inn'
  ClientHeight = 217
  ClientWidth = 403
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 403
    Height = 165
    ExplicitWidth = 403
    ExplicitHeight = 165
    object GroupBox2: TGroupBox
      Left = 135
      Top = 92
      Width = 260
      Height = 66
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Inn Messages'
      TabOrder = 0
      object chkElseBlock: TCheckBox
        Left = 10
        Top = 32
        Width = 247
        Height = 17
        Caption = 'Create blocks for "Stay" and "No stay"'
        TabOrder = 0
      end
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 92
      Width = 121
      Height = 66
      Caption = 'Price'
      TabOrder = 1
      object spnPrice: TJvSpinEdit
        Left = 10
        Top = 28
        Width = 100
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 999999.000000000000000000
        TabOrder = 0
      end
    end
  end
  inherited btnOK: TButton
    Left = 120
    Top = 174
    ExplicitLeft = 120
    ExplicitTop = 174
  end
  inherited btnCancel: TButton
    Left = 214
    Top = 174
    ExplicitLeft = 214
    ExplicitTop = 174
  end
  inherited btnHelp: TButton
    Left = 307
    Top = 174
    ExplicitLeft = 307
    ExplicitTop = 174
  end
  object grpMessage: TGroupBox
    Left = 8
    Top = 15
    Width = 387
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Inn Messages'
    TabOrder = 4
    DesignSize = (
      387
      66)
    object cboStyles: TComboBox
      Left = 10
      Top = 26
      Width = 367
      Height = 24
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object innVocab: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    OnCalcFields = innVocabCalcFields
    OnFilterRecord = innVocabFilterRecord
    Left = 8
    Top = 492
    object innVocabKey: TWideStringField
      FieldName = 'Key'
      Required = True
      Size = 32
    end
    object innVocabVal: TWideStringField
      FieldName = 'Val'
      Required = True
      Size = 255
    end
    object innVocabid: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'id'
      Calculated = True
    end
  end
end
