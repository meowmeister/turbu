inherited frmEditSwapObjects: TfrmEditSwapObjects
  Caption = 'Swap Map Objects'
  ClientHeight = 212
  ClientWidth = 290
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 290
    Height = 159
    object GroupBox1: TGroupBox
      Left = 7
      Top = 8
      Width = 275
      Height = 65
      Anchors = [akLeft, akTop, akRight]
      Caption = 'First Object'
      TabOrder = 0
      DesignSize = (
        275
        65)
      object cboObject1: TComboBox
        Left = 8
        Top = 24
        Width = 255
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
    object GroupBox2: TGroupBox
      Left = 7
      Top = 79
      Width = 274
      Height = 65
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Second Object'
      TabOrder = 1
      DesignSize = (
        274
        65)
      object cboObject2: TComboBox
        Left = 8
        Top = 24
        Width = 254
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
  inherited btnOK: TButton
    Left = 7
    Top = 169
  end
  inherited btnCancel: TButton
    Left = 101
    Top = 169
  end
  inherited btnHelp: TButton
    Left = 194
    Top = 169
  end
end
