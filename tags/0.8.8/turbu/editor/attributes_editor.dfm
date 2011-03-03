object frmAttributesEditor: TfrmAttributesEditor
  Left = 0
  Top = 0
  Caption = 'Attributes Editor'
  ClientHeight = 206
  ClientWidth = 255
  Color = clBtnFace
  Constraints.MaxWidth = 263
  Constraints.MinWidth = 263
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    255
    206)
  PixelsPerInch = 96
  TextHeight = 13
  object JvDBUltimGrid1: TJvDBUltimGrid
    Left = 8
    Top = 8
    Width = 237
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 17
    TitleRowHeight = 17
    Columns = <
      item
        Expanded = False
        FieldName = 'id'
        ReadOnly = True
        Width = 24
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'name'
        Width = 85
        Visible = True
      end
      item
        Alignment = taCenter
        Expanded = False
        FieldName = 'requiredForSkills'
        Title.Caption = 'Required For Skills'
        Width = 89
        Visible = True
      end>
  end
  object btnOK: TButton
    Left = 8
    Top = 173
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 170
    Top = 173
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object DataSource: TDataSource
    DataSet = dmDatabase.attributes
    Left = 112
    Top = 168
  end
end
