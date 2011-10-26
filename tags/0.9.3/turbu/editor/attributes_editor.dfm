object frmAttributesEditor: TfrmAttributesEditor
  Left = 0
  Top = 0
  Caption = 'Attributes Editor'
  ClientHeight = 269
  ClientWidth = 333
  Color = clBtnFace
  Constraints.MaxWidth = 450
  Constraints.MinWidth = 344
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    333
    269)
  PixelsPerInch = 120
  TextHeight = 17
  object JvDBUltimGrid1: TJvDBUltimGrid
    Left = 10
    Top = 10
    Width = 310
    Height = 190
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -14
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 21
    TitleRowHeight = 21
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
    Left = 10
    Top = 226
    Width = 99
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 222
    Top = 226
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
