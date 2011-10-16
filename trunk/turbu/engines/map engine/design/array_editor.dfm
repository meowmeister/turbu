object frmArrayEdit: TfrmArrayEdit
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '%s Selector'
  ClientHeight = 415
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 378
    Height = 335
    TabOrder = 0
    object lblArrayType: TLabel
      Left = 8
      Top = 8
      Width = 121
      Height = 23
      Alignment = taCenter
      AutoSize = False
      Caption = 'Variable'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -18
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object lstGroups: TListBox
      Left = 8
      Top = 32
      Width = 121
      Height = 250
      TabOrder = 0
      OnClick = lstGroupsClick
    end
    object Button1: TButton
      Left = 8
      Top = 291
      Width = 121
      Height = 33
      Caption = 'Array &Size'
      Enabled = False
      TabOrder = 1
    end
    object GroupBox1: TGroupBox
      Left = 142
      Top = 276
      Width = 228
      Height = 50
      Caption = 'Name'
      TabOrder = 2
      object lblFieldNumber: TLabel
        Left = 11
        Top = 22
        Width = 33
        Height = 16
        Caption = '0000:'
      end
      object DBEdit1: TDBEdit
        Left = 51
        Top = 19
        Width = 166
        Height = 24
        DataField = 'name'
        DataSource = srcList
        TabOrder = 0
      end
    end
    object RpgListGrid1: TRpgListGrid
      Left = 135
      Top = 8
      Width = 235
      Height = 264
      DataSource = srcList
      Options = [dgTabs, dgRowSelect, dgAlwaysShowSelection, dgCancelOnExit]
      TabOrder = 3
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -13
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'DisplayName'
          Width = 210
          Visible = True
        end>
    end
  end
  object btnOK: TButton
    Left = 40
    Top = 354
    Width = 80
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 136
    Top = 354
    Width = 80
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 232
    Top = 354
    Width = 80
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Apply'
    Enabled = False
    TabOrder = 3
  end
  object srcList: TDataSource
    DataSet = dmDatabase.Variables
    Left = 264
    Top = 32
  end
end
