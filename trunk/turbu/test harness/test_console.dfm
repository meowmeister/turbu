object frmTestConsole: TfrmTestConsole
  Left = 0
  Top = 0
  Caption = 'Test Console'
  ClientHeight = 206
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 144
    Top = 72
    object File1: TMenuItem
      Caption = '&File'
      object mnuTestConversion1: TMenuItem
        Caption = 'Test &Conversion'
        OnClick = mnuTestConversion1Click
      end
      object mnuSetDefaultProject: TMenuItem
        Caption = '&Set Default Project'
        OnClick = mnuSetDefaultProjectClick
      end
    end
    object Database1: TMenuItem
      Caption = '&Database'
      object mnuTestDatasets: TMenuItem
        Caption = 'Test &Datasets'
        OnClick = mnuTestDatasetsClick
      end
      object mnuTestLoading: TMenuItem
        Caption = 'Test Datafile &Loading'
        OnClick = mnuTestLoadingClick
      end
      object mnuTestDatabasewindow1: TMenuItem
        Caption = 'Test D&atabase window'
        OnClick = mnuTestDatabasewindow1Click
      end
    end
  end
end
