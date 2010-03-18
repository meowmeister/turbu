object frmTestConsole: TfrmTestConsole
  Left = 0
  Top = 0
  Caption = 'Test Console'
  ClientHeight = 269
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 17
  object MainMenu1: TMainMenu
    Left = 144
    Top = 72
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuTestConversion1: TMenuItem
        Caption = 'Test &Conversion'
        OnClick = mnuTestConversion1Click
      end
      object mnuSetDefaultProject: TMenuItem
        Caption = '&Set Default Project'
        OnClick = mnuSetDefaultProjectClick
      end
      object estLDBLoading1: TMenuItem
        Caption = 'Test L&DB Loading'
        OnClick = estLDBLoading1Click
      end
    end
    object mnuGraphics: TMenuItem
      Caption = '&Graphics'
      object mnuCreateSdlWindow: TMenuItem
        Caption = '&Create SDL window'
        OnClick = mnuCreateSdlWindowClick
      end
      object mnuTestSDL: TMenuItem
        Caption = '&Test SDL window'
        OnClick = mnuTestSDLClick
      end
      object mnuTestMapLoading: TMenuItem
        Caption = 'Test &Map Loading'
        OnClick = mnuTestMapLoadingClick
      end
      object mnuEditMapProperties: TMenuItem
        Caption = '&Edit Map Properties'
        OnClick = mnuEditMapPropertiesClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuTestMapResizing: TMenuItem
        Caption = 'Test Map &Resizing Code'
        OnClick = mnuTestMapResizingClick
      end
      object mnuDebugMapResizing: TMenuItem
        Caption = '&Debug Map Resizing Code'
        OnClick = mnuDebugMapResizingClick
      end
    end
    object mnuDatabase: TMenuItem
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
    object mnuTree: TMenuItem
      Caption = '&Tree'
      object mnuTestMapTree: TMenuItem
        Caption = '&Test Map Tree'
        OnClick = mnuTestMapTreeClick
      end
    end
  end
end
