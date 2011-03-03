object openForm: TopenForm
  Left = 0
  Top = 0
  Caption = 'openForm'
  ClientHeight = 206
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dlgOpen: TOpenDialog
    Filter = 
      'RPG Maker 2000 maps (*.lmu)|*.lmu|RPG Maker 2000 projects (*.lmt' +
      ')|*.lmt'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 160
    Top = 104
  end
end
