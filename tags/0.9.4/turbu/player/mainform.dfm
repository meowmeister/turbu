object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'TURBU Player'
  ClientHeight = 720
  ClientWidth = 960
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object imgGame: TSdlFrame
    Left = 0
    Top = 0
    Width = 960
    Height = 720
    Framerate = 0
    Active = False
    LogicalWidth = 960
    LogicalHeight = 720
    OnAvailable = imgGameAvailable
    Align = alClient
  end
end
