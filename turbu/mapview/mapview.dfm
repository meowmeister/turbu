object frmGameForm: TfrmGameForm
  Left = 183
  Top = 192
  Anchors = []
  Caption = 'Game Form'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object device: TAsphyreDevice
    Width = 320
    Height = 240
    BitDepth = bdHigh
    Refresh = 0
    Windowed = True
    VSync = False
    HardwareTL = True
    DepthBuffer = False
    WindowHandle = 0
    OnRender = deviceRender
    Left = 8
    Top = 8
  end
  object timer: TAsphyreTimer
    Speed = 40.000000000000000000
    MaxFPS = 40
    Enabled = False
    OnTimer = timerTimer
    Left = 48
    Top = 8
  end
  object fontEngine: TAsphyreFonts
    Publisher = device
    Left = 168
    Top = 8
  end
  object fontDB: TASDb
    FileName = 'rmfont.asdb'
    OpenMode = opReadOnly
    Left = 208
    Top = 8
  end
  object AsphyreTextures: TAsphyreTextures
    Publisher = device
    Left = 248
    Top = 8
  end
  object AsphyreKeyboard1: TAsphyreKeyboard
    Foreground = True
    Left = 288
    Top = 8
  end
end
