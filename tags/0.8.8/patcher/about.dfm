object AboutBox: TAboutBox
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'About TURBU Patcher'
  ClientHeight = 213
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 13
    Width = 281
    Height = 161
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object ProductName: TLabel
      Left = 8
      Top = 16
      Width = 81
      Height = 13
      Caption = 'TURBU Patcher,'
      IsControl = True
    end
    object Version: TLabel
      Left = 92
      Top = 16
      Width = 53
      Height = 13
      Caption = 'Version 1.0'
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 35
      Width = 255
      Height = 65
      Caption = 
        'Copyright 2009, Mason Wheeler.  Licensed under the MPL open-sour' +
        'ce license.  The source code to this program is freely available' +
        ', and both the program and the code may be freely modified, cust' +
        'omized and distributed in accordance with the terms of the'
      WordWrap = True
      IsControl = True
    end
    object Comments: TLabel
      Left = 8
      Top = 119
      Width = 270
      Height = 143
      Caption = 
        'TURBU Patcher does not support multi-file patching yet.  That'#39's ' +
        'planned for version 2.'
      WordWrap = True
      IsControl = True
    end
    object LinkLabel1: TLinkLabel
      Left = 8
      Top = 101
      Width = 111
      Height = 17
      Caption = 
        '<A HREF="http://www.mozilla.org/MPL/MPL-1.1.html">Mozilla Public' +
        ' License</a>.'
      TabOrder = 0
      OnLinkClick = LinkLabel1LinkClick
    end
  end
  object OKButton: TButton
    Left = 111
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
