inherited frmEBEditTeleport: TfrmEBEditTeleport
  Caption = 'Teleport'
  ClientHeight = 337
  ClientWidth = 585
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  inherited Panel1: TPanel
    Width = 585
    Height = 284
    object Panel2: TPanel
      Left = 10
      Top = 10
      Width = 570
      Height = 223
      Align = alClient
      TabOrder = 0
      object trvMapTree: TTreeView
        Left = 1
        Top = 1
        Width = 208
        Height = 221
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alLeft
        Indent = 19
        ReadOnly = True
        TabOrder = 0
      end
      object sbxMain: TScrollBox
        Left = 209
        Top = 1
        Width = 360
        Height = 221
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        HorzScrollBar.Tracking = True
        VertScrollBar.ButtonSize = 15
        VertScrollBar.Margin = 10
        VertScrollBar.Range = 131
        VertScrollBar.Size = 15
        VertScrollBar.Style = ssHotTrack
        VertScrollBar.ThumbSize = 10
        VertScrollBar.Tracking = True
        Align = alClient
        AutoScroll = False
        Color = clBlack
        ParentColor = False
        TabOrder = 1
        object imgBackground: TPaintBox
          Left = 0
          Top = 0
          Width = 334
          Height = 195
          Align = alClient
          OnPaint = imgBackgroundPaint
        end
        object pnlHorizScroll: TPanel
          Left = 0
          Top = 195
          Width = 356
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 0
          object sbHoriz: TScrollBar
            Left = 0
            Top = 0
            Width = 334
            Height = 22
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alClient
            PageSize = 100
            TabOrder = 0
            OnScroll = OnScrollMap
          end
          object pnlCorner: TPanel
            Left = 334
            Top = 0
            Width = 22
            Height = 22
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alRight
            Caption = 'pnlCorner'
            TabOrder = 1
          end
        end
        object pnlVertScroll: TPanel
          Left = 334
          Top = 0
          Width = 22
          Height = 195
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object sbVert: TScrollBar
            Left = 0
            Top = 0
            Width = 22
            Height = 195
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alClient
            Kind = sbVertical
            PageSize = 100
            TabOrder = 0
            OnScroll = OnScrollMap
          end
        end
        object imgView: TSdlFrame
          Left = 0
          Top = 0
          Width = 334
          Height = 195
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Framerate = 0
          Active = False
          LogicalWidth = 334
          LogicalHeight = 195
          OnAvailable = imgViewAvailable
          Align = alClient
          OnMouseDown = imgViewMouseDown
          OnMouseMove = imgViewMouseMove
          OnMouseUp = imgViewMouseUp
        end
      end
    end
    object radFacing: TRadioGroup
      Left = 10
      Top = 233
      Width = 570
      Height = 46
      Align = alBottom
      Caption = 'Facing After Teleport'
      Columns = 5
      ItemIndex = 0
      Items.Strings = (
        'Unchanged'
        'Up'
        'Right'
        'Down'
        'Left')
      TabOrder = 1
    end
  end
  inherited btnOK: TButton
    Left = 302
    Top = 294
  end
  inherited btnCancel: TButton
    Left = 396
    Top = 294
  end
  inherited btnHelp: TButton
    Left = 489
    Top = 294
  end
end
