object frameImageSelector: TframeImageSelector
  Left = 0
  Top = 0
  Width = 545
  Height = 240
  TabOrder = 0
  DesignSize = (
    545
    240)
  object imgSelection: TImage
    Left = 8
    Top = 0
    Width = 320
    Height = 240
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object lstFilename: TListBox
    Left = 343
    Top = 0
    Width = 195
    Height = 240
    Anchors = [akTop, akRight, akBottom]
    TabOrder = 0
    OnClick = lstFilenameClick
  end
end
