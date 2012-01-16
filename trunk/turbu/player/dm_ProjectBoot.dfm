object dmProjectBoot: TdmProjectBoot
  OldCreateOrder = False
  Height = 150
  Width = 170
  object Connection: TSQLConnection
    LoginPrompt = False
    Left = 80
    Top = 32
  end
  object dsBoot: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQL.Strings = (
      'select EngineName, version'
      'from BOOT'
      'where ID = 0')
    SQLConnection = Connection
    Left = 80
    Top = 88
    object dsBootEngineName: TWideStringField
      FieldName = 'EngineName'
      Size = 255
    end
    object dsBootversion: TIntegerField
      FieldName = 'version'
    end
  end
end
