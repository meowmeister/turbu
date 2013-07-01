object dmEngineManager: TdmEngineManager
  OldCreateOrder = False
  Height = 150
  Width = 215
  object pluginManager: TJvPluginManager
    Extension = 'TEP'
    PluginKind = plgPackage
    OnAfterLoad = pluginManagerAfterLoad
    OnBeforeUnload = pluginManagerBeforeUnload
    OnAfterUnload = pluginManagerAfterUnload
    Left = 40
    Top = 16
  end
end
