unit map_default_plugin;
{*****************************************************************************
* The contents of this file are used with permission, subject to
* the Mozilla Public License Version 1.1 (the "License"); you may
* not use this file except in compliance with the License. You may
* obtain a copy of the License at
* http://www.mozilla.org/MPL/MPL-1.1.html
*
* Software distributed under the License is distributed on an
* "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
* implied. See the License for the specific language governing
* rights and limitations under the License.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface
uses
   JvPlugin,
   turbu_plugin_interface;

type
   TRpgBasicMapPlugin = class(TJvPlugin, ITurbuPlugin)
   public
      function listPlugins: TEngineDataList;
   end;

function RegisterPlugin: TRpgBasicMapPlugin; stdcall;

exports
   RegisterPlugin;

implementation
uses turbu_2k_map_engine;

{$R *.DFM}

function RegisterPlugin: TRpgBasicMapPlugin;
begin
   result := TRpgBasicMapPlugin.Create(nil);
end;

{ TRpgBasicMapPlugin }

function TRpgBasicMapPlugin.listPlugins: TEngineDataList;
begin
   result := TEngineDataList.Create;
   result.Add(TEngineData.Create(et_map, T2kMapEngine));
end;

end.
