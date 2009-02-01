unit battle_default_plugin;
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
   Forms, Contnrs,
   JvPlugin,
   turbu_plugin_interface;

type
   TRpgBasicBattlePlugin = class(TJvPlugin, ITurbuPluginInterface)
   public
      function listPlugins: TObjectList;
   end;

function RegisterPlugin: TRpgBasicBattlePlugin; stdcall;

exports
   RegisterPlugin;

implementation
uses
   turbu_2k_battle_engine, turbu_2k3_battle_engine;

{$R *.DFM}

function RegisterPlugin: TRpgBasicBattlePlugin;
begin
   Result := TRpgBasicBattlePlugin.Create(nil);
end;

{ TRpgBasicBattlePlugin }

function TRpgBasicBattlePlugin.listPlugins: TObjectList;
begin
   result := TObjectList.Create;
   result.Add(TEngineData.Create(et_battle, T2kBattleEngine));
   result.Add(TEngineData.Create(et_battle, T2k3BattleEngine));
end;

end.
