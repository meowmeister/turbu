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
unit rs_battle;

interface
uses
   turbu_defs, turbu_battle_engine, turbu_script_engine;

function battle(which: integer; const background: string; firstStrike: boolean;
  results: TBattleResultSet): TBattleResult;
function battleEx(which: integer; background: string; formation: TBattleFormation;
  results: TBattleResultSet; bgMode, terrain: integer): TBattleResult;


procedure RegisterScriptUnit(engine: TScriptEngine);

implementation
uses
   turbu_database, turbu_monsters, turbu_battles, turbu_map_metadata,
   turbu_2k_map_engine, turbu_2k_environment, turbu_2k_sprite_engine,
   rs_media, rs_maps,
   rsCompiler, rsExec,
   sg_defs;

function battle(which: integer; const background: string; firstStrike: boolean;
  results: TBattleResultSet): TBattleResult;
var
   formation: TBattleFormation;
begin
   if firstStrike then
      formation := bf_firstStrike
   else formation := bf_normal;
   result := battleEx(which, background, formation, results, 0, 0);
end;

function GetTerrainBackground: string;
var
   loc: TSgPoint;
   terrain: integer;
begin
   loc := GEnvironment.Party.Sprite.location;
   terrain := GSpriteEngine.GetTile(loc.x, loc.y, 0).terrain;
   result := GDatabase.terrains[terrain].battleBg;
end;

function GetMapBackground(metadata: TMapMetadata): string;
begin
   if metadata.battleBgName <> '' then
      result := metadata.battleBgName
   else result := GetTerrainBackground;
end;

function GetCurrentBackground: string;
var
   metadata: TMapMetadata;
begin
   result := '';
   metadata := GDatabase.mapTree[GSpriteEngine.MapID];
   while not (metadata.battleBgState = id_parent) and (metadata.parent = 0) do
      case metadata.battleBgState of
         id_parent: metadata := GDatabase.mapTree[metadata.parent];
         id_no: Exit(GetTerrainBackground);
         id_yes: Exit(GetMapBackground(metadata));
      end;
end;

function battleEx(which: integer; background: string; formation: TBattleFormation;
  results: TBattleResultSet; bgMode, terrain: integer): TBattleResult;
var
   mParty: TRpgMonsterParty;
   engine: IBattleEngine;
   conditions: TBattleConditions;
   battleResult: TBattleResultData;
begin
   if background = '' then
   begin
      case bgMode of
         0, 1: background := GetCurrentBackground;
         2: background := GDatabase.terrains[terrain].battleBg;
      end;
   end;
   mParty := GDatabase.monsterParties[which];
   engine := GGameEngine.DefaultBattleEngine;
   conditions := TBattleConditions.Create(background, formation, results);
   try
      rs_media.fadeOutMusic(0);
      rs_media.PlaySystemMusic(bgmBattle);
      rs_maps.eraseScreenDefault(trnBattleStartErase);
      GScriptEngine.ThreadWait;
      battleResult := engine.StartBattle(GEnvironment.Party, mParty, conditions);
      rs_maps.showScreenDefault(trnBattleEndShow);
      GScriptEngine.ThreadWait;
      rs_media.fadeOutMusic(0);
      rs_media.fadeInLastMusic(0);
      assert(battleResult.data = nil);
      result := battleResult.result;
   finally
      conditions.free;
   end;
end;

procedure RegisterBattlesC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TBattleResult));
   input.ImportType(TypeInfo(TBattleResultSet));
   input.ImportType(TypeInfo(TBattleFormation));
   input.ImportFunction('function battle(which: integer; const background: string; firstStrike: boolean; results: TBattleResultSet): TBattleResult');
   input.ImportFunction('function battleEx(which: integer; const background: string; formation: TBattleFormation; results: TBattleResultSet; bgMode, terrain: integer): TBattleResult');
end;

procedure RegisterBattlesE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('battle', @Battle);
   RegisterFunction('battleEx', @BattleEx);
end;

procedure RegisterScriptUnit(engine: TScriptEngine);
begin
   engine.RegisterUnit('battles', RegisterBattlesC, RegisterBattlesE);
end;

end.
