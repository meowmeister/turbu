unit rm2_turbu_terrain;

interface
uses
   turbu_terrain, LDB;

type
   T2k2Terrain = class helper for TRpgTerrain
   public
      constructor Convert(base: TTerrainInfo; id: integer);
   end;

implementation
uses
   Generics.Collections,
   turbu_sounds, rm2_turbu_sounds, charset_data, turbu_database;

{ T2k2Terrain }

constructor T2k2Terrain.Convert(base: TTerrainInfo; id: integer);
var
   vh: TVehicleSet;
   legacy: TPair<integer, AnsiString>;
begin
   inherited Create;
   FId := id;
   FName := string(base.name);
   FDamage := base.damage;
   FEncounterMultiplier := base.encounterMultiplier;
   FBattleBg := string(base.battleBg);
   setLength(FVehiclePass, ord(high(TVehicleSet)) + 1);
   for vh := Low(TVehicleSet) to High(TVehicleSet) do
      FVehiclePass[ord(vh)] := base.vehiclePass[vh];
   FAirshipLanding := base.airshipLanding;
   FConcealment := base.Concealment;
   if assigned(FSoundEffect) then
      FSoundEffect := TRpgSound.convert(base.soundEffect);
   FFrame := string(base.frame);
   for legacy in base.legacy do
      GDatabase.AddLegacy('terrain', id, legacy.Key, legacy.Value);
end;

end.
