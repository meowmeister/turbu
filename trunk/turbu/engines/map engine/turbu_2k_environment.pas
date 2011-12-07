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
unit turbu_2k_environment;

interface
uses
   Generics.Collections,
   rsCompiler, rsImport,
   turbu_defs, turbu_heroes, turbu_database, turbu_mapchars, turbu_2k_images,
   turbu_2k_map_timer;

type
   TKeyRange = 0..24;
   TKeyMask = set of TKeyRange;
   THeroList = class(TObjectList<TRpgHero>);
   TVehicleList = class(TObjectList<TRpgVehicle>);

   T2kEnvironment = class
   private
      FDatabase: TRpgDatabase;
      FHeroes: THeroList;
      FVehicles: TVehicleList;
      FSwitches: TArray<boolean>;
      FInts: TArray<integer>;
      FImages: TArray<TRpgImage>;
      FParty: TRpgParty;
      function GetSwitch(const i: integer): boolean;
      procedure SetSwitch(const i: integer; const Value: boolean);
      function GetHero(const i: integer): TRpgHero;
      function GetHeroCount: integer;
      function GetVehicle(i: integer): TRpgVehicle;
      function getBattleCount: integer;
      function battleFlees: integer;
      function battleLosses: integer;
      function battleVictories: integer;
      procedure canDieOnHpChange(const Value: boolean);
      procedure enableMenu(const Value: boolean);
      function getCash: integer;
      function getPartySize: integer;
      function getSaveCount: integer;
      function isMenuEnabled: boolean;
      procedure notifyOnLevelGain(const Value: boolean);
      procedure setCash(const Value: integer);
      function GetImage(i: integer): TRpgImage;
      procedure SetImage(i: integer; const Value: TRpgImage);
      function GetMapObject(i: integer): TRpgEvent;
      function getTimer: TRpgTimer;
      function getTimer2: TRpgTimer;
      function GetThisObject: TRpgCharacter;
   public
      [NoImport]
      constructor Create(database: TRpgDatabase);
      destructor Destroy; override;

      function keyScan(mask: TKeyMask; wait: boolean): integer;
      procedure wait(duration: integer);
      function HeldItems(id: integer; equipped: boolean): integer;
      procedure Shop(shopType: TShopTypes; messageSet: integer; inventory: TIntArray);
      function Random(low, high: integer): integer;
      procedure EnableSave(value: boolean);
      procedure GameOver;
      procedure DeleteObject(permanant: boolean);

      property Heroes[const i: integer]: TRpgHero read GetHero;
      property HeroCount: integer read GetHeroCount;
      property Switch[const i: integer]: boolean read GetSwitch write SetSwitch;
      property Vehicle[i: integer]: TRpgVehicle read GetVehicle;
      property Image[i: integer]: TRpgImage read GetImage write SetImage;
      property MapObject[i: integer]: TRpgEvent read GetMapObject;
      property Party: TRpgParty read FParty;

      property money: integer read getCash write setCash;
      property partySize: integer read getPartySize;
      property saveCount: integer read getSaveCount;
      property battleCount: integer read getBattleCount;
      property victories: integer read battleVictories;
      property losses: integer read battleLosses;
      property flees: integer read battleFlees;
      property timer: TRpgTimer read getTimer;
      property timer2: TRpgTimer read getTimer2;
      property levelGainNotify: boolean write notifyOnLevelGain;
      property deathPossible: boolean write canDieOnHpChange;
      property menuEnabled: boolean read isMenuEnabled write enableMenu;
      property thisObject: TRpgCharacter read GetThisObject;
   end;

procedure RegisterEnvironment(compiler: TrsCompiler; importer: TrsTypeImporter);

var
   GEnvironment: T2kEnvironment;

implementation
uses
   Math, RTTI,
   Commons,
   turbu_characters;

{ T2kEnvironment }

constructor T2kEnvironment.Create(database: TRpgDatabase);
var
   hero: THeroTemplate;
   vehicle: TVehicleTemplate;
   list: THeroList;
begin
   assert(GEnvironment = nil);
   FDatabase := database;
   list := THeroList.Create;
   list.OwnsObjects := true;
   FHeroes := list;
   database.hero.download;
   for hero in database.hero.Values do
      FHeroes.Add(TRpgHero.Create(hero));
   setLength(FSwitches, database.switch.count + 1);
   setLength(FInts, database.variable.Count + 1);
   FVehicles := TVehicleList.Create;
   //TODO: Add vehicle support
{   for vehicle in database.vehicles.Values do
      FVehicles.Add(TRpgVehicle.Create(database.mapTree, vehicle.id));}
end;

procedure T2kEnvironment.DeleteObject(permanant: boolean);
begin
   //TODO: implement this
end;

destructor T2kEnvironment.Destroy;
begin
   FVehicles.Free;
   FHeroes.Free;
   GEnvironment := nil;
   inherited Destroy;
end;

function T2kEnvironment.getBattleCount: integer;
begin
   result := 0;
end;

function T2kEnvironment.battleFlees: integer;
begin
   result := 0;
end;

function T2kEnvironment.battleLosses: integer;
begin
   result := 0;
end;

function T2kEnvironment.battleVictories: integer;
begin
   result := 0;
end;

procedure T2kEnvironment.canDieOnHpChange(const Value: boolean);
begin
   FParty.deathPossible := value;
end;

function T2kEnvironment.getCash: integer;
begin
   result := FParty.money;
end;

procedure T2kEnvironment.setCash(const Value: integer);
begin
   FParty.money := clamp(value, 0, MAXGOLD);
end;

function T2kEnvironment.getPartySize: integer;
var
  I: Integer;
begin
   result := 0;
   for I := 1 to MAXPARTYSIZE do
      if FParty[i] <> FHeroes[0] then
         inc(result);
end;

function T2kEnvironment.getSaveCount: integer;
begin
   result := 0;
end;

procedure T2kEnvironment.enableMenu(const Value: boolean);
begin
   //TODO: implement this
end;

procedure T2kEnvironment.EnableSave(value: boolean);
begin
   //TODO: implement this
end;

procedure T2kEnvironment.GameOver;
begin
   //TODO: implement this
end;

function T2kEnvironment.isMenuEnabled: boolean;
begin
   //TODO: implement this
end;

procedure T2kEnvironment.notifyOnLevelGain(const Value: boolean);
begin
   FParty.levelNotify := value;
end;

function T2kEnvironment.Random(low, high: integer): integer;
var
   dummy: integer;
begin
   dummy := abs(high - low);
   result := system.Random(dummy) + min(high, low);
end;

function T2kEnvironment.GetHero(const i: integer): TRpgHero;
begin
   if clamp(i, 0, FHeroes.Count) = i then
      result := FHeroes[i]
   else result := FHeroes[0];
end;

function T2kEnvironment.GetHeroCount: integer;
begin
   result := FHeroes.Count - 1;
end;

function T2kEnvironment.GetImage(i: integer): TRpgImage;
begin
   i := clamp(i, 0, 50);
   if i >= length(FImages) then
      SetLength(FImages, i);
   if FImages[i] = nil then
      //TODO: Fix the first param
      FImages[i] := TRpgImage.Create(nil, '', 0, 0, 0, false);
   result := FImages[i];
end;

function T2kEnvironment.GetMapObject(i: integer): TRpgEvent;
begin
   //TODO: implement this
end;

procedure T2kEnvironment.SetImage(i: integer; const Value: TRpgImage);
begin
   i := clamp(i, 0, 50);
   if i >= length(FImages) then
      SetLength(FImages, i);
   FImages[i] := value;
end;

function T2kEnvironment.GetSwitch(const i: integer): boolean;
begin
   if clamp(i, 0, high(FSwitches)) = i then
      result := FSwitches[i]
   else result := false;
end;

function T2kEnvironment.GetThisObject: TRpgCharacter;
begin
   result := nil; //TODO: implement this
end;

function T2kEnvironment.getTimer: TRpgTimer;
begin
   result := nil; //TODO: implement this
end;

function T2kEnvironment.getTimer2: TRpgTimer;
begin
   result := nil; //TODO: implement this
end;

function T2kEnvironment.GetVehicle(i: integer): TRpgVehicle;
begin
   if clamp(i, 0, FVehicles.Count) = i then
      result := FVehicles[i]
   else result := FVehicles[0];
end;

function T2kEnvironment.HeldItems(id: integer; equipped: boolean): integer;
var
  I: Integer;
begin
   result := 0;
   if clamp(id, 0, GDatabase.items) <> id then
      Exit;

   case equipped of
      false: result := 0 {FParty.inventory.quantityOf(id)};    //TODO: implement this
      true:
      begin
         for I := 1 to MAXPARTYSIZE do
            if (FParty[i] <> FHeroes[0]) and (FParty[i].equipped(id)) then
               inc(result);
      end;
   end;
end;

function T2kEnvironment.keyScan(mask: TKeyMask; wait: boolean): integer;
begin
   result := 0; //not implemented  yet.
end;

procedure T2kEnvironment.SetSwitch(const i: integer; const Value: boolean);
begin
   if clamp(i, 0, high(FSwitches)) = i then
      FSwitches[i] := value;
end;

procedure T2kEnvironment.Shop(shopType: TShopTypes; messageSet: integer;
  inventory: TIntArray);
begin
   //TODO: implement this
end;

procedure T2kEnvironment.wait(duration: integer);
begin
   //TODO: implement this
end;

procedure RegisterEnvironment(compiler: TrsCompiler; importer: TrsTypeImporter);
var
   ext: TExternalClassType;
begin
   importer.ImportType(TypeInfo(TFacing));
   ext := importer.ImportClass(TRpgHero);
   ext.AddArrayProp('stat', 'integer', 'integer', true, true);
   ext.AddArrayProp('equipment', 'integer', 'integer', true, false);
   ext.AddArrayProp('skill', 'integer', 'boolean', true, true);
   ext.AddArrayProp('condition', 'integer', 'boolean', true, true);

   importer.ImportClass(TRpgVehicle);
   importer.ImportClass(TRpgEvent);
   importer.ImportClass(TRpgImage);

   ext := importer.ImportClass(TRpgParty);
   ext.AddArrayProp('hero', 'integer', 'TRpgHero', true, true, true);

   importer.ImportConstant('KS_DIRS', TValue.From<TKeyMask>([1..4]));
   importer.importConstant('KS_ACTION', TValue.From<TKeyMask>([5]));
   importer.importConstant('KS_CANCEL', TValue.From<TKeyMask>([6]));
   importer.ImportConstant('KS_ALL', TValue.From<TKeyMask>([1..24]));

   ext := compiler.RegisterEnvironment(T2kEnvironment);
   ext.AddArrayProp('switch', 'integer', 'boolean', true, true);
   ext.AddArrayProp('ints', 'integer', 'integer', true, true);
   ext.AddArrayProp('hero', 'integer', 'TRpgHero', true, false);
   ext.AddArrayProp('vehicle', 'integer', 'TRpgVehicle', true, false);
   ext.AddArrayProp('MapObject', 'integer', 'TRpgEvent', true, false);
   ext.AddArrayProp('image', 'integer', 'TRpgImage', true, true);
end;

end.
