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
   rsCompiler, rsImport, rsExec,
   turbu_defs, turbu_heroes, turbu_database, turbu_mapchars, turbu_2k_images,
   turbu_2k_map_timer, turbu_map_sprites, turbu_map_objects, turbu_operators,
   sdl_sprite,
   dwsJSON;

type
   TVehicleList = class(TObjectList<TRpgVehicle>);

   T2kEnvironment = class
   private
      FDatabase: TRpgDatabase;
      FHeroes: TArray<TRpgHero>;
      FVehicles: TVehicleList;
      FSwitches: TArray<boolean>;
      FInts: TArray<integer>;
      FImages: TArray<TRpgImage>;
      FEvents: TArray<TRpgEvent>;
      FParty: TRpgParty;
      FMenuEnabled: boolean;
      FSaveEnabled: boolean;
      FEventMap: TDictionary<TRpgMapObject, TRpgEvent>;
      FSaveCount: integer;
      FKeyLock: boolean;
      FPreserveSpriteOnTeleport: boolean;

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
      function isMenuEnabled: boolean;
      procedure notifyOnLevelGain(const Value: boolean);
      procedure setCash(const Value: integer);
      function GetImage(i: integer): TRpgImage;
      procedure SetImage(i: integer; const Value: TRpgImage);
      function GetMapObject(i: integer): TRpgEvent;
      function getTimer: TRpgTimer;
      function getTimer2: TRpgTimer;
      function GetThisObject: TRpgEvent;
      function EvalConditions(value: TRpgEventConditions): boolean;
      function EvalVar(index, value: integer; op: TComparisonOp): boolean;
      function GetInt(const i: integer): integer;
      procedure SetInt(const i, Value: integer);
      function GetMapObjectCount: integer;

      //serialization
      procedure SerializeVariables(writer: TdwsJSONWriter);
      procedure SerializeHeroes(writer: TdwsJSONWriter);
      procedure SerializeImages(writer: TdwsJSONWriter);
      procedure SerializeMapObjects(writer: TdwsJSONWriter);

      procedure DeserializeVariables(obj: TdwsJSONObject);
      procedure DeserializeHeroes(obj: TdwsJSONObject);
      procedure DeserializeImages(obj: TdwsJSONObject);
      procedure DeserializeMapObjects(obj: TdwsJSONObject);
      function GetImageCount: integer;
      function GetVehicleCount: integer;
   public
      [NoImport]
      constructor Create(database: TRpgDatabase);
      destructor Destroy; override;

      function keyScan(mask: TButtonCodes; wait: boolean): integer;
      procedure wait(duration: integer);
      function HeldItems(id: integer; equipped: boolean): integer;
      procedure Shop(shopType: TShopTypes; messageSet: integer; inventory: TIntArray);
      function Random(low, high: integer): integer;
      procedure EnableSave(value: boolean);
      procedure GameOver;
      procedure TitleScreen;
      procedure DeleteObject(permanant: boolean);
      function HeroPresent(id: integer): boolean;
      procedure CallScript(objectID, pageID: integer);

      [NoImport]
      procedure RemoveImage(image: TRpgImage);
      [NoImport]
      procedure AddEvent(base: TMapSprite);
      [NoImport]
      procedure ClearEvents();
      [NoImport]
      procedure UpdateEvents;
      [NoImport]
      procedure Serialize(writer: TdwsJSONWriter; explicitSave: boolean);
      [NoImport]
      procedure Deserialize(obj: TdwsJSONObject);

      property Heroes[const i: integer]: TRpgHero read GetHero;
      property HeroCount: integer read GetHeroCount;
      property Switch[const i: integer]: boolean read GetSwitch write SetSwitch;
      property Ints[const i: integer]: integer read GetInt write SetInt;
      property Vehicle[i: integer]: TRpgVehicle read GetVehicle;
      property VehicleCount: integer read GetVehicleCount;
      property Image[i: integer]: TRpgImage read GetImage write SetImage;
      property ImageCount: integer read GetImageCount;
      property MapObject[i: integer]: TRpgEvent read GetMapObject;
      property MapObjectCount: integer read GetMapObjectCount;
      property Party: TRpgParty read FParty;

      property money: integer read getCash write setCash;
      property partySize: integer read getPartySize;
      property saveCount: integer read FSaveCount;
      property battleCount: integer read getBattleCount;
      property victories: integer read battleVictories;
      property losses: integer read battleLosses;
      property flees: integer read battleFlees;
      property timer: TRpgTimer read getTimer;
      property timer2: TRpgTimer read getTimer2;
      property levelGainNotify: boolean write notifyOnLevelGain;
      property deathPossible: boolean write canDieOnHpChange;
      property menuEnabled: boolean read isMenuEnabled write enableMenu;
      property thisObject: TRpgEvent read GetThisObject;
      property preserveSpriteOnTeleport: boolean read FPreserveSpriteOnTeleport write FPreserveSpriteOnTeleport;
   end;

procedure RegisterEnvironment(compiler: TrsCompiler; importer: TrsTypeImporter; exec: TrsExec);

var
   GEnvironment: T2kEnvironment;

implementation
uses
   Windows, SysUtils, Math, RTTI, Classes,
   Commons,
   turbu_characters, turbu_script_engine, turbu_2k_sprite_engine, turbu_constants,
   turbu_2k_map_engine, turbu_classes, timing,
   rsDefs;

{ T2kEnvironment }

constructor T2kEnvironment.Create(database: TRpgDatabase);
var
   hero: THeroTemplate;
//   vehicle: TVehicleTemplate;
begin
   assert(GEnvironment = nil);
   GEnvironment := self;
   FDatabase := database;
   FParty := TRpgParty.Create;
   database.hero.download;
   SetLength(FHeroes, database.hero.Count);
   for hero in database.hero.Values do
      FHeroes[hero.id] := TRpgHero.Create(hero, FParty);
   setLength(FSwitches, database.switch.count + 1);
   setLength(FInts, database.variable.Count + 1);
   FVehicles := TVehicleList.Create;
{$MESSAGE WARN 'Commented out code in live unit'}
   //TODO: Add vehicle support
{   for vehicle in database.vehicles.Values do
      FVehicles.Add(TRpgVehicle.Create(database.mapTree, vehicle.id));}
   FMenuEnabled := true;
   TRpgEventConditions.OnEval := Self.EvalConditions;
   FEventMap := TDictionary<TRpgMapObject, TRpgEvent>.Create;
end;

procedure T2kEnvironment.DeleteObject(permanant: boolean);
var
   obj: TRpgCharacter;
   i: integer;
begin
   obj := thisObject;
   for i := 1 to High(FEvents) do
      if FEvents[i] = obj then
      begin
         runThreadsafe(procedure begin FreeAndNil(FEvents[i]) end, true);
         Break;
      end;
   if permanant then
{$MESSAGE WARN 'Incomplete feature in live unit'}
   //TODO: implement this
end;

destructor T2kEnvironment.Destroy;
var
   i: integer;
begin
   FParty.Free;
   ClearEvents;
   FVehicles.Free;
   for i := 0 to High(FHeroes) do
      FHeroes[i].free;
   for i := 0 to High(FImages) do
      FImages[i].free;
   FEventMap.Free;
   GEnvironment := nil;
   inherited Destroy;
end;

function T2kEnvironment.getBattleCount: integer;
begin
   result := 0;
end;

procedure T2kEnvironment.ClearEvents();
var
   i: integer;
begin
   for i := 1 to High(FEvents) do
      FreeAndNil(FEvents[i]);
   SetLength(FEvents, 1);
   FEventMap.Clear;
end;

procedure T2kEnvironment.AddEvent(base: TMapSprite);
var
   newItem: TRpgEvent;
begin
   newItem := TRpgEvent.create(base);
   if newItem.id > high(FEvents) then
      SetLength(FEvents, newItem.id + 1);
   FEvents[newItem.id] := newitem;
   FEventMap.Add(base.event, newItem);
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

procedure T2kEnvironment.CallScript(objectID, pageID: integer);
begin
   GScriptEngine.RunObjectScript(self.MapObject[objectID].mapObj, pageID);
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

procedure T2kEnvironment.enableMenu(const Value: boolean);
begin
   FMenuEnabled := value;
end;

procedure T2kEnvironment.EnableSave(value: boolean);
begin
   FSaveEnabled := value;
end;

function T2kEnvironment.EvalVar(index, value: integer; op: TComparisonOp): boolean;
var
   varValue: integer;
begin
   result := false;
   varValue := self.ints[index];
   case op of
      co_equals: result := varValue = value;
      co_gtE: result := varValue >= value;
      co_ltE: result := varValue <= value;
      co_gt: result := varValue > value;
      co_lt: result := varValue < value;
      co_notEquals: result := varValue <> value;
      else assert(false);
   end;
end;

function T2kEnvironment.EvalConditions(value: TRpgEventConditions): boolean;
var
   cond: TPageConditions;
begin
{$MESSAGE WARN 'Incomplete feature in live unit'}
   result := true;
   for cond in value.conditions do
   begin
      case cond of
         pc_switch1: result := self.Switch[value.switch1Set];
         pc_switch2: result := self.Switch[value.switch2Set];
         pc_var1: result := EvalVar(value.variable1Set, value.variable1Value, value.variable1Op);
         pc_var2: result := EvalVar(value.variable2Set, value.variable2Value, value.variable2Op);
         pc_item: result := self.HeldItems(value.itemNeeded, false) > 0;
         pc_hero: result := self.HeroPresent(value.heroNeeded);
         pc_timer1: result := true; //TODO: Fix this
         pc_timer2: result := true; //TODO: Fix this
      end;
      if not result then
         Exit;
   end;
   //TODO: implement script validation
end;

procedure T2kEnvironment.GameOver;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   //TODO: implement this
end;

function T2kEnvironment.isMenuEnabled: boolean;
begin
   result := FMenuEnabled
end;

procedure T2kEnvironment.notifyOnLevelGain(const Value: boolean);
begin
   FParty.levelNotify := value;
end;

function T2kEnvironment.Random(low, high: integer): integer;
var
   spread: integer;
begin
   spread := abs(high - low);
   result := system.Random(spread) + min(high, low);
end;

procedure T2kEnvironment.RemoveImage(image: TRpgImage);
var
   i: integer;
begin
   for I := 0 to high(FImages) do
      if FImages[i] = image then
         FImages[i] := nil;
end;

procedure T2kEnvironment.SerializeVariables(writer: TdwsJSONWriter);
var
   i: integer;
begin
   writer.WriteName('switch');
   writer.BeginArray;
      for i := 1 to High(FSwitches) do
         if FSwitches[i] then
            writer.WriteInteger(i);
   writer.EndArray;
   writer.WriteName('int');
   writer.BeginArray;
      for i := 1 to High(FInts) do
         if FInts[i] <> 0 then
         begin
            writer.WriteInteger(i);
            writer.WriteInteger(FInts[i]);
         end;
   writer.EndArray;
end;

procedure T2kEnvironment.DeserializeVariables(obj: TdwsJSONObject);
var
   arr: TdwsJSONArray;
   i: Integer;
begin
   arr := obj.Items['switch'] as TdwsJSONArray;
   for i := 0 to arr.ElementCount - 1 do
      FSwitches[arr.Elements[i].AsInteger] := true;
   arr.Free;
   arr := obj.Items['int'] as TdwsJSONArray;
   i := 0;
   while i < arr.ElementCount do
   begin
      FInts[arr.Elements[i].AsInteger] := arr.Elements[i + 1].AsInteger;
      inc(i, 2);
   end;
   arr.Free;
end;

procedure T2kEnvironment.SerializeHeroes(writer: TdwsJSONWriter);
var
   i: integer;
begin
   writer.WriteName('Heroes');
   writer.BeginArray;
      for i := 1 to high(FHeroes) do
         FHeroes[i].Serialize(writer);
   writer.EndArray;
end;

procedure T2kEnvironment.DeserializeHeroes(obj: TdwsJSONObject);
var
   arr: TdwsJSONArray;
   i: Integer;
begin
   arr := obj.Items['Heroes'] as TdwsJSONArray;
   for i := 0 to arr.ElementCount - 1 do
      FHeroes[i + 1].Deserialize(arr.Elements[i] as TdwsJSONObject);
   arr.Free;
end;

procedure T2kEnvironment.SerializeImages(writer: TdwsJSONWriter);
var
   i: integer;
begin
   writer.WriteName('Images');
   writer.BeginObject;
      for i := 1 to high(FImages) do
         if assigned(FImages[i]) then
         begin
            writer.WriteName(IntToStr(i));
            FImages[i].Serialize(writer);
         end;
   writer.EndObject;
end;

procedure T2kEnvironment.DeserializeImages(obj: TdwsJSONObject);
var
   sub, image: TdwsJSONObject;
   i: Integer;
   name: string;
   masked: boolean;
begin
   sub := obj.Items['Images'] as TdwsJSONObject;
   for i := 0 to sub.ElementCount - 1 do
   begin
      image := sub.Elements[i] as TdwsJSONObject;
      masked := image.items['Masked'].AsBoolean;
      name := image.items['name'].AsString;
      GGameEngine.loadRpgImage(name, masked);
      self.Image[StrToInt(sub.Names[i])] := TRpgImage.Deserialize(GGameEngine.ImageEngine, image);
   end;
   sub.Free;
end;

procedure T2kEnvironment.SerializeMapObjects(writer: TdwsJSONWriter);
var
   i: integer;
begin
   writer.WriteName('MapObjects');
   writer.BeginArray;
      for i := 1 to high(FEvents) do
         if assigned(FEvents[i]) then
            FEvents[i].Serialize(writer)
         else writer.WriteNull;
   writer.EndArray;
end;

procedure T2kEnvironment.DeserializeMapObjects(obj: TdwsJSONObject);
var
   arr: TdwsJSONArray;
   i: Integer;
begin
   arr := obj.Items['MapObjects'] as TdwsJSONArray;
   for i := 0 to arr.ElementCount - 1 do
      if arr.Elements[i].IsNull then
         FreeAndNil(FEvents[i + 1])
      else FEvents[i + 1].Deserialize(arr.Elements[i] as TdwsJSONObject);
      //TODO: in the future, when it's possible to dynamically create map objects,
      //this will have to be completely reworked
   arr.Free;
end;

procedure T2kEnvironment.Serialize(writer: TdwsJSONWriter; explicitSave: boolean);
begin
   writer.BeginObject;
      SerializeVariables(writer);
      SerializeHeroes(writer);
      SerializeImages(writer);
      SerializeMapObjects(writer);
      writer.WriteName('Party');
      FParty.Serialize(writer);
      writer.CheckWrite('MenuEnabled', FMenuEnabled, false);
      if explicitSave then
         inc(FSaveCount);
      writer.CheckWrite('SaveCount', FSaveCount, 0);
      writer.CheckWrite('PreserveSpriteOnTeleport', FPreserveSpriteOnTeleport, false);
      writer.CheckWrite('SaveEnabled', FSaveEnabled, false);
   writer.EndObject;
end;

procedure T2kEnvironment.Deserialize(obj: TdwsJSONObject);
var
   value: TdwsJSONValue;
begin
   DeserializeVariables(obj);
   DeserializeHeroes(obj);
   DeserializeImages(obj);
   DeserializeMapObjects(obj);
   value := obj.Items['Party'];
   assert(assigned(value));
   FParty.Deserialize(value as TdwsJSONObject);
   value.Free;
   obj.CheckRead('MenuEnabled', FMenuEnabled);
   obj.CheckRead('SaveCount', FSaveCount);
   obj.CheckRead('PreserveSpriteOnTeleport', FPreserveSpriteOnTeleport);
   obj.CheckRead('SaveEnabled', FSaveEnabled);
   obj.checkEmpty;
   self.UpdateEvents;
end;

function T2kEnvironment.GetHero(const i: integer): TRpgHero;
begin
   if clamp(i, 0, high(FHeroes)) = i then
      result := FHeroes[i]
   else result := FHeroes[0];
end;

function T2kEnvironment.GetHeroCount: integer;
begin
   result := high(FHeroes);
end;

function T2kEnvironment.GetImage(i: integer): TRpgImage;
begin
   i := clamp(i, 0, 250);
   if i >= length(FImages) then
      SetLength(FImages, i + 1);
   if FImages[i] = nil then
      FImages[i] := TRpgImage.Create(GSpriteEngine, '', 0, 0, 0, 0, 0, false, false);
   result := FImages[i];
end;

function T2kEnvironment.GetImageCount: integer;
begin
   result := high(FImages);
end;

procedure T2kEnvironment.SetImage(i: integer; const Value: TRpgImage);
begin
   i := clamp(i, 0, 250);
   if i >= length(FImages) then
      SetLength(FImages, i + 1)
   else if assigned(FImages[i]) then
      commons.runThreadsafe(procedure begin FImages[i].Destroy; end, true);

   FImages[i] := value;
end;

function T2kEnvironment.GetMapObject(i: integer): TRpgEvent;
begin
   if (clamp(i, 0, high(FEvents)) = i) and (assigned(FEvents[i])) then
      result := FEvents[i]
    else result := FEvents[0];
end;

function T2kEnvironment.GetMapObjectCount: integer;
begin
   result := high(FEvents);
end;

function T2kEnvironment.GetSwitch(const i: integer): boolean;
begin
   if clamp(i, 0, high(FSwitches)) = i then
      result := FSwitches[i]
   else result := false;
end;

procedure T2kEnvironment.SetSwitch(const i: integer; const Value: boolean);
begin
   if clamp(i, 0, high(FSwitches)) = i then
      FSwitches[i] := value;
end;

function T2kEnvironment.GetInt(const i: integer): integer;
begin
   if clamp(i, 0, high(FInts)) = i then
      result := FInts[i]
   else result := 0;
end;

procedure T2kEnvironment.SetInt(const i, Value: integer);
begin
   if clamp(i, 0, high(FInts)) = i then
      FInts[i] := value;
end;

function T2kEnvironment.GetThisObject: TRpgEvent;
var
   obj: TRpgMapObject;
begin
   obj := (tthread.CurrentThread as TScriptThread).CurrentObject;
   result := FEventMap[obj];
end;

function T2kEnvironment.getTimer: TRpgTimer;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   result := nil; //TODO: implement this
end;

function T2kEnvironment.getTimer2: TRpgTimer;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   result := nil; //TODO: implement this
end;

function T2kEnvironment.GetVehicle(i: integer): TRpgVehicle;
begin
   if clamp(i, 0, FVehicles.Count) = i then
      result := FVehicles[i]
   else result := FVehicles[0];
end;

function T2kEnvironment.GetVehicleCount: integer;
begin
   result := FVehicles.Count - 1;
end;

function T2kEnvironment.HeldItems(id: integer; equipped: boolean): integer;
var
  I: Integer;
begin
   result := 0;
   if clamp(id, 0, GDatabase.items) <> id then
      Exit;

   if equipped then
   begin
      for I := 1 to MAXPARTYSIZE do
         if (FParty[i] <> FHeroes[0]) and (FParty[i].equipped(id)) then
            inc(result);
   end
   else result := FParty.inventory.quantityOf(id);
end;

function T2kEnvironment.HeroPresent(id: integer): boolean;
begin
   result := FParty.indexOf(self.Heroes[id]) > 0;
end;

function WaitForKeyPress: boolean;
begin
   result := GGameEngine.ReadKeyboardState <> [];
end;

function T2kEnvironment.keyScan(mask: TButtonCodes; wait: boolean): integer;
var
   scan: TButtonCodes;
   thread: TScriptThread;
   btn: TButtonCode;
begin
   assert(TThread.CurrentThread.ThreadID <> MainThreadID);
   thread := TThread.CurrentThread as TScriptThread;
   while wait and FKeyLock do
      GScriptEngine.threadSleep(TRpgTimestamp.FrameLength);
   scan := GGameEngine.ReadKeyboardState;
   Sleep(TRpgTimestamp.FrameLength);
   scan := scan + GGameEngine.ReadKeyboardState;

   scan := scan * mask;
   if wait and (scan = []) then
   begin
      GScriptEngine.SetWaiting(WaitForKeyPress);
      repeat
         GScriptEngine.ThreadWait;
         scan := GGameEngine.ReadKeyboardState * mask
      until (scan <> []) or (thread.Terminated);
      FKeyLock := true;
   end;
   result := 0;
   if scan = [] then
      exit
   else for btn in scan do
      exit(ord(btn) + 1); //return lowest value found in set
end;

procedure T2kEnvironment.Shop(shopType: TShopTypes; messageSet: integer; inventory: TIntArray);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
   //TODO: implement this
end;

procedure T2kEnvironment.TitleScreen;
begin
   GScriptEngine.killAll;
   commons.runThreadsafe(
      procedure begin GGameEngine.TitleScreen; end, true);
end;

procedure T2kEnvironment.UpdateEvents;
var
   event: TRpgEvent;
begin
   for event in FEvents do
      if assigned(event) then
         event.update;

   if assigned(FParty) and assigned(FParty.base) then
      FParty.base.CheckMoveChange;
   if FKeyLock and (GGameEngine.ReadKeyboardState = []) then
      FKeyLock := false;
end;

procedure T2kEnvironment.wait(duration: integer);
begin
   GScriptEngine.threadSleep(duration * 100);
end;

function ReadHeroStat(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as TRpgHero).stat[index[0].AsInteger];
end;

procedure WriteHeroStat(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
begin
   (selfValue.AsObject as TRpgHero).stat[index[0].AsInteger] := value.AsInteger;
end;

function ReadHeroEQ(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as TRpgHero).equipment[index[0].AsInteger];
end;

function ReadHeroSkill(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as TRpgHero).skill[index[0].AsInteger];
end;

procedure WriteHeroSkill(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
begin
   (selfValue.AsObject as TRpgHero).skill[index[0].AsInteger] := value.AsBoolean;
end;

function ReadHeroCond(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as TRpgHero).condition[index[0].AsInteger];
end;

procedure WriteHeroCond(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
begin
   (selfValue.AsObject as TRpgHero).condition[index[0].AsInteger] := value.AsBoolean;
end;

function ReadPartyHero(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as TRpgParty).hero[index[0].AsInteger];
end;

procedure WritePartyHero(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
begin
   (selfValue.AsObject as TRpgParty).hero[index[0].AsInteger] := value.AsObject as TRpgHero;
end;

function ReadSwitch(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as T2kEnvironment).switch[index[0].AsInteger];
end;

procedure WriteSwitch(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
begin
   (selfValue.AsObject as T2kEnvironment).switch[index[0].AsInteger] := value.AsBoolean;
end;

function ReadInt(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as T2kEnvironment).Ints[index[0].AsInteger];
end;

procedure WriteInt(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
begin
   (selfValue.AsObject as T2kEnvironment).Ints[index[0].AsInteger] := value.AsInteger;
end;

function ReadHero(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as T2kEnvironment).Heroes[index[0].AsInteger];
end;

function ReadVehicle(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as T2kEnvironment).Vehicle[index[0].AsInteger];
end;

function ReadMapObj(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as T2kEnvironment).MapObject[index[0].AsInteger];
end;

function ReadImage(const selfValue: TValue; const index: TArray<TValue>): TValue;
begin
   result := (selfValue.AsObject as T2kEnvironment).Image[index[0].AsInteger];
end;

procedure WriteImage(const selfValue: TValue; const index: TArray<TValue>; value: TValue);
begin
   (selfValue.AsObject as T2kEnvironment).Image[index[0].AsInteger] := value.AsObject as TRpgImage;
end;

procedure RegisterEnvironment(compiler: TrsCompiler; importer: TrsTypeImporter; exec: TrsExec);
var
   ext: TExternalClassType;
   defined: boolean;
begin
   importer.ImportType(TypeInfo(TFacing));
   defined := NativeTypeDefined(TRttiContext.Create.GetType(TRpgHero));
   ext := importer.ImportClass(TRpgHero);
   if not defined then
   begin
      ext.AddArrayProp('stat', 'integer', 'integer', true, true);
      ext.AddArrayProp('equipment', 'integer', 'integer', true, false);
      ext.AddArrayProp('skill', 'integer', 'boolean', true, true);
      ext.AddArrayProp('condition', 'integer', 'boolean', true, true);
   end;

   importer.ImportClass(TRpgVehicle);
   importer.ImportClass(TRpgEvent);
   importer.ImportClass(TRpgImage);

   ext := importer.ImportClass(TRpgParty);
   if not defined then
      ext.AddArrayProp('hero', 'integer', 'TRpgHero', true, true, true);

   importer.ImportType(TypeInfo(TButtonCode));
   importer.ImportType(TypeInfo(TButtonCodes));
   importer.ImportConstant('KS_ALL', TValue.From<TButtonCodes>([low(TButtonCode)..high(TButtonCode)]));

   ext := compiler.RegisterEnvironment(T2kEnvironment);
   if not defined then
   begin
      ext.AddArrayProp('switch', 'integer', 'boolean', true, true);
      ext.AddArrayProp('ints', 'integer', 'integer', true, true);
      ext.AddArrayProp('hero', 'integer', 'TRpgHero', true, false);
      ext.AddArrayProp('vehicle', 'integer', 'TRpgVehicle', true, false);
      ext.AddArrayProp('MapObject', 'integer', 'TRpgEvent', true, false);
      ext.AddArrayProp('image', 'integer', 'TRpgImage', true, true);
   end;

   exec.SetEnvironment(GEnvironment);
   exec.RegisterStandardUnit('system',
      procedure (RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport)
      begin
         RegisterArrayProp('TRpgHero', 'stat', ReadHeroStat, WriteHeroStat);
         RegisterArrayProp('TRpgHero', 'equipment', ReadHeroEQ, nil);
         RegisterArrayProp('TRpgHero', 'skill', ReadHeroSkill, WriteHeroSkill);
         RegisterArrayProp('TRpgHero', 'condition', ReadHeroCond, WriteHeroCond);
         RegisterArrayProp('TRpgParty', 'hero', ReadPartyHero, WritePartyHero);
         RegisterArrayProp('T2kEnvironment', 'switch', ReadSwitch, WriteSwitch);
         RegisterArrayProp('T2kEnvironment', 'ints', ReadInt, WriteInt);
         RegisterArrayProp('T2kEnvironment', 'hero', ReadHero, nil);
         RegisterArrayProp('T2kEnvironment', 'vehicle', ReadVehicle, nil);
         RegisterArrayProp('T2kEnvironment', 'MapObject', ReadMapObj, nil);
         RegisterArrayProp('T2kEnvironment', 'image', ReadImage, WriteImage);
      end);
end;

end.
