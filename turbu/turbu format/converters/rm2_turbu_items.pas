unit rm2_turbu_items;
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
   LDB, item_data,
   turbu_items, turbu_defs, turbu_database,
   conversion_table;

type
   TAttributeHelper = class helper for LDB.TAttribute
   public
      function average: smallint;
   end;

   T2k2ItemTemplate = class helper for TItemTemplate
   public
      class procedure addNewItem(base: TItem);
      constructor Convert(base: TItem);
   end;

   T2k2UsableItemTemplate = class helper for TUsableItemTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2EquipmentTemplate = class helper for TEquipmentTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2WeaponTemplate = class helper for TWeaponTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2ArmorTemplate = class helper for TArmorTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2MedicineTemplate = class helper for TMedicineTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2SkillBookTemplate = class helper for TSkillBookTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2SkillItemTemplate = class helper for TSkillItemTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2StatItemTemplate = class helper for TStatItemTemplate
   public
      constructor Convert(base: TItem);
   end;

   T2k2VariableItemTemplate = class helper for TVariableItemTemplate
   public
      constructor Convert(base: TItem);
   end;

implementation
uses
   formats, rm2_turbu_database;

{ T2k2ItemTemplate }

{$WARN USE_BEFORE_DEF OFF}
class procedure T2k2ItemTemplate.addNewItem(base: TItem);
var
   newclass: turbu_items.TItemType;
   newItem: TItemTemplate;
begin
   case base.itemType of
      commonItem: newClass := it_junk;
      weaponItem: newClass := it_weapon;
      shieldItem..accessoryItem: newClass := it_armor;
      medicineItem: newClass := it_medicine;
      bookItem: newClass := it_book;
      materialItem: newClass := it_upgrade;
      uniqueItem: newClass := it_skill;
      switchItem: newClass := it_variable;
      else assert(false);
   end;
   case newClass of
      it_junk: newItem := TJunkTemplate.Convert(base);
      it_weapon: newItem := TWeaponTemplate.Convert(base);
      it_armor: newItem := TArmorTemplate.Convert(base);
      it_medicine: newItem := TMedicineTemplate.Convert(base);
      it_upgrade: newItem := TStatItemTemplate.Convert(base);
      it_book: newItem := TSkillBookTemplate.Convert(base);
      it_skill: newItem := TSkillItemTemplate.Convert(base);
      it_variable: newItem := TVariableItemTemplate.Convert(base);
      else assert(false);
   end;
   GDatabase.addItem(newItem, newClass);
end;
{$WARN USE_BEFORE_DEF ON}

constructor T2k2ItemTemplate.Convert(base: TItem);
begin
   inherited Create;
   self.id := base.id;
   self.name := unicodeString(base.name);
   self.desc := unicodeString(base.desc);
   self.cost := base.cost;
end;

{ T2k2UsableItemTemplate }

constructor T2k2UsableItemTemplate.Convert(base: TItem);
var
   i, j: integer;
   mySet: TByteSet;
begin
   inherited Convert(base);
   self.usesLeft := base.usesLeft;
   self.usableWhere := us_both; //later constructors can change this
   mySet := [];
   for I := 0 to GDatabase.heroes do
      if base.usableBy[i] then
         include(mySet, i);
   self.usableByHero := mySet;
   if GProjectFormat = pf_2k then
      //leave mySet as it is
   else begin //I sure hope this works
      assert(GProjectFormat = pf_2k3);
      mySet := [];
      for I := 1 to GLcfDatabase.charClasses do
         if base.usableByClass[i] then
            include(mySet, i);
      i := GLcfDatabase.charClasses;
      for j := 1 to GLcfDatabase.heroes do
         if GLcfDatabase.hero[j].classNum = 0 then
         begin
            inc(i);
            if (base.usableBy[j]) then
               include(mySet, i);
         end;
      //end for
   end;
   self.usableByClass := mySet;

   //handle stats in later constructors
end;

{ T2k2EquipmentTemplate }

constructor T2k2EquipmentTemplate.Convert(base: TItem);
var
   i: integer;
   conditions: TByteSet;
begin
   inherited Convert(base);
   self.stat[3] := base.attack;
   self.stat[4] := base.defense;
   self.stat[5] := base.mind;
   self.stat[6] := base.speed;
   //deal with evasion in armor/weapon
   self.cursed := base.cursed;
   conditions := [];
   for I := 0 to base.conditions do
      if base.condition[i] then
         include(conditions, i);
   self.condition := conditions;
   // handle attributes in subclasses
end;

{ T2k2WeaponTemplate }

constructor T2k2WeaponTemplate.Convert(base: TItem);
var
   i: integer;
   counter: word;
   percentage, runningtotal: extended;
   points: PPointArray;
begin
   inherited Convert(base);
   counter := 0;
   runningtotal := 0;
   self.evasion := base.ignoreEvasion;

   points := @self.attribute;
   for I := 1 to GLcfDatabase.attributes do
   begin
      if base.attribute[i] then
         inc(counter);
   end;
   if counter > 0 then
   begin
      percentage := 100 / counter;
      for I := 1 to GLcfDatabase.attributes do
         if base.attribute[i] then
         begin
            runningtotal := runningtotal + counter;
            setLength(points^, length(self.attribute) + 1);
            with self.attribute[high(self.attribute)] do
            begin
               x := i;
               y := trunc(percentage);
               if runningTotal - trunc(runningtotal) >= 0.5 then
                  inc(y);
            end;
         end;
      //end for
   end;
   self.twoHanded := base.twoHanded;
   self.attackTwice := base.attackTwice;
   self.areaHit := base.areaHit;
   self.battleAnim := base.battleAnim;
   self.mpCost := base.mpCost;
   self.conditionChance := base.conditionChance;
end;

{ T2k2ArmorTemplate }

constructor T2k2ArmorTemplate.Convert(base: TItem);
var
   i: integer;
   points: PPointArray;
begin
   inherited Convert(base);
   self.evasion := base.boostEvade;

   points := @self.attribute;
   for I := 1 to GLcfDatabase.attributes do
      if base.attribute[i] then
      begin
         setLength(points^, length(self.attribute) + 1);
         with self.attribute[high(self.attribute)] do
         begin
            x := i;
            y := GLcfDatabase.attribute[i].average;
         end;
      end;
   //end for
   self.slot := ord(base.itemType);
end;

{ T2k2MedicineTemplate }

constructor T2k2MedicineTemplate.Convert(base: TItem);
begin
   inherited Convert(base);
   self.stat[1] := base.hpHeal;
   self.stat[2] := base.mpHeal;
   self.hpPercent := base.hpPercent;
   self.mpPercent := base.mpPercent;
   self.deadOnly := base.deadOnly;
   self.areaMedicine := base.areaMedicine;
   if base.outOfBattle then
      self.usableWhere := us_field;
end;

{ T2k2SkillBookTemplate }

constructor T2k2SkillBookTemplate.Convert(base: TItem);
begin
   inherited Convert(base);
   self.skill := base.skill;
end;

{ T2k2SkillItemTemplate }

constructor T2k2SkillItemTemplate.Convert(base: TItem);
begin
   inherited Convert(base);
   self.customSkillMessage := base.customSkillMessage;
end;

{ T2k2StatItemTemplate }

constructor T2k2StatItemTemplate.Convert(base: TItem);
begin
   inherited Convert(base);
   self.stat[1] := base.permHP;
   self.stat[2] := base.permMP;
   self.stat[3] := base.permAttack;
   self.stat[4] := base.permDefense;
   self.stat[5] := base.permMind;
   self.stat[6] := base.permSpeed;
end;

{ T2k2VariableItemTemplate }

constructor T2k2VariableItemTemplate.Convert(base: TItem);
begin
   inherited Convert(base);
   self.which := base.switch;
   self.style := vs_switch;
   self.operation := bo_add;
end;

{ TAttributeHelper }

function TAttributeHelper.average: smallint;
var
   i: integer;
   space: integer;
begin
   result := 0;
   for i := 2 to 4 do //leaving out 5 since it being the only one that can go
   begin              //below 0 could skew the results
      space := self.rate[i] - self.rate[i - 1];
      inc(result, space);
   end;
   result := round(result / 3);
end;

end.
