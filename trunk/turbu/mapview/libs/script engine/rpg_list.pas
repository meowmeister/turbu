unit rpg_list;
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
uses contnrs, //windows libs
     turbu_database, turbu_items;

type
   TRpgItem = class(TObject)
   private
      FTemplate: TItemTemplate;
      FQuantity: byte;
      FLevel: cardinal;

      function getCost: word;
      function getName: string;
      function getDesc: string;
      function getID: word;
   protected
      function getUses: smallint; virtual;
      procedure setUses(const Value: smallint); virtual;
      function getOnField: boolean; virtual; abstract;
      procedure useOnce; inline;
   public
      constructor create(const item, quantity: word); virtual;
      class function newItem(const item, quantity: word): TRpgItem;
      function usableBy(hero: word): boolean; virtual; abstract;

      property template: TItemTemplate read FTemplate;
      property quantity: byte read FQuantity write FQuantity;
      property usesLeft: smallint read getUses write setUses;
      property level: cardinal read FLevel write FLevel;
      property id: word read getID;
      property desc: string read getDesc;
      property name: string read getName;
      property cost: word read getCost;
      property usableOnField: boolean read getOnField;
   end;

   TRpgInventory = class(TObjectList)
   private
      FSorted: boolean;
   public
      procedure Add(const id, number: word); reintroduce; overload;
      procedure Add(const value: TRpgItem); reintroduce; overload;
      function indexOf(const id: smallint): smallint; overload;
      function quantityOf(const id: word): byte;
      function contains(id: word): boolean;
      procedure Remove(const id, number: word); reintroduce;
      procedure sort; reintroduce;
   end;

const
   MAXITEMS = 99;

implementation

uses
   math,
   {script_engine, chipset_graphics,}
   commons;

type
   TItemClass = class of TRpgItem;

{ TRpgItem }

constructor TRpgItem.create(const item, quantity: word);
begin
   inherited Create;
   FTemplate := GDatabase.findItemById(item);
   FQuantity := quantity;
end;


function TRpgItem.getCost: word;
begin
   result := template.cost;
end;

function TRpgItem.getDesc: string;
begin
   result := template.desc;
end;

function TRpgItem.getID: word;
begin
   result := template.id;
end;

function TRpgItem.getName: string;
begin
   result := template.name;
end;

function TRpgItem.getUses: smallint;
begin
   result := FQuantity;
end;

{$WARN USE_BEFORE_DEF OFF}
class function TRpgItem.newItem(const item, quantity: word): TRpgItem;
var
   subtype: TItemClass;
begin
{   case GDatabase.findItemById(item).itemType of
      commonItem: subtype := TJunkItem;
      weaponItem: subtype := TWeapon;
      shieldItem: subtype := TShield;
      armorItem: subtype := TArmor;
      helmetItem: subtype := THelmet;
      accessoryItem: subtype := TRelic;
      medicineItem: subtype := TRecoveryItem;
      bookItem: subtype := TBookItem;
      materialItem: subtype := TStatItem;
      uniqueItem: subtype := TSkillItem;
      switchItem: subtype := TSwitchItem;
      else assert(false);
   end;
   result := subtype.create(item, quantity);}
   result := nil; //fill this in later
end;
{$WARN USE_BEFORE_DEF ON}

procedure TRpgItem.setUses(const Value: smallint);
begin
   //this method deliberately left blank
end;

procedure TRpgItem.useOnce;
begin
   dec(FQuantity);
end;

{ TRpgList }

function itemSortCompare(item1, item2: pointer): integer;
begin
   result := (TObject(item1) as TRpgItem).template.id - (TObject(item2) as TRpgItem).template.id;
end;

procedure TRpgInventory.Add(const id, number: word);
var
  I: Integer;
  dummy: TRpgItem;
begin
   if between(id, 1, GDatabase.items) <> id then
      Exit;

   dummy := nil;
   I := 0;
   while (dummy = nil) and (i < count) do
   begin
      if TRpgItem(self[i]).FTemplate = GDatabase.findItemById(id) then
         dummy := TRpgItem(self[i]);
      inc(i);
   end;
   if dummy = nil then
   begin
      inherited Add(TRpgItem.newItem(id, number));
      FSorted := false;
   end
   else
      dummy.FQuantity := min(MAXITEMS, dummy.FQuantity + number);
   //end if
end;

procedure TRpgInventory.Add(const value: TRpgItem);
var
  I: Integer;
  dummy: TRpgItem;
  total: word;
begin
   dummy := nil;
   I := 0;
   while (dummy = nil) and (i < count) do
   begin
      if TRpgItem(self[i]).FTemplate = value.FTemplate then
         dummy := TRpgItem(self[i]);
      inc(i);
   end;
   if dummy = nil then
   begin
      inherited Add(value);
      FSorted := false;
   end
   else begin
      total := value.quantity + dummy.quantity;
      if total > MAXITEMS then
         dec(value.FQuantity, total - MAXITEMS);
      inc(dummy.FQuantity, value.FQuantity);
   end;
end;

function TRpgInventory.contains(id: word): boolean;
begin
   result := indexOf(id) <> -1;
end;

function TRpgInventory.indexOf(const id: smallint): smallint;
var
  I: Integer;
begin
   result := -1;
   if between(id, 1, GDatabase.items) <> id then
      Exit;

   for I := 0 to self.count - 1 do
   begin
      if (self[i] as TRpgItem).FTemplate.id = id then
      begin
         result := i;
         Exit;
      end;
   end;
end;

function TRpgInventory.quantityOf(const id: word): byte;
var dummy: smallint;
begin
   dummy := indexOf(id);
   if dummy = -1 then
      result := 0
   else result := (self[dummy] as TRpgItem).quantity;
end;

procedure TRpgInventory.Remove(const id, number: word);
var
  I: Integer;
  dummy: TRpgItem;
begin
   if (id < 1) or (id > GDatabase.items) then
      Exit;

   dummy := nil;
   I := 0;
   while (dummy = nil) and (i < count) do
   begin
      if TRpgItem(self[i]).FTemplate = GDatabase.findItemById(id) then
         dummy := TRpgItem(self[i]);
      inc(i);
   end;
   if dummy <> nil then
      if dummy.FQuantity <= number then
         inherited remove(dummy)
      else
         dec(dummy.FQuantity, number);
      //end if
   //end if
end;

procedure TRpgInventory.sort;
begin
   inherited sort(itemSortCompare);
end;

end.
