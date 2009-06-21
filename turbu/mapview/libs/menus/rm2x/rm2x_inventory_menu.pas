unit rm2x_inventory_menu;
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
   windows,
   chipset_data, item_data, rpg_list, script_backend,
   rm2x_menu_components, menu_basis, frames,
   {asphyreSprite} SDL_sprite;

type
   TCharStatBox = class(TSystemFrame)
   private
      FOrigin: TPoint;
      FChar: TRpgHero;
   public
      procedure moveTo(coords: TRect); override;
      procedure Draw; override;

      property char: TRpgHero write FChar;
   end;

   TEqInventoryMenu = class(TCustomScrollBox)
   private
      FCurrentItem: TRpgItem;
      FChar: TRpgHero;
      FCurrentSlot: byte;
      FPotential: array[1..4] of smallint;
   protected
      procedure drawItem(id, x, y: word; color: byte); override;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure show(slot: TItemType);
      procedure Draw; override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;

      property char: TRpgHero write FChar;
      property slot: byte write FCurrentSlot;
   end;

   TGameEquipmentMenu = class(TRm2kGameMenuBox)
   private
      FPassiveCursor: TStaticSysFrame;
      FChar: TRpgHero;

      procedure setChar(const Value: TRpgHero);
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      destructor Destroy; override;
      procedure Draw; override;
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;

      property char: TRpgHero read FChar write setChar;
   end;

   TEquipmentPage = class(TMenuPage)
   private
      FDescBox: TOnelineLabelBox;
      FStatBox: TCharStatBox;
      FInventoryMenu: TEqInventoryMenu;
      FEquipmentMenu: TGameEquipmentMenu;

      function topRect(input: TRect): TRect; inline;
      function leftRect(input: TRect): TRect; inline;
      function rightRect(input: TRect): TRect; inline;
      function bottomRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine); override;
   end;

implementation
uses
   sysUtils, types,
   LDB, item_code, chipset_graphics, script_engine, text_graphics, rm2X_menu_engine;

{ TEqInventoryBox }

procedure TEqInventoryMenu.doButton(const input: TButtonCode);
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      if FParsedText.Objects[FCursorPosition] = nil then
         FChar.unequip(FCurrentSlot - 1)
      else FChar.equip((FParsedText.Objects[FCursorPosition] as TEquipment).template.id, FCurrentSlot);
      self.show(TItemType(FCurrentSlot));
      TEquipmentPage(FOwner).FEquipmentMenu.doSetup(CURSOR_UNCHANGED);
      self.return;
   end;
end;

constructor TEqInventoryMenu.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   FOwner := owner;
   FColumns := 2;
   FDisplayCapacity := 12;
end;

procedure TEqInventoryMenu.Draw;
var
   i: Integer;
   color: byte;
   origin2: TPoint;
begin
   inherited Draw;
   if self.focused then
   begin
      origin2 := TEquipmentPage(FOwner).FStatBox.FOrigin;
      for i := 1 to 4 do
      begin
         if FPotential[i] > FChar.stat[i] then
            color := 2
         else if FPotential[i] < FChar.stat[i] then
            color := 3
         else color := 0;
         drawTextTo(intToStr(FPotential[i]), origin2.x + 108, origin2.Y + (i * 16), color);
      end;
   end;
end;

procedure TEqInventoryMenu.drawItem(id, x, y: word; color: byte);
begin
   if id < FParsedText.Count - 1 then
   begin
      drawText(FParsedText[id], x, y, color);
      drawTextTo(intToStr((FParsedText.Objects[id] as TEquipment).quantity), x + 140, y, color);
   end;
end;

procedure TEqInventoryMenu.doCursor(position: smallint);
var
   i: Integer;
   coords: TRect;
begin
   assert(position >= 0);
   if word(position) >= FParsedText.count then
      position := FParsedText.count - 1;
   if position < FTopPosition then
      FTopPosition := position - (position mod FColumns)
   else if position > FTopPosition + FDisplayCapacity then
      FTopPosition := (position - (position mod FColumns) + FColumns - FDisplayCapacity);
   coords := rect(6 + (position mod 2) * 156, ((position div 2)) * 15 + origin.y + 8, 150, 18);
   FMenuEngine.cursor.layout(coords);
   FCursorPosition := position;

   if position < FParsedText.Count - 1 then
   begin
      FCurrentItem := FParsedText.Objects[position] as TEquipment;
      for i := 1 to 4 do
         FPotential [i] := FChar.potentialStat(FCurrentItem.template.id, FCurrentSlot, i)
      //end FOR
   end
   else begin
      FCurrentItem := nil;
      for i := 1 to 4 do
         FPotential [i] := FChar.potentialStat(0, FCurrentSlot, i);
      //end FOR
   end;
end;

procedure TEqInventoryMenu.show(slot: TItemType);
var
   i: Integer;
   dummy: TRpgItem;
begin
   FParsedText.Clear;
   for i := 0 to GParty.inventory.Count - 1 do
   begin
      dummy := (GParty.inventory[i] as TRpgItem);
{      if (dummy.template.itemType = slot) and (dummy.usableBy(FChar.template.id)) then
         FParsedText.AddObject(dummy.template.name, dummy);
      //end if}
   end;
   FParsedText.AddObject('', nil);
   setLength(FOptionEnabled, FParsedText.Count);
   for I := low(FOptionEnabled) to high(FOptionEnabled) do
      FOptionEnabled[i] := true;
   FCurrentSlot := byte(slot);
end;

{ TGameEquipmentMenu }

procedure TGameEquipmentMenu.setChar(const Value: TRpgHero);
begin
   FChar := Value;
   TEquipmentPage(FOwner).FStatBox.char := Value;
   TEquipmentPage(FOwner).FInventoryMenu.char := Value;
end;

constructor TGameEquipmentMenu.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
var
  I: Integer;
begin
   inherited Create(parent, coords, main, owner);
   setLength(FOptionEnabled, 5);
   for I := 1 to 5 do
   begin
      FParsedText.Add('');
      FOptionEnabled[i - 1] := true;
   end;
   FPassiveCursor := TStaticSysFrame.Create(GGameEngine.cursor);
end;

destructor TGameEquipmentMenu.Destroy;
begin
   FPassiveCursor.Free;
   inherited;
end;

procedure TGameEquipmentMenu.doButton(const input: TButtonCode);

   function nextChar: TRpgHero;
   var dummy: integer;
   begin
      dummy := GParty.indexOf(FChar);
      if dummy = GParty.size then
         result := GParty[1]
      else result := GParty[dummy + 1];
   end;

   function prevChar: TRpgHero;
   var dummy: integer;
   begin
      dummy := GParty.indexOf(FChar);
      if dummy = 1 then
         result := GParty[GParty.size]
      else result := GParty[dummy - 1];
   end;

var
   dummy: TRect;
   newChar: TRpgHero;
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      dummy := GGameEngine.cursor.bounds;
      dec(dummy.left, round(FEngine.WorldX));
      dec(dummy.Top, round(FEngine.worldY));
      FPassiveCursor.layout(dummy);
      self.focusMenu((FOwner as TEquipmentPage).FInventoryMenu);
      TEquipmentPage(FOwner).FInventoryMenu.doSetup(0);
   end
   else if (input in [btn_left, btn_right]) and (GParty.size > 1) then
   begin
   if input = btn_left then
      newChar := prevChar
   else newChar := nextChar;
      self.doSetup(newChar.template.id);
   end;
end;

procedure TGameEquipmentMenu.doCursor(position: smallint);
var
   coords: trect;
begin
{   coords := rect(origin.x + 6, origin.y + trunc(position * TGameMap(Engine).fontEngine[0].TextHeight('A')) + 6 + (4 * position),
      self.width - 12, trunc(TGameMap(Engine).fontEngine[0].TextHeight('J')) + 6);}
   with TGameMap(Engine).cursor do
   begin
      Visible := true;
      layout(coords);
   end;

   FCursorPosition := position;
{   if FChar.equipment[position + 1] <> 0 then
      TEquipmentPage(FOwner).FDescBox.text := GDatabase.item[FChar.equipment[position + 1]].desc
   else TEquipmentPage(FOwner).FDescBox.text := '';}
   if not((position = 1) and (FChar.dualWield)) then
      TEquipmentPage(FOwner).FInventoryMenu.show(TItemType(position + 1))
   else begin
      TEquipmentPage(FOwner).FInventoryMenu.show(weaponItem);
      TEquipmentPage(FOwner).FInventoryMenu.slot := ord(shieldItem);
   end;
end;

procedure TGameEquipmentMenu.doSetup(value: integer);
var
   ourHero: TRpgHero;
   i: byte;
begin
   inherited doSetup(value);
//   ourHero := GCurrentEngine.hero[FSetupValue];
   self.char := ourHero;
   for i := 0 to 4 do
      if ourHero.equipment[i + 1] <> 0 then
//         FParsedText[i] := GDatabase.item[ourHero.equipment[i + 1]].name
      else FParsedText[i] := '';
   //end FOR
   self.doCursor(FCursorPosition);
end;

procedure TGameEquipmentMenu.Draw;
var
   i: byte;
begin
   inherited Draw;

   if not self.focused then
      FPassiveCursor.Draw;
   FOrigin := point(round(FCorners[topLeft].x - engine.WorldX) + 10, round(FCorners[topLeft].y - engine.WorldY) + 9);
{   drawText(GDatabase.vocabulary[weapon], FOrigin.x, FOrigin.Y, 1);
   case FChar.dualWield of
      false: drawText(GDatabase.vocabulary[shield], FOrigin.x, FOrigin.Y + 16, 1);
      true: drawText(GDatabase.vocabulary[weapon], FOrigin.x, FOrigin.Y + 16, 1);
   end;
   drawText(GDatabase.vocabulary[armor], FOrigin.x, FOrigin.Y + 32, 1);
   drawText(GDatabase.vocabulary[helmet], FOrigin.x, FOrigin.Y + 48, 1);
   drawText(GDatabase.vocabulary[relic], FOrigin.x, FOrigin.Y + 64, 1);}

   for I := 0 to 4 do
      drawText(FParsedText[i], FOrigin.X + 52, FOrigin.Y + (i * 16), 0);
end;

{ TCharStatBox }

procedure TCharStatBox.Draw;
var
   database: TLcfDataBase;
   I: Integer;
begin
   inherited Draw;
   drawText(FChar.name, FOrigin.x, FOrigin.Y, 0);
//   database := GDatabase;

   drawText(database.vocabulary[attack], FOrigin.x, FOrigin.Y + 16, 1);
   drawText(database.vocabulary[defense], FOrigin.x, FOrigin.Y + 32, 1);
   drawText(database.vocabulary[mind], FOrigin.x, FOrigin.Y + 48, 1);
   drawText(database.vocabulary[speed], FOrigin.x, FOrigin.Y + 64, 1);

   drawTextTo(intToStr(FChar.attack), FOrigin.x + 76, FOrigin.Y + 16, 0);
   drawTextTo(intToStr(FChar.defense), FOrigin.x + 76, FOrigin.Y + 32, 0);
   drawTextTo(intToStr(FChar.mind), FOrigin.x + 76, FOrigin.Y + 48, 0);
   drawTextTo(intToStr(FChar.agility), FOrigin.x + 76, FOrigin.Y + 64, 0);
   for I := 1 to 4 do
      drawText('->', FOrigin.X + 76, FOrigin.Y + (i * 16), 1);
end;

procedure TCharStatBox.moveTo(coords: TRect);
begin
   inherited;
   FOrigin := point(round(FCorners[topLeft].x - engine.WorldX) + 8, round(FCorners[topLeft].y - engine.WorldY) + 8);
end;

{ TEquipmentPage }

function TEquipmentPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.left, input.Top + 128, input.right, 112);
end;

function TEquipmentPage.leftRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 32, 128, 96);
end;

function TEquipmentPage.rightRect(input: TRect): TRect;
begin
   result := rect(input.Left + 128, input.Top + 32, input.right - 128, 96);
end;

function TEquipmentPage.topRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top, input.Right, 32);
end;

constructor TEquipmentPage.Create(parent: TSpriteEngine; coords: TRect;
  main: TMenuEngine);
begin
   inherited Create(parent, coords, main);
   FEquipmentMenu := TGameEquipmentMenu.Create(parent, rightRect(coords), main, self);
   registerComponent(FEquipmentMenu);
   FDescBox := TOnelineLabelBox.Create(parent, topRect(coords));
   registerComponent(FDescBox);
   FStatBox := TCharStatBox.Create(parent, leftRect(coords));
   registerComponent(FStatBox);
   FInventoryMenu := TEqInventoryMenu.Create(parent, bottomRect(coords), main, self);
   registerComponent(FInventoryMenu);
end;

end.
