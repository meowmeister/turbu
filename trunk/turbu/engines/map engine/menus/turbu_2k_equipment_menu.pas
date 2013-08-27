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
unit turbu_2k_equipment_menu;

interface
uses
   Types,
   turbu_defs, turbu_heroes, turbu_items,
   turbu_2k_items, turbu_2k_menu_components, turbu_2k_menu_basis, turbu_2k_frames,
   sg_defs, SDL_sprite;

type
   TCharStatBox = class(TGameMenuBox)
   private
      FOrigin: TsgPoint;
      FChar: TRpgHero;
      FPotentialItem: TRpgItem;
      FPotential: array[1..4] of integer;
      FCurrentSlot: TSlot;
      FActive: boolean;
      procedure SetItem(const Value: TRpgItem);
   protected
      procedure DrawText; override;
   public
      property char: TRpgHero write FChar;
      property potentialItem: TRpgItem write SetItem;
      property active: boolean write FActive;
   end;

   TEqInventoryMenu = class(TCustomScrollBox)
   private
      FCurrentItem: TRpgItem;
      FChar: TRpgHero;
      FCurrentSlot: TSlot;
   protected
      procedure drawItem(id, x, y, color: integer); override;
      procedure DrawText; override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
      procedure show(slot: TSlot);
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;

      property char: TRpgHero write FChar;
      property slot: TSlot write FCurrentSlot;
   end;

   TGameEquipmentMenu = class(TGameMenuBox)
   private
      FPassiveCursor: TSysFrame;
      FChar: TRpgHero;
      FPlacingCursor: boolean;

      procedure setChar(const Value: TRpgHero);
   protected
      procedure DrawText; override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;

      property char: TRpgHero read FChar write setChar;
   end;

implementation
uses
   sysUtils,
   turbu_constants, turbu_text_utils, turbu_database,
   turbu_2k_item_types, turbu_2k_environment,
   sg_utils;

{ TEqInventoryBox }

constructor TEqInventoryMenu.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   FOwner := owner;
   FColumns := 2;
   FDisplayCapacity := 12;
end;

procedure TEqInventoryMenu.DrawText;
var
   i: Integer;
   color: byte;
   origin2: TsgPoint;
begin
   inherited DrawText;
end;

procedure TEqInventoryMenu.drawItem(id, x, y, color: integer);
begin
   if id < FParsedText.Count - 1 then
   begin
      GFontEngine.drawText(FParsedText[id], x, y, color);
      GFontEngine.drawTextRightAligned(intToStr((FParsedText.Objects[id] as TEquipment).quantity), x + 140, y, color);
   end;
end;

procedure TEqInventoryMenu.doCursor(position: smallint);
var
   i: Integer;
   coords: TRect;
   stat: TCharStatBox;
begin
   assert(position >= 0);
   if word(position) >= FParsedText.count then
      position := FParsedText.count - 1;
   if position < FTopPosition then
      FTopPosition := position - (position mod FColumns)
   else if position > FTopPosition + FDisplayCapacity then
      FTopPosition := (position - (position mod FColumns) + FColumns - FDisplayCapacity);
   coords := rect(6 + (position mod 2) * 156, ((position div 2)) * 15 + FOrigin.y + 8, 150, 18);
   FMenuEngine.cursor.layout(SdlRectToTRect(coords));
   FCursorPosition := position;

   if position < FParsedText.Count - 1 then
      FCurrentItem := FParsedText.Objects[position] as TEquipment
   else FCurrentItem := nil;
   stat := (FOwner.menu('Stat') as TCharStatBox);
   stat.potentialItem := FCurrentItem;
   stat.active := true;
end;

procedure TEqInventoryMenu.doButton(const input: TButtonCode);
var
   stat: TCharStatBox;
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      if FParsedText.Objects[FCursorPosition] = nil then
         FChar.unequip(FCurrentSlot)
      else FChar.equip((FParsedText.Objects[FCursorPosition] as TEquipment).template.id);
      self.show(FCurrentSlot);
      stat := (FOwner.menu('Stat') as TCharStatBox);
      stat.potentialItem := nil;
      stat.active := false;
      self.return;
   end;
end;

procedure TEqInventoryMenu.show(slot: TSlot);
var
   i: Integer;
   item: TRpgItem;
begin
   FParsedText.Clear;
   for i := 0 to GEnvironment.Party.inventory.Count - 1 do
   begin
      item := GEnvironment.Party.inventory[i];
      if (item is TEquipment) and (TEquipmentTemplate(item.template).slot = slot)
         and (item.usableBy(FChar.template.id)) then
         FParsedText.AddObject(item.template.name, item);
   end;
   FParsedText.AddObject('', nil);
   setLength(FOptionEnabled, FParsedText.Count);
   for I := low(FOptionEnabled) to high(FOptionEnabled) do
      FOptionEnabled[i] := true;
   FCurrentSlot := slot;
   InvalidateText;
end;

{ TGameEquipmentMenu }

constructor TGameEquipmentMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
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
   FPassiveCursor := TSysFrame.Create(GMenuEngine, ORIGIN, 0, GMenuEngine.Cursor.bounds);
end;

procedure TGameEquipmentMenu.setChar(const Value: TRpgHero);
begin
   FChar := Value;
   (FOwner.menu('Stat') as TCharStatBox).char := Value;
   (FOwner.menu('Inventory') as TEqInventoryMenu).char := Value;
end;

procedure TGameEquipmentMenu.doButton(const input: TButtonCode);

   function nextChar: TRpgHero;
   var dummy: integer;
   begin
      dummy := GEnvironment.Party.indexOf(FChar);
      if dummy = GEnvironment.Party.size then
         result := GEnvironment.Party[1]
      else result := GEnvironment.Party[dummy + 1];
   end;

   function prevChar: TRpgHero;
   var dummy: integer;
   begin
      dummy := GEnvironment.Party.indexOf(FChar);
      if dummy = 1 then
         result := GEnvironment.Party[GEnvironment.Party.size]
      else result := GEnvironment.Party[dummy - 1];
   end;

var
   dummy: TRect;
   newChar: TRpgHero;
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      dummy := GMenuEngine.cursor.bounds;
      dec(dummy.left, round(FEngine.WorldX));
      dec(dummy.Top, round(FEngine.worldY));
      FPassiveCursor.layout(dummy);
      self.focusMenu('Inventory', 0);
   end
   else if (input in [btn_left, btn_right]) and (GEnvironment.Party.size > 1) then
   begin
      if input = btn_left then
         newChar := prevChar
      else newChar := nextChar;
         self.doSetup(newChar.template.id);
   end;
end;

procedure TGameEquipmentMenu.doCursor(position: smallint);
var
   coords: TRect;
   inv: TEqInventoryMenu;
begin
   if FPlacingCursor then
      Exit;
   FPlacingCursor := true;
   try
      if position = FCursorPosition then
         Setup(CURSOR_UNCHANGED);
      coords := rect(FOrigin.x + 6, FOrigin.y + 8 + position * 16, self.width - 12, 16);
      GMenuEngine.cursor.Visible := true;
      GMenuEngine.cursor.layout(SdlRectToTRect(coords));

      FCursorPosition := position;
      if FChar.equipment[TSlot(position)] <> 0 then
         FOwner.menu('Desc').text := GDatabase.findItem(FChar.equipment[TSlot(position)]).desc
      else FOwner.menu('Desc').text := '';
      inv := FOwner.menu('Inventory') as TEqInventoryMenu;
      if ((position = 1) and (FChar.dualWield = ws_dual)) then
         inv.show(eq_weapon)
      else inv.show(TSlot(position));
      (FOwner.menu('Stat') as TCharStatBox).FCurrentSlot := TSlot(position);
   finally
      FPlacingCursor := false;
   end;
end;

procedure TGameEquipmentMenu.doSetup(value: integer);
var
   ourHero: TRpgHero;
   i: byte;
begin
   inherited doSetup(value);
   InvalidateText;
   ourHero := GEnvironment.Heroes[FSetupValue];
   self.char := ourHero;
   for i := 0 to ord(high(TSlot)) do
      if ourHero.equipment[TSlot(i)] <> 0 then
         FParsedText[i] := GDatabase.findItem(ourHero.equipment[TSlot(i)]).name
      else FParsedText[i] := '';
   self.doCursor(FCursorPosition);
end;

procedure TGameEquipmentMenu.DrawText;
var
   i: integer;
   lOrigin: TSgPoint;
begin
   lOrigin := ORIGIN;
   if not self.focused then
      FPassiveCursor.Draw;
   GFontEngine.drawText(GDatabase.vocab[V_EQ_WEAPON], lOrigin.x, lOrigin.Y, 2);
   case FChar.dualWield of
      ws_single, ws_shield: GFontEngine.drawText(GDatabase.vocab[V_EQ_SHIELD], lOrigin.x, lOrigin.Y + 16, 2);
      else GFontEngine.drawText(GDatabase.vocab[V_EQ_WEAPON], lOrigin.x, lOrigin.Y + 16, 2);
   end;
   GFontEngine.drawText(GDatabase.vocab[V_EQ_ARMOR], lOrigin.x, lOrigin.Y + 32, 2);
   GFontEngine.drawText(GDatabase.vocab[V_EQ_HELMET], lOrigin.x, lOrigin.Y + 48, 2);
   GFontEngine.drawText(GDatabase.vocab[V_EQ_ACCESSORY], lOrigin.x, lOrigin.Y + 64, 2);

   for I := 0 to 4 do
      GFontEngine.drawText(FParsedText[i], lOrigin.X + 52, lOrigin.Y + (i * 16), 1);
end;

{ TCharStatBox }

procedure TCharStatBox.DrawText;
var
   database: TRpgDatabase;
   i, color: Integer;
begin
   GFontEngine.drawText(FChar.name, FOrigin.x, FOrigin.Y, 1);
   database := GDatabase;

   GFontEngine.drawText(database.vocab[V_STAT_ATTACK], FOrigin.x, FOrigin.Y + 16, 2);
   GFontEngine.drawText(database.vocab[V_STAT_DEFENSE], FOrigin.x, FOrigin.Y + 32, 2);
   GFontEngine.drawText(database.vocab[V_STAT_MIND], FOrigin.x, FOrigin.Y + 48, 2);
   GFontEngine.drawText(database.vocab[V_STAT_SPEED], FOrigin.x, FOrigin.Y + 64, 2);

   GFontEngine.drawTextRightAligned(intToStr(FChar.attack), FOrigin.x + 76, FOrigin.Y + 16, 1);
   GFontEngine.drawTextRightAligned(intToStr(FChar.defense), FOrigin.x + 76, FOrigin.Y + 32, 1);
   GFontEngine.drawTextRightAligned(intToStr(FChar.mind), FOrigin.x + 76, FOrigin.Y + 48, 1);
   GFontEngine.drawTextRightAligned(intToStr(FChar.agility), FOrigin.x + 76, FOrigin.Y + 64, 1);
   for I := 1 to 4 do
      GFontEngine.drawText('->', FOrigin.X + 76, FOrigin.Y + (i * 16), 2);

   if FActive then
   begin
      for i := 1 to 4 do
      begin
         if FPotential[i] > FChar.stat[i] then
            color := 3
         else if FPotential[i] < FChar.stat[i] then
            color := 4
         else color := 1;
         GFontEngine.drawTextRightAligned(intToStr(FPotential[i]), FOrigin.x + 108, FOrigin.Y + (i * 16), color);
      end;
   end;
end;

procedure TCharStatBox.SetItem(const Value: TRpgItem);
var
   i: integer;
begin
   FPotentialItem := value;
   InvalidateText;
   if assigned(value) then
      for i := 1 to 4 do
         FPotential [i] := FChar.potentialStat(value.template.id, i, FCurrentSlot)
   else
      for i := 1 to 4 do
         FPotential [i] := FChar.potentialStat(0, i, FCurrentSlot)
end;

const EQUIP_LAYOUT =
  '[{"Name": "Equip",     "Class": "TGameEquipmentMenu", "Coords": [128, 32,  320, 128]},' +
   '{"Name": "Desc",      "Class": "TOnelineLabelBox",   "Coords": [0,   0,   320, 32 ]},' +
   '{"Name": "Stat",      "Class": "TCharStatBox",       "Coords": [0,   32,  128, 128]},' +
   '{"Name": "Inventory", "Class": "TEqInventoryMenu",   "Coords": [0,   128, 320, 240]}]';

initialization
   TMenuEngine.RegisterMenuPage('Equipment', EQUIP_LAYOUT);
   TMenuEngine.RegisterMenuBoxClass(TCharStatBox);
   TMenuEngine.RegisterMenuBoxClass(TEqInventoryMenu);
   TMenuEngine.RegisterMenuBoxClass(TGameEquipmentMenu);
end.
