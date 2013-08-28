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
unit turbu_2k_party_target_menu;

interface
uses
   windows,
   turbu_2k_items, turbu_defs,
   turbu_2k_frames, turbu_2k_main_menu, turbu_2k_menu_components, turbu_2k_skills,
   turbu_2k_menu_basis,
   SDL_sprite;

type
   TQuantityBox = class(TGameMenuBox)
   private
      FItem: TRpgItem;
      FSkill: TRpgSkill;

      procedure clear; inline;
      procedure setItem(const Value: TRpgItem);
      procedure setSkill(const Value: TRpgSkill);
   public
      procedure DrawText; override;

      property item: TRpgItem write setItem;
      property skill: TRpgSkill write setSkill;
   end;

   TGameMiniPartyPanel = class(TCustomPartyPanel)
   private
      FItem: TRpgItem;
      FSkill: TRpgSkill;
      procedure UseItem;
      procedure UseSkill;
      procedure SetSkill(value: TRpgSkill);
   public
      destructor Destroy; override;
      procedure DrawText; override;
      procedure doSetup(value: integer); override;
      procedure moveTo(coords: TRect); override;
      procedure doButton(const input: TButtonCode); override;
   end;

{
   TPartyTargetPage = class(TMenuPage)
   private
      FTargetPanel: TGameMiniPartyPanel;
      FItemBox: TOnelineLabelBox;
      FQuantityBox: TQuantityBox;

      function bottomRect(input: TRect): TRect; inline;
      function topRect(input: TRect): TRect; inline;
      function sideRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
   end;
}

implementation
uses
   sysUtils, types,
   turbu_database, turbu_text_utils, turbu_constants, turbu_heroes, turbu_skills,
   turbu_resists,
   turbu_2k_environment, turbu_2k_item_types,
   rs_media;

{ TQuantityBox }

procedure TQuantityBox.clear;
begin
   fitem := nil;
   fskill := nil;
end;

procedure TQuantityBox.drawText;
begin
   if (not assigned(FItem)) and (not assigned(FSkill)) then
      Exit;

   if assigned(FItem) then
   begin
      GFontEngine.drawText(GDatabase.vocab[V_ITEMS_OWNED], 10, 2, 2);
      GFontEngine.drawTextRightAligned(intToStr(FItem.quantity), 116, 2, 1);
   end
   else begin
      assert(assigned(FSkill));
      GFontEngine.drawText(GDatabase.vocab[V_MP_COST], 10, 2, 2);
      GFontEngine.drawTextRightAligned(intToStr(FSkill.template.cost), 116, 2, 1);
   end;
end;

procedure TQuantityBox.setItem(const Value: TRpgItem);
begin
   self.clear;
   FItem := value;
end;

procedure TQuantityBox.setSkill(const Value: TRpgSkill);
begin
   self.clear;
   FSkill := value;
end;

{ TGameMiniPartyPanel }

destructor TGameMiniPartyPanel.Destroy;
begin
   FSkill.Free;
   inherited;
end;

procedure TGameMiniPartyPanel.doSetup(value: integer);
var
   quantity: TQuantityBox;
   itemBox: TOnelineLabelBox;
begin
   inherited doSetup(value);
   FItem := nil;
   SetSkill(nil);
   quantity := FOwner.menu('Quantity') as TQuantityBox;
   itemBox := FOwner.menu('Item') as TOnelineLabelBox;
   if FSetupValue > 0 then
   begin
      SetSkill(TRpgSkill.Create(FSetupValue));
      quantity.skill := FSkill;
      itemBox.text := FSkill.template.name;
   end
   else begin
      FItem := GEnvironment.Party.inventory[abs(FSetupValue)] as TRpgItem;
      quantity.item := FItem;
      itemBox.text := FItem.template.name;
   end;
end;

procedure TGameMiniPartyPanel.UseItem;
var
   target: TRpgHero;
   sound: TSfxTypes;
begin
   assert(FItem is TAppliedItem);
   sound := TSfxTypes(-1);
   if FItem.quantity > 0 then
   begin
      target := GEnvironment.Party[FCursorPosition + 1];
      if TAppliedItem(FItem).areaItem then
      begin
         if TAppliedItem(FItem).usableArea then
         begin
            if not (FItem is TSkillItem) then
               sound := sfxAccept;
            TAppliedItem(FItem).useArea;
         end
         else sound := sfxBuzzer;
      end
      else if FItem.usableBy(target.template.id) then
      begin
         if not (FItem is TSkillItem) then
            sound := sfxAccept;
         (FItem as TAppliedItem).use(target)
      end
      else sound := sfxBuzzer;
   end
   else sound := sfxBuzzer;
   if sound <> TSfxTypes(-1) then
      PlaySound(sound)
   else begin
      assert(FItem is TSkillItem);
      rs_media.PlaySoundData(TSkillItem(FItem).skill.firstSound);
   end;
end;

procedure TGameMiniPartyPanel.UseSkill;
var
   caster, target: TRpgHero;
   sound: TSfxTypes;
begin
   sound := TSfxTypes(-1);
   assert(assigned(FSkill));

   caster := FMenuEngine.currentHero;
   if caster.mp < FSkill.template.cost then
      sound := sfxBuzzer
   else begin
      target := GEnvironment.Party[FCursorPosition + 1];
      if FSkill.template.range = sr_area then
      begin
         if FSkill.usableParty then
            FSkill.useParty(caster)
         else sound := sfxBuzzer;
      end
      else if FSkill.usableOn(target.template.id) then
         FSkill.useHero(caster, target)
      else sound := sfxBuzzer;
   end;
   if sound <> TSfxTypes(-1) then
      PlaySound(sound)
   else
   begin
      PlaySoundData(FSkill.firstSound);
      caster.mp := caster.mp - FSkill.template.cost;
   end;
end;

procedure TGameMiniPartyPanel.doButton(const input: TButtonCode);
begin
   if input <> btn_enter then
      inherited doButton(input);
   case input of
      btn_enter:
      begin
         if assigned(FItem) then
            useItem
         else UseSkill;
      end;
      btn_cancel:
      begin
         if (assigned(FItem)) and (FItem.quantity = 0) then
            GEnvironment.Party.inventory.Remove(FItem.template.id, 0);
      end;
      else ;
   end;
end;

procedure TGameMiniPartyPanel.drawText;
var
  I: Integer;
  origin: TPoint;
  hero: TRpgHero;
  cond: TConditionTemplate;
begin
   i := 1;
   while GEnvironment.Party[i] <> GEnvironment.Heroes[0] do
   begin
      FPortrait[i].Draw;
      origin := point(round(FPortrait[i].x) + 54, round(FPortrait[i].Y) + 3);
      hero := GEnvironment.Party[i];
      GFontEngine.drawText(hero.name, origin.X + 1, origin.Y, 1);
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_LV], origin.x + 1, origin.Y + 16, 1);
      GFontEngine.drawText(intToStr(hero.level), origin.X + 17, origin.y + 16, 1);
      if hero.highCondition = 0 then
         GFontEngine.drawText(GDatabase.vocab[V_NORMAL_STATUS], origin.X + 1, origin.Y + 32, 1)
      else begin
         cond := GDatabase.conditions[hero.highCondition];
         GFontEngine.drawText(cond.name, origin.X + 1, origin.Y + 32, cond.color);
      end;
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_HP], origin.x + 52, origin.Y + 16, 2);
      GFontEngine.drawTextRightAligned(intToStr(hero.hp), origin.X + 86, origin.Y + 16, 1);
      GFontEngine.drawText('/', origin.X + 86, origin.Y + 16, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.maxHp), origin.X + 110, origin.Y + 16, 1);
         GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_MP], origin.X + 52, origin.Y + 32, 2);
      GFontEngine.drawTextRightAligned(intToStr(hero.mp), origin.X + 86, origin.Y + 32, 1);
      GFontEngine.drawText('/', origin.X + 86, origin.Y + 32, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.maxMp), origin.X + 110, origin.Y + 32, 1);

      inc(i);
   end;
end;

procedure TGameMiniPartyPanel.moveTo(coords: TRect);
var
   i: byte;
begin
   inherited moveTo(coords);
   for I := 1 to 4 do
   if assigned(FPortrait[i]) then
   with FPortrait[i] do begin
      x := self.X + 8;
      y := self.Y + 8 + ((i - 1) * 56);
   end;
end;

procedure TGameMiniPartyPanel.SetSkill(value: TRpgSkill);
begin
   FSkill.Free;
   FSkill := value;
end;

const PARTY_TARGET_LAYOUT =
  '[{"Name": "Party",    "Class": "TGameMiniPartyPanel", "Coords": [136, 0,  320, 240]},' +
   '{"Name": "Item",     "Class": "TOnelineLabelBox",    "Coords": [0,   0,  136, 32 ]},' +
   '{"Name": "Quantity", "Class": "TQuantityBox",        "Coords": [0,   32, 136, 64 ]}]';
initialization
   TMenuEngine.RegisterMenuPage('PartyTarget', PARTY_TARGET_LAYOUT);
   TMenuEngine.RegisterMenuBoxClass(TGameMiniPartyPanel);
   TMenuEngine.RegisterMenuBoxClass(TQuantityBox);
end.
