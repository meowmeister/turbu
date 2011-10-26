unit rm2x_party_target_menu;
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
   frames, chipset_data, rpg_list, skill_code, rm2x_main_menu,
   rm2x_menu_components, menu_basis,
   {asphyreSprite} SDL_sprite;

type
   TQuantityBox = class(TSystemFrame)
   private
      FItem: TRpgItem;
      FSkill: TRpgSkill;

      procedure clear; inline;
      procedure setItem(const Value: TRpgItem);
      procedure setSkill(const Value: TRpgSkill);
   public
      procedure Draw; override;

      property item: TRpgItem write setItem;
      property skill: TRpgSkill write setSkill;
   end;

   TGameMiniPartyPanel = class(TCustomPartyPanel)
   private
      FItem: TRpgItem;
      FSkill: TRpgSkill;
   public
      procedure Draw; override;
      procedure doSetup(value: integer); override;
      procedure moveTo(coords: TRect); override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TPartyTargetPage = class(TMenuPage)
   private
      FTargetPanel: TGameMiniPartyPanel;
      FItemBox: TOnelineLabelBox;
      FQuantityBox: TQuantityBox;

      function bottomRect(input: TRect): TRect; inline;
      function topRect(input: TRect): TRect; inline;
      function sideRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine); override;
   end;

implementation
uses
   sysUtils, types,
   LDB, chipset_graphics, script_engine, script_backend, item_code, skill_data, text_graphics,
   rm2X_menu_engine, turbu_defs;

{ TQuantityBox }

procedure TQuantityBox.clear;
begin
   fitem := nil;
   fskill := nil;
end;

procedure TQuantityBox.draw;
begin
   inherited;
   if (not assigned(FItem)) and (not assigned(FSkill)) then
      Exit;

   if assigned(FItem) then
   begin
//      drawText(GDatabase.vocabulary[ownedItems], origin.x + 10, origin.y + 10, 1);
      drawTextTo(intToStr(FItem.quantity), origin.X + 116, origin.Y + 10, 0);
   end
   else begin
      assert(assigned(FSkill));
//      drawText(GDatabase.vocabulary[mpCost], origin.x + 10, origin.y + 10, 1);
      drawTextTo(intToStr(FSkill.template.cost), origin.X + 116, origin.Y + 10, 0);
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

procedure TGameMiniPartyPanel.doSetup(value: integer);
begin
   inherited doSetup(value);
   FItem := nil;
   FSkill := nil;
   with FOwner as TPartyTargetPage do
   begin
      if FSetupValue > 0 then
      begin
         FSkill := GSkills[FSetupValue];
         FQuantityBox.skill := FSkill;
         FItemBox.text := FSkill.template.name;
      end
      else begin
         FItem := GParty.inventory[abs(FSetupValue)] as TRpgItem;
         FQuantityBox.item := FItem;
         FItemBox.text := FItem.template.name;
      end;
   end;
end;

procedure TGameMiniPartyPanel.doButton(const input: TButtonCode);
var
   caster, target: TRpgHero;
   sound: TSfxTypes;
begin
   if input <> btn_enter then
      inherited doButton(input);
   case input of //extra handlers for Enter and Cancel
      btn_enter:
      begin
         sound := sfxNil;
         if assigned(FItem) then
         begin
            assert(FItem is TAppliedItem);
            if FItem.quantity > 0 then
            begin
               target := GParty[FCursorPosition + 1];
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
            if sound <> sfxNil then
               GScriptEngine.mediaPlayer.playSystemSound(sound)
            else begin
               assert(FItem is TSkillItem);
               GScriptEngine.mediaPlayer.playSfx(TSkillItem(FItem).skill.firstSound);
            end;
         end
         else begin
            assert(assigned(FSkill));
            caster := GScriptEngine.currentHero;
            if caster.mp < FSkill.template.cost then
               sound := sfxBuzzer
            else begin
               target := GParty[FCursorPosition + 1];
               if FSkill.areaSkill then
               begin
                  if FSkill.usableArea then
                     FSkill.useArea(caster)
                  else sound := sfxBuzzer;
               end
               else if FSkill.usableOn(target.template.id) then
                  FSkill.useHero(caster, target)
               else sound := sfxBuzzer;
            end;
            if sound <> sfxNil then
               GScriptEngine.mediaPlayer.playSystemSound(sound)
            else
            begin
               GScriptEngine.mediaPlayer.playSfx(FSkill.firstSound);
               caster.mp := caster.mp - FSkill.template.cost;
            end;
         end;
      end;
      btn_cancel:
      begin
         if (assigned(FItem)) and (FItem.quantity = 0) then
         begin
            GParty.inventory.Remove(FItem.template.id, 0);
            GInventoryPage.itemMenu.setup(CURSOR_UNCHANGED);
         end;
      end;
      else ; //default case
   end;
end;

procedure TGameMiniPartyPanel.Draw;
var
  I: Integer;
  origin: TPoint;
begin
   inherited;
   i := 1;
   while GParty[i] <> GScriptEngine.hero[0] do
   begin
      FPortrait[i].Draw;
      origin := point(round(FPortrait[i].x - engine.WorldX) + 54, round(FPortrait[i].Y - engine.WorldY) + 2);
      with GParty[i] do
      begin
         drawText(name, origin.X + 1, origin.Y, 0);
//         drawText(GDatabase.vocabulary[lvShort], origin.x + 1, origin.Y + 16, 1);
         drawText(intToStr(level), origin.X + 17, origin.y + 16, 0);
{         if highCondition = 0 then
            drawText(GDatabase.vocabulary[normalStatus], origin.X + 1, origin.Y + 32, 0)
         else with GDatabase.condition[highCondition] do
         begin
            drawText(name, origin.X + 1, origin.Y + 32, color);
         end;
         drawText(GDatabase.vocabulary[hpShort], origin.x + 52, origin.Y + 16, 1);}
         drawTextTo(intToStr(hp), origin.X + 86, origin.Y + 16, 0);
         drawText('/', origin.X + 86, origin.Y + 16, 0);
         drawTextTo(intToStr(maxHp), origin.X + 110, origin.Y + 16, 0);
//         drawText(GDatabase.vocabulary[mpShort], origin.X + 52, origin.Y + 32, 1);
         drawTextTo(intToStr(mp), origin.X + 86, origin.Y + 32, 0);
         drawText('/', origin.X + 86, origin.Y + 32, 0);
         drawTextTo(intToStr(maxMp), origin.X + 110, origin.Y + 32, 0);
      end;

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

{ TPartyTargetPage }

function TPartyTargetPage.topRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top, 136, 32);
end;

constructor TPartyTargetPage.Create(parent: TSpriteEngine; coords: TRect;
  main: TMenuEngine);
begin
   inherited Create(parent, coords, main);
   FTargetPanel := TGameMiniPartyPanel.Create(parent, sideRect(coords), main, self);
   registerComponent(FTargetPanel);
   FItemBox := TOnelineLabelBox.Create(parent, topRect(coords));
   registerComponent(FItemBox);
   FQuantityBox := TQuantityBox.Create(parent, bottomRect(coords));
   registerComponent(FQuantityBox);
end;

function TPartyTargetPage.sideRect(input: TRect): TRect;
begin
   result := rect(input.Left + 136, input.Top, input.right - 136, input.Bottom);
end;

function TPartyTargetPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 32, 136, 32);
end;

end.
