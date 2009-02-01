unit rm2x_skill_menu;
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
   chipset_data, menu_basis, rm2X_menu_components,
   {asphyreSprite} SDL_sprite;

type
   TGameSkillMenu = class(TCustomScrollBox)
   private
      FWhichHero: word;
      FSkillIndex: array of word;
   protected
      procedure drawItem(id, x, y: word; color: byte); override;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;

      property hero: word write FWhichHero;
   end;

   TSkillMenuPage = class(TMenuPage)
   private
      FSkillMenu: TGameSkillMenu;
      FEffectDisplay: TOnelineLabelBox;
      FCharData: TOnelineCharReadout;

      function topRect(input: TRect): TRect; inline;
      function midRect(input: TRect): TRect; inline;
      function bottomRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine); override;
   end;

implementation
uses
   sysUtils, types,
   script_engine, script_backend, LDB, text_graphics, frames, rm2X_menu_engine;

{ TGameSkillMenu }

procedure TGameSkillMenu.doButton(const input: TButtonCode);
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      if FOptionEnabled[FCursorPosition] then
      begin
         GPartyTargetPage.setup(FSkillIndex[FCursorPosition]);
         self.focusPage(GPartyTargetPage);
         if GSkills[FSkillIndex[FCursorPosition]].areaSkill then
            FMenuEngine.placeCursor(-1)
         else FMenuEngine.placeCursor(0);
      end;
   end;
end;

constructor TGameSkillMenu.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   FColumns := 2;
   FDisplayCapacity := 20;
end;

procedure TGameSkillMenu.drawItem(id, x, y: word; color: byte);
begin
   drawText(FParsedText[id], x, y, color);
   drawText('-', x + 112, y, color);
   drawTextTo(intToStr(GDatabase.skill[FSkillIndex[id]].cost), x + 136, y, color);
end;

procedure TGameSkillMenu.doCursor(position: smallint);
begin
   inherited doCursor(position);
   if GCurrentEngine.hero[FWhichHero].skills > 0 then
      TSkillMenuPage(FOwner).FEffectDisplay.text := GDatabase.skill[FSkillIndex[position]].desc;
end;

procedure TGameSkillMenu.doSetup(value: integer);
var
  i, index: Integer;
  ourHero: TRpgHero;
begin
   inherited doSetup(value);
   assert(FSetupValue > 0);
   FWhichHero := FSetupValue;
   ourHero := GCurrentEngine.hero[FWhichHero];
   GCurrentEngine.currentHero := ourHero;
   TSkillMenuPage(FOwner).FCharData.character := FWhichHero;
   SetLength(FOptionEnabled, ourHero.skills);
   FParsedText.Clear;
   setLength(FSkillIndex, ourHero.skills);
   index := 0;
   for i := 1 to GDatabase.skills do
      if ourHero.skill[i] then
      begin
         FSkillIndex[index] := i;
         FOptionEnabled[index] := (GDatabase.skill[i].usableOnField);
         inc(index);
         FParsedText.Add(GDatabase.skill[i].name);
      end;
   //end for
   self.Visible := true;
   self.doCursor(0);
end;

{ TSkillMenuPage }

function TSkillMenuPage.topRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top, input.Right, 32);
end;

function TSkillMenuPage.midRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.top + 32, input.Right, 32);
end;

function TSkillMenuPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.left, input.top + 64, input.right, input.bottom - 64);
end;

constructor TSkillMenuPage.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine);
begin
   inherited Create(parent, coords, main);

   FSkillMenu := TGameSkillMenu.Create(parent, bottomRect(coords), main, self);
   registerComponent(FSkillMenu);
   FEffectDisplay := TOnelineLabelBox.Create(parent, topRect(coords));
   registerComponent(FEffectDisplay);
   FCharData := TOnelineCharReadout.Create(parent, midRect(coords));
   registerComponent(FCharData);
end;

end.
