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
unit turbu_2k_skill_menu;

interface
uses
   windows,
   turbu_defs, turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_menu_components,
   SDL_sprite;

type
   TGameSkillMenu = class(TCustomScrollBox)
   private
      FWhichHero: word;
      FSkillIndex: array of word;
   protected
      procedure drawItem(id, x, y, color: integer); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;

      property hero: word write FWhichHero;
   end;

implementation
uses
   sysUtils, types,
   turbu_database, turbu_heroes, turbu_skills, turbu_text_utils,
   turbu_2k_environment;

{ TGameSkillMenu }

constructor TGameSkillMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   FColumns := 2;
   FDisplayCapacity := 20;
end;

procedure TGameSkillMenu.doButton(const input: TButtonCode);
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      if (FCursorPosition < length(FOptionEnabled)) and (FOptionEnabled[FCursorPosition]) then
      begin
         self.focusPage('PartyTarget', FSkillIndex[FCursorPosition]);
         if GDatabase.skill[FSkillIndex[FCursorPosition]].range = sr_area then
            FMenuEngine.placeCursor(-1)
         else FMenuEngine.placeCursor(0);
      end
      else if FCursorPosition >= length(FOptionEnabled) then
         PlaySound(sfxBuzzer);
   end;
end;

procedure TGameSkillMenu.drawItem(id, x, y, color: integer);
begin
   GFontEngine.drawText(FParsedText[id], x, y, color);
   GFontEngine.drawText('-', x + 112, y, color);
   GFontEngine.drawTextRightAligned(intToStr(GDatabase.skill[FSkillIndex[id]].cost), x + 136, y, color);
end;

procedure TGameSkillMenu.doCursor(position: smallint);
begin
   inherited doCursor(position);
   if GEnvironment.Heroes[FWhichHero].skills > 0 then
      FOwner.menu('Effect').text := GDatabase.skill[FSkillIndex[position]].desc;
end;

procedure TGameSkillMenu.doSetup(value: integer);
var
  i, index: Integer;
  ourHero: TRpgHero;
begin
   inherited doSetup(value);
   assert(FSetupValue > 0);
   FWhichHero := FSetupValue;
   ourHero := GEnvironment.heroes[FWhichHero];
   FMenuEngine.currentHero := ourHero;
   (FOwner.menu('CharData') as TOnelineCharReadout).character := FWhichHero;
   SetLength(FOptionEnabled, ourHero.skills);
   FParsedText.Clear;
   setLength(FSkillIndex, ourHero.skills);
   index := 0;
   for i := 1 to GDatabase.skill.Count -1 do
      if ourHero.skill[i] then
      begin
         FSkillIndex[index] := i;
         FOptionEnabled[index] := (GDatabase.skill[i].usableWhere in [us_field, us_both]);
         inc(index);
         FParsedText.Add(GDatabase.skill[i].name);
      end;
   self.Visible := true;
   self.doCursor(0);
end;

const SKILL_LAYOUT =
  '[{"Name": "Skill",    "Class": "TGameSkillMenu",      "Coords": [0, 64, 320, 240]},' +
   '{"Name": "Effect",   "Class": "TOnelineLabelBox",    "Coords": [0, 0,  320, 32 ]},' +
   '{"Name": "CharData", "Class": "TOnelineCharReadout", "Coords": [0, 32, 320, 64 ]}]';
initialization
   TMenuEngine.RegisterMenuPage('Skills', SKILL_LAYOUT);
   TMenuEngine.RegisterMenuBoxClass(TGameSkillMenu);
end.
