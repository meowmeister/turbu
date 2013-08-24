unit turbu_2k_main_menu;
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
   Types, Classes,
   turbu_defs, turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_menu_components,
   SDL_sprite;

type
   TMainPanelState = (ms_choosing, ms_party_skill, ms_party_eq);

   TGamePartyPanel = class(TCustomPartyPanel)
   private
      FState: TMainPanelState;
   public
      procedure DrawText; override;
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TGameMainMenu = class(TGameMenuBox)
   protected
      procedure ParseText(const input: string); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
      procedure DrawText; override;
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
   end;

implementation

uses
   sysUtils, math, OpenGL,
   turbu_2k_environment, turbu_heroes, turbu_text_utils, turbu_database,
   turbu_constants, turbu_resists,
   commons;

{ TGamePartyPanel }

procedure TGamePartyPanel.doButton(const input: TButtonCode);
begin
   inherited doButton(input);
   case input of
      btn_enter:
      begin
         if FState = ms_party_skill then
            self.focusPage('Skills', GEnvironment.Party[FCursorPosition + 1].template.id)
         else if FState = ms_party_eq then
            self.focusPage('Equipment', GEnvironment.Party[FCursorPosition + 1].template.id)
         else assert(false);
      end;
      btn_cancel: FState := ms_choosing;
      else ;
   end;
end;

procedure TGamePartyPanel.DrawText;
var
   i: byte;
   origin2: TPoint;
   hero: TRpgHero;
   cond: TConditionTemplate;
begin
   i := 1;
   while GEnvironment.Party[i] <> GEnvironment.heroes[0] do
   begin
      FPortrait[i].Draw;
      origin2 := point(round(FPortrait[i].x - engine.WorldX) + 54, round(FPortrait[i].Y - engine.WorldY) + 2);
      hero := GEnvironment.Party[i] ;
      GFontEngine.drawText(hero.name, origin2.X, origin2.Y, 1);
      GFontEngine.drawText(hero.title, origin2.X + 92, origin2.Y, 1);
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_LV], origin2.x, origin2.Y + 16, 2);
      GFontEngine.drawText(intToStr(hero.level), origin2.X + 16, origin2.y + 16, 1);
      if hero.highCondition = 0 then
         GFontEngine.drawText(GDatabase.vocab[V_NORMAL_STATUS], origin2.X + 38, origin2.Y + 16, 1)
      else begin
         cond := GDatabase.conditions[hero.highCondition];
         GFontEngine.drawText(cond.name, origin2.X + 38, origin2.Y + 16, cond.color);
      end;
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_HP], origin2.x + 102, origin2.Y + 16, 2);
      GFontEngine.drawTextRightAligned(intToStr(hero.hp), origin2.X + 138, origin2.Y + 16, 1);
      GFontEngine.drawText('/', origin2.X + 138, origin2.Y + 16, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.maxHp), origin2.X + 162, origin2.Y + 16, 1);
      GFontEngine.drawText(GDatabase.vocab[V_STAT_EXP], origin2.X, origin2.Y + 32, 2);
      GFontEngine.drawTextRightAligned(intToStr(hero.exp), origin2.X + 54, origin2.Y + 32, 1);
      GFontEngine.drawText('/', origin2.X + 54, origin2.Y + 32, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.exp + hero.expNeeded), origin2.X + 98, origin2.Y + 32, 1);
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_MP], origin2.X + 102, origin2.Y + 32, 2);
      GFontEngine.drawTextRightAligned(intToStr(hero.mp), origin2.X + 138, origin2.Y + 32, 1);
      GFontEngine.drawText('/', origin2.X + 138, origin2.Y + 32, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.maxMp), origin2.X + 162, origin2.Y + 32, 1);
      inc(i);
   end;
end;

procedure TGamePartyPanel.doSetup(value: integer);
begin
   inherited doSetup(value);
   if FSetupValue in [0..2] then
      FState := TMainPanelState(FSetupValue);
end;

{ TGameMainMenu }

constructor TGameMainMenu.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
var
   i: Integer;
const
   MENUTEXT: string = 'Item' + #13#10 + 'Skill' + #13#10 + 'Equipment' + #13#10 + 'Save' + #13#10 + 'End Game';
begin
   inherited Create(parent, coords, main, owner);
   self.text := MENUTEXT;
   setLength(FOptionEnabled, 5);
   for i := 0 to 4 do
      FOptionEnabled[i] := true;
end;

procedure TGameMainMenu.doButton(const input: TButtonCode);
begin
   inherited doButton(input);
   if (input = btn_enter) and (FOptionEnabled[FCursorPosition]) then
   begin
      case FCursorPosition of
         0: self.focusPage('Inventory', 0); //Item
         1: self.focusMenu('party', 1); //skill
         2: self.focusMenu('party', 2); //equip
         {$MESSAGE WARN 'Incomplete feature in live unit'}
         3:; //save
         4:; //quit
      end;
   end;
end;

procedure TGameMainMenu.ParseText(const input: string);
begin
   FParsedText.text := input;
end;

procedure TGameMainMenu.DrawText;
var
   i, yVal: Integer;
begin
   if self.Visible then
      for i := 0 to min(FParsedText.count - 1, 4) do
      begin
         yVal := 3 + (i * 15);
         if FOptionEnabled[i] then
            GFontEngine.drawText(FParsedText[i], 4, yVal, 1)
         else GFontEngine.drawText(FParsedText[i], 4, yVal, 4);
      end;
end;

procedure TGameMainMenu.doSetup (value: integer);
begin
   inherited doSetup(value);
   FOptionEnabled[1] := GEnvironment.Party.size > 0;
   FOptionEnabled[2] := GEnvironment.Party.size > 0;
   FOptionEnabled[3] := GEnvironment.saveEnabled;
   self.placeCursor(FSetupValue);
end;

const MAIN_LAYOUT =
  '[{"Name": "Main",  "Class": "TGameMainMenu",   "Coords": [0,  0,   88,  96 ]},' +
   '{"Name": "Party", "Class": "TGamePartyPanel", "Coords": [88, 0,   320, 240]},' +
   '{"Name": "Cash",  "Class": "TGameCashMenu",   "Coords": [0,  208, 88,  240]}]';

initialization
   TMenuEngine.RegisterMenuPage('Main', MAIN_LAYOUT);
   TMenuENgine.RegisterMenuBoxClass(TGameMainMenu);
   TMenuENgine.RegisterMenuBoxClass(TGamePartyPanel);
   TMenuENgine.RegisterMenuBoxClass(TGameCashMenu);
   TMenuENgine.RegisterMenuBoxClass(TGameItemMenu);
end.
