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
   Types,
   turbu_defs, turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_menu_components,
   SDL_sprite;

type
   TMainPanelState = (ms_choosing, ms_party_skill, ms_party_eq);

   TMainMenuPage = class;

   TGamePartyPanel = class(TCustomPartyPanel)
   private
      FState: TMainPanelState;
      FMainMenu: TMainMenuPage;
   public
      procedure Draw; override;
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TGameMainMenu = class(TRm2kGameMenuBox)
   private
      FMainMenu: TMainMenuPage;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure Draw; override;
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TMainMenuPage = class(TMenuPage)
   private
      FCash: TGameCashMenu;
      FPartyPanel: TGamePartyPanel;
      FMainMenu: TGameMainMenu;
      FSkillPage: TMenuPage;
      FEquipmentPage: TMenuPage;
      FInventoryPage: TMenuPage;

      function rightRect(input: TRect): TRect; inline;
      function bottomRect(input: TRect): TRect; inline;
      function topRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine); override;
   end;

implementation

uses
   sysUtils, math,
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
         begin
            self.focusPage(FMainMenu.FSkillPage);
            FMainMenu.FSkillPage.setup(GEnvironment.Party[FCursorPosition + 1].template.id);
         end
         else if FState = ms_party_eq then
         begin
            self.focusPage(FMainMenu.FEquipmentPage);
            FMainMenu.FEquipmentPage.setup(GEnvironment.Party[FCursorPosition + 1].template.id);
         end
         else assert(false);
      end;
      btn_cancel: FState := ms_choosing;
      else ;
   end;
end;

procedure TGamePartyPanel.Draw;
var
   i: byte;
   origin2: TPoint;
   hero: TRpgHero;
   cond: TConditionTemplate;
begin
   inherited;
   i := 1;
   while GEnvironment.Party[i] <> GEnvironment.heroes[0] do
   begin
      FPortrait[i].Draw;
      origin2 := point(round(FPortrait[i].x - engine.WorldX) + 54, round(FPortrait[i].Y - engine.WorldY) + 2);
      hero := GEnvironment.Party[i] ;
      GFontEngine.drawText(hero.name, origin2.X, origin2.Y, 0);
      GFontEngine.drawText(hero.title, origin2.X + 92, origin2.Y, 0);
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_LV], origin2.x, origin2.Y + 16, 1);
      GFontEngine.drawText(intToStr(hero.level), origin2.X + 16, origin2.y + 16, 0);
      if hero.highCondition = 0 then
         GFontEngine.drawText(GDatabase.vocab[V_NORMAL_STATUS], origin2.X + 38, origin2.Y + 16, 0)
      else begin
         cond := GDatabase.conditions[hero.highCondition];
         GFontEngine.drawText(cond.name, origin2.X + 38, origin2.Y + 16, cond.color);
      end;
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_HP], origin2.x + 102, origin2.Y + 16, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.hp), origin2.X + 138, origin2.Y + 16, 0);
      GFontEngine.drawText('/', origin2.X + 138, origin2.Y + 16, 0);
      GFontEngine.drawTextRightAligned(intToStr(hero.maxHp), origin2.X + 162, origin2.Y + 16, 0);
      GFontEngine.drawText(GDatabase.vocab[V_STAT_EXP], origin2.X, origin2.Y + 32, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.exp), origin2.X + 54, origin2.Y + 32, 0);
      GFontEngine.drawText('/', origin2.X + 54, origin2.Y + 32, 0);
      GFontEngine.drawTextRightAligned(intToStr(hero.exp + hero.expNeeded), origin2.X + 98, origin2.Y + 32, 0);
      GFontEngine.drawText(GDatabase.vocab[V_STAT_SHORT_MP], origin2.X + 102, origin2.Y + 32, 1);
      GFontEngine.drawTextRightAligned(intToStr(hero.mp), origin2.X + 138, origin2.Y + 32, 0);
      GFontEngine.drawText('/', origin2.X + 138, origin2.Y + 32, 0);
      GFontEngine.drawTextRightAligned(intToStr(hero.maxMp), origin2.X + 162, origin2.Y + 32, 0);
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

procedure TGameMainMenu.doButton(const input: TButtonCode);
begin
   inherited doButton(input);
   if (input = btn_enter) and (FOptionEnabled[FCursorPosition]) then
   begin
      case FCursorPosition of
         0: //item
         begin
            self.focusPage(FMainMenu.FInventoryPage);
            FMainMenu.FInventoryPage.setup(0);
         end;
         1: //skill
         begin
            self.focusMenu((FOwner as TMainMenuPage).FPartyPanel);
            TMainMenuPage(FOwner).FPartyPanel.setup(1);
         end;
         2: //equip
         begin
            self.focusMenu((FOwner as TMainMenuPage).FPartyPanel);
            TMainMenuPage(FOwner).FPartyPanel.setup(2);         end;
         {$MESSAGE WARN 'Incomplete feature in live unit'}
         3:; //save
         4:; //quit
      end;
   end;
end;

constructor TGameMainMenu.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
var
   I: Integer;
const
   MENUTEXT: string = 'Item' + #3 + 'Skill' + #3 + 'Equipment' + #3 + 'Save' + #3 + 'End Game';
begin
   inherited Create(parent, coords, main, owner);
   self.text := MENUTEXT;
   setLength(FOptionEnabled, 5);
   for I := 0 to 4 do
      FOptionEnabled[i] := true;
end;

procedure TGameMainMenu.Draw;
var
   i: Integer;
   yVal: smallint;
begin
   inherited;
   for i := 0 to min(FParsedText.count - 1, 4) do
   begin
      yVal := 11 + (i * 12);
      if FOptionEnabled[i] then
         GFontEngine.drawText(FParsedText[i], 12, yVal, 0)
      else GFontEngine.drawText(FParsedText[i], 12, yVal, 3);
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

{ TMainMenuPage }

function TMainMenuPage.rightRect(input: TRect): TRect;
begin
   result := rect(input.Left + 88, input.Top, input.Right - 88, input.bottom);
end;

function TMainMenuPage.topRect(input: TRect): TRect;
begin
   result := rect(input.left, input.top, 88, 96)
end;

function TMainMenuPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 208, 88, input.bottom - 208);
end;

constructor TMainMenuPage.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine);
begin
   inherited Create(parent, coords, main);
   FMainMenu := TGameMainMenu.Create(parent, topRect(coords), main, self);
   FMainMenu.FMainMenu := self;
   registerComponent(FMainMenu);
   FPartyPanel := TGamePartyPanel.Create(parent, rightRect(coords), main, self);
   FPartyPanel.FMainMenu := self;
   registerComponent(FPartyPanel);
   FCash := TGameCashMenu.Create(parent, bottomRect(coords), main, self);
   registerComponent(FCash);
end;

end.
