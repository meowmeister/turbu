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
unit turbu_2k_battle_menu;

interface
uses
   Types,
   turbu_heroes, turbu_defs, turbu_monsters, turbu_battles,
   turbu_2k_frames, turbu_2k_menu_basis;

type
   T2kBattleData = class
   private
    FCond: TBattleConditions;
    FMonsters: TRpgMonsterParty;
   public
      constructor Create(monsters: TRpgMonsterParty; cond: TBattleConditions);
      property monsters: TRpgMonsterParty read FMonsters;
      property cond: TBattleConditions read FCond;
   end;

   T2kBattleMainMenu = class(TGameMenuBox)
   protected
      procedure DrawText; override;
      procedure doButton(const input: TButtonCode); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
   end;

   TBattlePartyDisplay = class(TGameMenuBox)
   private
      procedure DrawHeroLine(hero: TRpgHero; y: integer);
   protected
      procedure DrawText; override;
   end;

   T2kBattlePage = class(TMenuPage)
   public
      procedure setupEx(const data: TObject); override;
   end;

implementation
uses
   SysUtils,
   Commons,
   turbu_constants, turbu_database, turbu_text_utils,
   turbu_2k_environment;

{ T2kBattleMainMenu }

constructor T2kBattleMainMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
begin
   inherited create(parent, coords, main, owner);
   FParsedText.Add(GDatabase.vocab[V_BATTLE_FIGHT]);
   FParsedText.Add(GDatabase.vocab[V_BATTLE_AUTO]);
   FParsedText.Add(GDatabase.vocab[V_BATTLE_FLEE]);
   SetLength(FOptionEnabled, 3);
   FOptionEnabled[0] := true;
end;

procedure T2kBattleMainMenu.doButton(const input: TButtonCode);
begin
   if input <> btn_cancel then
      inherited doButton(input);
end;

procedure T2kBattleMainMenu.DrawText;
begin
   GFontEngine.drawText(FParsedText[0], 0, 0,  1);
   GFontEngine.drawText(FParsedText[1], 0, 16, 4);
   GFontEngine.drawText(FParsedText[2], 0, 32, 4);
end;

{ TBattlePartyDisplay }

procedure TBattlePartyDisplay.DrawHeroLine(hero: TRpgHero; y: integer);
var
   cond: string;
   condColor: integer;
begin
   GFontEngine.drawText(hero.name, 0, y, 1);
   if hero.highCondition = 0 then
   begin
      cond := GDatabase.vocab['Normal Status'];
      condColor := 1;
   end
   else begin
      cond := GDatabase.conditions[hero.highCondition].name;
      condColor := GDatabase.conditions[hero.highCondition].color;
   end;
   GFontEngine.drawTextRightAligned(cond, 94, y, condColor);
   GFontEngine.drawText(GDatabase.vocab['StatShort-HP'], 112, y, 2);
   GFontEngine.drawTextRightAligned(intToStr(hero.hp), 150, y, 1);
   GFontEngine.drawText('/', 150, y, 1);
   GFontEngine.drawTextRightAligned(intToStr(hero.maxHp), 174, y, 1);
   GFontEngine.drawText(GDatabase.vocab['StatShort-MP'], 180, y, 2);
   GFontEngine.drawTextRightAligned(intToStr(hero.mp), 208, y, 1);
   GFontEngine.drawText('/', 208, y, 1);
   GFontEngine.drawTextRightAligned(intToStr(hero.maxMp), 232, y, 1);
end;

procedure TBattlePartyDisplay.DrawText;
var
   i: integer;
begin
   for i := 1 to GEnvironment.partySize do
      DrawHeroLine(GEnvironment.Party[i], (i - 1) * 16);
end;

{ T2kBattleData }

constructor T2kBattleData.Create(monsters: TRpgMonsterParty; cond: TBattleConditions);
begin
   FMonsters := monsters;
   FCond := cond;
end;

{ T2kBattlePage }

procedure T2kBattlePage.setupEx(const data: TObject);
var
   battleData: T2kBattleData;
   bg, imagename: string;
begin
   battleData := data as T2kBattleData;
   bg := battleData.cond.background;
   RunThreadsafe(
      procedure
      begin
         self.setBG(format('Battle BG\%s.png', [bg]), format('BATTLE*%s', [bg]));
      end, true);
   inherited setupEx(data);
end;

const BATTLE_LAYOUT =
  '[{"Name": "Main", "Class": "T2kBattleMainMenu",    "Coords": [0,  160, 72,  240]},' +
   '{"Name": "Party", "Class": "TBattlePartyDisplay", "Coords": [72, 160, 320, 240]}]';

initialization
   TMenuEngine.RegisterMenuPageEx(T2kBattlePage, 'Battle 2K', BATTLE_LAYOUT);
   TMenuEngine.RegisterMenuBoxClass(T2kBattleMainMenu);
   TMenuEngine.RegisterMenuBoxClass(TBattlePartyDisplay);
end.
