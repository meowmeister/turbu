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
   Types, Classes, Generics.Collections, SyncObjs,
   turbu_heroes, turbu_defs, turbu_monsters, turbu_battles, turbu_battle_logic,
   turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_monster_party,
   turbu_2k_menu_components,
   sg_defs, sdl_sprite;

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

   TBattleTargetMenu = class(TCustomScrollBox)
   private
      FMonsters: T2kMonsterParty;
   protected
      procedure doSetup(value: integer); override;
      procedure drawItem(id, x, y, color: integer); override;
      procedure doButton(const input: TButtonCode); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
      procedure SetMonsters(monsters: T2kMonsterParty);
   end;

   T2kBattleCommands = class(TGameMenuBox)
   protected
      procedure doSetup(value: integer); override;
      procedure DrawText; override;
      procedure doButton(const input: TButtonCode); override;
   end;

   T2kBattlePage = class(TMenuPage)
   private type
      TTransitionMode = (tmNone, tmOpening, tmClosing);
      TBattleState = (bsMenu, bsScriptCheck, bsScript, bsBattle);
   private
      FMonsters: T2kMonsterParty;
      FMoves: TObjectList<TBattleCommand>;
      FTransitionMode: TTransitionMode;
      FBattleState: TBattleState;
      FMoveIndex: integer;
      FRound: integer;
      FSignal: TSimpleEvent;
      FAnims: TSpriteEngine;
      procedure LoadMonster(element: TRpgMonsterElement; list: TStringList;
        var IDs: TArray<integer>);
      procedure CheckState;
      procedure AddMove(value: TBattleCommand);
      procedure TargetComplete;
      procedure InputComplete;
      procedure Reset(newBattle: boolean);
      procedure CheckTransition;
      procedure CheckScripts;
      procedure CheckBattleCommand;
      procedure EndRound;
   protected
      procedure setVisible(const value: boolean); override;
      procedure DoDraw; override;
      procedure Cleanup; override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine;
        const layout: string); override;
      destructor Destroy; override;

      procedure setupEx(const data: TObject); override;
      procedure BeginInput;
   end;

implementation
uses
   SysUtils, Math, Generics.Defaults,
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
   if (FOwner as T2kBattlePage).FTransitionMode <> tmNone then
      Exit;
   if input <> btn_cancel then
      inherited doButton(input);
   if input = btn_enter then
   begin
      case FCursorPosition of
         0: (FOwner as T2kBattlePage).BeginInput; //fight
         1:; //auto
         2:; //flee
      end;
   end;
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

{ T2kBattleCommands }

procedure T2kBattleCommands.doButton(const input: TButtonCode);
var
   owner: T2kBattlePage;
begin
   owner := FOwner as T2kBattlePage;
   if owner.FTransitionMode <> tmNone then
      Exit;

   inherited doButton(input);
   if input = btn_cancel then
      (FOwner as T2kBattlePage).FTransitionMode := tmClosing
   else if input = btn_enter then
   begin
      case FCursorPosition of
         0: owner.focusMenu(self, 'Target', FSetupValue);
         1:;
         2:;
         3:;
      end;
   end;
end;

procedure T2kBattleCommands.doSetup(value: integer);
var
   hero: TRpgHero;
   i: integer;
begin
   inherited doSetup(value);
   if FParsedText.Count = 0 then
   begin
      FParsedText.Add(GDatabase.vocab[V_BATTLE_ATTACK]);
      FParsedText.Add('');
      FParsedText.Add(GDatabase.vocab[V_BATTLE_DEFEND]);
      FParsedText.Add(GDatabase.vocab[V_BATTLE_ITEM]);
      SetLength(FOptionEnabled, 4);
      for i := 0 to High(FOptionEnabled) do
         FOptionEnabled[i] := true;
   end;
   hero := GEnvironment.Party[value];
   FParsedText[1] := hero.SkillCommand;
   InvalidateText;
end;

procedure T2kBattleCommands.DrawText;
begin
   GFontEngine.drawText(FParsedText[0], 0, 0,  1);
   GFontEngine.drawText(FParsedText[1], 0, 16, 1);
   GFontEngine.drawText(FParsedText[2], 0, 32, 1);
   GFontEngine.drawText(FParsedText[3], 0, 48, 1);
end;

{ TBattleTargetMenu }

constructor TBattleTargetMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   FDisplayCapacity := 4;
end;

procedure TBattleTargetMenu.doButton(const input: TButtonCode);
var
   cmd: TTargetCommand;
   owner: T2kBattlePage;
begin
   inherited doButton(input);
   if input = btn_enter then
   begin
      cmd := TAttackCommand.Create(GEnvironment.Party[FSetupValue],
                                   FParsedText.Objects[FCursorPosition] as T2kMonster);
      owner := FOwner as T2kBattlePage;
      owner.AddMove(cmd);
      owner.TargetComplete;
   end;
end;

procedure TBattleTargetMenu.doSetup(value: integer);
var
   monster: T2kMonster;
   i: integer;
begin
   inherited doSetup(value);
   FParsedText.Clear;
   for monster in FMonsters.monsters do
      if monster.visible and not (monster.dead) then
         FParsedText.AddObject(monster.name, monster);
   setLength(FOptionEnabled, FParsedText.Count);
   for i := 0 to High(FOptionEnabled) do
      FOptionEnabled[i] := true;
end;

procedure TBattleTargetMenu.drawItem(id, x, y, color: integer);
begin
   GFontEngine.drawText(FParsedText[id], x, y, color);
end;

procedure TBattleTargetMenu.SetMonsters(monsters: T2kMonsterParty);
begin
   FMonsters := monsters;
end;

{ T2kBattleData }

constructor T2kBattleData.Create(monsters: TRpgMonsterParty; cond: TBattleConditions);
begin
   FMonsters := monsters;
   FCond := cond;
end;

{ T2kBattlePage }

function SpeedOrder(const Left, Right: TBattleCommand): Integer;
begin
   //*descending* sort by priority
   result := right.priority - left.priority;
end;

constructor T2kBattlePage.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; const layout: string);
begin
   FMoves := TObjectList<TBattleCommand>.Create(TComparer<TBattleCommand>.Construct(SpeedOrder));
   FSignal := TSimpleEvent.Create;
   FSignal.SetEvent;
   FAnims := TSpriteEngine.Create(nil, GMenuEngine.Canvas);
   FAnims.Images := GMenuEngine.Images;
   inherited Create(parent, coords, main, layout);
end;

destructor T2kBattlePage.Destroy;
begin
   FMoves.Free;
   FSignal.Free;
   FAnims.Free;
   inherited Destroy;
end;

procedure T2kBattlePage.AddMove(value: TBattleCommand);
begin
   FMoves.Add(value);
end;

procedure T2kBattlePage.BeginInput;
begin
   FTransitionMode := tmOpening;
   Self.menu('Select').setup(1);
end;

procedure T2kBattlePage.CheckTransition;
var
   party: TGameMenuBox;
begin
   party := self.menu('Party');
   case FTransitionMode of
      tmNone: ;
      tmOpening:
      begin
         party.moveTo(shiftX(party.bounds, -2, 0));
         if party.bounds.left = 0 then
         begin
            FTransitionMode := tmNone;
            focusMenu(FCurrentMenu, 'Select', 1);
         end;
      end;
      tmClosing:
      begin
         party.moveTo(shiftX(party.bounds, 2, 72));
         if party.bounds.left = 72 then
         begin
            FTransitionMode := tmNone;
            self.backTo(self.menu('Main'));
         end;
      end;
   end;
end;

procedure T2kBattlePage.CheckScripts;
begin
   FBattleState := bsBattle; //TODO: implement this
end;

procedure T2kBattlePage.CheckBattleCommand;
begin
   if FSignal.WaitFor(0) = wrSignaled then
   begin
      inc(FMoveIndex);
      if FMoveIndex >= FMoves.Count then
         EndRound
      else FMoves[FMoveIndex].Execute(FSignal, FAnims);
   end;
end;

procedure T2kBattlePage.CheckState;
begin
   case FBattleState of
      bsMenu: CheckTransition;
      bsScriptCheck: CheckScripts;
      bsScript: ; //do nothing; let the script run
      bsBattle: CheckBattleCommand;
   end;
end;

procedure T2kBattlePage.Cleanup;
begin
   FreeAndNil(FMonsters);
end;

procedure T2kBattlePage.DoDraw;
begin
   CheckState;
   inherited DoDraw;
   FMonsters.Draw;
   FAnims.Draw;
   FAnims.Dead;
end;

procedure T2kBattlePage.LoadMonster(element: TRpgMonsterElement;
  list: TStringList; var IDs: TArray<integer>);
var
   id: integer;
begin
   for id in IDs do
      if id = element.monster then
         Exit;
   id := element.monster;
   list.add(GDatabase.monsters[id].filename);
   setLength(IDs, length(IDs) + 1);
   IDs[high(IDs)] := id;
end;

procedure T2kBattlePage.Reset(newBattle: boolean);
begin
   FMoveIndex := -1;
   FBattleState := bsMenu;
   FMoves.Clear;
   if newBattle then
      FRound := 0;
end;

procedure T2kBattlePage.setupEx(const data: TObject);
var
   battleData: T2kBattleData;
   element: TRpgMonsterElement;
   list: TStringList;
   IDs: TArray<integer>;
begin
   battleData := data as T2kBattleData;
   IDs := nil;
   list := TStringList.Create;
   list.Sorted := true;
   list.Duplicates := dupIgnore;
   try
      for element in battleData.monsters.monsters do
         LoadMonster(element, list, IDs);
      RunThreadsafe(
         procedure
         var
            bg, imagename: string;
         begin
            bg := battleData.cond.background;
            self.setBG(format('Battle BG\%s.png', [bg]), format('BATTLE*%s', [bg]));
            for imagename in list do
               LoadFullImage(format('Monsters\%s.png', [imagename]),
                             format('MONSTER*%s', [imagename]),
                             false);
         end, true);
      FMonsters := T2kMonsterParty.Create(battleData.monsters, GMenuEngine.Images);
      (self.menu('Target') as TBattleTargetMenu).SetMonsters(FMonsters);
      reset(true);
      inherited setupEx(data);
   finally
      list.Free;
   end;
end;

procedure T2kBattlePage.setVisible(const value: boolean);
begin
   inherited SetVisible(false);
   if value then
   begin
      menu('Main').Visible := true;
      menu('Select').Visible := true;
      menu('Party').Visible := true;
   end;
end;

procedure T2kBattlePage.InputComplete;
begin
   FTransitionMode := tmClosing;
//   FMoves.AddRange(FMonsters.GetMoves);
   FMoves.Sort;
   FBattleState := bsScriptCheck;
end;

procedure T2kBattlePage.EndRound;
begin
   FBattleState := bsMenu;
   FTransitionMode := tmClosing;
   reset(false);
   if FMonsters.defeated then
      Self.Return;
end;

procedure T2kBattlePage.TargetComplete;
begin
   Self.menu('Target').Visible := false;
   if FMoves.Count = GEnvironment.partySize then
      InputComplete
   else begin
       backTo(menu('Select'));
       menu('Select').setup(FMoves.Count + 1);
   end;
end;

const BATTLE_LAYOUT =
  '[{"Name": "Main",   "Class": "T2kBattleMainMenu",   "Coords": [0,   160, 72,  240]},' +
   '{"Name": "Select", "Class": "T2kBattleCommands",   "Coords": [248, 160, 320, 240]},' +
   '{"Name": "Party",  "Class": "TBattlePartyDisplay", "Coords": [72,  160, 320, 240]},' +
   '{"Name": "Target", "Class": "TBattleTargetMenu",   "Coords": [0,   160, 100, 240]}]';

initialization
   TMenuEngine.RegisterMenuPageEx(T2kBattlePage, 'Battle 2K', BATTLE_LAYOUT);
   TMenuEngine.RegisterMenuBoxClass(T2kBattleMainMenu);
   TMenuEngine.RegisterMenuBoxClass(TBattlePartyDisplay);
   TMenuEngine.RegisterMenuBoxClass(T2kBattleCommands);
   TMenuEngine.RegisterMenuBoxClass(TBattleTargetMenu);
end.
