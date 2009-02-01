unit rm2x_main_menu;
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
   windows, classes,
   chipset_data, frames, menu_basis, LDB, rm2X_menu_components,
   {asphyreSprite} SDL_sprite;

type
   TMainPanelState = (ms_choosing, ms_party_skill, ms_party_eq);

   TGamePartyPanel = class(TCustomPartyPanel)
   private
      FState: TMainPanelState;
   public
      procedure Draw; override;
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TGameMainMenu = class(TRm2kGameMenuBox)
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure Draw; override;
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TMainMenuPage = class(TMenuPage)
   private
      FCash: TGameCashMenu;
      FPartyPanel: TGamePartyPanel;
      FMainMenu: TGameMainMenu;

      function rightRect(input: TRect): TRect; inline;
      function bottomRect(input: TRect): TRect; inline;
      function topRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine); override;
   end;

implementation

uses
   types, sysUtils, forms,
   commons, chipset_graphics, script_engine, script_interface, rm2X_menu_engine,
   text_graphics;

{ TGamePartyPanel }

procedure TGamePartyPanel.doButton(const input: TButtonCode);
begin
   inherited doButton(input);
   case input of
      btn_enter:
      begin
         if FState = ms_party_skill then
         begin
            self.focusPage(GSkillPage);
            GSkillPage.setup(GParty[FCursorPosition + 1].template.id);
         end
         else if FState = ms_party_eq then
         begin
            self.focusPage(GEquipmentPage);
            GEquipmentPage.setup(GParty[FCursorPosition + 1].template.id);
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
begin
   inherited;
   i := 1;
   while GParty[i] <> GCurrentEngine.hero[0] do
   begin
      FPortrait[i].Draw;
      origin2 := point(round(FPortrait[i].x - engine.WorldX) + 54, round(FPortrait[i].Y - engine.WorldY) + 2);
      with GParty[i] do
      begin
         drawText(name, origin2.X, origin2.Y, 0);
         drawText(charClass, origin2.X + 92, origin2.Y, 0);
         drawText(GDatabase.vocabulary[lvShort], origin2.x, origin2.Y + 16, 1);
         drawText(intToStr(level), origin2.X + 16, origin2.y + 16, 0);
         if highCondition = 0 then
            drawText(GDatabase.vocabulary[normalStatus], origin2.X + 38, origin2.Y + 16, 0)
         else with GDatabase.condition[highCondition] do
         begin
            drawText(name, origin2.X + 38, origin2.Y + 16, color);
         end;
         drawText(GDatabase.vocabulary[hpShort], origin2.x + 102, origin2.Y + 16, 1);
         drawTextTo(intToStr(hp), origin2.X + 138, origin2.Y + 16, 0);
         drawText('/', origin2.X + 138, origin2.Y + 16, 0);
         drawTextTo(intToStr(maxHp), origin2.X + 162, origin2.Y + 16, 0);
         drawText(GDatabase.vocabulary[expShort], origin2.X, origin2.Y + 32, 1);
         drawTextTo(intToStr(exp), origin2.X + 54, origin2.Y + 32, 0);
         drawText('/', origin2.X + 54, origin2.Y + 32, 0);
         drawTextTo(intToStr(exp + expNeeded), origin2.X + 98, origin2.Y + 32, 0);
         drawText(GDatabase.vocabulary[mpShort], origin2.X + 102, origin2.Y + 32, 1);
         drawTextTo(intToStr(mp), origin2.X + 138, origin2.Y + 32, 0);
         drawText('/', origin2.X + 138, origin2.Y + 32, 0);
         drawTextTo(intToStr(maxMp), origin2.X + 162, origin2.Y + 32, 0);
      end;
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
            self.focusPage(GInventoryPage);
            GInventoryPage.setup(0);
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
         3:; //save
         4:
         begin
            sleep(500);
            GCurrentThread.syncRun(Application.Terminate);
         end;
      end;
   end;
end;

constructor TGameMainMenu.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
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
  I: Integer;
  yVal: smallint;
begin
   inherited;
   for I := 0 to lesserOf(FParsedText.count - 1, 4) do
   begin
      yVal := 11 + i * (trunc(TGameMap(Engine).fontEngine[0].TextHeight('A') + 3));
      if FOptionEnabled[i] then
         drawText(FParsedText[i], 12, yVal, 0)
      else drawText(FParsedText[i], 12, yVal, 3);
   end;
end;

procedure TGameMainMenu.doSetup (value: integer);
begin
   inherited doSetup(value);
   FOptionEnabled[1] := GParty.size > 0;
   FOptionEnabled[2] := GParty.size > 0;
   FOptionEnabled[3] := GCurrentEngine.canSave;
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

constructor TMainMenuPage.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine);
begin
   inherited Create(parent, coords, main);
   FMainMenu := TGameMainMenu.Create(parent, topRect(coords), main, self);
   registerComponent(FMainMenu);
   FPartyPanel := TGamePartyPanel.Create(parent, rightRect(coords), main, self);
   registerComponent(FPartyPanel);
   FCash := TGameCashMenu.Create(parent, bottomRect(coords));
   registerComponent(FCash);
end;

end.
