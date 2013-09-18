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
   turbu_2k_frames, turbu_2k_menu_basis;

type
   T2kBattleMainMenu = class(TGameMenuBox)
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
   end;

   T2kBattlePage = class(TMenuPage);

implementation
uses
   turbu_constants, turbu_database;

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

end.
