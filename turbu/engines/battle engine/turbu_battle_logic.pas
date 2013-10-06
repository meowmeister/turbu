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
unit turbu_battle_logic;

interface
uses
   turbu_heroes;

type
   TBattleCommand = class
   private
      FCharacter: TRpgBattleCharacter;
   public
      constructor Create(user: TRpgBattleCharacter);
      property user: TRpgBattleCharacter read FCharacter;
   end;

   TTargetCommand = class(TBattleCommand)
   private
      FTarget: TRpgBattleCharacter;
   public
      constructor Create(user, target: TRpgBattleCharacter);
   end;

   TAttackCommand = class(TTargetCommand);

implementation

{ TBattleCommand }

constructor TBattleCommand.Create(user: TRpgBattleCharacter);
begin
   FCharacter := user;
end;

{ TTargetCommand }

constructor TTargetCommand.Create(user, target: TRpgBattleCharacter);
begin
   inherited Create(user);
   FTarget := target;
end;

end.
