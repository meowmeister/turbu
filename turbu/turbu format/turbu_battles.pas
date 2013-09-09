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
unit turbu_battles;

interface
uses
   turbu_battle_engine, turbu_defs;

type
   TBattleConditions = class
   private
      FBackground: string;
      FResults: TBattleResultSet;
      FFormation: TBattleFormation;
   public
      constructor Create(const background: string; formation: TBattleFormation;
        results: TBattleResultSet);
      property background: string read FBackground;
      property formation: TBattleFormation read FFormation;
      property results: TBattleResultSet read FResults;
   end;

implementation

{ TBattleConditions }

constructor TBattleConditions.Create(const background: string;
  formation: TBattleFormation; results: TBattleResultSet);
begin
   FBackground := background;
   FFormation := formation;
   FResults := results;
end;

end.
