unit turbu_defs;
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
   Types, TypInfo, Generics.Collections;

type
   TCharEvent = procedure(character, party: TObject) of object;
   TDamageCalcEvent = function(character, target: TObject; var1, var2, var3, var4: integer; offensive: boolean): integer of object;
   TToHitEvent = function(character, target: TObject; effectiveness: integer; offensive: boolean): boolean of object;
   TCondOnTurnEvent = procedure(character, condition: TObject; var1, var2, var3, var4: integer) of object;
   TExpCalcEvent = function(level, var1, var2, var3, var4: integer): integer of object;

   TRpgMethod = procedure of object;
   TStringSetProc = procedure (header: string) of object;

   TIntArray = array of integer;
   T4IntArray = array[1..4] of integer;
   TPByteArray = packed array of byte;
   TPWordArray = packed array of word;
   TByteSet = set of byte;
   TPointArray = array of TPoint;
   TStringArray = array of string;

   TLocation = packed record
   case boolean of
      false: ( map: integer;
               x: smallint;
               y: smallint;);
      true:  ( value: int64);
      end;

   TNameType = record
      name: string;
      flags: TParamFlags;
      typeVar: integer;
   end;

   PByteArray = ^TPByteArray;
   PPointArray = ^TPointArray;

   TVarOps = (vo_add, vo_sub, vo_mult, vo_div, vo_equals);
   TVarSets = (vs_switch, vs_integer, vs_float, vs_string);
   TUsableWhere = (us_field, us_battle, us_both);
   TColorSet = (cs_red, cs_green, cs_blue, cs_sat);

   TScriptStyle = (sc_exp, sc_skill);

   TSfxTypes = (sfxCursor, sfxAccept, sfxCancel, sfxBuzzer, sfxBattleStart,
                sfxEscape, sfxEnemyAttack, sfxEnemyDamage, sfxAllyDamage,
                sfxEvade, sfxEnemyDies, sfxItemUsed, sfxNil);

   TBgmTypes = (bgmTitle, bgmBattle, bgmBossBattle, bgmVictory, bgmInn,
                bgmBoat, bgmShip, bgmAirship, bgmGameOver);

implementation

end.
