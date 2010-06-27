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
   Types, TypInfo;

type
   TCharEvent = procedure(character, party: TObject) of object;
   TDamageCalcEvent = function(character, target: TObject; var1, var2, var3, var4: integer; offensive: boolean): integer of object;
   TToHitEvent = function(character, target: TObject; effectiveness: integer; offensive: boolean): boolean of object;
   TCondOnTurnEvent = procedure(character, condition: TObject; var1, var2, var3, var4: integer) of object;
   TExpCalcEvent = function(level, var1, var2, var3, var4: integer): integer of object;

   TStringSetProc = procedure (header: string) of object;

   TIntArray = array of integer;
   T4IntArray = array[1..4] of integer;
   T4StrArray = array[1..4] of string;
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

   PPointArray = ^TPointArray; //TODO: Do some refactoring and remove this

   TWeaponStyle = (ws_single, ws_shield, ws_dual, ws_all);

   TVarSets = (vs_switch, vs_integer, vs_float, vs_string);
   TUsableWhere = (us_none, us_field, us_battle, us_both);
   TColorSet = (cs_red, cs_green, cs_blue, cs_sat);
   TComparisonOp = (co_equals, co_gtE, co_ltE, co_gt, co_lt, co_notEquals);
   TBinaryOp = (bo_add, bo_sub, bo_mult, bo_div, bo_mod, bo_equals);

   TScriptStyle = (sc_exp, sc_skill);

   TSfxTypes = (sfxCursor, sfxAccept, sfxCancel, sfxBuzzer, sfxBattleStart,
                sfxEscape, sfxEnemyAttack, sfxEnemyDamage, sfxAllyDamage,
                sfxEvade, sfxEnemyDies, sfxItemUsed, sfxNil);

   TBgmTypes = (bgm_Battle, bgm_Victory, bgm_Inn, bgm_Boat, bgm_Ship,
                bgm_Airship, bgm_GameOver, bgm_Title, bgm_BossBattle);

   TTransitionTypes = (trn_MapEnter, trn_MapExit, trn_BattleStartFOut, trn_BattleStartFIn,
                trn_BattleEndFOut, trn_BattleEndFIn);

   TTransitions = (trn_Default, trn_FadeOut, trn_Blocks, trn_BlockUp, trn_BlockDn,
                   trn_Blinds, trn_StripeHiLo, trn_StripeLR, trn_OutIn, trn_InOut,
                   trn_ScrollUp, trn_ScrollDn, trn_ScrollLeft, trn_ScrollRight,
                   trn_DivHiLow, trn_DivLR, trn_DivQuarters, trn_Zoom, trn_Twist,
                   trn_Ripple, trn_None);

   TShopTypes = (st_BuyAndSell, st_Buy, st_Sell);

   TFacing = (facing_up, facing_right, facing_down, facing_left);

   TWeatherEffects = (we_none, we_rain, we_snow, we_fog, we_sand);

   TImageEffects = (ie_none, ie_rotate, ie_wave);

implementation

end.
