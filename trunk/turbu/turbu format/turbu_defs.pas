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
   Types, TypInfo,
   turbu_constants;

type
   TCharEvent = procedure(character, party: TObject) of object;
   TDamageCalcEvent = function(character, target: TObject; var1, var2, var3, var4: integer; offensive: boolean): integer of object;
   TToHitEvent = function(character, target: TObject; effectiveness: integer; offensive: boolean): boolean of object;
   TCondOnTurnEvent = procedure(character, condition: TObject; var1, var2, var3, var4: integer) of object;
   TExpCalcEvent = function(level, var1, var2, var3, var4: integer): integer of object;

   TStringSetProc = procedure (header: string) of object;

   TIntArray = array of integer;
   TBoolArray = array of boolean;
   T4IntArray = array[1..4] of integer;
   T4StrArray = array[1..4] of string;
   TPByteArray = packed array of byte;
   TPWordArray = packed array of word;
   TByteSet = set of byte;
   TPointArray = array of TPoint;
   TStringArray = array of string;
   TStatArray = array[1..STAT_COUNT] of integer;

   TLocation = packed record
      map: integer;
      x: smallint;
      y: smallint;
      constructor Create(map: integer; x, y: smallint);
   end;

   TNameType = record
      name: string;
      flags: TParamFlags;
      typeVar: integer;
   end;

   TWeaponStyle = (ws_single, ws_shield, ws_dual, ws_all);

   TVarSets = (vs_switch, vs_integer, vs_float, vs_string);
   TUsableWhere = (us_none, us_field, us_battle, us_both);
   TColorSet = (cs_red, cs_green, cs_blue, cs_sat);

   TScriptStyle = (sc_exp, sc_skill);

   TSfxTypes = (sfxCursor, sfxAccept, sfxCancel, sfxBuzzer, sfxBattleStart,
                sfxEscape, sfxEnemyAttack, sfxEnemyDamage, sfxAllyDamage,
                sfxEvade, sfxEnemyDies, sfxItemUsed);

   TBgmTypes = (bgmBattle, bgmVictory, bgmInn, bgmGameOver, bgmTitle, bgmBossBattle);

   TTransitionTypes = (trn_MapExit, trn_MapEnter, trn_BattleStartErase, trn_BattleStartShow,
                trn_BattleEndErase, trn_BattleEndShow);

   TTransitions = (trn_Default, trn_FadeOut, trn_Blocks, trn_BlockUp, trn_BlockDn,
                   trn_Blinds, trn_StripeHiLo, trn_StripeLR, trn_OutIn, trn_InOut,
                   trn_ScrollUp, trn_ScrollDn, trn_ScrollLeft, trn_ScrollRight,
                   trn_DivHiLow, trn_DivLR, trn_DivQuarters, trn_Zoom, trn_Mosaic,
                   trn_Ripple, trn_Instant, trn_None);

   TShopTypes = (st_BuySell, st_Buy, st_Sell);

   TFacing = (facing_up, facing_right, facing_down, facing_left);
   TFacingSet = set of TFacing;

   TWeatherEffects = (we_none, we_rain, we_snow, we_fog, we_sand);

   TImageEffects = (ie_none, ie_rotate, ie_wave);

   TButtonCode = (btn_enter, btn_cancel, btn_up, btn_down, btn_left, btn_right);
   TButtonCodes = set of TButtonCode;

   TGameState = (gs_map, gs_message, gs_menu, gs_battle, gs_sleeping, gs_fading, gs_minigame);

   TBattleFormation = (bf_normal, bf_initiative, bf_surprised, bf_surrounded, bf_pincer, bf_firstStrike);

   TConcealmentFactor = (cf_none, cf_low, cf_med, cf_high);

   TMboxLocation = (mb_top, mb_middle, mb_bottom);

   TSlot = (eq_weapon, eq_shield, eq_armor, eq_helmet, eq_relic, eq_all);

implementation

{ TLocation }

constructor TLocation.Create(map: integer; x, y: smallint);
begin
   self.map := map;
   self.x := x;
   self.y := y;
end;

end.
