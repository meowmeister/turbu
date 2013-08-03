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
unit turbu_2k_transitions;

interface
uses
   turbu_defs;

type
   TBlockStyle = (ss_random, ss_fromTop, ss_fromBottom);

   procedure erase(which: TTransitions);
   procedure show(which: TTransitions);

const
   STRIPESIZE = 4;
   WAVESIZE = 100;
   MAXZOOM = 13.333333333333333333333333333333;

implementation
uses
   turbu_2k_sprite_engine, turbu_script_engine, turbu_2k_environment, commons,
   turbu_2k_transitions_graphics, turbu_2k_map_engine, turbu_transition_interface,
   SDL, sdl_canvas;

var
   GStripeArray: array of word;

procedure swap(var x, y: cardinal); inline;
var
   dummy: cardinal;
begin
   dummy := x;
   x := y;
   y := dummy;
end;

procedure erase(which: TTransitions);
var
   tran: ITransition;
begin
   if (GSpriteEngine.state = gs_fading) or (GSpriteEngine.blank) then
      Exit;

   case which of
      trnDefault: assert(false);
      trnFadeout: tran := turbu_2k_transitions_graphics.fadeOut;
      trnBlocks: tran := turbu_2k_transitions_graphics.blocks(dw_random);
      trnBlockUp: tran := turbu_2k_transitions_graphics.blocks(dw_downward);
      trnBlockDn: tran := turbu_2k_transitions_graphics.blocks(dw_upward);
      trnBlinds: tran := turbu_2k_transitions_graphics.blinds(true);
      trnStripeHiLo: tran := turbu_2k_transitions_graphics.stripes(true, false);
      trnStripeLR: tran := turbu_2k_transitions_graphics.stripes(true, true);
      trnOutIn: tran := turbu_2k_transitions_graphics.outIn(true);
      trnInOut: tran := turbu_2k_transitions_graphics.inOut(true);
      trnScrollUp: tran := turbu_2k_transitions_graphics.scroll(true, facing_up);
      trnScrollDn: tran := turbu_2k_transitions_graphics.scroll(true, facing_down);
      trnScrollLeft: tran := turbu_2k_transitions_graphics.scroll(true, facing_left);
      trnScrollRight: tran := turbu_2k_transitions_graphics.scroll(true, facing_right);
      trnDivHiLow: tran := turbu_2k_transitions_graphics.divide(ds_vert);
      trnDivLR: tran := turbu_2k_transitions_graphics.divide(ds_horiz);
      trnDivQuarters: tran := turbu_2k_transitions_graphics.divide(ds_both);
      trnZoom: tran := turbu_2k_transitions_graphics.zoom(true);
      trnMosaic: tran := mosaic(true);
//      trnTwist: turbu_2k_transitions_graphics.bof2(true);
      trnRipple: tran := turbu_2k_transitions_graphics.wave(true);
      trnNone:
      begin
         GSpriteEngine.endErase;
         Exit;
      end;
      else assert(false);
   end;
   runThreadsafe(
      procedure
      begin
         tran.Setup(false, GSpriteEngine.endErase);
         GSpriteEngine.beginTransition(true);
         GGameEngine.Transition := tran;
      end, true);
end;

procedure show(which: TTransitions);
var
   tran: ITransition;
begin
   case which of
      trnDefault: assert(false);
      trnFadeout: tran := turbu_2k_transitions_graphics.fadeIn;
      trnBlocks: tran := turbu_2k_transitions_graphics.blocks(dw_random);
      trnBlockUp: tran := turbu_2k_transitions_graphics.blocks(dw_downward);
      trnBlockDn: tran := turbu_2k_transitions_graphics.blocks(dw_upward);
      trnBlinds: tran := turbu_2k_transitions_graphics.blinds(false);
      trnStripeHiLo: tran := turbu_2k_transitions_graphics.stripes(false, false);
      trnStripeLR: tran := turbu_2k_transitions_graphics.stripes(false, true);
      trnOutIn: tran := turbu_2k_transitions_graphics.outIn(false);
      trnInOut: tran := turbu_2k_transitions_graphics.inOut(false);
      trnScrollUp: tran := turbu_2k_transitions_graphics.scroll(false, facing_up);
      trnScrollDn: tran := turbu_2k_transitions_graphics.scroll(false, facing_down);
      trnScrollLeft: tran := turbu_2k_transitions_graphics.scroll(false, facing_left);
      trnScrollRight: tran := turbu_2k_transitions_graphics.scroll(false, facing_right);
      trnDivHiLow: tran := turbu_2k_transitions_graphics.combine(ds_vert);
      trnDivLR: tran := turbu_2k_transitions_graphics.combine(ds_horiz);
      trnDivQuarters: tran := turbu_2k_transitions_graphics.combine(ds_both);
      trnZoom: tran := turbu_2k_transitions_graphics.zoom(false);
      trnMosaic: tran := mosaic(false);
//      trnTwist: turbu_2k_transitions_graphics.bof2(false);
      trnRipple: tran := turbu_2k_transitions_graphics.wave(false);
      trnNone, trnInstant:
      begin
         GSpriteEngine.endShow;
         exit;
      end
      else assert(false);
   end;
   runThreadsafe(
      procedure
      begin
         tran.Setup(true, GSpriteEngine.endShow);
         GSpriteEngine.beginTransition(false);
         GGameEngine.Transition := tran;
      end, true);
end;

end.
