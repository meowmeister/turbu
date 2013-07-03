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

   procedure init;
   procedure erase(which: TTransitions);
   procedure show(which: TTransitions);
   procedure swap(var x, y: cardinal);

const
   BLOCKSIZE = 4;
   BLINDSIZE = 8;
   STRIPESIZE = 4;
   WAVESIZE = 100;
   MAXZOOM = 13.333333333333333333333333333333;
var
   GBlockArray: array of cardinal;
   GStripeArray: array of word;

implementation
uses
   turbu_2k_sprite_engine, turbu_script_engine, turbu_2k_environment,
   turbu_2k_transitions_graphics,
{   chipset_data, charset_data, chipset_graphics, transition_graphics,
   script_engine,}
   SDL, sdl_canvas;
const
   VERTICAL_RANGE = 12;
   HALFRANGE = VERTICAL_RANGE div 2;

procedure init;
var
   w, h: integer;
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   w := canvas.Width div BLOCKSIZE;
   h := canvas.Height div BLOCKSIZE;
   if canvas.Width mod BLOCKSIZE <> 0 then
      inc(w);
   if canvas.Height mod BLOCKSIZE <> 0 then
      inc(h);
   setLength(GBlockArray, w * h);
end;

procedure setupBlockArray;
var
   i: cardinal;
begin
   for I := low(GBlockArray) to high(GBlockArray) do
      GBlockArray[i] := i;
end;

procedure setupStripeArray(vertical: boolean);
var i, j: integer;
begin
   if vertical then
      setLength(GStripeArray, GSpriteEngine.Canvas.height div STRIPESIZE)
   else
      setLength(GStripeArray, GSpriteEngine.Canvas.width div STRIPESIZE);
   i := 0;
   j := high(GStripeArray);
   if j mod 2 = 0 then
      dec(j);
   repeat
      GStripeArray[i] := i;
      if j >= 0 then
         GStripeArray[i + 1] := j;
      inc(i, 2);
      dec(j, 2);
   until (i > high(GStripeArray)) or (j < 0);
end;

procedure swap(var x, y: cardinal); inline;
var
   dummy: cardinal;
begin
   dummy := x;
   x := y;
   y := dummy;
end;

procedure shuffleBlocksRandomly;
var
   i: cardinal;
begin
   for I := low(GBlockArray) to high(GBlockArray) do
      swap(GBlockArray[i], GBlockArray[GEnvironment.random(low(GBlockArray), high(GBlockArray))]);
   //end for
end;

procedure shuffleBlocksFromTop;
var
   width: cardinal;
   freeFloor, freeCeiling: integer;
   rangeFloor, rangeCeiling: integer;
   i: integer;
begin
   width := GSpriteEngine.Canvas.Width div BLOCKSIZE;
   freeFloor := width * HALFRANGE;
   freeCeiling := high(GBlockArray) - (freeFloor);
   rangeFloor := width * VERTICAL_RANGE;
   rangeCeiling := high(GBlockArray) - rangeFloor;
   for i := 0 to freeFloor do
      swap(GBlockArray[i], GBlockArray[GEnvironment.random(0, rangeFloor)]);
   for I := freeFloor + 1 to freeCeiling do
      swap(GBlockArray[i], GBlockArray[GEnvironment.random(i - (freeFloor), i + (freeFloor))]);
   for i := freeCeiling + 1 to high(GBlockArray) do
      swap(GBlockArray[i], GBlockArray[GEnvironment.random(rangeCeiling, high(GBlockArray))]);
   //end FOR
end;

procedure shuffleBlocksFromBottom;
var i: integer;
begin
   shuffleBlocksFromTop;
   for I := 0 to high(GBlockArray) div 2 do
      swap(GBlockArray[i], GBlockArray[high(GBlockArray) - i]);
   //end FOR
end;

procedure shuffleBlockArray(const style: TBlockStyle);
begin
   setupBlockArray;
   case style of
      ss_random: shuffleBlocksRandomly;
      ss_fromTop: shuffleBlocksFromTop;
      ss_fromBottom: shuffleBlocksFromBottom;
   end;
end;

procedure erase(which: TTransitions);
begin
   if (GSpriteEngine.state = gs_fading) or (GSpriteEngine.blank) then
      Exit;

   case which of
      trnDefault: assert(false);
      trnFadeout: turbu_2k_transitions_graphics.fadeOut;
      trnBlocks:
      begin
         shuffleBlockArray(ss_random);
         turbu_2k_transitions_graphics.blocks(true);
      end;
      trnBlockUp:
      begin
         shuffleBlockArray(ss_fromTop);
         turbu_2k_transitions_graphics.blocks(true);
      end;
      trnBlockDn:
      begin
         shuffleBlockArray(ss_fromBottom);
         turbu_2k_transitions_graphics.blocks(true);
      end;
      trnBlinds: turbu_2k_transitions_graphics.blinds(true);
      trnStripeHiLo:
      begin
         setupStripeArray(true);
         turbu_2k_transitions_graphics.stripes(true, false);
      end;
      trnStripeLR:
      begin
         setupStripeArray(false);
         turbu_2k_transitions_graphics.stripes(true, true);
      end;
      trnOutIn: turbu_2k_transitions_graphics.outIn(true);
      trnInOut: turbu_2k_transitions_graphics.inOut(true);
      trnScrollUp: turbu_2k_transitions_graphics.scroll(true, facing_up);
      trnScrollDn: turbu_2k_transitions_graphics.scroll(true, facing_down);
      trnScrollLeft: turbu_2k_transitions_graphics.scroll(true, facing_left);
      trnScrollRight: turbu_2k_transitions_graphics.scroll(true, facing_right);
      trnDivHiLow: turbu_2k_transitions_graphics.divide(ds_vert);
      trnDivLR: turbu_2k_transitions_graphics.divide(ds_horiz);
      trnDivQuarters: turbu_2k_transitions_graphics.divide(ds_both);
      trnZoom: turbu_2k_transitions_graphics.zoom(true);
//      trnTwist: turbu_2k_transitions_graphics.bof2(true);
      trnRipple: turbu_2k_transitions_graphics.wave(true);
      trnNone:
      begin
         GSpriteEngine.endErase;
         Exit;
      end;
      else assert(false);
   end;
   GSpriteEngine.beginTransition;
end;

procedure show(which: TTransitions);
begin
   if not GSpriteEngine.blank then
      Exit;

   GSpriteEngine.beginTransition;
   GSpriteEngine.initialRender := true;
   case which of
      trnDefault: assert(false);
      trnFadeout: turbu_2k_transitions_graphics.fadeIn;
      trnBlocks:
      begin
         shuffleBlockArray(ss_random);
         turbu_2k_transitions_graphics.blocks(false);
      end;
      trnBlockUp:
      begin
         shuffleBlockArray(ss_fromTop);
         turbu_2k_transitions_graphics.blocks(false);
      end;
      trnBlockDn:
      begin
         shuffleBlockArray(ss_fromBottom);
         turbu_2k_transitions_graphics.blocks(false);
      end;
      trnBlinds: turbu_2k_transitions_graphics.blinds(false);
      trnStripeHiLo:
      begin
         setupStripeArray(true);
         turbu_2k_transitions_graphics.stripes(false, false);
      end;
      trnStripeLR:
      begin
         setupStripeArray(false);
         turbu_2k_transitions_graphics.stripes(false, true);
      end;
      trnOutIn: turbu_2k_transitions_graphics.outIn(false);
      trnInOut: turbu_2k_transitions_graphics.inOut(false);
      trnScrollUp: turbu_2k_transitions_graphics.scroll(false, facing_up);
      trnScrollDn: turbu_2k_transitions_graphics.scroll(false, facing_down);
      trnScrollLeft: turbu_2k_transitions_graphics.scroll(false, facing_left);
      trnScrollRight: turbu_2k_transitions_graphics.scroll(false, facing_right);
      trnDivHiLow: turbu_2k_transitions_graphics.combine(ds_vert);
      trnDivLR: turbu_2k_transitions_graphics.combine(ds_horiz);
      trnDivQuarters: turbu_2k_transitions_graphics.combine(ds_both);
      trnZoom: turbu_2k_transitions_graphics.zoom(false);
//      trnTwist: turbu_2k_transitions_graphics.bof2(false);
      trnRipple: turbu_2k_transitions_graphics.wave(false);
      trnNone, trnInstant: GSpriteEngine.endShow;
      else assert(false);
   end;
end;

end.
