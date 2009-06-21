unit transitions;
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
type
   TBlockStyle = (ss_random, ss_fromTop, ss_fromBottom);
   TTransitions = (trnDefault, trnFadeout, trnBlocks, trnBlockUp, trnBlockDn, trnBlinds, trnStripeHiLo, trnStripeLR, trnOutIn, trnInOut, trnScrollUp, trnScrollDn, trnScrollLeft, trnScrollRight, trnDivHiLow, trnDivLR, trnDivQuarters, trnZoom, trnTwist, trnRipple, trnNone);

   procedure init;
   procedure erase(which: TTransitions);
   procedure show(which: TTransitions);
   procedure swap(var x, y: cardinal);

const
//IT IS EXPECTED THAT: With any custom screen resolutions, both width and height
//must be evenly divisible by BLOCKSIZE
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
   chipset_data, charset_data, chipset_graphics, transition_graphics,
   script_engine,
   SDL;
const
   VERTICAL_RANGE = 12;
   HALFRANGE = VERTICAL_RANGE div 2;

procedure init;
begin
   with GGameEngine.Canvas do
   begin
      setLength(GBlockArray, (Width div BLOCKSIZE) * (Height div BLOCKSIZE));
   end;
end;

procedure setupBlockArray;
var
   i: cardinal;
begin
   for I := low(GBlockArray) to high(GBlockArray) do
      GBlockArray[i] := i;
   //end FOR
end;

procedure setupStripeArray(vertical: boolean);
var i, j: integer;
begin
   if vertical then
      setLength(GStripeArray, GGameEngine.Canvas.height div STRIPESIZE)
   else
      setLength(GStripeArray, GGameEngine.Canvas.width div STRIPESIZE);
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
      swap(GBlockArray[i], GBlockArray[GScriptEngine.rpgRandom(low(GBlockArray), high(GBlockArray))]);
   //end for
end;

procedure shuffleBlocksFromTop;
var
   width: cardinal;
   freeFloor, freeCeiling: integer;
   rangeFloor, rangeCeiling: integer;
   i: integer;
begin
   width := GGameEngine.Canvas.Width div BLOCKSIZE;
   freeFloor := width * HALFRANGE;
   freeCeiling := high(GBlockArray) - (freeFloor);
   rangeFloor := width * VERTICAL_RANGE;
   rangeCeiling := high(GBlockArray) - rangeFloor;
   for i := 0 to freeFloor do
      swap(GBlockArray[i], GBlockArray[GScriptEngine.rpgRandom(0, rangeFloor)]);
   for I := freeFloor + 1 to freeCeiling do
      swap(GBlockArray[i], GBlockArray[GScriptEngine.rpgRandom(i - (freeFloor), i + (freeFloor))]);
   for i := freeCeiling + 1 to high(GBlockArray) do
      swap(GBlockArray[i], GBlockArray[GScriptEngine.rpgRandom(rangeCeiling, high(GBlockArray))]);
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
   if (GGameEngine.state = gs_fading) or (GGameEngine.blank) then
      Exit;

   case which of
      trnDefault: assert(false);
      trnFadeout: transition_graphics.fadeOut;
      trnBlocks:
      begin
         shuffleBlockArray(ss_random);
         transition_graphics.blocks(true);
      end;
      trnBlockUp:
      begin
         shuffleBlockArray(ss_fromTop);
         transition_graphics.blocks(true);
      end;
      trnBlockDn:
      begin
         shuffleBlockArray(ss_fromBottom);
         transition_graphics.blocks(true);
      end;
      trnBlinds: transition_graphics.blinds(true);
      trnStripeHiLo:
      begin
         setupStripeArray(true);
         transition_graphics.stripes(true, false);
      end;
      trnStripeLR:
      begin
         setupStripeArray(false);
         transition_graphics.stripes(true, true);
      end;
      trnOutIn: transition_graphics.outIn(true);
      trnInOut: transition_graphics.inOut(true);
      trnScrollUp: transition_graphics.scroll(true, facing_up);
      trnScrollDn: transition_graphics.scroll(true, facing_down);
      trnScrollLeft: transition_graphics.scroll(true, facing_left);
      trnScrollRight: transition_graphics.scroll(true, facing_right);
      trnDivHiLow: transition_graphics.divide(ds_vert);
      trnDivLR: transition_graphics.divide(ds_horiz);
      trnDivQuarters: transition_graphics.divide(ds_both);
      trnZoom: transition_graphics.zoom(true);
      trnTwist: transition_graphics.bof2(true);
      trnRipple: transition_graphics.wave(true);
      trnNone:
      begin
         GGameEngine.endErase;
         Exit;
      end;
      else assert(false);
   end;
   GGameEngine.beginTransition;
end;

procedure show(which: TTransitions);
begin
   if not GGameEngine.blank then
      Exit;

   GGameEngine.beginTransition;
   GGameEngine.initialRender := true;
   case which of
      trnDefault: assert(false);
      trnFadeout: transition_graphics.fadeIn;
      trnBlocks:
      begin
         shuffleBlockArray(ss_random);
         transition_graphics.blocks(false);
      end;
      trnBlockUp:
      begin
         shuffleBlockArray(ss_fromTop);
         transition_graphics.blocks(false);
      end;
      trnBlockDn:
      begin
         shuffleBlockArray(ss_fromBottom);
         transition_graphics.blocks(false);
      end;
      trnBlinds: transition_graphics.blinds(false);
      trnStripeHiLo:
      begin
         setupStripeArray(true);
         transition_graphics.stripes(false, false);
      end;
      trnStripeLR:
      begin
         setupStripeArray(false);
         transition_graphics.stripes(false, true);
      end;
      trnOutIn: transition_graphics.outIn(false);
      trnInOut: transition_graphics.inOut(false);
      trnScrollUp: transition_graphics.scroll(false, facing_up);
      trnScrollDn: transition_graphics.scroll(false, facing_down);
      trnScrollLeft: transition_graphics.scroll(false, facing_left);
      trnScrollRight: transition_graphics.scroll(false, facing_right);
      trnDivHiLow: transition_graphics.combine(ds_vert);
      trnDivLR: transition_graphics.combine(ds_horiz);
      trnDivQuarters: transition_graphics.combine(ds_both);
      trnZoom: transition_graphics.zoom(false);
      trnTwist: transition_graphics.bof2(false);
      trnRipple: transition_graphics.wave(false);
      trnNone: GGameEngine.endShow;
      else assert(false);
   end;
end;

end.
