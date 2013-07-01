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
unit turbu_2k_transitions_graphics;

interface
uses
   turbu_defs,
   SDL_Canvas;

type TDivideStyle = (ds_vert, ds_horiz, ds_both);

   procedure fadeOut;
   procedure fadeIn;
   procedure blocks(vanishing: boolean);
   procedure blinds(vanishing: boolean);
   procedure stripes(vanishing, vertical: boolean);
   procedure outIn(vanishing: boolean);
   procedure inOut(vanishing: boolean);
   procedure scroll(vanishing: boolean; direction: TFacing);
   procedure divide(style: TDivideStyle);
   procedure combine(style: TDivideStyle);
   procedure zoom(vanishing: boolean);
   procedure bof2(vanishing: boolean);
   procedure wave(vanishing: boolean);

var
   GRenderTargets: TSdlRenderTargets;
   GCurrentTarget: integer;

implementation
uses
   types, math, SysUtils,
   commons, turbu_2k_transitions, turbu_2k_sprite_engine, timing, turbu_2k_distortions,
   SDL_13, SG_defs;

const
   FADETIME: array[1..2] of integer = (12, 18);
var
   LTimer: word;
   LVertical: boolean;
   LShowing: boolean;
   LDirection: TFacing;
   LCenter: TPoint;
   LCurrentX, LCurrentY: single;

procedure initReveal; inline;
begin
   GSpriteEngine.Canvas.Clear(SDL_BLACK);
   LTimer := 0;
end;

procedure initErase; inline;
begin
   GRenderTargets[2].DrawFull;
   LTimer := 0;
end;

procedure singleEraseRender;
begin
   GRenderTargets[GCurrentTarget].DrawFull;
end;

procedure flipRender;
begin
   singleEraseRender;
   if GCurrentTarget > 1 then
      GCurrentTarget := 1
   else GCurrentTarget := GCurrentTarget xor 1;
end;

procedure simpleMaskRender;
var
   tx: TSdlTexture;
   blend: TSdlBlendModes;
begin
   GRenderTargets[2].DrawFull;
   tx := GRenderTargets[GCurrentTarget].handle;
   SDL_GetTextureBlendMode(tx, blend);
   try
      SDL_SetTextureBlendMode(tx, [sdlbBlend]);
      GRenderTargets[GCurrentTarget].DrawFull;
   finally
      SDL_SetTextureBlendMode(tx, blend)
   end;
end;

procedure fadeOut;
begin
   GSpriteEngine.fadeOut(FADETIME[1]);
end;

procedure fadeIn;
begin
   GSpriteEngine.fadeOut(TRpgTimestamp.FrameLength);
   GSpriteEngine.blank := false;
   repeat
      sleep(TRpgTimestamp.FrameLength);
   until GSpriteEngine.blank;
   GSpriteEngine.fadeIn(FADETIME[1]);
   GSpriteEngine.blank := false;
end;

{BLOCKS}
{$REGION BLOCKS}
procedure blockErase(var location: integer);
var
   i: cardinal;
   workload: word;
   corner: TPoint;
   width: word;
begin
   if location = 0 then
      initErase;
   workload := high(GBlockArray) div (FADETIME[1] div TRpgTimestamp.FrameLength);
   width := GSpriteEngine.Canvas.Width div BLOCKSIZE;
   for i := location to min(location + workload, high(GBlockArray)) do
   begin
      corner := point((GBlockArray[i] mod width) * BLOCKSIZE, (GBlockArray[i] div width) * BLOCKSIZE);
      GSpriteEngine.Canvas.FillRect(rect(corner.X, corner.Y, BLOCKSIZE, BLOCKSIZE), SDL_BLACK);
      inc(location);
   end;
   if location >= high(GBlockArray) then
      GSpriteEngine.endErase;
end;

procedure blockReveal(var location: integer);
var
   i: cardinal;
   workload: word;
   corner: TPoint;
   width: word;
begin
   if location = 0 then
      initReveal;
   workload := high(GBlockArray) div (FADETIME[1] div TRpgTimestamp.FrameLength);
   width := GSpriteEngine.Canvas.Width div BLOCKSIZE;
   for i := location to min(location + workload, high(GBlockArray)) do
   begin
      corner := point((GBlockArray[i] mod width) * BLOCKSIZE, (GBlockArray[i] div width) * BLOCKSIZE);
      GSpriteEngine.Canvas.FillRect(rect(corner.X, corner.Y, BLOCKSIZE, BLOCKSIZE), SDL_WHITE);
      inc(location);
   end;
   if location >= high(GBlockArray) then
      GSpriteEngine.endShow;
end;

procedure blocks(vanishing: boolean);
begin
   case vanishing of
      true:
      begin
         GSpriteEngine.transProc := blockErase;
         GSpriteEngine.renderProc := singleEraseRender;
      end;
      false:
      begin
         GSpriteEngine.transProc := blockReveal;
         GSpriteEngine.renderProc := simpleMaskRender;
      end;
   end;
end; {$ENDREGION}

{BLINDS}
{$REGION BLINDS}
procedure blindErase(var location: integer);
var
   i: integer;
   interval: word;
   width: word;
begin
   interval := FADETIME[1] div BLINDSIZE;
   if location = 0 then
   begin
      initErase;
      LTimer := interval - TRpgTimestamp.FrameLength;
   end;
   inc(LTimer, TRpgTimestamp.FrameLength);
   if LTimer >= interval then
   begin
      width := GSpriteEngine.Canvas.Width;
      i := location;
      repeat
         GSpriteEngine.Canvas.FillRect(rect(0, i, width, 1), SDL_BLACK);
         inc(i, BLINDSIZE);
      until i >= GSpriteEngine.Canvas.Height;
      inc(location);
      dec(LTimer, interval);
   end;
   if location > BLINDSIZE then
      GSpriteEngine.endErase;
end;

procedure blindReveal(var location: integer);
var
   i: integer;
   interval: word;
   width: word;
begin
   interval := FADETIME[1] div BLINDSIZE;
   if location = 0 then
   begin
      initReveal;
      LTimer := interval - TRpgTimestamp.FrameLength;
   end;
   inc(LTimer, TRpgTimestamp.FrameLength);
   if LTimer >= interval then
   begin
      width := GSpriteEngine.Canvas.Width;
      i := location;
      repeat
         GSpriteEngine.Canvas.FillRect(rect(GCurrentTarget, i, width, 1), SDL_WHITE);
         inc(i, BLINDSIZE);
      until i >= GSpriteEngine.Canvas.Height;
      inc(location);
      dec(LTimer, interval);
   end;
   if location > BLINDSIZE then
      GSpriteEngine.endShow;
end;

procedure blinds(vanishing: boolean);
begin
   case vanishing of
      true:
      begin
         GSpriteEngine.transProc := blindErase;
         GSpriteEngine.renderProc := singleEraseRender;
      end;
      false:
      begin
         GSpriteEngine.transProc := blindReveal;
         GSpriteEngine.renderProc := simpleMaskRender;
      end;
   end;
end; {$ENDREGION}

{STRIPES}
{$REGION STRIPES}
procedure doStripes(var location: integer);
var
   i: cardinal;
   workload: word;
   corner: TPoint;
   color: TSDL_Color;
begin
   if location = 0 then
      if LShowing then
         initReveal
      else initErase;
   if LShowing then
      color := SDL_WHITE
   else color := SDL_BLACK;
   workload := high(GStripeArray) div (FADETIME[1] div TRpgTimestamp.FrameLength);
   for i := location to min(location + workload, high(GStripeArray)) do
   begin
      if LVertical then
      begin
         corner := point(GStripeArray[i] * STRIPESIZE, 0);
         GSpriteEngine.Canvas.FillRect(rect(corner.X, corner.Y, STRIPESIZE, GSpriteEngine.Canvas.Height), color);
      end else begin
         corner := point(0, GStripeArray[i] * STRIPESIZE);
         GSpriteEngine.Canvas.FillRect(rect(corner.X, corner.Y, GSpriteEngine.Canvas.Width, STRIPESIZE), color);
      end;
      inc(location);
   end;
   if location >= high(GStripeArray) then
      if LShowing then
         GSpriteEngine.endShow
      else GSpriteEngine.endErase;
end;

procedure stripes(vanishing, vertical: boolean);
begin
   LVertical := vertical;
   LShowing := not vanishing;
   GSpriteEngine.transProc := doStripes;
   case vanishing of
      true: GSpriteEngine.renderProc := singleEraseRender;
      false: GSpriteEngine.renderProc := simpleMaskRender;
   end;
end; {$ENDREGION}

{IN/OUT}
{$REGION IN/OUT}
procedure doOutIn(var location: integer);
var
   i: cardinal;
   workload: word;
   ratio: single;
   mask: TRect;
   color: TSDL_Color;
begin
   if LShowing then
   begin
      initErase;
      color := SDL_BLACK;
   end
   else begin
      initReveal;
      color := SDL_WHITE;
   end;
   workload := (GSpriteEngine.Canvas.Height div 2) div max((FADETIME[1] div TRpgTimestamp.FrameLength), 1);
   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   i := min(location + workload, GSpriteEngine.Canvas.Height div 2);
   mask.TopLeft := point(round(i * ratio), i);
   mask.Right := GSpriteEngine.Canvas.Width - mask.Left;
   mask.Bottom := GSpriteEngine.Canvas.Height - mask.Top;
   GSpriteEngine.Canvas.FillRect(mask, color);
   inc(location, workload);
   if location >= GSpriteEngine.Canvas.Height div 2 then
      if LShowing then
         GSpriteEngine.endShow
      else GSpriteEngine.endErase;
end;

procedure outIn(vanishing: boolean);
begin
   LShowing := not vanishing;
   GSpriteEngine.transProc := doOutIn;
   case vanishing of
      true: GSpriteEngine.renderProc := simpleMaskRender;
      false: GSpriteEngine.renderProc := singleEraseRender;
   end;
end;

procedure doInOut(var location: integer);
var
   i: integer;
   workload: word;
   ratio: single;
   mask: TRect;
   center: TPoint;
   color: TSDL_Color;
begin
   if LShowing then
   begin
      initReveal;
      color := SDL_WHITE;
   end
   else begin
      initErase;
      color := SDL_BLACK;
   end;
   center := point(GSpriteEngine.Canvas.Width div 2, GSpriteEngine.Canvas.Height div 2);
   workload := (GSpriteEngine.Canvas.Height div 2) div max((FADETIME[1] div TRpgTimestamp.FrameLength), 1);
   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   i := min(location + workload, GSpriteEngine.Canvas.Height div 2);
   mask.TopLeft := point(center.x - round(i * ratio), center.y - i);
   mask.bottomRight := point(center.x + round(i * ratio), center.y + i);
   GSpriteEngine.Canvas.FillRect(mask, color);
   inc(location, workload);
   if location >= GSpriteEngine.Canvas.Height div 2 then
      if LShowing then
         GSpriteEngine.endShow
      else GSpriteEngine.endErase;
end;

procedure inOut(vanishing: boolean);
begin
   LShowing := not vanishing;
   GSpriteEngine.transProc := doInOut;
   case vanishing of
      true: GSpriteEngine.renderProc := singleEraseRender;
      false: GSpriteEngine.renderProc := simpleMaskRender;
   end;
end;{$ENDREGION}

{SCROLL}
{$REGION SCROLL}
procedure doScroll(var location: integer);
var
   workload, boundary: word;
   i: integer;
   dst: TSgPoint;
   canvas: TSdlCanvas;
begin
   initReveal;
   case LDirection of
      facing_up, facing_down:
      begin
         workload := GSpriteEngine.Canvas.Height div (FADETIME[1] div TRpgTimestamp.FrameLength);
         boundary := GSpriteEngine.Canvas.Height;
      end;
      facing_right, facing_left:
      begin
         workload := GSpriteEngine.Canvas.Width div (FADETIME[1] div TRpgTimestamp.FrameLength);
         boundary := GSpriteEngine.Canvas.Width;
      end;
      else raise Exception.Create('Invalid direction');
   end;
   i := location + workload;
   if LDirection in [facing_up, facing_left] then
      i := i * -1;
   canvas := GSpriteEngine.Canvas;
   if LShowing then
      case LDirection of
         facing_up: inc(i, canvas.Height);
         facing_left: inc(i, canvas.Width);
         facing_down: dec(i, canvas.Height);
         facing_right: dec(i, canvas.Width);
      end;
   if LDirection in [facing_up, facing_down] then
      dst := sgPoint(0, i)
   else dst := sgPoint(i, 0);
   GSpriteEngine.Canvas.Draw(GRenderTargets[2], dst);
   inc(location, workload);
   if location >= boundary then
      if LShowing then
         GSpriteEngine.endShow
      else GSpriteEngine.endErase;
end;

procedure scroll(vanishing: boolean; direction: TFacing);
begin
   LShowing := not vanishing;
   LDirection := direction;
   GSpriteEngine.transProc := doScroll;
   GSpriteEngine.renderProc := singleEraseRender;
end;{$ENDREGION}

{DIVIDE}
{$REGION DIVIDE}
procedure divideVert(var location: integer);
var
   workload: byte;
   boundary: word;
   alternate: byte;
   canvas: TSdlCanvas;
begin
   if (location = 0) then
   begin
      initErase;
      if (GCurrentTarget = 0) then
      begin
         GCurrentTarget := 2;
         Exit;
      end;
   end else initReveal;

   alternate := GCurrentTarget xor 1;
   boundary := GSpriteEngine.Canvas.Height div 2;
   workload := boundary div (FADETIME[1] div TRpgTimestamp.FrameLength);
   canvas := GSpriteEngine.Canvas;
   Canvas.DrawRect(GRenderTargets[alternate], sgPoint(0, -workload), rect(0, 0, canvas.Width, boundary));
   Canvas.DrawRect(GRenderTargets[alternate], sgPoint(0, boundary + workload), rect(0, boundary, canvas.Width, canvas.Height));
   inc(location, workload);
   if location >= boundary then
      GSpriteEngine.endErase;
end;

procedure divideHoriz(var location: integer);
var
   workload: byte;
   boundary: word;
   alternate: byte;
   canvas: TSdlCanvas;
begin
   if (location = 0) then
   begin
      initErase;
      if (GCurrentTarget = 0) then
      begin
         GCurrentTarget := 2;
         Exit;
      end;
   end else initReveal;

   alternate := GCurrentTarget xor 1;
   boundary := GSpriteEngine.Canvas.Width div 2;
   workload := boundary div (FADETIME[1] div TRpgTimestamp.FrameLength);
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRect(GRenderTargets[alternate], sgPoint(-workload, 0), rect(0, 0, boundary, canvas.Height));
   canvas.DrawRect(GRenderTargets[alternate], sgPoint(boundary + workload, 0), rect(boundary, 0, canvas.Width, canvas.Height));
   inc(location, workload);
   if location >= boundary then
      GSpriteEngine.endErase;
end;

procedure divideQuarters(var location: integer);
var
   workloadH, workloadV: byte;
   boundaryH, boundaryV: word;
   ratio: single;
   alternate: byte;
   canvas: TSdlCanvas;
begin
   if (location = 0) then
   begin
      initErase;
      if (GCurrentTarget = 0) then
      begin
         GCurrentTarget := 2;
         Exit;
      end;
   end else initReveal;

   alternate := GCurrentTarget xor 1;
   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   boundaryH := GSpriteEngine.Canvas.Width div 2;
   workloadH := boundaryH div (FADETIME[1] div TRpgTimestamp.FrameLength);
   boundaryV := round(boundaryH / ratio);
   workloadV := round(workloadH / ratio);
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRect(GRenderTargets[alternate], sgPoint(-workloadH, -workloadV),
     rect(0, 0, boundaryH, boundaryV));
   canvas.DrawRect(GRenderTargets[alternate], sgPoint(-workloadH, boundaryV + workloadV),
     rect(0, boundaryV, boundaryH, canvas.Height));
   canvas.DrawRect(GRenderTargets[alternate], sgPoint(boundaryH + workloadH, -workloadv),
     rect(boundaryH, 0, canvas.Width, boundaryV));
   canvas.DrawRect(GRenderTargets[alternate], sgPoint(boundaryH + workloadH, boundaryV + workloadV),
     rect(boundaryH, boundaryV, canvas.Width, canvas.Height));
   inc(location, workloadH);
   if location >= boundaryH then
      GSpriteEngine.endErase;
end;

procedure divide(style: TDivideStyle);
begin
   case style of
      ds_vert: GSpriteEngine.transProc := divideVert;
      ds_horiz: GSpriteEngine.transProc := divideHoriz;
      ds_both: GSpriteEngine.transProc := divideQuarters;
   end;
   GSpriteEngine.renderProc := flipRender;
end; {$ENDREGION}

{COMBINE}
{$REGION COMBINE}
procedure combineVert(var location: integer);
var
   workload: byte;
   boundary: word;
   canvas: TSdlCanvas;
begin
   if location = 0 then
      initReveal;
   boundary := GSpriteEngine.Canvas.Height div 2;
   workload := boundary div (FADETIME[1] div TRpgTimestamp.FrameLength);
   inc(location, workload);
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRect(GRenderTargets[2], sgPoint(0, (boundary - location) * -1),
     rect(0, 0, canvas.Width, boundary));
   canvas.DrawRect(GRenderTargets[2], sgPoint(0, canvas.Height - location),
     rect(0, boundary, canvas.Width, canvas.Height));
   if location >= boundary then
      GSpriteEngine.endShow
end;

procedure combineHoriz(var location: integer);
var
   workload: byte;
   boundary: word;
   canvas: TSdlCanvas;
begin
   if location = 0 then
      initReveal;
   boundary := GSpriteEngine.Canvas.Width div 2;
   workload := boundary div (FADETIME[1] div TRpgTimestamp.FrameLength);
   inc(location, workload);
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRect(GRenderTargets[2], sgPoint((boundary - location) * -1, 0),
     rect(0, 0, boundary, canvas.Height));
   canvas.DrawRect(GRenderTargets[2], sgPoint(canvas.Width - location, 0),
     rect(boundary, 0, canvas.Width, canvas.Height));
   if location >= boundary then
      GSpriteEngine.endShow
end;

procedure combineQuarters(var location: integer);
var
   workload: byte;
   boundaryH, boundaryV: word;
   locationV: integer;
   ratio: single;
   canvas: TSdlCanvas;
begin
   if location = 0 then
      initReveal;
   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   boundaryH := GSpriteEngine.Canvas.Width div 2;
   workload := boundaryH div (FADETIME[1] div TRpgTimestamp.FrameLength);
   boundaryV := round(boundaryH / ratio);
   locationV := round(location / ratio);
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRect(GRenderTargets[2], sgPoint((boundaryH - location) * -1, (boundaryV - locationV) * -1),
     rect(0, 0, boundaryH, boundaryV));
   canvas.DrawRect(GRenderTargets[2], sgPoint((boundaryH - location) * -1, canvas.Height - locationV),
     rect(0, boundaryV, boundaryH, canvas.Height));
   canvas.DrawRect(GRenderTargets[2], sgPoint(canvas.Width - location, (boundaryV - locationV) * -1),
     rect(boundaryH, 0, canvas.Width, boundaryV));
   canvas.DrawRect(GRenderTargets[2], sgPoint(canvas.Width - location, canvas.Height - locationV),
     rect(boundaryH, boundaryV, canvas.Width, canvas.Height));
   inc(location, workload);
   if location >= boundaryH then
      GSpriteEngine.endShow
end;

procedure combine(style: TDivideStyle);
begin
   case style of
      ds_vert: GSpriteEngine.transProc := combineVert;
      ds_horiz: GSpriteEngine.transProc := combineHoriz;
      ds_both: GSpriteEngine.transProc := combineQuarters;
   end;
   GSpriteEngine.renderProc := singleEraseRender;
end; {$ENDREGION}

{ZOOM}
{$REGION ZOOM}
procedure doZoom(var location: integer);
var
   ratio: single;
   viewRect: TRect;
   minimum: byte;
   workload: word;
begin
   minimum := round(GSpriteEngine.Canvas.Width / MAXZOOM);
   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   if (location = 0) then
      if LShowing then
      begin
         location := minimum;
         LCurrentX := LCenter.X - (minimum / 2);
         LCurrentY := LCenter.Y - ((minimum / ratio) / 2);
      end else
      begin
         LCurrentX := 0;
         LCurrentY := 0;
         location := GSpriteEngine.Canvas.Width;
      end;
   workload := GSpriteEngine.Canvas.Width div (FADETIME[1] div TRpgTimestamp.FrameLength);
   if LShowing then
      location := min(location + workload, GSpriteEngine.Canvas.Width)
   else location := max(location - workload, minimum);
   viewRect.BottomRight := point(location, round(location / ratio));
   if LShowing then
   begin
      LCurrentX := LCurrentX - (LCenter.x / GSpriteEngine.Canvas.Width) * workload;
      LCurrentY := LCurrentY - (LCenter.Y / GSpriteEngine.Canvas.Height) * (workload / ratio);
   end else begin
      LCurrentX := LCurrentX + (LCenter.x / GSpriteEngine.Canvas.Width) * workload;
      LCurrentY := LCurrentY + (LCenter.Y / GSpriteEngine.Canvas.Height) * (workload / ratio);
   end;
   viewRect.Left := max(round(LCurrentX), 0);
   viewRect.Top := max(round(LCurrentY), 0);
   GSpriteEngine.Canvas.DrawRectTo(GRenderTargets[2], viewRect, rect(0, 0, GSpriteEngine.Canvas.Width, GSpriteEngine.Canvas.Height));
   if LShowing then
   begin
      if location >= GSpriteEngine.Canvas.Width then
         GSpriteEngine.endShow;
   end
   else if location <= minimum then
      GSpriteEngine.endErase;
end;

procedure zoom(vanishing: boolean);
begin
   with GSpriteEngine.currentParty.baseTile do
      LCenter := point(trunc(x) + 8, trunc(y) + 8);
   dec(LCenter.x, round(GSpriteEngine.WorldX));
   dec(LCenter.Y, round(GSpriteEngine.WorldY));
   LShowing := not vanishing;
   GSpriteEngine.transProc := doZoom;
   GSpriteEngine.renderProc := singleEraseRender;
end; {$ENDREGION}

{BOF2}
{$REGION BOF2}
procedure doBof2(var location: integer);
var
   i, j: smallint;
   workload: word;
   endpoint: word;
   color: TSDL_Color;
   width: smallint;
begin
   if location = 0 then
      if LShowing then
         initReveal
      else initErase;
   width := GSpriteEngine.Canvas.Width;
   endpoint := (width * 4) + GSpriteEngine.Canvas.Height;
   workload := endpoint div (FADETIME[2] div TRpgTimestamp.FrameLength);
   inc(location, workload);
   j := location div 4;
   if LShowing then
      color := SDL_WHITE
   else color := SDL_BLACK;
   i := 0;
   repeat
      if j <= width then
         if i mod 2 = 0 then
            GSpriteEngine.Canvas.FillRect(rect(0, i, j, 1), color)
         else GSpriteEngine.Canvas.FillRect(rect(width - j, i, j, 1), color);
      if (i mod 4 = 0) and (i > 0) then
         dec(j);
      inc(i);
   until (i > GSpriteEngine.Canvas.Height) or (j <= 0);
   if location > endpoint then
      if LShowing then
         GSpriteEngine.endShow
      else GSpriteEngine.endErase;
end;

procedure bof2(vanishing: boolean);
begin
   LShowing := not vanishing;
   GSpriteEngine.transProc := doBof2;
   if LShowing then
      GSpriteEngine.renderProc := simpleMaskRender
   else GSpriteEngine.renderProc := singleEraseRender;
end;
{$ENDREGION}

{WAVE}
{$REGION WAVE}
procedure doWave(var location: integer);
var
   workload: word;
const
   WAVE_PERIOD = 5;
begin
   initReveal;
   workload := WAVESIZE div (FADETIME[2] div 32);
   inc(location, workload);
   if LShowing then
      drawWave(GRenderTargets[2], rect(0, 0, GSpriteEngine.Canvas.Width, GSpriteEngine.Canvas.Height), WAVESIZE - location, WAVE_PERIOD, WAVESIZE - location)
   else drawWave(GRenderTargets[2], rect(0, 0, GSpriteEngine.Canvas.Width, GSpriteEngine.Canvas.Height), location, WAVE_PERIOD, location);
   if location >= WAVESIZE then
   if LShowing then
      GSpriteEngine.endShow
   else GSpriteEngine.endErase;
end;

procedure wave(vanishing: boolean);
begin
   LShowing := not vanishing;
   GSpriteEngine.transProc := doWave;
   GSpriteEngine.renderProc := singleEraseRender;
end;
{$ENDREGION}

initialization
   GCurrentTarget := -1;
   GRenderTargets := TSdlRenderTargets.Create;
finalization
   GRenderTargets.Free;
end.
