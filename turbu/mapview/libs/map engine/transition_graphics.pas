unit transition_graphics;
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
   charset_data, chipset_graphics,
   {AsphyreTextures, AsphyreDevices;}
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
   GCurrentTarget: shortint;

implementation
uses
   windows, types, graphics, math,
   commons, transitions, distortions,
   SDL,
   {AsphyreDef} SG_defs;

const
   FADETIME: array[1..2] of integer = (1200, 1800);
var
   FTimer: word;
   FVertical: boolean;
   FShowing: boolean;
   FDirection: TFacing;
   FCenter: TPoint;
   FCurrentX, FCurrentY: single;

procedure initReveal; inline;
begin
//fixme
//   GGameEngine.Canvas.fillRect(rect(0, 0, GGameEngine.canvas.device.width, GGameEngine.canvas.device.height), clBlack4, fxNone);
   FTimer := 0;
end;

procedure initErase; inline;
begin
//fixme
//   GGameEngine.Canvas.TexMap(GRenderTargets[2], pbounds4(0, 0, GGameEngine.canvas.device.width, GGameEngine.canvas.device.height), clWhite4, tcNull, fxNone);
   FTimer := 0;
end;

procedure singleEraseRender;
begin
//fixme
{   with GGameEngine do
      Canvas.TexMap(GRenderTargets[GCurrentTarget], pBounds4(0, 0, canvas.Device.Width, canvas.Device.Height), clWhite4, tcNull, fxNone)}
   //end WITH
end;

procedure flipRender;
begin
   singleEraseRender;
   if GCurrentTarget > 1 then
      GCurrentTarget := 1
   else GCurrentTarget := GCurrentTarget xor 1;
end;

procedure simpleMaskRender;
begin
//fixme
{   with GGameEngine do
   begin
      Canvas.TexMap(GRenderTargets[2], pBounds4(0, 0, canvas.Device.Width, canvas.Device.Height), clWhite4, tcNull, fxNone);
      Canvas.TexMap(GRenderTargets[GCurrentTarget], pBounds4(0, 0, canvas.Device.Width, canvas.Device.Height), clWhite4, tcNull, fxBlend);
   end;}
end;

procedure fadeOut;
begin
   GGameEngine.fadeOut(FADETIME[1]);
end;

procedure fadeIn;
begin
   GGameEngine.fadeOut(GFrameLength);
   GGameEngine.blank := false;
   repeat
      sleep(GFrameLength);
   until GGameEngine.blank;
   GGameEngine.fadeIn(FADETIME[1]);
   GGameEngine.blank := false;
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
   //end if
   workload := high(GBlockArray) div (FADETIME[1] div GFrameLength);
   width := GGameEngine.Canvas.Width div BLOCKSIZE;
   for i := location to min(location + workload, high(GBlockArray)) do
   begin
      corner := point((GBlockArray[i] mod width) * BLOCKSIZE, (GBlockArray[i] div width) * BLOCKSIZE);
//fixme
//      GGameEngine.Canvas.FillRect(corner.X, corner.Y, BLOCKSIZE, BLOCKSIZE, clBlack, fxNone);
      inc(location);
   end;
   if location >= high(GBlockArray) then
      GGameEngine.endErase;
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
   workload := high(GBlockArray) div (FADETIME[1] div GFrameLength);
   width := GGameEngine.Canvas.Width div BLOCKSIZE;
   for i := location to min(location + workload, high(GBlockArray)) do
   begin
      corner := point((GBlockArray[i] mod width) * BLOCKSIZE, (GBlockArray[i] div width) * BLOCKSIZE);
//fixme
//      GGameEngine.Canvas.FillRect(corner.X, corner.Y, BLOCKSIZE, BLOCKSIZE, clWhite, fxNone);
      inc(location);
   end;
   if location >= high(GBlockArray) then
      GGameEngine.endShow;
end;

procedure blocks(vanishing: boolean);
begin
   case vanishing of
      true:
      begin
         GGameEngine.transProc := blockErase;
         GGameEngine.renderProc := singleEraseRender;
      end;
      false:
      begin
         GGameEngine.transProc := blockReveal;
         GGameEngine.renderProc := simpleMaskRender;
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
      FTimer := interval - GFrameLength;
   end;
   inc(FTimer, GFrameLength);
   if FTimer >= interval then
   begin
      width := GGameEngine.Canvas.Width;
      i := location;
      repeat
//fixme
//         GGameEngine.Canvas.FillRect(0, i, width, 1, clBlack, fxNone);
         inc(i, BLINDSIZE);
      until i >= GGameEngine.Canvas.Height;
      inc(location);
      dec(FTimer, interval);
   end;
   if location > BLINDSIZE then
      GGameEngine.endErase;
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
      FTimer := interval - GFrameLength;
   end;
   inc(FTimer, GFrameLength);
   if FTimer >= interval then
   begin
      width := GGameEngine.Canvas.Width;
      i := location;
      repeat
//fixme
//         GGameEngine.Canvas.FillRect(GCurrentTarget, i, width, 1, clWhite, fxNone);
         inc(i, BLINDSIZE);
      until i >= GGameEngine.Canvas.Height;
      inc(location);
      dec(FTimer, interval);
   end;
   if location > BLINDSIZE then
      GGameEngine.endShow;
end;

procedure blinds(vanishing: boolean);
begin
   case vanishing of
      true:
      begin
         GGameEngine.transProc := blindErase;
         GGameEngine.renderProc := singleEraseRender;
      end;
      false:
      begin
         GGameEngine.transProc := blindReveal;
         GGameEngine.renderProc := simpleMaskRender;
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
   color: cardinal;
begin
   if location = 0 then
      if FShowing then
         initReveal
      else initErase;
   if FShowing then
      color := clWhite
   else color := clBlack;
   workload := high(GStripeArray) div (FADETIME[1] div GFrameLength);
   for i := location to min(location + workload, high(GStripeArray)) do
   begin
      if FVertical then
      begin
         corner := point(GStripeArray[i] * STRIPESIZE, 0);
//fixme
//         GGameEngine.Canvas.FillRect(corner.X, corner.Y, STRIPESIZE, GGameEngine.Canvas.Device.Height, color, fxNone);
      end else begin
         corner := point(0, GStripeArray[i] * STRIPESIZE);
//fixme
//         GGameEngine.Canvas.FillRect(corner.X, corner.Y, GGameEngine.Canvas.Device.Width, STRIPESIZE, color, fxNone);
      end;
      inc(location);
   end;
   if location >= high(GStripeArray) then
      if FShowing then
         GGameEngine.endShow
      else GGameEngine.endErase;
end;

procedure stripes(vanishing, vertical: boolean);
begin
   FVertical := vertical;
   FShowing := not vanishing;
   GGameEngine.transProc := doStripes;
   case vanishing of
      true: GGameEngine.renderProc := singleEraseRender;
      false: GGameEngine.renderProc := simpleMaskRender;
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
   color: cardinal;
begin
   if FShowing then
   begin
      initErase;
      color := clBlack;
   end
   else begin
      initReveal;
      color := clWhite;
   end;
   workload := (GGameEngine.Canvas.Height div 2) div max((FADETIME[1] div GFrameLength), 1);
   ratio := GGameEngine.Canvas.Width / GGameEngine.Canvas.Height;
   i := min(location + workload, GGameEngine.Canvas.Height div 2);
   mask.TopLeft := point(round(i * ratio), i);
   mask.Right := GGameEngine.Canvas.Width - mask.Left;
   mask.Bottom := GGameEngine.Canvas.Height - mask.Top;
//fixme
//   GGameEngine.Canvas.FillRect(mask, color, fxNone);
   inc(location, workload);
   if location >= GGameEngine.Canvas.Height div 2 then
      if FShowing then
         GGameEngine.endShow
      else GGameEngine.endErase;
   //end if
end;

procedure outIn(vanishing: boolean);
begin
   FShowing := not vanishing;
   GGameEngine.transProc := doOutIn;
   case vanishing of
      true: GGameEngine.renderProc := simpleMaskRender;
      false: GGameEngine.renderProc := singleEraseRender;
   end;
end;

procedure doInOut(var location: integer);
var
   i: integer;
   workload: word;
   ratio: single;
   mask: TRect;
   center: TPoint;
   color: cardinal;
begin
   if FShowing then
   begin
      initReveal;
      color := clWhite;
   end
   else begin
      initErase;
      color := clBlack;
   end;
   center := point(GGameEngine.Canvas.Width div 2, GGameEngine.Canvas.Height div 2);
   workload := (GGameEngine.Canvas.Height div 2) div max((FADETIME[1] div GFrameLength), 1);
   ratio := GGameEngine.Canvas.Width / GGameEngine.Canvas.Height;
   i := min(location + workload, GGameEngine.Canvas.Height div 2);
   mask.TopLeft := point(center.x - round(i * ratio), center.y - i);
   mask.bottomRight := point(center.x + round(i * ratio), center.y + i);
//fixme
//   GGameEngine.Canvas.FillRect(mask, color, fxNone);
   inc(location, workload);
   if location >= GGameEngine.Canvas.Height div 2 then
      if FShowing then
         GGameEngine.endShow
      else GGameEngine.endErase;
   //end if
end;

procedure inOut(vanishing: boolean);
begin
   FShowing := not vanishing;
   GGameEngine.transProc := doInOut;
   case vanishing of
      true: GGameEngine.renderProc := singleEraseRender;
      false: GGameEngine.renderProc := simpleMaskRender;
   end;
end;{$ENDREGION}

{SCROLL}
{$REGION SCROLL}
{$WARN USE_BEFORE_DEF OFF}
procedure doScroll(var location: integer);
var
   workload, boundary: word;
   i: integer;
begin
   initReveal;
   case FDirection of
      facing_up, facing_down:
      begin
         workload := GGameEngine.Canvas.Height div (FADETIME[1] div GFrameLength);
         boundary := GGameEngine.Canvas.Height;
      end;
      facing_right, facing_left:
      begin
         workload := GGameEngine.Canvas.Width div (FADETIME[1] div GFrameLength);
         boundary := GGameEngine.Canvas.Width;
      end;
   end;
   i := location + workload;
   if FDirection in [facing_up, facing_left] then
      i := i * -1;
   if FShowing then
      with GGameEngine.Canvas do
         case FDirection of
            facing_up: inc(i, Height);
            facing_left: inc(i, Width);
            facing_down: dec(i, Height);
            facing_right: dec(i, Width);
         end;
      //end with
   //end if
//fixme
{   if FDirection in [facing_up, facing_down] then
      GGameEngine.Canvas.TexMap(GRenderTargets[2], pbounds4(0, i, GGameEngine.canvas.device.width, GGameEngine.canvas.device.height), clWhite4, tcNull, fxNone)
   else
      GGameEngine.Canvas.TexMap(GRenderTargets[2], pbounds4(i, 0, GGameEngine.canvas.device.width, GGameEngine.canvas.device.height), clWhite4, tcNull, fxNone);}
   inc(location, workload);
   if location >= boundary then
      if FShowing then
         GGameEngine.endShow
      else GGameEngine.endErase;
   //end if
end;
{$WARN USE_BEFORE_DEF ON}

procedure scroll(vanishing: boolean; direction: TFacing);
begin
   FShowing := not vanishing;
   FDirection := direction;
   GGameEngine.transProc := doScroll;
   GGameEngine.renderProc := singleEraseRender;
end;{$ENDREGION}

{DIVIDE}
{$REGION DIVIDE}
procedure divideVert(var location: integer);
var
   workload: byte;
   boundary: word;
   alternate: byte;
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
   boundary := GGameEngine.Canvas.Height div 2;
   workload := boundary div (FADETIME[1] div GFrameLength);
//fixme
{   with GGameEngine.Canvas do
   begin
      DrawPortion(GRenderTargets[alternate], 0, -workload, 0, 0, Device.Width, boundary, clWhite4, fxNone);
      DrawPortion(GRenderTargets[alternate], 0, boundary + workload, 0, boundary, Device.Width, Device.Height, clWhite4, fxNone);
   end;}
   inc(location, workload);
   if location >= boundary then
      GGameEngine.endErase;
   //end if
end;

procedure divideHoriz(var location: integer);
var
   workload: byte;
   boundary: word;
   alternate: byte;
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
   boundary := GGameEngine.Canvas.Width div 2;
   workload := boundary div (FADETIME[1] div GFrameLength);
//fixme
{   with GGameEngine.Canvas do
   begin
      DrawPortion(GRenderTargets[alternate], -workload, 0, 0, 0, boundary, Device.Height, clWhite4, fxNone);
      DrawPortion(GRenderTargets[alternate], boundary + workload, 0, boundary, 0, Device.Width, Device.Height, clWhite4, fxNone);
   end;}
   inc(location, workload);
   if location >= boundary then
      GGameEngine.endErase;
   //end if
end;

procedure divideQuarters(var location: integer);
var
   workloadH, workloadV: byte;
   boundaryH, boundaryV: word;
   ratio: single;
   alternate: byte;
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
   ratio := GGameEngine.Canvas.Width / GGameEngine.Canvas.Height;
   boundaryH := GGameEngine.Canvas.Width div 2;
   workloadH := boundaryH div (FADETIME[1] div GFrameLength);
   boundaryV := round(boundaryH / ratio);
   workloadV := round(workloadH / ratio);
//fixme
{   with GGameEngine.Canvas do
   begin
      DrawPortion(GRenderTargets[alternate], -workloadH, -workloadV, 0, 0, boundaryH, boundaryV, clWhite4, fxNone);
      DrawPortion(GRenderTargets[alternate], -workloadH, boundaryV + workloadV, 0, boundaryV, boundaryH, Device.Height, clWhite4, fxNone);
      DrawPortion(GRenderTargets[alternate], boundaryH + workloadH, -workloadv, boundaryH, 0, Device.Width, boundaryV, clWhite4, fxNone);
      DrawPortion(GRenderTargets[alternate], boundaryH + workloadH, boundaryV + workloadV, boundaryH, boundaryV, Device.Width, Device.Height, clWhite4, fxNone);
   end;}
   inc(location, workloadH);
   if location >= boundaryH then
      GGameEngine.endErase;
   //end if
end;

procedure divide(style: TDivideStyle);
begin
   case style of
      ds_vert: GGameEngine.transProc := divideVert;
      ds_horiz: GGameEngine.transProc := divideHoriz;
      ds_both: GGameEngine.transProc := divideQuarters;
   end;
   GGameEngine.renderProc := flipRender;
end; {$ENDREGION}

{COMBINE}
{$REGION COMBINE}
procedure combineVert(var location: integer);
var
   workload: byte;
   boundary: word;
begin
   if location = 0 then
      initReveal;
   boundary := GGameEngine.Canvas.Height div 2;
   workload := boundary div (FADETIME[1] div GFrameLength);
   inc(location, workload);
//fixme
{   with GGameEngine.Canvas do
   begin
      DrawPortion(GRenderTargets[2], 0, (boundary - location) * -1, 0, 0, Device.Width, boundary, clWhite4, fxNone);
      DrawPortion(GRenderTargets[2], 0, Device.Height - location, 0, boundary, Device.Width, Device.Height, clWhite4, fxNone);
   end;}
   if location >= boundary then
      GGameEngine.endShow
end;

procedure combineHoriz(var location: integer);
var
   workload: byte;
   boundary: word;
begin
   if location = 0 then
      initReveal;
   boundary := GGameEngine.Canvas.Width div 2;
   workload := boundary div (FADETIME[1] div GFrameLength);
   inc(location, workload);
//fixme
{   with GGameEngine.Canvas do
   begin
      DrawPortion(GRenderTargets[2], (boundary - location) * -1, 0, 0, 0, boundary, Device.Height, clWhite4, fxNone);
      DrawPortion(GRenderTargets[2], Device.Width - location, 0, boundary, 0, Device.Width, Device.Height, clWhite4, fxNone);
   end;}
   if location >= boundary then
      GGameEngine.endShow
   //end if
end;

procedure combineQuarters(var location: integer);
var
   workload: byte;
   boundaryH, boundaryV: word;
   locationV: integer;
   ratio: single;
begin
   if location = 0 then
      initReveal;
   ratio := GGameEngine.Canvas.Width / GGameEngine.Canvas.Height;
   boundaryH := GGameEngine.Canvas.Width div 2;
   workload := boundaryH div (FADETIME[1] div GFrameLength);
   boundaryV := round(boundaryH / ratio);
   locationV := round(location / ratio);
//fixme
{   with GGameEngine.Canvas do
   begin
      DrawPortion(GRenderTargets[2], (boundaryH - location) * -1, (boundaryV - locationV) * -1, 0, 0, boundaryH, boundaryV, clWhite4, fxNone);
      DrawPortion(GRenderTargets[2], (boundaryH - location) * -1, Device.Height - locationV, 0, boundaryV, boundaryH, Device.Height, clWhite4, fxNone);
      DrawPortion(GRenderTargets[2], Device.Width - location, (boundaryV - locationV) * -1, boundaryH, 0, Device.Width, boundaryV, clWhite4, fxNone);
      DrawPortion(GRenderTargets[2], Device.Width - location, Device.Height - locationV, boundaryH, boundaryV, Device.Width, Device.Height, clWhite4, fxNone);
   end;}
   inc(location, workload);
   if location >= boundaryH then
      GGameEngine.endShow
   //end if
end;

procedure combine(style: TDivideStyle);
begin
   case style of
      ds_vert: GGameEngine.transProc := combineVert;
      ds_horiz: GGameEngine.transProc := combineHoriz;
      ds_both: GGameEngine.transProc := combineQuarters;
   end;
   GGameEngine.renderProc := singleEraseRender;
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
   minimum := round(GGameEngine.Canvas.Width / MAXZOOM);
   ratio := GGameEngine.Canvas.Width / GGameEngine.Canvas.Height;
   if (location = 0) then
      if FShowing then
      begin
         location := minimum;
         FCurrentX := FCenter.X - (minimum / 2);
         FCurrentY := FCenter.Y - ((minimum / ratio) / 2);
      end else
      begin
         FCurrentX := 0;
         FCurrentY := 0;
         location := GGameEngine.Canvas.Width;
      end;
   workload := GGameEngine.Canvas.Width div (FADETIME[1] div GFrameLength);
   if FShowing then
      location := min(location + workload, GGameEngine.Canvas.Width)
   else location := max(location - workload, minimum);
   viewRect.BottomRight := point(location, round(location / ratio));
   if FShowing then
   begin
      FCurrentX := FCurrentX - (FCenter.x / GGameEngine.Canvas.Width) * workload;
      FCurrentY := FCurrentY - (FCenter.Y / GGameEngine.Canvas.Height) * (workload / ratio);
   end else begin
      FCurrentX := FCurrentX + (FCenter.x / GGameEngine.Canvas.Width) * workload;
      FCurrentY := FCurrentY + (FCenter.Y / GGameEngine.Canvas.Height) * (workload / ratio);
   end;
   viewRect.Left := max(round(FCurrentX), 0);
   viewRect.Top := max(round(FCurrentY), 0);
//fixme
//   GGameEngine.Canvas.DrawRectStretch(GRenderTargets[2], 0, 0, GGameEngine.Canvas.Device.Width, GGameEngine.Canvas.Device.Height, viewRect, clWhite4, fxNone);
   if FShowing then
   begin
      if location >= GGameEngine.Canvas.Width then
         GGameEngine.endShow;
      //end if
   end
   else if location <= minimum then
      GGameEngine.endErase;
   //end if
end;

procedure zoom(vanishing: boolean);
begin
   with GGameEngine.currentParty.baseTile do
      FCenter := point(trunc(x) + 8, trunc(y) + 8);
   dec(FCenter.x, round(GGameEngine.WorldX));
   dec(FCenter.Y, round(GGameEngine.WorldY));
   FShowing := not vanishing;
   GGameEngine.transProc := doZoom;
   GGameEngine.renderProc := singleEraseRender;
end; {$ENDREGION}

{BOF2}
{$REGION BOF2}
procedure doBof2(var location: integer);
var
   i, j: smallint;
   workload: word;
   endpoint: word;
   color: cardinal;
   width: smallint;
begin
   if location = 0 then
      if FShowing then
         initReveal
      else initErase;
   width := GGameEngine.Canvas.Width;
   endpoint := (width * 4) + GGameEngine.Canvas.Height;
   workload := endpoint div (FADETIME[2] div GFrameLength);
   inc(location, workload);
   j := location div 4;
   if FShowing then
      color := clWhite
   else color := clBlack;
   i := 0;
//fixme
{   repeat
      if j <= width then
         if i mod 2 = 0 then
            GGameEngine.Canvas.FillRect(0, i, j, 1, color, fxNone)
         else GGameEngine.Canvas.FillRect(width - j, i, j, 1, color, fxNone);
      if (i mod 4 = 0) and (i > 0) then
         dec(j);
      inc(i);
   until (i > GGameEngine.Canvas.Height) or (j <= 0);}
   if location > endpoint then
      if FShowing then
         GGameEngine.endShow
      else GGameEngine.endErase;
   //end if
end;

procedure bof2(vanishing: boolean);
begin
   FShowing := not vanishing;
   GGameEngine.transProc := doBof2;
   if FShowing then
      GGameEngine.renderProc := simpleMaskRender
   else GGameEngine.renderProc := singleEraseRender;
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
   if FShowing then
      drawWave(GRenderTargets[2], rect(0, 0, GGameEngine.Canvas.Width, GGameEngine.Canvas.Height), WAVESIZE - location, WAVE_PERIOD, WAVESIZE - location, fxNone)
   else drawWave(GRenderTargets[2], rect(0, 0, GGameEngine.Canvas.Width, GGameEngine.Canvas.Height), location, WAVE_PERIOD, location, fxNone);
   if location >= WAVESIZE then
   if FShowing then
      GGameEngine.endShow
   else GGameEngine.endErase;
end;

procedure wave(vanishing: boolean);
begin
   FShowing := not vanishing;
   GGameEngine.transProc := doWave;
   GGameEngine.renderProc := singleEraseRender;
end;
{$ENDREGION}

initialization
begin
   GCurrentTarget := -1;
end;

end.
