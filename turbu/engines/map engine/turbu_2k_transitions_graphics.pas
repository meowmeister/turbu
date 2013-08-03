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
   turbu_defs, turbu_transition_interface,
   SDL_13, SDL_Canvas, SG_defs;

type
   TDivideStyle = (ds_vert, ds_horiz, ds_both);
   TDirectionWipe = (dw_random, dw_downward, dw_upward, dw_leftward, dw_rightward);

   TTransition = class(TInterfacedObject, ITransition)
   protected
      FCurrentTarget: integer;
      FFirstRenderDone: boolean;
      FOnFinished: TTransitionDoneEvent;
      FShowing: boolean;
      FProgress: integer;
      procedure Setup(showing: boolean; OnFinished: TTransitionDoneEvent); virtual;
      function InternalDraw: boolean; virtual; abstract;
      function Draw: boolean;
   end;

   TMaskTransition = class(TTransition)
   private
      procedure DrawMask(before, after, tran: TSdlRenderTarget);
   protected
      function DoDraw: boolean; virtual; abstract;
      function InternalDraw: boolean; override;
      procedure Setup(showing: boolean; OnFinished: TTransitionDoneEvent); override;
   end;

   TFadeTransition = class(TMaskTransition)
   protected
      function DoDraw: boolean; override;
   end;

   TBlockTransition = class(TMaskTransition)
   private
      FBlockArray: array of integer;
      procedure shuffleBlockArray(direction: TDirectionWipe);
      procedure shuffleBlocksRandomly;
      procedure shuffleBlocksDownward;
      procedure shuffleBlocksUpward;
   protected
      function DoDraw: boolean; override;
   public
      constructor Create(direction: TDirectionWipe);
   end;

   TBlindsTransition = class(TMaskTransition)
   private
      FTimer: integer;
      FInterval: integer;
   protected
      function DoDraw: boolean; override;
      procedure Setup(showing: boolean; OnFinished: TTransitionDoneEvent); override;
   public
      constructor Create;
   end;

   TStripeTransition = class(TMaskTransition)
   private
      FVertical: boolean;
      FStripeArray: array of integer;
      procedure setupStripeArray(vertical: boolean);
   protected
      function DoDraw: boolean; override;
   public
      constructor Create(vertical: boolean);
   end;

   TRectIrisTransition = class(TMaskTransition)
   private
      FCenter: TsgPoint;
      FColor, FEraseColor: SDL_Color;
      FInOut: boolean;
   protected
      function DoDraw: boolean; override;
   public
      constructor Create(inOut: boolean);
   end;

   TBof2Transition = class(TMaskTransition)
   protected
      function DoDraw: boolean; override;
   end;

   TScrollTransition = class(TTransition)
   private
      FDirection: TFacing;
      FBoundary: integer;
   protected
      function InternalDraw: boolean; override;
   public
      constructor Create(direction: TFacing);
   end;

   TDivideTransition = class(TTransition)
   private
      FStyle: TDivideStyle;
      procedure DivideVert(workload, boundary: integer);
      procedure DivideHoriz(workload, boundary: integer);
      procedure DivideQuarters(workloadH, boundaryH, workloadV, boundaryV: integer);
   protected
      function InternalDraw: boolean; override;
   public
      constructor Create(style: TDivideStyle);
   end;

   TCombineTransition = class(TTransition)
   private
      FStyle: TDivideStyle;
      procedure CombineVert(workload, boundary: integer);
      procedure CombineHoriz(workload, boundary: integer);
      procedure CombineQuarters(workloadH, boundaryH, workloadV, boundaryV: integer);
   protected
      function InternalDraw: boolean; override;
   public
      constructor Create(style: TDivideStyle);
   end;

   TZoomTransition = class(TTransition)
   private
      FZoomIn: boolean;
      FMinimum: integer;
      FRatio: single;
      FCenter: TSgPoint;
      FCurrentX: single;
      FCurrentY: single;
      FTarget: integer;
   protected
      function InternalDraw: boolean; override;
      procedure Setup(showing: boolean; OnFinished: TTransitionDoneEvent); override;
   public
      constructor Create(zoomIn: boolean);
   end;

   TMosaicTransition = class(TTransition)
   const MIN_RESOLUTION = 8;
   private
      FBlockSize: single;
      FMaxSize: single;
      FTarget: integer;
   protected
      function InternalDraw: boolean; override;
      procedure Setup(showing: boolean; OnFinished: TTransitionDoneEvent); override;
   end;

   TWaveTransition = class(TTransition)
   protected
      function InternalDraw: boolean; override;
   end;

   function fadeOut: ITransition;
   function fadeIn: ITransition;
   function blocks(direction: TDirectionWipe): ITransition;
   function blinds(vanishing: boolean): ITransition;
   function stripes(vanishing, vertical: boolean): ITransition;
   function outIn(vanishing: boolean): ITransition;
   function inOut(vanishing: boolean): ITransition;
   function scroll(vanishing: boolean; direction: TFacing): ITransition;
   function divide(style: TDivideStyle): ITransition;
   function combine(style: TDivideStyle): ITransition;
   function zoom(vanishing: boolean): ITransition;
   function mosaic(vanishing: boolean): ITransition;
   function bof2(vanishing: boolean): ITransition;
   function wave(vanishing: boolean): ITransition;

var
   GRenderTargets: TSdlRenderTargets;

const
   RENDERER_MAIN = 0;
   RENDERER_ALT = 1;
   RENDERER_MAP = 2;
   RENDERER_TRAN = 3;

implementation
uses
   types, math, SysUtils, OpenGL,
   commons, turbu_2k_transitions, turbu_2k_sprite_engine, timing, turbu_2k_distortions,
   turbu_OpenGL, dm_shaders, turbu_2k_environment, turbu_2k_map_engine,
   sg_utils;

const
   FADETIME: array[0..2] of integer = (800, 1200, 5200);
procedure initReveal;
begin
   GSpriteEngine.Canvas.Clear(SDL_BLACK);
end;

procedure initErase;
begin
   GRenderTargets[RENDERER_ALT].DrawFull;
end;

{ TTransition }

function TTransition.Draw: boolean;
begin
   result := InternalDraw;
   if not result then
      FOnFinished;
end;

procedure TTransition.Setup(showing: boolean; OnFinished: TTransitionDoneEvent);
begin
   FShowing := showing;
   FOnFinished := OnFinished;
end;

{ TMaskTransition }

procedure TMaskTransition.DrawMask(before, after, tran: TSdlRenderTarget);
var
   shaders: TdmShaders;
   shaderProgram: integer;
begin
   glEnable(GL_MULTISAMPLE);
   glActiveTextureARB(GL_TEXTURE1);
   glEnable(GL_TEXTURE_RECTANGLE_ARB);
   after.handle.bind;
   glActiveTextureARB(GL_TEXTURE2);
   glEnable(GL_TEXTURE_RECTANGLE_ARB);
   tran.handle.bind;
   glActiveTextureARB(GL_TEXTURE0);
   before.handle.bind;
   shaders := GSpriteEngine.ShaderEngine;
   shaderProgram := shaders.ShaderProgram('default', 'maskF');
   shaders.UseShaderProgram(shaderProgram);
   shaders.SetUniformValue(shaderProgram, 'before', 0);
   shaders.SetUniformValue(shaderProgram, 'after', 1);
   shaders.SetUniformValue(shaderProgram, 'mask', 2);
   before.DrawFull;
   glDisable(GL_MULTISAMPLE);
end;

procedure TMaskTransition.Setup(showing: boolean;
  OnFinished: TTransitionDoneEvent);
var
   tranTarget: TSdlRenderTarget;
begin
   inherited Setup(Showing, OnFinished);
   runThreadsafe(
      procedure
      begin
         GSpriteEngine.Canvas.pushRenderTarget;
         tranTarget := GRenderTargets[RENDERER_TRAN];
         tranTarget.SetRenderer;
         tranTarget.parent.Clear(SDL_BLACK);
         tranTarget.parent.popRenderTarget;
      end, true);
end;

function TMaskTransition.InternalDraw: boolean;
var
   tranTarget: TSdlRenderTarget;
   prog: GLint;
begin
   GSpriteEngine.Canvas.pushRenderTarget;
   tranTarget := GRenderTargets[RENDERER_TRAN];
   tranTarget.SetRenderer;
   result := DoDraw;
   tranTarget.parent.popRenderTarget;
   glGetIntegerv(GL_CURRENT_PROGRAM, @prog);
   DrawMask(GRenderTargets[RENDERER_MAIN], GRenderTargets[RENDERER_ALT], tranTarget);
   glUseProgram(prog);
end;

{ TFadeTransition }

function TFadeTransition.DoDraw: boolean;
var
   workload: integer;
   fadeColor: SDL_Color;
begin
   workload := max(255 div (FADETIME[0] div TRpgTimestamp.FrameLength), 1);
   inc(FProgress, workload * 2);
   FProgress := min(FProgress, 255);
   fadeColor.r := FProgress;
   fadeColor.g := FProgress;
   fadeColor.b := FProgress;
   GSpriteEngine.Canvas.Clear(fadeColor);
   result := FProgress < 255;
end;

function fadeOut: ITransition;
begin
   result := TFadeTransition.Create;
end;

function fadeIn: ITransition;
begin
   result := TFadeTransition.Create;
end;

{BLOCKS}
{$REGION BLOCKS}

{ TBlockTransition }

const
   BLOCKSIZE = 4;

constructor TBlockTransition.Create(direction: TDirectionWipe);
var
   w, h, i: integer;
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   w := canvas.Width div BLOCKSIZE;
   h := canvas.Height div BLOCKSIZE;
   if canvas.Width mod BLOCKSIZE <> 0 then
      inc(w);
   if canvas.Height mod BLOCKSIZE <> 0 then
      inc(h);
   setLength(FBlockArray, w * h);
   for I := low(FBlockArray) to high(FBlockArray) do
      FBlockArray[i] := i;
   ShuffleBlockArray(direction);
end;

procedure TBlockTransition.shuffleBlocksRandomly;
var
   i: integer;
begin
   for I := low(FBlockArray) to high(FBlockArray) do
      swap(FBlockArray[i], FBlockArray[Random(length(FBlockArray))]);
end;

procedure TBlockTransition.shuffleBlocksDownward;
const
   VERTICAL_RANGE = 12;
   HALFRANGE = VERTICAL_RANGE div 2;
var
   width: integer;
   freeFloor, freeCeiling: integer;
   rangeFloor, rangeCeiling: integer;
   i: integer;
begin
   width := GSpriteEngine.Canvas.Width div BLOCKSIZE;
   freeFloor := width * HALFRANGE;
   freeCeiling := high(FBlockArray) - (freeFloor);
   rangeFloor := width * VERTICAL_RANGE;
   rangeCeiling := high(FBlockArray) - rangeFloor;
   for i := 0 to freeFloor do
      swap(FBlockArray[i], FBlockArray[GEnvironment.random(0, rangeFloor)]);
   for I := freeFloor + 1 to freeCeiling do
      swap(FBlockArray[i], FBlockArray[GEnvironment.random(i - (freeFloor), i + (freeFloor))]);
   for i := freeCeiling + 1 to high(FBlockArray) do
      swap(FBlockArray[i], FBlockArray[GEnvironment.random(rangeCeiling, high(FBlockArray))]);
end;

procedure TBlockTransition.shuffleBlocksUpward;
var i: integer;
begin
   shuffleBlocksDownward;
   for I := 0 to high(FBlockArray) div 2 do
      swap(FBlockArray[i], FBlockArray[high(FBlockArray) - i]);
end;

procedure TBlockTransition.shuffleBlockArray(direction: TDirectionWipe);
begin
   case direction of
      dw_random: shuffleBlocksRandomly;
      dw_downward: shuffleBlocksDownward;
      dw_upward: shuffleBlocksUpward;
      else raise Exception.Create('Shuffle style not implemented');
   end;
end;

function TBlockTransition.DoDraw: boolean;
var
   i, workload, width: integer;
   corner: TPoint;
begin
   workload := max(high(FBlockArray) div (FADETIME[1] div TRpgTimestamp.FrameLength), 1);
   width := GSpriteEngine.Canvas.Width div BLOCKSIZE;
   for i := FProgress to min(FProgress + workload, high(FBlockArray)) do
   begin
      corner := point((FBlockArray[i] mod width) * BLOCKSIZE, (FBlockArray[i] div width) * BLOCKSIZE);
      GSpriteEngine.Canvas.FillRect(rect(corner.X, corner.Y, BLOCKSIZE, BLOCKSIZE), SDL_WHITE);
      inc(FProgress);
   end;
   result := FProgress < high(FBlockArray);
end;

function blocks(direction: TDirectionWipe): ITransition;
begin
   result := TBlockTransition.Create(direction);
end; {$ENDREGION}

{BLINDS}
{$REGION BLINDS}
const
   BLINDSIZE = 8;

{ TBlindsTransition }

constructor TBlindsTransition.Create;
begin
   inherited Create;
   FInterval := FADETIME[0] div BLINDSIZE;
   FTimer := FInterval - TRpgTimestamp.FrameLength;
end;

function TBlindsTransition.DoDraw: boolean;
var
   i, width: integer;
begin
   inc(FTimer, TRpgTimestamp.FrameLength);
   if FTimer >= FInterval then
   begin
      width := GSpriteEngine.Canvas.Width;
      i := FProgress;
      repeat
         GSpriteEngine.Canvas.FillRect(rect(0, i, width, 1), SDL_WHITE);
         inc(i, BLINDSIZE);
      until i >= GSpriteEngine.Canvas.Height;
      if FShowing then
         dec(FProgress)
      else inc(FProgress);
      dec(FTimer, FInterval);
   end;
   if FShowing then
      result := FProgress >= 0
   else result := FProgress < BLINDSIZE;
end;

procedure TBlindsTransition.Setup(showing: boolean; OnFinished: TTransitionDoneEvent);
begin
   inherited Setup(showing, OnFinished);
   if FShowing then
      FProgress := BLINDSIZE;
end;

function blinds(vanishing: boolean): ITransition;
begin
   result := TBlindsTransition.Create;
end; {$ENDREGION}

{STRIPES}
{$REGION STRIPES}
{ TStripeTransition }

constructor TStripeTransition.Create(vertical: boolean);
begin
   inherited Create;
   FVertical := vertical;
   setupStripeArray(vertical);
end;

procedure TStripeTransition.setupStripeArray(vertical: boolean);
var
   i, j: integer;
begin
   if vertical then
      setLength(FStripeArray, GSpriteEngine.Canvas.height div STRIPESIZE)
   else
      setLength(FStripeArray, GSpriteEngine.Canvas.width div STRIPESIZE);
   i := 0;
   j := high(FStripeArray);
   if j mod 2 = 0 then
      dec(j);
   repeat
      FStripeArray[i] := i;
      if j >= 0 then
         FStripeArray[i + 1] := j;
      inc(i, 2);
      dec(j, 2);
   until (i > high(FStripeArray)) or (j < 0);
end;

function TStripeTransition.DoDraw: boolean;
var
   i, workload: integer;
   corner: TSgPoint;
begin
   workload := high(FStripeArray) div (FADETIME[1] div TRpgTimestamp.FrameLength);
   for i := FProgress to min(FProgress + workload, high(FStripeArray)) do
   begin
      if FVertical then
      begin
         corner := sgPoint(FStripeArray[i] * STRIPESIZE, 0);
         GSpriteEngine.Canvas.FillRect(rect(corner.X, corner.Y, STRIPESIZE, GSpriteEngine.Canvas.Height), SDL_WHITE);
      end else begin
         corner := sgPoint(0, FStripeArray[i] * STRIPESIZE);
         GSpriteEngine.Canvas.FillRect(rect(corner.X, corner.Y, GSpriteEngine.Canvas.Width, STRIPESIZE), SDL_WHITE);
      end;
      inc(FProgress);
   end;
   result := FProgress < high(FStripeArray);
end;

function stripes(vanishing, vertical: boolean): ITransition;
begin
   result := TStripeTransition.Create(vertical);
end; {$ENDREGION}

{IN/OUT}
{$REGION IN/OUT}

{ TRectIrisTransition }

constructor TRectIrisTransition.Create(inOut: boolean);
begin
   inherited Create;
   FCenter := point(GSpriteEngine.Canvas.Width div 2, GSpriteEngine.Canvas.Height div 2);
   FInOut := inOut;
   if FInOut then
   begin
      FEraseColor := SDL_BLACK;
      FColor := SDL_WHITE;
   end
   else begin
      FEraseColor := SDL_WHITE;
      FColor := SDL_BLACK;
   end;
end;

function TRectIrisTransition.DoDraw: boolean;
var
   i, workload: integer;
   ratio: single;
   mask: TRect;
begin
   GSpriteEngine.Canvas.Clear(FEraseColor);
   workload := (GSpriteEngine.Canvas.Height div 2) div max((FADETIME[0] div TRpgTimestamp.FrameLength), 1);
   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   i := min(FProgress + workload, GSpriteEngine.Canvas.Height div 2);
   if FInOut then
   begin
      mask.TopLeft := point(FCenter.x - round(i * ratio), FCenter.y - i);
      mask.bottomRight := point(FCenter.x + round(i * ratio), FCenter.y + i);
   end
   else begin
      mask.TopLeft := point(round(i * ratio), i);
      mask.Right := GSpriteEngine.Canvas.Width - mask.Left;
      mask.Bottom := GSpriteEngine.Canvas.Height - mask.Top;
   end;
   GSpriteEngine.Canvas.FillRect(TRectToSdlRect(mask), FColor);
   inc(FProgress, workload);
   result := FProgress < GSpriteEngine.Canvas.Height div 2;
end;

function outIn(vanishing: boolean): ITransition;
begin
   result := TRectIrisTransition.Create(false);
end;

function inOut(vanishing: boolean): ITransition;
begin
   result := TRectIrisTransition.Create(true);
end;{$ENDREGION}

{SCROLL}
{$REGION SCROLL}

{ TScrollTransition }

constructor TScrollTransition.Create(direction: TFacing);
begin
   inherited Create;
   FDirection := direction;
   case FDirection of
      facing_up, facing_down: FBoundary := GSpriteEngine.Canvas.Height;
      facing_right, facing_left: FBoundary := GSpriteEngine.Canvas.Width;
      else raise Exception.Create('Invalid direction');
   end;
end;

function TScrollTransition.InternalDraw: boolean;
var
   workload, timeslice, i: integer;
   dst, dst2: TSgPoint;
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   canvas.Clear(SDL_BLACK);
   timeslice := max(FADETIME[0] div TRpgTimestamp.FrameLength, 1);
   case FDirection of
      facing_up, facing_down: workload := GSpriteEngine.Canvas.Height div timeslice;
      facing_right, facing_left: workload := GSpriteEngine.Canvas.Width div timeslice;
      else raise Exception.Create('Invalid direction');
   end;
   i := FProgress + workload;
   if FDirection in [facing_up, facing_left] then
      i := i * -1;
   if FDirection in [facing_up, facing_down] then
      dst := sgPoint(0, i)
   else dst := sgPoint(i, 0);
   case FDirection of
      facing_up: inc(i, canvas.Height);
      facing_left: inc(i, canvas.Width);
      facing_down: dec(i, canvas.Height);
      facing_right: dec(i, canvas.Width);
   end;
   if FDirection in [facing_up, facing_down] then
      dst2 := sgPoint(0, i)
   else dst2 := sgPoint(i, 0);
   canvas.Draw(GRenderTargets[RENDERER_MAIN], dst);
   canvas.Draw(GRenderTargets[RENDERER_ALT], dst2);
   inc(FProgress, workload);
   result := FProgress < FBoundary;
end;

function scroll(vanishing: boolean; direction: TFacing): ITransition;
begin
   result := TScrollTransition.Create(direction);
end;{$ENDREGION}

{DIVIDE}
{$REGION DIVIDE}
{ TDivideTransition }

constructor TDivideTransition.Create(style: TDivideStyle);
begin
   inherited Create;
   FStyle := style;
end;

procedure TDivideTransition.DivideVert(workload, boundary: integer);
var
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   Canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(0, -workload, canvas.Width, canvas.Height div 2),
                     rect(0, 0, canvas.Width, boundary));
   Canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(0, boundary + workload, canvas.Width, canvas.Height div 2),
                     rect(0, boundary, canvas.Width, canvas.Height));
end;

procedure TDivideTransition.DivideHoriz(workload, boundary: integer);
var
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   Canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(-workload, 0, canvas.Width div 2, canvas.Height),
                     rect(0, 0, boundary, canvas.Height));
   Canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(boundary + workload, 0, canvas.Width div 2, canvas.Height),
                     rect(boundary, 0, canvas.Width, canvas.Height));
end;

procedure TDivideTransition.DivideQuarters(workloadH, boundaryH, workloadV, boundaryV: integer);
var
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(-workloadH, -workloadV, canvas.Width div 2, canvas.Height div 2),
                     rect(0, 0, boundaryH, boundaryV));
   canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(-workloadH, boundaryV + workloadV, canvas.Width div 2, canvas.Height div 2),
                     rect(0, boundaryV, boundaryH, canvas.Height));
   canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(boundaryH + workloadH, -workloadv, canvas.Width div 2, canvas.Height div 2),
                     rect(boundaryH, 0, canvas.Width, boundaryV));
   canvas.DrawRectTo(GRenderTargets[RENDERER_MAIN],
                     rect(boundaryH + workloadH, boundaryV + workloadV, canvas.Width div 2, canvas.Height div 2),
                     rect(boundaryH, boundaryV, canvas.Width, canvas.Height));
end;

function TDivideTransition.InternalDraw: boolean;
var
   ratio: single;
   workloadH, boundaryH, boundaryV: integer;
begin
   GRenderTargets[RENDERER_ALT].DrawFull;

   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   boundaryH := GSpriteEngine.Canvas.Width div 2;
   workloadH := boundaryH div (FADETIME[0] div TRpgTimestamp.FrameLength);
   boundaryV := round(boundaryH / ratio);

   case FStyle of
      ds_vert: divideVert(FProgress, boundaryV);
      ds_horiz: divideHoriz(round(FProgress * ratio), boundaryH);
      ds_both: divideQuarters(round(FProgress * ratio), boundaryH, FProgress, boundaryV);
   end;
   inc(FProgress, workloadH);
   result := FProgress < boundaryH;
end;

function divide(style: TDivideStyle): ITransition;
begin
   result := TDivideTransition.Create(style);
end; {$ENDREGION}

{COMBINE}
{$REGION COMBINE}

{ TCombineTransition }

constructor TCombineTransition.Create(style: TDivideStyle);
begin
   inherited Create;
   FStyle := style;
end;

procedure TCombineTransition.CombineHoriz(workload, boundary: integer);
var
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect((boundary - workload) * -1, 0, canvas.Width div 2, canvas.Height),
                     rect(0, 0, boundary, canvas.Height));
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect(canvas.Width - workload, 0, canvas.Width div 2, canvas.Height),
                     rect(boundary, 0, canvas.Width, canvas.Height));
end;

procedure TCombineTransition.CombineVert(workload, boundary: integer);
var
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect(0, (boundary - workload) * -1, canvas.Width, canvas.Height div 2),
                     rect(0, 0, canvas.Width, boundary));
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect(0, canvas.Height - workload, canvas.Width, canvas.Height div 2),
                     rect(0, boundary, canvas.Width, canvas.Height));
end;

procedure TCombineTransition.CombineQuarters(workloadH, boundaryH, workloadV,
  boundaryV: integer);
var
   canvas: TSdlCanvas;
begin
   canvas := GSpriteEngine.Canvas;
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect((boundaryH - workloadH) * -1, (boundaryV - workloadV) * -1, canvas.Width div 2, canvas.Height div 2),
                     rect(0, 0, boundaryH, boundaryV));
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect((boundaryH - workloadH) * -1, canvas.Height - workloadV, canvas.Width div 2, canvas.Height div 2),
                     rect(0, boundaryV, boundaryH, canvas.Height));
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect(canvas.Width - workloadH, (boundaryV - workloadV) * -1, canvas.Width div 2, canvas.Height div 2),
                     rect(boundaryH, 0, canvas.Width, boundaryV));
   canvas.DrawRectTo(GRenderTargets[RENDERER_ALT],
                     rect(canvas.Width - workloadH, canvas.Height - workloadV, canvas.Width div 2, canvas.Height div 2),
                     rect(boundaryH, boundaryV, canvas.Width, canvas.Height));
end;

function TCombineTransition.InternalDraw: boolean;
var
   workload, boundaryH, boundaryV: integer;
   ratio: single;
begin
   GRenderTargets[RENDERER_MAIN].DrawFull;
   ratio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   boundaryH := GSpriteEngine.Canvas.Width div 2;
   workload := boundaryH div (FADETIME[1] div TRpgTimestamp.FrameLength);
   boundaryV := round(boundaryH / ratio);
   case FStyle of
      ds_vert: CombineVert(FProgress, boundaryV);
      ds_horiz: CombineHoriz(round(FProgress * ratio), boundaryH);
      ds_both: CombineQuarters(round(FProgress * ratio), boundaryH, FProgress, boundaryV);
   end;
   inc(FProgress, workload);
   result := FProgress < boundaryV;
end;

function combine(style: TDivideStyle): ITransition;
begin
   result := TCombineTransition.Create(style);
end; {$ENDREGION}

{ZOOM}
{$REGION ZOOM}
{ TZoomTransition }

constructor TZoomTransition.Create(zoomIn: boolean);
begin
   inherited Create;
   FZoomIn := zoomIn;
   FMinimum := round(GSpriteEngine.Canvas.Width / MAXZOOM);
   FRatio := GSpriteEngine.Canvas.Width / GSpriteEngine.Canvas.Height;
   with GSpriteEngine.currentParty.baseTile do
      FCenter := sgPoint(trunc(x) + 8, trunc(y) + 8);
   dec(FCenter.x, round(GSpriteEngine.WorldX));
   dec(FCenter.Y, round(GSpriteEngine.WorldY));

   if zoomIn then
   begin
      FCurrentX := 0;
      FCurrentY := 0;
      FProgress := GSpriteEngine.Canvas.Width;
   end else
   begin
      FProgress := FMinimum;
      FCurrentX := FCenter.X - (FMinimum / 2);
      FCurrentY := FCenter.Y - ((FMinimum / FRatio) / 2);
   end;
end;

procedure TZoomTransition.Setup(showing: boolean; OnFinished: TTransitionDoneEvent);
begin
   inherited Setup(showing, OnFinished);
   if showing then
      FTarget := RENDERER_ALT
   else FTarget := RENDERER_MAIN;
end;

function TZoomTransition.InternalDraw: boolean;
var
   viewRect: TRect;
   workload: integer;
begin
   workload := GSpriteEngine.Canvas.Width div max((FADETIME[1] div TRpgTimestamp.FrameLength), 1);
   if FZoomIn then
      FProgress := max(FProgress - workload, FMinimum)
   else FProgress := min(FProgress + workload, GSpriteEngine.Canvas.Width);
   viewRect.BottomRight := point(FProgress, round(FProgress / FRatio));
   if FZoomIn then
   begin
      FCurrentX := FCurrentX + (FCenter.x / GSpriteEngine.Canvas.Width) * workload;
      FCurrentY := FCurrentY + (FCenter.Y / GSpriteEngine.Canvas.Height) * (workload / FRatio);
   end else begin
      FCurrentX := FCurrentX - (FCenter.x / GSpriteEngine.Canvas.Width) * workload;
      FCurrentY := FCurrentY - (FCenter.Y / GSpriteEngine.Canvas.Height) * (workload / FRatio);
   end;
   viewRect.Left := max(round(FCurrentX), 0);
   viewRect.Top := max(round(FCurrentY), 0);
   GSpriteEngine.Canvas.DrawRectTo(GRenderTargets[FTarget],
                                   rect(0, 0, GSpriteEngine.Canvas.Width, GSpriteEngine.Canvas.Height),
                                   viewRect);
   if FZoomIn then
      result := FProgress > FMinimum
   else result := FProgress < GSpriteEngine.Canvas.Width;
end;

function zoom(vanishing: boolean): ITransition;
begin
   result := TZoomTransition.Create(vanishing);
end; {$ENDREGION}

{ TMosaicTransition }

procedure TMosaicTransition.Setup(showing: boolean;
  OnFinished: TTransitionDoneEvent);
begin
   inherited Setup(showing, OnFinished);
   FMaxSize := GSpriteEngine.Canvas.Width / MIN_RESOLUTION;
   if showing then
   begin
      FBlockSize := FMaxSize;
      FTarget := RENDERER_ALT
   end
   else begin
      FBlockSize := 1;
      FTarget := RENDERER_MAIN;
   end;
end;

function TMosaicTransition.InternalDraw: boolean;
var
   workload: single;
   prog, mosaicProg: integer;
   shaders: TdmShaders;
begin
   workload := GSpriteEngine.Canvas.Width / max((FADETIME[2] / TRpgTimestamp.FrameLength), 1);
   if FShowing then
      FBlockSize := FBlockSize - workload
   else FBlockSize := FBlockSize + workload;
   clamp(FBlockSize, 1, FMaxSize);
   glGetIntegerv(GL_CURRENT_PROGRAM, @prog);
   shaders := GSpriteEngine.ShaderEngine;
   mosaicProg := shaders.ShaderProgram('default', 'mosaic');
   shaders.UseShaderProgram(mosaicProg);
   shaders.SetUniformValue(mosaicProg, 'blockSize', FBlockSize);
   GRenderTargets[FTarget].DrawFull;
   glUseProgram(prog);
   if FShowing then
      result := FBlockSize > 1
   else result := FBlockSize < FMaxSize;
end;

function mosaic(vanishing: boolean): ITransition;
begin
   result := TMosaicTransition.Create;
end;

{BOF2}
{$REGION BOF2}
function TBof2Transition.DoDraw: boolean;
var
   i, j, width: integer;
   workload, endpoint: integer;
begin
   width := GSpriteEngine.Canvas.Width;
   endpoint := (width * 4) + GSpriteEngine.Canvas.Height;
   workload := endpoint div (FADETIME[1] div TRpgTimestamp.FrameLength);
   inc(FProgress, workload);
   j := FProgress div 4;
   i := 0;
   repeat
      if j <= width then
         if i mod 2 = 0 then
            GSpriteEngine.Canvas.FillRect(rect(0, i, j, 1), SDL_WHITE)
         else GSpriteEngine.Canvas.FillRect(rect(width - j, i, j, 1), SDL_WHITE);
      if (i mod 4 = 0) and (i > 0) then
         dec(j);
      inc(i);
   until (i > GSpriteEngine.Canvas.Height) or (j <= 0);
   result := FProgress <= endpoint;
end;

function bof2(vanishing: boolean): ITransition;
begin
   result := TBof2Transition.Create;
end;
{$ENDREGION}

{WAVE}
{$REGION WAVE}

{ TWaveTransition }

function TWaveTransition.InternalDraw: boolean;
var
   workload: integer;
const
   WAVE_PERIOD = 5;
begin
   GSpriteEngine.Canvas.Clear(SDL_BLACK);
   workload := WAVESIZE div (FADETIME[2] div 64);
   inc(FProgress, workload);
   if FShowing then
      drawWave(GRenderTargets[RENDERER_ALT],
               rect(0, 0, GSpriteEngine.Canvas.Width, GSpriteEngine.Canvas.Height),
               WAVESIZE - FProgress, WAVE_PERIOD, WAVESIZE - FProgress)
   else drawWave(GRenderTargets[RENDERER_MAIN],
                 rect(0, 0, GSpriteEngine.Canvas.Width, GSpriteEngine.Canvas.Height),
                 FProgress, WAVE_PERIOD, FProgress);
   result := FProgress < WAVESIZE;
end;

function wave(vanishing: boolean): ITransition;
begin
   result := TWaveTransition.Create;
end;
{$ENDREGION}

{ TBof2Transition }

initialization
   GRenderTargets := TSdlRenderTargets.Create;
finalization
   GRenderTargets.Free;
end.
