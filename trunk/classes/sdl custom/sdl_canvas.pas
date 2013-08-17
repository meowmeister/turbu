unit sdl_canvas;
{*****************************************************************************
* SDL_Canvas.pas                                         Modified: 19-Apr-2008
******************************************************************************
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
******************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface
uses
   types, classes, Generics.Collections, SysUtils,
   SDL_ImageManager, SG_Defs,
   SDL, SDL_13;

type
   TSdlCanvas = class;

   TSdlRenderSurface = class abstract(TObject)
   private
      FParent: TSdlCanvas;
      FSize: TSgPoint;
      procedure SetBgColor(const Value: TSgColor);
//      function GetBitDepth: byte;
   public
      procedure Clear; overload;
      procedure SetRenderer; virtual; abstract;

      property Width: Integer read FSize.X;
      property Height: Integer read FSize.Y;
      property size: TSgPoint read FSize;
//      property Depth: Byte read GetBitDepth;
      property parent: TSdlCanvas read FParent;
   end;

   TSdlRenderTarget = class(TSdlRenderSurface)
   private
      FHandle: TSdlTexture;
   public
      constructor Create(size: TSgPoint); overload;
      destructor Destroy; override;
      procedure SetRenderer; override;
      procedure DrawFull; overload;
      procedure DrawFull(const TopLeft: TPoint); overload;
      property handle: TSdlTexture read FHandle;
   end;

   TSdlRenderTargets = class(TObjectList<TSdlRenderTarget>)
   public
      function RenderOn(Index: Integer; Event: TNotifyEvent; Bkgrnd: Cardinal = 0;
        FillBk: Boolean = true; composite: boolean = false): Boolean;
   end;

   TRenderStack = class(TStack<TSdlRenderSurface>);
   TSdlCanvasNotifyEvent = procedure(sender: TSdlCanvas) of object;

   {***************************************************************************
   * Encapsulates an SDL_Surface as the principal video buffer for an
   * application.
   ***************************************************************************}
   TSdlCanvas = class(TSdlRenderSurface)
   private
      FRenderer: TSdlRenderer;
      FRenderStack: TRenderStack;
      FRenderTarget: TSdlRenderSurface;
//      FFullscreen: boolean;
      FWindow: TSdlWindow;
      FOnResize: TSdlCanvasNotifyEvent;
      procedure SetRenderTarget(const Value: TSdlRenderSurface);
//      procedure SetFullscreen(const Value: boolean);
//      function IsFullscreen: boolean;
   public
      {************************************************************************
      * Creates a TSdlCanvas.  The parameters are pretty self-explanatory.
      ************************************************************************}
      constructor Create(title: UTF8String; size: TRect; flags: TSdlWindowFlags);
      constructor CreateFrom(value: TSdlWindow);

      destructor Destroy; override;

      {************************************************************************
      * Draws a TSdlImage to the current render target.  The dest parameter
      * represents the position of the top-left corner.
      ************************************************************************}
      procedure Draw(image: TSdlImage; dest: TSgPoint; flip: TSdlFlipAxes); overload;
      procedure Draw(target: TSdlRenderTarget; dest: TSgPoint); overload;
      procedure DrawTo(image: TSdlImage; dest: TRect);

      {************************************************************************
      * Like Draw, but only draws a portion of the image, as defined by the
      * source parameter.
      ************************************************************************}
      procedure DrawRect(image: TSdlImage; dest: TSgPoint; source: TRect; flip: TSdlFlipAxes); overload;
      procedure DrawRect(target: TSdlRenderTarget; dest: TSgPoint; source: TRect); overload;
      procedure DrawRectTo(image: TSdlImage; dest, source: TRect); overload;
      procedure DrawRectTo(target: TSdlRenderTarget; dest, source: TRect); overload;

      {************************************************************************
      * Draws a box on screen
      ************************************************************************}
      procedure DrawBox(const region: TRect; const color: SDL_Color; const alpha: byte = $FF);
      procedure DrawDashedBox(const region: TRect; const color: SDL_Color; const alpha: byte = $FF);
      procedure FillRect(const region: TRect; const color: SDL_Color; const alpha: byte = $FF);

      procedure Clear(const color: SDL_Color; const alpha: byte = $FF); overload;

      {************************************************************************
      * Flips the buffer, rendering the canvas to the screen.
      ************************************************************************}
      procedure Flip; virtual;

      {************************************************************************
      * Pushes the current render target to the render target stack, or loads a
      * previously pushed target.
      ************************************************************************}
      procedure pushRenderTarget;
      procedure popRenderTarget;

      {************************************************************************
      * Notify the canvas that the SDL_Window has been resized
      ************************************************************************}
      procedure Resize;

      {************************************************************************
      * The current render target.  Setting this property to nil will set the
      * render target back to the screen canvas.
      ************************************************************************}
      property RenderTarget: TSdlRenderSurface read FRenderTarget write SetRenderTarget;

      procedure SetRenderer; override;

      property Renderer: TSdlRenderer read FRenderer;

      property Window: TSdlWindow read FWindow;

      property OnResize: TSdlCanvasNotifyEvent read FOnResize write FOnResize;

//      property Fullscreen: boolean read IsFullscreen write SetFullscreen;
   end;

   ECanvas = class(Exception);

function currentRenderTarget: TSdlRenderSurface;

implementation
uses
   OpenGL,
   turbu_OpenGL, dm_shaders;

var
   lCurrentRenderTarget: TSdlRenderSurface;

{ TSdlRenderSurface }

procedure TSdlRenderSurface.Clear;
begin
   assert(lCurrentRenderTarget = self);
   SDL_RenderFillRect(FParent.FRenderer, nil);
end;

constructor TSdlRenderTarget.Create(size: TSgPoint);
var
   info: TSDL_RendererInfo;
begin
   inherited Create;
   assert(assigned(lCurrentRenderTarget));
   FParent := lCurrentRenderTarget.FParent;
   SDL_GetRendererInfo(FParent.FRenderer, info);

   FHandle := TSdlTexture.Create(FParent.FRenderer, info.texture_formats[0], sdltaRenderTarget, size.x, size.y);
   SDL_SetTextureBlendMode(FHandle, [sdlbBlend]);
   FSize := FHandle.size;
end;

destructor TSdlRenderTarget.Destroy;
begin
   FHandle.Free;
   inherited Destroy;
end;

procedure TSdlRenderTarget.DrawFull(const TopLeft: TPoint);
var
   xScale, yScale: single;
begin
glCheckError;
   glEnable(GL_TEXTURE_RECTANGLE_ARB);
glCheckError;
   self.handle.bind;
glCheckError;
   SDL_RenderGetScale(FParent.FRenderer, xScale, yScale);
   glBegin(GL_QUADS);
      glTexCoord2i(0, 0); glVertex2i(round(topleft.X * xScale), round(topleft.y * yScale));
      glTexCoord2i(Width, 0); glVertex2i(round((topleft.X + Width) * xScale), round(topLeft.y * yScale));
      glTexCoord2i(Width, Height); glVertex2i(round((topleft.X + Width) * xScale), round((topleft.y + Height) * yScale));
      glTexCoord2i(0, Height); glVertex2i(round(topleft.X * xScale), round((topleft.y + Height) * yScale));
   glEnd;
glCheckError;
end;

procedure TSdlRenderTarget.DrawFull;
var
   xScale, yScale: single;
   lHeight, lWidth: integer;
begin
glCheckError;
   glEnable(GL_TEXTURE_RECTANGLE_ARB);
glCheckError;
   self.handle.bind;
glCheckError;
   SDL_RenderGetScale(FParent.FRenderer, xScale, yScale);
   lWidth := round(width * xScale);
   lHeight := round(height * yScale);
   glBegin(GL_QUADS);
      glTexCoord2i(0, 0); glVertex2i(0, 0);
      glTexCoord2i(0, Height); glVertex2i(0, lHeight);
      glTexCoord2i(Width, Height); glVertex2i(lWidth, lHeight);
      glTexCoord2i(Width, 0); glVertex2i(lWidth, 0);
   glEnd;
glCheckError;
end;

procedure TSdlRenderTarget.SetRenderer;
begin
   if SDL_SetRenderTarget(FParent.FRenderer, FHandle) <> 0 then
      asm int 3 end;
   FParent.SetRenderTarget(self);
end;

{function TSdlRenderSurface.GetBitDepth: byte;
begin
   result := FSurface.format.BitsPerPixel;
end;}

procedure TSdlRenderSurface.SetBgColor(const Value: TSgColor);
begin
   assert(lCurrentRenderTarget = self);
   SDL_SetRenderDrawColor(FParent.FRenderer, value.rgba[1], value.rgba[2], value.rgba[3], value.rgba[4]);
end;

{ TSdlCanvas }

constructor TSdlCanvas.Create(title: UTF8String; size: TRect; flags: TSdlWindowFlags);
begin
   inherited Create;
   FRenderStack := TRenderStack.Create;

   if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
      SDL_InitSubSystem(SDL_INIT_VIDEO);

   FWindow := SDL_CreateWindow(PAnsiChar(title), size.Left, size.Top, size.Right, size.Bottom, flags);
   SDL_RenderGetLogicalSize(FRenderer, FSize.x, FSize.y);
   FRenderer := TSDLRenderer.Create(FWindow, SDL_RendererIndex('opengl'), [sdlrAccelerated]);
   SDL_RenderPresent(FRenderer);
   self.RenderTarget := self;
   FParent := self;
end;

constructor TSdlCanvas.CreateFrom(value: TSdlWindow);
begin
   inherited Create;
   FRenderStack := TRenderStack.Create;

   if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
      SDL_InitSubSystem(SDL_INIT_VIDEO);

   FWindow := value;

   self.RenderTarget := self;
   FRenderer := SDL_GetRenderer(value);
   SDL_RenderGetLogicalSize(FRenderer, FSize.x, FSize.y);
   FParent := self;
end;

destructor TSdlCanvas.Destroy;
begin
   FRenderStack.Free;
   inherited Destroy;
end;

procedure TSdlCanvas.Draw(image: TSdlImage; dest: TSgPoint; flip: TSdlFlipAxes);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   dummy.BottomRight := image.surface.size;
   if flip = [] then
      assert(SDL_RenderCopy(FRenderer, image.surface, nil, @dummy) = 0)
   else assert(SDL_RenderCopyFlipped(FRenderer, image.surface, nil, @dummy, flip) = 0)
end;

procedure TSdlCanvas.Draw(target: TSdlRenderTarget; dest: TSgPoint);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   dummy.BottomRight := target.handle.size;
   assert(SDL_RenderCopy(FRenderer, target.handle, nil, @dummy) = 0);
end;

procedure TSdlCanvas.DrawTo(image: TSdlImage; dest: TRect);
begin
   assert(SDL_RenderCopy(FRenderer, image.surface, nil, @dest) = 0);
end;

procedure TSdlCanvas.FillRect(const region: TRect; const color: SDL_Color; const alpha: byte);
begin
   assert(SDL_SetRenderDrawColor(FRenderer, color.r, color.g, color.b, alpha) = 0);
   SDL_RenderFillRect(FRenderer, @region);
end;

procedure TSdlCanvas.DrawBox(const region: TRect; const color: SDL_Color;
  const alpha: byte);
begin
   assert(SDL_SetRenderDrawColor(FRenderer, color.r, color.g, color.b, alpha) = 0);
   SDL_RenderDrawRect(FRenderer, @region);
end;

procedure TSdlCanvas.DrawDashedBox(const region: TRect; const color: SDL_Color; const alpha: byte);

   procedure DrawHorizLine(x1, x2, y: integer);
   var
      x: integer;
   begin
      x := x1;
      while x < x2 - 2 do
      begin
         SDL_RenderDrawLine(FRenderer, x, y, x + 2, y);
         inc(x, 4);
      end;
   end;

   procedure DrawVertLine(y1, y2, x: integer);
   var
      y: integer;
   begin
      y := y1;
      while y < y2 - 2 do
      begin
         SDL_RenderDrawLine(FRenderer, x, y1, x, y + 2);
         inc(y, 4);
      end;
   end;

begin
   assert(SDL_SetRenderDrawColor(FRenderer, color.r, color.g, color.b, alpha) = 0);
   DrawHorizLine(region.Left, region.Right, region.Top);
   DrawHorizLine(region.Left, region.Right, region.Bottom);
   DrawVertLine(region.Top, region.Bottom, region.Left);
   DrawVertLine(region.Top, region.Bottom, region.Right);
end;

procedure TSdlCanvas.Clear(const color: SDL_Color; const alpha: byte);
begin
   assert(SDL_SetRenderDrawColor(FRenderer, color.r, color.g, color.b, alpha) = 0);
   //keep SDL state in sync with GL state
   glColor4f(color.r / 255, color.g / 255, color.b / 255, alpha / 255);
   SDL_RenderFillRect(FRenderer, nil);
end;

procedure TSdlCanvas.DrawRect(image: TSdlImage; dest: TSgPoint; source: TRect; flip: TSdlFlipAxes);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   dummy.BottomRight := source.BottomRight;
   if flip = [] then
      assert(SDL_RenderCopy(FRenderer, image.surface, @source, @dummy) = 0)
   else assert(SDL_RenderCopyFlipped(FRenderer, image.surface, @source, @dummy, flip) = 0);
end;

procedure TSdlCanvas.DrawRect(target: TSdlRenderTarget; dest: TSgPoint;
  source: TRect);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   dummy.BottomRight := source.BottomRight;
   assert(SDL_RenderCopy(FRenderer, target.handle, @source, @dummy) = 0);
end;

procedure TSdlCanvas.DrawRectTo(image: TSdlImage; dest, source: TRect);
begin
   if SDL_RenderCopy(FRenderer, image.surface, @source, @dest) <> 0 then
      raise ECanvas.CreateFmt('SDL_RenderCopy failed: %s', [AnsiString(SDL_GetError)]);
end;

procedure TSdlCanvas.DrawRectTo(target: TSdlRenderTarget; dest, source: TRect);
begin
   if SDL_RenderCopy(FRenderer, target.handle, @source, @dest) <> 0 then
      raise ECanvas.CreateFmt('SDL_RenderCopy failed: %s', [AnsiString(SDL_GetError)]);
end;

procedure TSdlCanvas.Flip;
begin
//   assert(lCurrentRenderTarget = self);
   SDL_RenderPresent(FRenderer);
end;

procedure TSdlCanvas.popRenderTarget;
begin
   FRenderStack.Pop.SetRenderer;
end;

procedure TSdlCanvas.pushRenderTarget;
begin
   FRenderStack.Push(FRenderTarget);
end;

procedure TSdlCanvas.Resize;
var
   x, y: integer;
begin
   SDL_RenderGetLogicalSize(FRenderer, x, y);
   if (x <> FSize.x) or (y <> FSize.y) then
   begin
      FSize := sgPoint(x, y);
      if assigned(FOnResize) then
         FOnResize(self);
   end;
end;

(*
procedure TSdlCanvas.SetFullscreen(const Value: boolean);
begin
   raise Exception.Create('SetFullscreen not currently supported');
{   if FFullscreen <> Value then
      FSurface := SDL_SetVideoMode(self.width, self.height, self.depth, FSurface.flags xor SDL_FULLSCREEN);}
end;

function TSdlCanvas.IsFullscreen: boolean;
begin
   result := {FSurface.flags and SDL_FULLSCREEN = SDL_FULLSCREEN} false;
end;
*)

procedure TSdlCanvas.SetRenderer;
begin
   SetRenderTarget(nil);
   SDL_ResetTargetTexture(FRenderer);
end;

procedure TSdlCanvas.SetRenderTarget(const Value: TSdlRenderSurface);
begin
   if value = nil then
      SetRenderTarget(self)
   else FRenderTarget := Value;
   lCurrentRenderTarget := FRenderTarget;
end;

{ TSdlRenderTargets }

function TSdlRenderTargets.RenderOn(Index: Integer; Event: TNotifyEvent;
  Bkgrnd: Cardinal; FillBk, composite: Boolean): Boolean;
var
   Target: TSdlRenderTarget;
   color: TSgColor;
begin
   result := false;
   if not ((Index >= 0) and (Index < count)) then
      Exit;

   Target := self[Index];
   CurrentRenderTarget.FParent.pushRenderTarget;
   try
      target.SetRenderer;
      if (FillBk) then
      begin
         color := TSgColor(bkgrnd);
         if composite then
            color.rgba[4] := 0
         else color.rgba[4] := 255;
         glDisable(GL_BLEND);
         target.parent.Clear(color);
         glEnable(GL_BLEND);
      end;
      if assigned(event) then
         Event(Self);
      result := true;
   finally
      CurrentRenderTarget.FParent.popRenderTarget;
   end;
end;

{ Classless }

function currentRenderTarget: TSdlRenderSurface;
begin
   result := lCurrentRenderTarget;
end;

end.
