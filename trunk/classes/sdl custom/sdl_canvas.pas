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
   types, classes, Generics.Collections,
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
   protected
      function GetID: UInt32; virtual; abstract;
   public
      procedure Clear;
      procedure SetRenderer; virtual; abstract;

      property Width: Integer read FSize.X;
      property Height: Integer read FSize.Y;
      property size: TSgPoint read FSize;
//      property Depth: Byte read GetBitDepth;
      property parent: TSdlCanvas read FParent;
      property id: UInt32 read GetID;
   end;

   TSdlRenderTarget = class(TSdlRenderSurface)
   private
      FHandle: TSdlTexture;
   protected
      function GetID: UInt32; override;
   public
      constructor Create(size: TSgPoint); overload;
      procedure SetRenderer; override;
      property handle: TSdlTexture read FHandle;
   end;

   TSdlRenderTargets = class(TObjectList<TSdlRenderTarget>)
   public
      function RenderOn(Index: Integer; Event: TNotifyEvent; Bkgrnd: Cardinal; FillBk: Boolean): Boolean;
   end;

   TRenderStack = class(TStack<TSdlRenderTarget>);

   {***************************************************************************
   * Encapsulates an SDL_Surface as the principal video buffer for an
   * application.
   ***************************************************************************}
   TSdlCanvas = class(TSdlRenderSurface)
   private
      FRenderStack: TRenderStack;
      FRenderTarget: TSdlRenderSurface;
      FRenderHandle: Cardinal;
//      FFullscreen: boolean;
      FWindow: TSdlWindowId;
      procedure SetRenderTarget(const Value: TSdlRenderSurface);
//      procedure SetFullscreen(const Value: boolean);
//      function IsFullscreen: boolean;
   protected
      {************************************************************************
      * The ClipRect property of a canvas will return/set the clip rect for the
      * current render target, not for the canvas itself, if a render target is
      * selected.
      ************************************************************************}
      function GetID: UInt32; override;
   public
      {************************************************************************
      * Creates a TSdlCanvas.  The parameters are pretty self-explanatory.
      ************************************************************************}
      constructor Create(title: UTF8String; size: TRect; flags: TSdlWindowFlags);
      constructor CreateFrom(value: TSdlWindowId);

      destructor Destroy; override;

      {************************************************************************
      * Draws a TSdlImage to the current render target.  The dest parameter
      * represents the position of the top-left corner.
      ************************************************************************}
      procedure Draw(image: TSdlImage; dest: TSgPoint);
      procedure DrawTo(image: TSdlImage; dest: TRect);

      {************************************************************************
      * Like Draw, but only draws a portion of the image, as defined by the
      * source parameter.
      ************************************************************************}
      procedure DrawRect(image: TSdlImage; dest: TSgPoint; source: TRect);
      procedure DrawRectTo(image: TSdlImage; dest, source: TRect);

      {************************************************************************
      * Draws a box on screen
      ************************************************************************}
      procedure DrawBox(const region: TRect; const color: SDL_Color; const alpha: byte = $FF);

      {************************************************************************
      * Flips the buffer, rendering the canvas to the screen.
      ************************************************************************}
      procedure Flip; virtual;

      {************************************************************************
      * Sets the canvas as the current rendering device.
      ************************************************************************}
      procedure SetRenderer; override;

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

//      property Fullscreen: boolean read IsFullscreen write SetFullscreen;
   end;

function currentRenderTarget: TSdlRenderSurface;

implementation
uses
   sysUtils;

var
   lCurrentRenderTarget: TSdlRenderSurface;

{ TSdlRenderSurface }

procedure TSdlRenderSurface.Clear;
begin
   assert(lCurrentRenderTarget = self);
   SDL_RenderFillRect(nil);
end;

constructor TSdlRenderTarget.Create(size: TSgPoint);
var
   info: TSDL_RendererInfo;
begin
   inherited Create;
   assert(assigned(lCurrentRenderTarget));
   SDL_GetRendererInfo(info);

   FHandle := TSdlTexture.Create(info.texture_formats[0], sdltaRenderTarget, size.x, size.y);
   FSize := FHandle.size;
end;

function TSdlRenderTarget.GetID: UInt32;
begin
   result := FHandle.ID;
end;

procedure TSdlRenderTarget.SetRenderer;
begin
   SDL_SetTargetTexture(FHandle);
   lCurrentRenderTarget := self;
end;

{function TSdlRenderSurface.GetBitDepth: byte;
begin
   result := FSurface.format.BitsPerPixel;
end;}

procedure TSdlRenderSurface.SetBgColor(const Value: TSgColor);
begin
   assert(lCurrentRenderTarget = self);
   SDL_SetRenderDrawColor(value.rgba[1], value.rgba[2], value.rgba[3], value.rgba[4]);
end;

{ TSdlCanvas }

constructor TSdlCanvas.Create(title: UTF8String; size: TRect; flags: TSdlWindowFlags);
begin
   inherited Create;
   FRenderStack := TRenderStack.Create;

   if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
      SDL_InitSubSystem(SDL_INIT_VIDEO);

   FWindow := SDL_CreateWindow(PAnsiChar(title), size.Left, size.Top, size.Right, size.Bottom, flags);
   SDL_GetWindowSize(FWindow, FSize.x, FSize.y);
   if SDL_CreateRenderer(FWindow, -1, [sdlrPresentFlip3]) <> 0 then
      raise EBadHandle.Create(string(SDL_GetError));
   SDL_SelectRenderer(FWindow);
   SDL_RenderPresent;

   self.RenderTarget := self;
end;

constructor TSdlCanvas.CreateFrom(value: TSdlWindowId);
begin
   inherited Create;
   FRenderStack := TRenderStack.Create;

   if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
      SDL_InitSubSystem(SDL_INIT_VIDEO);

   FWindow := value;
   SDL_GetWindowSize(FWindow, FSize.x, FSize.y);

   self.RenderTarget := self;
end;

destructor TSdlCanvas.Destroy;
begin
   FRenderStack.Free;
   inherited Destroy;
end;

procedure TSdlCanvas.Draw(image: TSdlImage; dest: TSgPoint);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   dummy.BottomRight := image.surface.size;
   assert(SDL_RenderCopy(image.surface, nil, @dummy) = 0);
end;

procedure TSdlCanvas.DrawTo(image: TSdlImage; dest: TRect);
begin
   SDL_RenderCopy(image.surface, nil, @dest);
end;

procedure TSdlCanvas.DrawBox(const region: TRect; const color: SDL_Color;
  const alpha: byte);
begin
   assert(SDL_SetRenderDrawColor(color.r, color.g, color.b, alpha) = 0);
   SDL_RenderDrawRect(@region);
end;

procedure TSdlCanvas.DrawRect(image: TSdlImage; dest: TSgPoint; source: TRect);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   dummy.BottomRight := source.BottomRight;
   SDL_RenderCopy(image.surface, @source, @dummy);
end;

procedure TSdlCanvas.DrawRectTo(image: TSdlImage; dest, source: TRect);
begin
   SDL_RenderCopy(image.surface, @source, @dest);
end;

procedure TSdlCanvas.Flip;
begin
   assert(lCurrentRenderTarget = self);
   SDL_RenderPresent;
end;

function TSdlCanvas.GetID: UInt32;
begin
   result := FWindow;
end;

procedure TSdlCanvas.popRenderTarget;
begin
   setRenderTarget(FRenderStack.Pop);
end;

procedure TSdlCanvas.pushRenderTarget;
begin
   FRenderStack.Push(FRenderTarget as TSdlRenderTarget);
end;

procedure TSdlCanvas.Resize;
begin
   SDL_GetWindowSize(FWindow, FSize.x, FSize.y);
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
   SDL_SelectRenderer(FRenderTarget.ID);
end;

procedure TSdlCanvas.SetRenderTarget(const Value: TSdlRenderSurface);
begin
   if value = nil then
      SetRenderTarget(self)
   else begin
      FRenderTarget := Value;
      FRenderHandle := Value.id;
   end;
   lCurrentRenderTarget := FRenderTarget;
end;

{ TSdlRenderTargets }

function TSdlRenderTargets.RenderOn(Index: Integer; Event: TNotifyEvent;
  Bkgrnd: Cardinal; FillBk: Boolean): Boolean;
var
   Target: TSdlRenderTarget;
begin
   result := false;
   if not ((Index >= 0) and (Index < count)) then
      Exit;

   Target := self[Index];
   target.SetRenderer;
   if (FillBk) then
   begin
      target.SetBgColor(TSgColor(bkgrnd));
      target.Clear;
   end;
   Event(Self);
end;

{ Classless }

function currentRenderTarget: TSdlRenderSurface;
begin
   result := lCurrentRenderTarget;
end;

end.
