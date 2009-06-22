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
   TSdlSurfaceMode = (cmSoftware, cmHardware);

   TSdlRenderTarget = class(TObject)
   private
      function GetWidth: Integer; inline;
      function GetHeight: Integer; inline;
      procedure SetBgColor(const Value: TSgColor);
      function GetBitDepth: byte;
   protected
      FSurface: PSdlSurface; //the actual SDL_Surface
      FMustLock: Boolean;
      FStyle: TSdlSurfaceMode;
      FBgColor: TSgColor;
      function GetClipRect: TRect; virtual;
      procedure SetClipRect(const Value: TRect); virtual;
   public
      constructor Create(size: TSgPoint); overload;
      procedure ClearClipRect; inline;
      procedure Clear;

      property surface: PSdlSurface read FSurface;
      property style: TSdlSurfaceMode read FStyle;
      property Width: Integer read GetWidth;
      property Height: Integer read GetHeight;
      property Depth: Byte read GetBitDepth;
      property mustLock: Boolean read FMustLock;
      property clipRect: TRect read GetClipRect write SetClipRect;
      property BgColor: TSgColor read FBgColor write SetBgColor;
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
   TSdlCanvas = class(TSdlRenderTarget)
   class var
      FRenderStack: TRenderStack;
   private
      FRenderTarget: TSdlRenderTarget;
      FRenderSurface: PSdlSurface;
      FFullscreen: boolean;
      procedure SetRenderTarget(const Value: TSdlRenderTarget);
      procedure SetFullscreen(const Value: boolean);
      function IsFullscreen: boolean;
   protected
      {************************************************************************
      * The ClipRect property of a canvas will return/set the clip rect for the
      * current render target, not for the canvas itself, if a render target is
      * selected.
      ************************************************************************}
      function GetClipRect: TRect; override;
      procedure SetClipRect(const Value: TRect); override;
   public
      {************************************************************************
      * Creates a TSdlCanvas.  The parameters are pretty self-explanatory.
      * PixelDepth is in bits per pixel.
      ************************************************************************}
      constructor Create(mode: TSdlSurfaceMode; fullscreen: boolean; size: TRect; pixelDepth: cardinal);

      destructor Destroy; override;

      {************************************************************************
      * Draws a TSdlImage to the current render target.  The dest parameter
      * represents the position of the top-left corner.
      ************************************************************************}
      procedure Draw(image: TSdlImage; dest: TSgPoint);

      {************************************************************************
      * Like Draw, but only draws a portion of the image, as defined by the
      * source parameter.
      ************************************************************************}
      procedure DrawRect(image: TSdlImage; dest: TSgPoint; source: TSDLRect);

      {************************************************************************
      * Copies data from the current render target to either a TSdlImage or
      * another render target.
      ************************************************************************}
      procedure Copy(image: TSdlImage; dest: TSgPoint); overload;
      procedure Copy(image: TSdlRenderTarget; dest: TSgPoint); overload;
      procedure CopyRect(image: TSdlImage; dest: TSgPoint; source: TRect); overload;
      procedure CopyRect(image: TSdlRenderTarget; dest: TSgPoint; source: TRect); overload;

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
      * The current render target.  Setting this property to nil will set the
      * render target back to the screen canvas.
      ************************************************************************}
      property RenderTarget: TSdlRenderTarget read FRenderTarget write SetRenderTarget;

      property Fullscreen: boolean read IsFullscreen write SetFullscreen;
   end;

var
   screenCanvas: TSdlCanvas;

implementation
uses
   sysUtils;

{ TSdlRenderTarget }

function TSdlRenderTarget.GetHeight: Integer;
begin
   result := FSurface.height;
end;

function TSdlRenderTarget.GetWidth: Integer;
begin
   result := FSurface.width;
end;

procedure TSdlRenderTarget.Clear;
begin
   FSurface.Fill(nil, FBgColor);
end;

procedure TSdlRenderTarget.ClearClipRect;
begin
   FSurface.ClearClipRect;
end;

constructor TSdlRenderTarget.Create(size: TSgPoint);
var
   format: PSDL_PixelFormat;
begin
   inherited Create;
   assert(assigned(screenCanvas));
   format := SDL_GetVideoInfo.vfmt;

   FSurface := TSdlSurface.Create(size.x, size.y, format.BitsPerPixel, format.RMask, format.GMask, format.BMask, format.AMask);
{   if FSurface.flags and SDL_HWSURFACE = SDL_HWSURFACE then
      FStyle := cmHardware
   else FStyle := cmSoftware;}
   FMustLock := FSurface.MustLock;
end;

function TSdlRenderTarget.GetBitDepth: byte;
begin
   result := FSurface.format.BitsPerPixel;
end;

function TSdlRenderTarget.GetClipRect: TRect;
begin
   result := FSurface.ClipRect;
end;

procedure TSdlRenderTarget.SetBgColor(const Value: TSgColor);
begin
   FBgColor := SDL_MapRGBA(FSurface.format, Value.rgba[1], Value.rgba[2], Value.rgba[3], Value.rgba[4])
end;

procedure TSdlRenderTarget.SetClipRect(const Value: TRect);
begin
   FSurface.ClipRect := Value;
end;

{ TSdlCanvas }

procedure TSdlCanvas.Copy(image: TSdlImage; dest: TSgPoint);
var
   dstRect: TSdlRect;
begin
   dstRect := rect(dest, point(0,0));
   SDL_UpperBlit(FRenderSurface, nil, image.surface, @dstRect);
end;

procedure TSdlCanvas.Copy(image: TSdlRenderTarget; dest: TSgPoint);
var
   dstRect: TSdlRect;
begin
   dstRect := rect(dest, point(0,0));
   SDL_UpperBlit(FRenderSurface, nil, image.surface, @dstRect);
end;

procedure TSdlCanvas.CopyRect(image: TSdlImage; dest: TSgPoint; source: TRect);
var
   srcRect, dstRect: TSdlRect;
begin
   dstRect := rect(dest, point(0,0));
   srcRect := source;
   SDL_UpperBlit(FRenderSurface, @srcRect, image.surface, @dstRect);
end;

procedure TSdlCanvas.CopyRect(image: TSdlRenderTarget; dest: TSgPoint;
  source: TRect);
var
   srcRect, dstRect: TSdlRect;
begin
   dstRect := rect(dest, point(0,0));
   srcRect := source;
   SDL_UpperBlit(FRenderSurface, @srcRect, image.surface, @dstRect);
end;

constructor TSdlCanvas.Create(mode: TSdlSurfaceMode; fullscreen: boolean; size: TRect; pixelDepth: cardinal);
var
   flags: cardinal;
begin
   assert(screenCanvas = nil);
   inherited Create;
   FRenderStack := TRenderStack.Create;

   if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
      SDL_InitSubSystem(SDL_INIT_VIDEO);

   flags := SDL_DOUBLEBUF;
   if fullscreen then
      flags := flags or SDL_FULLSCREEN
   else flags := flags or SDL_RESIZABLE;
   case mode of
      cmSoftware: flags := flags or SDL_SWSURFACE;
      cmHardware: flags := flags or SDL_HWSURFACE;
   end;
   SDL_putenv(PAnsiChar('SDL_VIDEO_WINDOW_POS=' + ansiString(intToStr(size.Left)) + ',' + ansiString(intToStr(size.Top))));
    FSurface := PSdlSurface(SDL_SetVideoMode(size.right, size.bottom, pixelDepth, flags));
{   if not assigned(FSurface) then
      OutOfMemoryError;
   if FSurface^.flags and SDL_HWSURFACE = SDL_HWSURFACE then
      FStyle := cmHardware
   else FStyle := cmSoftware;}
   FMustLock := FSurface.MustLock;
   screenCanvas := self;
   self.RenderTarget := self;
end;

destructor TSdlCanvas.Destroy;
begin
   SDL_QuitSubSystem(SDL_INIT_VIDEO);
   FRenderStack.Free;
   screenCanvas := nil;
   inherited Destroy;
end;

procedure TSdlCanvas.Draw(image: TSdlImage; dest: TSgPoint);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   SDL_UpperBlit(image.surface, nil, FRenderSurface, @dummy);
end;

procedure TSdlCanvas.DrawRect(image: TSdlImage; dest: TSgPoint; source: TSDLRect);
var
   dummy: TSDLRect;
begin
   dummy.TopLeft := dest;
   SDL_UpperBlit(image.surface, @source, FRenderSurface, @dummy);
end;

procedure TSdlCanvas.Flip;
begin
   SDL_Flip(PSdl_surface(FSurface));
end;

procedure TSdlCanvas.SetClipRect(const Value: TRect);
begin
   if FRenderTarget = self then
      inherited SetClipRect(Value)
   else FRenderTarget.SetClipRect(Value);
end;

function TSdlCanvas.IsFullscreen: boolean;
begin
   result := {FSurface.flags and SDL_FULLSCREEN = SDL_FULLSCREEN} false;
end;

procedure TSdlCanvas.popRenderTarget;
begin
   setRenderTarget(FRenderStack.Pop);
end;

procedure TSdlCanvas.pushRenderTarget;
begin
   FRenderStack.Push(FRenderTarget);
end;

procedure TSdlCanvas.SetFullscreen(const Value: boolean);
begin
   raise Exception.Create('SetFullscreen not currently supported');
{   if FFullscreen <> Value then
      FSurface := SDL_SetVideoMode(self.width, self.height, self.depth, FSurface.flags xor SDL_FULLSCREEN);}
end;

function TSdlCanvas.GetClipRect: TRect;
begin
   if FRenderTarget = self then
      result := inherited GetClipRect
   else result := FRenderTarget.ClipRect;
end;

procedure TSdlCanvas.SetRenderTarget(const Value: TSdlRenderTarget);
begin
   if value = nil then
      SetRenderTarget(self)
   else begin
      FRenderTarget := Value;
      FRenderSurface := Value.surface;
   end;
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
   if target.mustLock then
      target.surface.LockSurface;
   try
      if (FillBk) then
         if (not target.surface.Fill(nil, bkgrnd)) then
            Exit;
      Event(Self);
   finally
      if target.mustLock then
         target.surface.UnlockSurface;
   end;
end;

initialization
begin
   screenCanvas := nil;
end;

finalization
begin
   screenCanvas.free;
end;

end.
