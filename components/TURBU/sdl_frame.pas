unit sdl_frame;

{*******************************************************************************
* NOTE: If you quit the video subsystem before all TSdlFrame objects have
* been destroyed, DestroyWindowHandle will fail and raise an exception that
* breaks your cleanup.
* If your TSdlFrames are created at design-time, this happens at program
* cleanup, after the 'end.' line in the DPR has been hit, so it's difficult to
* find a good place to call SDL_Quit in the normal flow of operation.  This unit
* calls SDL_QuitSubSystem(SDL_INIT_VIDEO) in its finalization, which is
* guaranteed to happen later than that.
*******************************************************************************}

interface

uses
   Types, Classes, Controls, ExtCtrls, Messages, SysUtils, Generics.Collections,
   SDL_13, SDL_ImageManager;

type
   TRendererType = (rtSoftware, rtGDI, rtOpenGL, rtD3D);

   TSdlFrame = class(TCustomControl)
   private
      FWindowID: TSdlWindowId;
      FFlags: TSdlWindowFlags;
      FTimer: TTimer;
      FFramerate: word;
      FActive: boolean;
      FRenderer: boolean;
      FRendererType: TRendererType;

      FImageManager: TSdlImages;

      FOnTimer: TNotifyEvent;
      FOnAvailable: TNotifyEvent;

      FLogicalWidth: integer;
      FLogicalHeight: integer;

      function CreateWindow: boolean;
      function CreateRenderer: boolean;
      procedure InternalOnTimer(Sender: TObject);
      procedure SetFramerate(Value: word);
      procedure SetActive(const Value: boolean);
      function GetTextureByName(name: string): TSdlTexture;
      procedure SetLogicalWidth(const Value: integer);
      procedure SetLogicalHeight(const Value: integer);
      function GetAspectRatio: double;
   protected
      procedure CreateWnd; override;
      procedure DestroyWnd; override;
      procedure Paint; override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
         X, Y: Integer); override;
      procedure WMGetDlgCode(var msg: TMessage); message WM_GETDLGCODE;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure BeforeDestruction; override;

      procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
      procedure ResetLogicalSize;
      procedure Clear;
      procedure FillColor(color: SDL_Color; alpha: byte);
      procedure DrawRect(const region: TRect; const color: SDL_Color; const alpha: byte = $FF);
      procedure DrawBox(const region: TRect; const color: SDL_Color; const alpha: byte = $FF);
      function AddTexture(surface: PSdlSurface): integer; overload;
      procedure AddTexture(surface: PSdlSurface; const name: string); overload;
      function AddImage(image: TSdlImage): integer; inline;
      procedure DrawTexture(texture: TSdlTexture; src: PSdlRect = nil; dst: PSdlRect = nil); overload;
      procedure DrawTexture(const name: string; src: PSdlRect = nil; dst: PSdlRect = nil); overload;
      procedure DrawTexture(index: integer; src: PSdlRect = nil; dst: PSdlRect = nil); overload;
      procedure Flip;
      function ContainsName(const name: string): boolean;
      function IndexOfName(const name: string): integer;
      function LogicalCoordinates(x, y: integer): TPoint;

      property Available: boolean read FRenderer;
      property SdlWindow: TSdlWindowId read FWindowID;
      property Flags: TSdlWindowFlags read FFlags;
      property TextureByName[name: string]: TSdlTexture read GetTextureByName {write SetTextureByName};
      property Images: TSdlImages read FImageManager;
      property AspectRatio: double read GetAspectRatio;
   published
      property Framerate: word read FFramerate write SetFramerate;
      property Active: boolean read FActive write SetActive;
      property RendererType: TRendererType read FRendererType write FRendererType default rtOpenGL;
      property LogicalWidth: integer read FLogicalWidth write SetLogicalWidth;
      property LogicalHeight: integer read FLogicalHeight write SetLogicalHeight;
      property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
      property OnAvailable: TNotifyEvent read FOnAvailable write FOnAvailable;
   published
      property Align;
      property Anchors;
      property OnClick;
      property OnDblClick;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
   end;

procedure Register;

implementation

uses
   math, forms, windows,
   SDL;

procedure Register;
begin
  RegisterComponents('SDL', [TSdlFrame]);
end;

{ TSdlFrame }

constructor TSdlFrame.Create(AOwner: TComponent);
begin
   inherited;
   FTimer := TTimer.Create(self);
   FTimer.Interval := 100;
   FTimer.OnTimer := Self.InternalOnTimer;
   FRendererType := rtOpenGL;
   self.ControlStyle := [csCaptureMouse,csClickEvents,csOpaque,csDoubleClicks];
   {$IF CompilerVersion >= 21}
   self.ControlStyle := self.ControlStyle + [csGestures];
   {$IFEND}
   self.TabStop := true;
   FImageManager := TSdlImages.Create;
   FLogicalWidth := 1;
   FLogicalHeight := 1;
   if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
      SDL_InitSubSystem(SDL_INIT_VIDEO);
end;

destructor TSdlFrame.Destroy;
begin
   FImageManager.Free;
   FTimer.Free;
   inherited;
end;

procedure TSdlFrame.CreateWnd;
begin
   inherited;
   self.CreateWindow;
end;

procedure TSdlFrame.DestroyWnd;
begin
   FTimer.OnTimer := nil;
   SDL_DestroyRenderer(FWindowID);
   SDL_DestroyWindow(FWindowID);
   FWindowID := 0;
   FFlags := [];
   self.Framerate := 0;
   inherited;
end;

procedure TSdlFrame.DrawBox(const region: TRect; const color: SDL_Color;
  const alpha: byte = $FF);
begin
   if CreateRenderer then
   begin
      assert(SDL_SetRenderDrawColor(color.r, color.g, color.b, alpha) = 0);
      assert(SDL_RenderDrawRect(@region) = 0);
   end;
end;

procedure TSdlFrame.DrawRect(const region: TRect; const color: SDL_Color;
  const alpha: byte = $FF);
begin
   if CreateRenderer then
   begin
      assert(SDL_SetRenderDrawColor(color.r, color.g, color.b, alpha) = 0);
      assert(SDL_RenderDrawLine(region.Left, region.Top, region.Right, region.Top) = 0);
      assert(SDL_RenderDrawLine(region.right, region.Top, region.Right, region.bottom) = 0);
      assert(SDL_RenderDrawLine(region.right, region.bottom, region.left, region.bottom) = 0);
      assert(SDL_RenderDrawLine(region.Left, region.bottom, region.left, region.Top) = 0);
   end;
end;

procedure TSdlFrame.DrawTexture(index: integer; src, dst: PSdlRect);
begin
   drawTexture(FImageManager[index].surface, src, dst);
end;

procedure TSdlFrame.DrawTexture(const name: string; src, dst: PSdlRect);
begin
   drawTexture(FImageManager.Image[name].surface, src, dst);
end;

procedure TSdlFrame.DrawTexture(texture: TSdlTexture; src, dst: PSdlRect);
begin
   if not FRenderer then
      raise EBadHandle.Create('No renderer available.');
   if not ((SDL_SelectRenderer(FWindowID) = 0) and
          (SDL_RenderCopy(texture, src, dst) = 0)) then
      raise EBadHandle.Create(string(SDL_GetError));
end;

procedure TSdlFrame.FillColor(color: SDL_Color; alpha: byte);
begin
   if CreateRenderer then
   begin
      assert(SDL_SetRenderDrawColor(color.r, color.g, color.b, alpha) = 0);
      assert(SDL_RenderFillRect(nil) = 0);
   end;
end;

procedure TSdlFrame.Flip;
begin
   if not (SDL_SelectRenderer(FWindowID) = 0) then
      raise EBadHandle.Create(string(SDL_GetError));
   SDL_RenderPresent;
end;

function TSdlFrame.GetAspectRatio: double;
begin
   result := self.Width / self.Height;
end;

function TSdlFrame.GetTextureByName(name: string): TSdlTexture;
begin
   result := FImageManager.Image[name].surface;
end;

procedure TSdlFrame.Paint;
begin
   if SDL_SelectRenderer(FWindowID) = -1 then
      CreateRenderer;
   self.Flip;
end;

{******************************************************************************}
function TSdlFrame.CreateWindow: boolean;
begin
   if FWindowID = 0 then
      FWindowID := SDL_CreateWindowFrom(self.Handle);
   result := FWindowID <> 0;
   if FWindowID = 0 then
      OutputDebugStringA(SDL_GetError);
end;

function TSdlFrame.CreateRenderer: boolean;
const
   pf: tagPIXELFORMATDESCRIPTOR = (nSize: sizeof(pf); nVersion: 1;
       dwFlags: PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
       iPixelType: PFD_TYPE_RGBA; cColorBits: 24; cAlphaBits: 8;
       iLayerType: PFD_MAIN_PLANE);

   RENDERERS: array[TRendererType] of AnsiString = ('software', 'gdi', 'opengl', 'd3d');
var
   pFormat: integer;
begin
   if (SDL_SelectRenderer(FWindowID) = 0) then
   begin
      result := true;
      Exit;
   end;
   if FRendererType = rtOpenGL then
   begin
      pFormat := ChoosePixelFormat(canvas.Handle, @pf);
      if not SetPixelFormat(canvas.Handle, pFormat, @pf) then
         outputDebugString(PChar(SysErrorMessage(GetLastError)));
      if wglCreateContext(canvas.Handle) = 0 then
         outputDebugString(PChar(SysErrorMessage(GetLastError)));
   end;
   if (SDL_CreateRenderer(FWindowID, SDL_RendererIndex(RENDERERS[FRendererType]), [sdlrPresentFlip3, sdlrAccelerated]) = 0) then
   begin
      SDL_ShowWindow(FWindowID);
      assert(SDL_SetRenderDrawColor(0, 0, 0, 255) = 0);
      SDL_RenderFillRect(nil);
      FFlags := SDL_GetWindowFlags(FWindowID);
      FRenderer := true;
      if assigned(FOnAvailable) then
         FOnAvailable(self);
   end
   else outputDebugString(pChar(format('SDL_CreateRenderer failed: %s', [sdl_GetError])));
   result := SDL_SelectRenderer(FWindowID) = 0;
end;

function TSdlFrame.IndexOfName(const name: string): integer;
begin
   result := FImageManager.IndexOf(name);
end;

procedure TSdlFrame.InternalOnTimer(Sender: TObject);
begin
   if csDestroying in self.ComponentState then
      FTimer.Enabled := false
   else if assigned(FOnTimer) then
      FOnTimer(self);
end;

function TSdlFrame.LogicalCoordinates(x, y: integer): TPoint;
begin
   result.X := trunc(x * self.LogicalWidth / self.Width);
   result.Y := trunc(y * self.LogicalHeight / self.Height);
end;

procedure TSdlFrame.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
   if self.CanFocus then
      Windows.SetFocus(Handle);
   inherited;
end;

function TSdlFrame.ContainsName(const name: string): boolean;
begin
   result := FImageManager.Contains(name);
end;

procedure TSdlFrame.SetActive(const Value: boolean);
begin
   FActive := Value;
   FTimer.Enabled := Value;
end;

procedure TSdlFrame.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
   size: TPoint;
   ratioX, ratioY: double;
begin
   if (AWidth <> self.Width) or (AHeight <> self.Height) then
   begin
      if not FRenderer then
      begin
         FLogicalWidth := aWidth;
         FLogicalHeight := aHeight;
      end
      else begin
         SDL_GetWindowLogicalSize(FWindowID, size.X, size.Y);
         ratioX := size.X / self.Width;
         ratioY := size.Y / self.Height;
         self.LogicalWidth := round(AWidth * ratioX);
         self.LogicalHeight := round(AHeight * ratioY);
      end;
   end;
   inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TSdlFrame.SetFramerate(Value: word);
begin
   Value := min(Value, 100);
   FFramerate := Value;
   if Value = 0 then
      SetActive(false)
   else FTimer.Interval := round(1000 / value);
end;

procedure TSdlFrame.SetLogicalHeight(const Value: integer);
begin
   if FLogicalHeight = Value then
      Exit;

   FLogicalHeight := Value;
   SDL_SetWindowLogicalSize(FWindowID, FLogicalWidth, FLogicalHeight);
   if FRenderer then
      self.Flip;
end;

procedure TSdlFrame.SetLogicalWidth(const Value: integer);
begin
   FLogicalWidth := Value;
   SDL_SetWindowLogicalSize(FWindowID, FLogicalWidth, FLogicalHeight);
   if FRenderer then
      self.Flip;
end;

procedure TSdlFrame.WMGetDlgCode(var msg: TMessage);
begin
   msg.Result := msg.Result or DLGC_WANTCHARS or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

procedure TSdlFrame.ResetLogicalSize;
begin
   FLogicalWidth := self.Width;
   FLogicalHeight := self.Height;
   SDL_SetWindowLogicalSize(FWindowID, FLogicalWidth, FLogicalHeight);
   self.Flip;
end;

function TSdlFrame.AddTexture(surface: PSdlSurface): integer;
begin
   if not assigned(surface) then
      Exit(-1);

   SDL_SelectRenderer(FWindowID);
   result := FImageManager.Add(TSdlImage.Create(surface, '', nil));
end;

function TSdlFrame.AddImage(image: TSdlImage): integer;
begin
  result := FImageManager.Add(image);
end;

procedure TSdlFrame.AddTexture(surface: PSdlSurface; const name: string);
var
   index: integer;
begin
   if self.ContainsName(name) then
      raise EListError.CreateFmt('Texture "%s" already exists', [name]);
   index := FIMageManager.Add(TSdlImage.Create(surface, name, nil));
   if index = -1 then
      raise EInvalidArgument.Create('Invalid surface');
end;

procedure TSdlFrame.BeforeDestruction;
begin
  FTimer.Enabled := false;
  FTimer.OnTimer := nil;
  inherited;
end;

procedure TSdlFrame.Clear;
var
   i: integer;
begin
   if CreateRenderer then
      for I := 1 to 3 do
      begin
         assert(SDL_SetRenderDrawColor(0, 0, 0, 255) = 0);
         assert(SDL_RenderFillRect(nil) = 0);
         SDL_RenderPresent;
      end;
end;

initialization
finalization
SDL_QuitSubSystem(SDL_INIT_VIDEO);

end.
