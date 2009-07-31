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
   Classes, Controls, ExtCtrls, Messages, SysUtils,
   SDL_13 {$IF CompilerVersion >= 20}, Generics.Collections{$IFEND};

type
   TTextureList = {$IF CompilerVersion >= 20} TList<TSdlTexture>; {$ELSE} class(TList)
   protected
      function Get(Index: Integer): TSdlTexture; inline;
      procedure Put(Index: Integer; Item: TSdlTexture); inline;
   public
      function Add(Item: TSdlTexture): Integer; inline;
      property Items [Index: Integer]: TSdlTexture read Get write Put; default;
   end;
   {$IFEND}

   TSdlFrame = class(TCustomControl)
   private
      FWindowID: TSdlWindowId;
      FFlags: TSdlWindowFlags;
      FTimer: TTimer;
      FFramerate: word;
      FActive: boolean;
      FRenderer: boolean;

      FBuffer: TSdlTexture;
      FTextureList: TTextureList;
      FOnTimer: TNotifyEvent;
      FOnAvailable: TNotifyEvent;

      function CreateWindow: boolean;
      function CreateRenderer: boolean;
      procedure InternalOnTimer(Sender: TObject);
      procedure SetFramerate(Value: word);
      procedure SetActive(const Value: boolean);
   protected
      procedure CreateWnd; override;
      procedure DestroyWnd; override;
      procedure Paint; override;
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure BeforeDestruction; override;

      procedure Clear;
      procedure FillColor(color: SDL_Color; alpha: byte);
      procedure assignImage(image: PSdlSurface); deprecated;
      function AddTexture(surface: PSdlSurface; out texture: TSdlTexture): integer;
      procedure DrawTexture(texture: TSdlTexture; src: PSdlRect = nil; dst: PSdlRect = nil);
      procedure Flip;

      property Available: boolean read FRenderer;
      property SdlWindow: TSdlWindowId read FWindowID;
      property Flags: TSdlWindowFlags read FFlags;
      property Textures: TTextureList read FTextureList;
   published
      property Framerate: word read FFramerate write SetFramerate;
      property Active: boolean read FActive write SetActive;
      property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
      property OnAvailable: TNotifyEvent read FOnAvailable write FOnAvailable;
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
   FTextureList := TTextureList.Create;
   FTimer.OnTimer := Self.InternalOnTimer;
end;

destructor TSdlFrame.Destroy;
begin
   FTimer.Free;
   FTextureList.Free;
   inherited;
end;

procedure TSdlFrame.CreateWnd;
begin
   inherited;
   if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
      SDL_InitSubSystem(SDL_INIT_VIDEO);
   self.CreateWindow;
end;

procedure TSdlFrame.DestroyWnd;
begin
   FBuffer.Free;
   FTimer.OnTimer := nil;
   SDL_DestroyRenderer(FWindowID);
   SDL_DestroyWindow(FWindowID);
   FWindowID := 0;
   FFlags := [];
   self.Framerate := 0;
   inherited;
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
      assert(SDL_RenderFill(nil) = 0);
      SDL_RenderPresent;
   end;
end;

procedure TSdlFrame.Flip;
begin
   if not (SDL_SelectRenderer(FWindowID) = 0) then
      raise EBadHandle.Create(string(SDL_GetError));
   SDL_RenderPresent;
end;

procedure TSdlFrame.Paint;
begin
   if SDL_SelectRenderer(FWindowID) = -1 then
      CreateRenderer;
   SDL_RenderPresent;
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
   SDL_BLACK: sdl_13.TSDL_Color = (r: 0; g: 0; b:0; unused: 0);
begin
   if (SDL_SelectRenderer(FWindowID) = 0) then
   begin
      result := true;
      Exit;
   end;
   if (SDL_CreateRenderer(FWindowID, SDL_RendererIndex('opengl'), [sdlrPresentFlip3, sdlrAccelerated]) = 0) then
   begin
      SDL_ShowWindow(FWindowID);
      assert(SDL_SetRenderDrawColor(0, 0, 0, 0) = 0);
      assert(SDL_RenderFill(nil) = 0);
      FFlags := SDL_GetWindowFlags(FWindowID);
      FBuffer := TSdlTexture.Create(0, sdltaStreaming, self.Width, self.Height);
      FRenderer := true;
      fillColor(SDL_BLACK, 255);
      if assigned(FOnAvailable) then
         FOnAvailable(self);
   end
   else outputDebugString(pChar(format('SDL_CreateRenderer failed: %s', [sdl_GetError])));
   result := SDL_SelectRenderer(FWindowID) = 0;
end;

procedure TSdlFrame.InternalOnTimer(Sender: TObject);
begin
   if csDestroying in self.ComponentState then
      FTimer.Enabled := false
   else if assigned(FOnTimer) then
      FOnTimer(self);
end;

procedure TSdlFrame.SetActive(const Value: boolean);
begin
   FActive := Value;
   FTimer.Enabled := Value;
end;

procedure TSdlFrame.SetFramerate(Value: word);
begin
   Value := min(Value, 100);
   FFramerate := Value;
   if Value = 0 then
      SetActive(false)
   else FTimer.Interval := round(1000 / value);
end;

function TSdlFrame.AddTexture(surface: PSdlSurface; out texture: TSdlTexture): integer;
begin
   if not assigned(surface) then
      Exit(-1);

   texture := TSdlTexture.Create(0, surface);
   result := FTextureList.Add(texture);
end;

procedure TSdlFrame.assignImage(image: PSdlSurface);
begin
   if not assigned(image) then
      Exit;

   FBuffer.Free;
   FBuffer.Create(0, image);
   if SDL_SelectRenderer(FWindowID) = 0 then
   begin
      SDL_RenderCopy(FBuffer, nil, nil);
      SDL_RenderPresent;
   end;
end;

procedure TSdlFrame.BeforeDestruction;
begin
  FTimer.Enabled := false;
  FTimer.OnTimer := nil;
  inherited;
end;

procedure TSdlFrame.Clear;
begin
   if CreateRenderer then
   begin
      assert(SDL_SetRenderDrawColor(0, 0, 0, 255) = 0);
      assert(SDL_RenderFill(nil) = 0);
      SDL_RenderPresent;
   end;
end;

{$IF CompilerVersion < 20}
{ TTextureList }

function TTextureList.Add(Item: TSdlTexture): Integer;
begin
   inherited Add(pointer(item.ID));
end;

function TTextureList.Get(Index: Integer): TSdlTexture;
begin
   result.id := TSdlTextureID(inherited Get(index));
end;

procedure TTextureList.Put(Index: Integer; Item: TSdlTexture);
begin
   inherited Put(index, pointer(item.ID));
end;
{$IFEND}

initialization
finalization
SDL_QuitSubSystem(SDL_INIT_VIDEO);

end.
