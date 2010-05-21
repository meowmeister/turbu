unit sdl_ScrollFrame;

interface
uses
   Types, Classes, StdCtrls, ExtCtrls, Controls, Forms, Messages,
   sdl, sdl_13, sdl_frame;

type
   TSdlScrollFrame = class;

   TScrollBarSet = set of TScrollBarKind;
   TFrameScrollEvent = procedure(sender: TSdlScrollFrame; const area: TRect);

   TSdlScrollFrame = class(TCustomControl)
   private
      FPanelH, FPanelCorner, FPanelV: TPanel;
      FSdlFrame: TSdlFrame;
      FVscroll, FHscroll: TScrollBar;
      FScrollBars: TScrollBarSet;
      FSceneHeight: integer;
      FSceneWidth: integer;
      FOnScroll: TFrameScrollEvent;
      FHEqual: boolean;
      FWEqual: boolean;

      procedure WMSize(var Message: TWMSize); message WM_SIZE;
      procedure ScrollBarScroll (Sender: TObject; ScrollCode: TScrollCode;
                var ScrollPos: Integer);

      function IsAvailable: boolean;
      function GetSdlWindow: TSdlWindowId;
      function GetFlags: TSdlWindowFlags;
      procedure SetScrollBars(const Value: TScrollBarSet);
      function GetFramerate: word;
      procedure SetFramerate(const Value: word);
      function GetActive: boolean;
      procedure SetActive(const Value: boolean);
      function GetRendererType: TRendererType;
      procedure SetRendererType(const Value: TRendererType);
      function GetOnTimer: TNotifyEvent;
      procedure SetOnTimer(const Value: TNotifyEvent);
      function GetOnAvailable: TNotifyEvent;
      procedure SetOnAvailable(const Value: TNotifyEvent);
      procedure SetSceneHeight(const Value: integer);
      procedure SetSceneWidth(const Value: integer);
   public
      constructor Create(AOwner: TComponent); override;
      procedure Clear;
      procedure FillColor(color: SDL_Color; alpha: byte);
      function AddTexture(surface: PSdlSurface): integer;
      procedure DrawTexture(texture: TSdlTexture; src: PSdlRect = nil; dst: PSdlRect = nil);
      procedure Flip;

      property Available: boolean read IsAvailable;
      property SdlWindow: TSdlWindowId read GetSdlWindow;
      property Flags: TSdlWindowFlags read GetFlags;
   published
      property Framerate: word read GetFramerate write SetFramerate;
      property Active: boolean read GetActive write SetActive;
      property RendererType: TRendererType read GetRendererType write SetRendererType default rtOpenGL;
      property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
      property OnAvailable: TNotifyEvent read GetOnAvailable write SetOnAvailable;
      property ScrollBars: TScrollBarSet read FScrollBars write SetScrollBars default [sbHorizontal, sbVertical];
      property SceneHeight: integer read FSceneHeight write SetSceneHeight;
      property SceneWidth: integer read FSceneWidth write SetSceneWidth;
   published
      property Align;
      property Anchors;
   end;

procedure Register;

implementation
uses
   math;

procedure Register;
begin
  RegisterComponents('SDL', [TSdlScrollFrame]);
end;

{ TSdlScrollFrame }

constructor TSdlScrollFrame.Create(AOwner: TComponent);
var
   component: TComponent;
begin
   inherited Create(AOwner);
   self.BevelOuter := bvNone;

   FPanelH := TPanel.Create(self);
   FPanelH.parent := self;
   FPanelH.Align := alBottom;
   FPanelH.Height := 17;
   FPanelH.BevelOuter := bvNone;

   FPanelCorner := TPanel.Create(self);
   FPanelCorner.Parent := FPanelH;
   FPanelCorner.Align := alRight;
   FPanelCorner.Width := 17;
   FPanelCorner.BevelOuter := bvLowered;

   FHScroll := TScrollBar.Create(FPanelH);
   FHScroll.parent := FPanelH;
   FHScroll.Align := alClient;

   FPanelV := TPanel.Create(self);
   FPanelV.Parent := self;
   FPanelV.Align := alRight;
   FPanelV.Width := 17;
   FPanelV.BevelOuter := bvNone;

   FVScroll := TScrollBar.Create(FPanelV);
   FVScroll.parent := FPanelV;
   FVScroll.Align := alClient;
   FVScroll.Kind := sbVertical;

   FSdlFrame := TSdlFrame.Create(self);
   FSdlFrame.parent := self;
   FSdlFrame.Align := alClient;

   FScrollBars := [sbHorizontal, sbVertical];
   {$R-}
   self.SceneHeight := self.Height - FPanelH.Height;
   self.SceneWidth := self.Width - FPanelV.Width;
   FVscroll.PageSize := SceneHeight;
   FHscroll.PageSize := SceneWidth;
   FVscroll.LargeChange := SceneHeight;
   FHscroll.LargeChange := SceneWidth;
   {$R+}
   FHEqual := true;
   FWEqual := true;

   for component in self do
      TControl(component).ControlStyle := TControl(component).ControlStyle + [csNoDesignVisible] - [csAcceptsControls];

   FHScroll.OnScroll := self.ScrollBarScroll;
   FVScroll.OnScroll := self.ScrollBarScroll;
end;

function TSdlScrollFrame.AddTexture(surface: PSdlSurface): integer;
begin
   result := FSdlFrame.AddTexture(surface);
end;

procedure TSdlScrollFrame.Clear;
begin
   FSdlFrame.Clear;
end;

procedure TSdlScrollFrame.DrawTexture(texture: TSdlTexture; src, dst: PSdlRect);
begin
   FSdlFrame.DrawTexture(texture, src, dst);
end;

procedure TSdlScrollFrame.FillColor(color: SDL_Color; alpha: byte);
begin
   FSdlFrame.FillColor(color, alpha);
end;

procedure TSdlScrollFrame.Flip;
begin
   FSdlFrame.Flip;
end;

function TSdlScrollFrame.GetFlags: TSdlWindowFlags;
begin
   result := FSdlFrame.Flags;
end;

function TSdlScrollFrame.GetSdlWindow: TSdlWindowId;
begin
   result := FSdlFrame.SdlWindow;
end;

function TSdlScrollFrame.IsAvailable: boolean;
begin
   result := FSdlFrame.Available;
end;

function TSdlScrollFrame.GetFramerate: word;
begin
   result := FSdlFrame.Framerate;
end;

function TSdlScrollFrame.GetActive: boolean;
begin
   result := FSdlFrame.Active;
end;

procedure TSdlScrollFrame.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
   sceneRect: TRect;
begin
   if not assigned(FOnScroll) then
      Exit;

   sceneRect := rect(FHscroll.Position, FVscroll.Position, FSceneHeight, FSceneWidth);
   if sender = FHscroll then
      sceneRect.Left := ScrollPos
   else if sender = FVscroll then
      sceneRect.Top := ScrollPos
   else assert(false, 'ScrollBarScroll called from an invalid Sender');
   FOnScroll(self, sceneRect);
end;

procedure TSdlScrollFrame.SetActive(const Value: boolean);
begin
   FSdlFrame.Active := Value;
end;

procedure TSdlScrollFrame.SetFramerate(const Value: word);
begin
   FSdlFrame.Framerate := value;
end;

function TSdlScrollFrame.GetOnAvailable: TNotifyEvent;
begin
   result := FSdlFrame.OnAvailable;
end;

procedure TSdlScrollFrame.SetOnAvailable(const Value: TNotifyEvent);
begin
   FSdlFrame.OnAvailable := Value;
end;

function TSdlScrollFrame.GetOnTimer: TNotifyEvent;
begin
   result := FSdlFrame.OnTimer;
end;

procedure TSdlScrollFrame.SetOnTimer(const Value: TNotifyEvent);
begin
   FSdlFrame.OnTimer := Value;
end;

function TSdlScrollFrame.GetRendererType: TRendererType;
begin
   result := FSdlFrame.RendererType;
end;

procedure TSdlScrollFrame.SetRendererType(const Value: TRendererType);
begin
   FSdlFrame.RendererType := value;
end;

procedure TSdlScrollFrame.SetSceneHeight(const Value: integer);
begin
   FSceneHeight := max(Value, 0);
   FVScroll.PageSize := min(FVScroll.PageSize, FSceneHeight);
   FVscroll.Max := FSceneHeight;
   if self.HandleAllocated then
      FHEqual := FSceneHeight = self.ClientHeight;
end;

procedure TSdlScrollFrame.SetSceneWidth(const Value: integer);
begin
   FSceneWidth := max(Value, 0);
   FHScroll.PageSize := min(FHScroll.PageSize, FSceneWidth);
   FHscroll.Max := FSceneWidth;
   if self.HandleAllocated then
      FWEqual := FSceneWidth = self.ClientWidth;
end;

procedure TSdlScrollFrame.SetScrollBars(const Value: TScrollBarSet);
begin
   FScrollBars := Value;
   FPanelH.Visible := sbHorizontal in Value;
   FPanelV.Visible := sbVertical in Value;
   FPanelCorner.Visible := value = [sbHorizontal, sbVertical];
end;

procedure TSdlScrollFrame.WMSize(var Message: TWMSize);
begin
   inherited;
   if FHEqual or (self.ClientHeight >= FSceneHeight) then
      SetSceneHeight(self.ClientHeight);
   if FWEqual or (self.ClientWidth >= FSceneWidth) then
      SetSceneWidth(self.ClientWidth);
   FVscroll.PageSize := min(message.Height, self.ClientHeight);
   FHscroll.PageSize := min(message.Width, self.ClientWidth);
   FVscroll.LargeChange := FVscroll.PageSize;
   FHscroll.LargeChange := FHscroll.PageSize;
end;

end.
