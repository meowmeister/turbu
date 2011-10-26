unit mapobject_container;

interface
uses
   turbu_containers,
   tiles, turbu_map_sprites,
   sdl_sprite, sdl_canvas;

type
   TMapObjectContainer = class(TParentSprite)
   private
      FBase: TMapSprite;
      FMainTile: TTile;
      FCanvas: TSdlCanvas;
   public
      constructor Create(base: TMapSprite; canvas: TSdlCanvas); reintroduce;
      destructor Destroy; override;
      procedure DoDraw; override;
      procedure Draw; override;
      property base: TMapSprite read FBase;
   end;

   TMapObjectContainerList = class(TRpgObjectList<TMapObjectContainer>);

implementation
uses
   types, SysUtils,
   turbu_constants,
   sg_utils, sg_defs, sdl_13;

{ TMapObjectContainer }

var
   counter: integer = 0;

constructor TMapObjectContainer.Create(base: TMapSprite; canvas: TSdlCanvas);
begin
   inherited Create(base.baseTile.Parent);
   self.Name := format('Map Object Container %d', [counter]);
   inc(counter);
   base.tiles[1].parent := self;
   if assigned(base.tiles[2]) then
   begin
      FMainTile := base.tiles[2];
      base.tiles[2].parent := self;
   end
   else FMainTile := base.tiles[1];
   self.X := (trunc(base.baseTile.X + (TILE_SIZE.X div 2)) div TILE_SIZE.x) * TILE_SIZE.x;
   self.Y := base.baseTile.Y;
   self.Width := TILE_SIZE.x;
   self.Height := TILE_SIZE.y;
   self.Z := 11;
   FCanvas := canvas;
   FBase := base;
   FRenderSpecial := true;
end;

destructor TMapObjectContainer.Destroy;
var
   i: integer;
begin
   for i := self.Count - 1 downto 0 do
      self[i].Parent := self.Parent;
   inherited Destroy;
end;

procedure TMapObjectContainer.DoDraw;

   function normalizeRect(const drawRect: TRect): TRect;
   begin
      result.Left := drawRect.Left - trunc(self.Engine.WorldX);
      result.Top := drawRect.Top - trunc(self.Engine.WorldY);
      result.BottomRight := drawRect.BottomRight;
   end;

var
   drawRect: TRect;
   key: TSDL_Color;
   renderer: TSdlRenderer;
begin
   renderer := FCanvas.Renderer;
   drawRect := normalizeRect(rect(trunc(self.X), trunc(self.Y), self.Width, self.Height));
   FCanvas.drawBox(constrictSdlRect(drawRect, 2), SDL_WHITE);
   drawRect := constrictSdlRect(drawRect, 3);
   if assigned(FMainTile.Image) then
   begin
      key := FMainTile.Image.colorkey;
      SDL_SetRenderDrawColor(renderer, key.r, key.g, key.b, $ff);
      SDL_RenderFillRect(renderer, @drawRect);
      FMainTile.drawTo(drawRect);
   end
   else begin
      SDL_SetRenderDrawColor(renderer, SDL_GREEN);
      SDL_RenderFillRect(renderer, @drawRect);
   end;
end;

procedure TMapObjectContainer.Draw;
begin
   if self.InVisibleRect then
      DoDraw;
end;

end.
