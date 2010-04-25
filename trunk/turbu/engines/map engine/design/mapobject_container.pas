unit mapobject_container;

interface
uses
   Generics.Collections,
   tiles, turbu_map_sprites,
   sdl_sprite, sdl_canvas;

type
   TMapObjectContainer = class(TParentSprite)
   private
      FMainTile: TTile;
      FCanvas: TSdlCanvas;
   public
      constructor Create(base: TMapSprite; canvas: TSdlCanvas); reintroduce;
      destructor Destroy; override;
      procedure Draw; override;
   end;

   TMapObjectContainerList = class(TObjectList<TMapObjectContainer>);

implementation
uses
   types,
   turbu_constants,
   sg_utils, sg_defs, sdl_13;

{ TMapObjectContainer }

constructor TMapObjectContainer.Create(base: TMapSprite; canvas: TSdlCanvas);
begin
   inherited Create(base.baseTile.Parent);
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
end;

destructor TMapObjectContainer.Destroy;
var
   i: integer;
begin
   for i := self.Count - 1 downto 0 do
      self[i].Parent := self.Parent;
   inherited Destroy;
end;

procedure TMapObjectContainer.Draw;

   function normalizeRect(const drawRect: TRect): TRect;
   begin
      result.Left := drawRect.Left - trunc(self.Engine.WorldX);
      result.Right := (drawRect.Right - trunc(self.Engine.WorldX)) + result.left;
      result.Top := drawRect.Top - trunc(self.Engine.WorldY);
      result.Bottom := drawRect.Bottom - trunc(self.Engine.WorldY) + result.top;
   end;

var
   drawRect: TRect;
   key: TSDL_Color;
begin
   drawRect := normalizeRect(rect(trunc(self.X), trunc(self.Y), self.Width, self.Height));
   FCanvas.drawBox(constrictRect(drawRect, 2), SDL_WHITE);
   drawRect := TRectToSdlRect(constrictRect(drawRect, 3));
   if assigned(FMainTile.Image) then
   begin
      key := FMainTile.Image.colorkey;
      SDL_SetRenderDrawColor(key.r, key.g, key.b, $ff);
      SDL_RenderFillRect(@drawRect);
      FMainTile.drawTo(drawRect);
   end
   else begin
      SDL_SetRenderDrawColor(SDL_GREEN);
      SDL_RenderFillRect(@drawRect);
   end;
end;

end.
