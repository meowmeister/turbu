unit tiles;
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
   types, classes, Generics.Collections, OpenGL,
   commons, timing, charset_data, turbu_maps, turbu_tilesets, turbu_map_objects,
   turbu_defs, dm_shaders,
   SDL_sprite, SG_Defs;

type
   TTile = class;
   TNeighborSet = set of TDirs8;

   TBroadcastProc = procedure of object;
   TMustFlashEvent = function: boolean of object;
   TGetFlashColorEvent = function: TGlArrayF4 of object;

   TTile = class abstract(TParentSprite)
   protected
   class var
      FHeartbeat: integer;
      FBroadcastList: TList<TBroadcastProc>;
      class constructor Create;
      class destructor Destroy;
      class procedure EnsureBroadcastList;
   private
      FGridLoc: TSgPoint; //where this tile is located on the map
      FTerrainID: word;
      FAttributes: TTileAttributes;
   protected
      FTileID: word; // which tile from the overall x,y grid of the chipset
      function InVisibleRect: boolean; override;
      procedure DoDraw; override;
      procedure AdjustOverlap(overlap: TFacingSet);
      procedure setEngine(newEngine: TSpriteEngine); virtual;
   public
      constructor Create(const AParent: TSpriteEngine; tileset: string); reintroduce; overload; virtual;
      procedure Assign(const value: TTile); reintroduce;
      function place(const xCoord,yCoord, layer: word; const tileData: TTileRef;
                     chip_data: TTileSet): TTileAttributes; virtual;
      function open(exceptFor: TObject): boolean; virtual;
      function canEnter: boolean;
      procedure UpdateGridLoc;

      class procedure heartbeat;

      property id: word read FTileID write FTileID;
      property location: TSgPoint read FGridLoc write FGridLoc;
      property terrain: word read FTerrainID write FTerrainID;
      property attributes: TTileAttributes read FAttributes write FAttributes;
   end;

   TEventTile = class(TTile)
   private
      FEvent: TRpgMapObject;
      FOnMustFlash: TMustFlashEvent;
      FOnGetFlashColor: TGetFlashColorEvent;
      procedure DrawFlash;
      procedure DrawSelf(SpriteRect: TRect);
      procedure PrepareShader(shaders: TdmShaders);
      function MustFlash: boolean;
   public
      constructor Create(baseEvent: TRpgMapObject; const AParent: TSpriteEngine); reintroduce;
      procedure assign(data: TEventTile); reintroduce;
      procedure update(newPage: TRpgEventPage);

      procedure Draw; override;
      property event: TRpgMapObject read FEvent write FEvent;
      property OnMustFlash: TMustFlashEvent read FOnMustFlash write FOnMustFlash;
      property OnFlashColor: TGetFlashColorEvent read FOnGetFlashColor write FOnGetFlashColor;
   end;

   TScrollData = class(TObject)
   private
      FX: shortint;
      FY: shortint;
      FAutoX: boolean;
      FAutoY: boolean;
   public
      constructor Create(input: TRpgMap); overload;
      constructor Create(x, y: shortint; autoX, autoY: boolean); overload;

      property x: shortint read FX write FX;
      property y: shortint read FY write FY;
      property autoX: boolean read FAutoX write FAutoX;
      property autoY: boolean read FAutoY write FAutoY;
   end;

   TBackgroundSprite = class(TSprite)
   private
      FScroll: TScrollData;
      FSavedOrigin: TSgFloatPoint;
   protected
      function InVisibleRect: boolean; override;
   public
      constructor Create(parent: TSpriteEngine; input: TRpgMap); reintroduce; overload;
      constructor Create(parent: TSpriteEngine; x, y: shortint; autoX, autoY: boolean); reintroduce; overload;
      destructor Destroy; override;
      procedure scroll;

      property scrollData: TScrollData read FScroll write FScroll;
   end;

const
   BG_SCROLL_RATE = 0.1;
   ANIM_LCM = 8 * 9 * 5 * 7 * 11 ; //least common multiple of all numbers 1..12

implementation

uses
   SysUtils, Math,
   turbu_constants, turbu_2k_sprite_engine, turbu_OpenGL,
   SDL_ImageManager;

constructor TTile.Create(const AParent: TSpriteEngine; tileset: string);
begin
   assert(AParent is T2kSpriteEngine);
   inherited create(AParent);
   imageName := tileset;
end;

procedure TTile.assign(const value: TTile);
begin
   self.FTileID := value.id;
   self.FGridLoc := value.location;
   self.FEngine := value.engine;
   self.FAttributes := value.FAttributes;

   inherited assign(value);
end;

function TTile.canEnter: boolean;
var i: TTileAttribute;
begin
   result := false;
   for i := taUp to taRight do
      result := result or (i in FAttributes);
end;

class constructor TTile.Create;
begin
   EnsureBroadcastList;
end;

class destructor TTile.Destroy;
begin
   FBroadcastList.Free;
end;

class procedure TTile.EnsureBroadcastList;
begin
   if not assigned(FBroadcastList) then
      FBroadcastList := TList<TBroadcastProc>.Create;
end;

procedure TTile.AdjustOverlap(overlap: TFacingSet);
var
   viewport: TRect;
   mapSize: TSgFloatPoint;
begin
   viewport := T2kSpriteEngine(FEngine).viewport;
   mapSize := TSgPoint(T2kSpriteEngine(FEngine).mapRect.BottomRight) * TILE_SIZE;
   if facing_left in overlap then
   begin
      if FGridLoc.X > viewport.Right then
         self.X := self.X - mapSize.x;
   end
   else if facing_right in overlap then
   begin
      if FGridLoc.X < viewport.Left then
         self.X := self.X + mapSize.x;
   end;
   if facing_up in overlap then
   begin
      if FGridLoc.Y > viewport.Bottom then
         self.Y := self.Y - mapSize.Y;
   end
   else if facing_down in overlap then
   begin
      if FGridLoc.Y < viewport.Top then
         self.Y := self.Y + mapSize.Y;
   end;
end;

{$WARN USE_BEFORE_DEF OFF}
procedure TTile.DoDraw;
var
   lX, lY: single;
   overlap: TFacingSet;
begin
   overlap := T2kSpriteEngine(FEngine).overlapping;
   if overlap <> [] then
   begin
      lX := self.X;
      lY := self.Y;
      adjustOverlap(overlap);
   end;
   inherited DoDraw;
   if overlap <> [] then
   begin
      self.X := lX;
      self.Y := lY;
   end;
end;
{$WARN USE_BEFORE_DEF ON}

function TTile.InVisibleRect: boolean;

   function NormalizePoint(aPoint: TSgPoint; aRect: TRect): TSgPoint;
   var
      Width, Height: integer;
   begin
      Width  := aRect.Right - aRect.Left;
      Height := aRect.Bottom - aRect.Top;

      if (aPoint.X< aRect.Left) then
         inc(aPoint.X,Width )
      else if (aPoint.X>=aRect.Right) then
         dec(aPoint.X,Width );

      if (aPoint.Y< aRect.Top) then
         inc(aPoint.Y,Height)
      else if (aPoint.Y>=aRect.Bottom) then
         dec(aPoint.Y,Height);
      Result := aPoint;
   end;

var
   corrected: TSgPoint;
begin
   if T2kSpriteEngine(FEngine).overlapping = [] then
      result := inherited InVisibleRect
   else begin
      corrected := NormalizePoint(FGridLoc, T2kSpriteEngine(FEngine).mapRect) * TILE_SIZE;

      result := (corrected.X >= FEngine.WorldX - (Width * 2)) and
      (corrected.Y >= FEngine.WorldY - (Height * 2)) and
      (corrected.X < FEngine.WorldX + FEngine.VisibleWidth + Width)  and
      (corrected.Y < FEngine.WorldY + FEngine.VisibleHeight + Height);
   end;
end;

class procedure TTile.heartbeat;
var
   proc: TBroadcastProc;
begin
   FHeartbeat := (FHeartbeat + 1) mod ANIM_LCM;
   for proc in FBroadcastList do
      proc;
end;

function TTile.open(exceptFor: TObject): boolean;
begin
   result := Self.canEnter;
end;

function DecodeZOrder(const value: TTileAttributes): byte;
begin
   if taOverhang in value then
      result := 10
   else if taCeiling in value then
      result := 6
   else result := 1;
end;

function TTile.place(const xCoord, yCoord, layer: word; const tileData: TTileRef;
                    chip_data: TTileSet): TTileAttributes;
var
   tileGroup: TTileGroupRecord;
begin
   X := TILE_SIZE.X * xCoord;
   Y := TILE_SIZE.Y * yCoord;
   FGridLoc.X := xCoord;
   FGridLoc.Y := yCoord;
   self.Width := TILE_SIZE.X;
   self.Height := TILE_SIZE.Y;

   self.ImageIndex := tileData.tile;
   tileGroup := chip_data.Records[tiledata.group];
   imageName := tileGroup.group.filename;
   result := tileGroup.attributes[tileData.tile];
   fAttributes := result;
   z := decodeZOrder(result) + layer;
end;

procedure TTile.setEngine(newEngine: TSpriteEngine);
begin
   FEngine := newEngine;
end;

procedure TTile.UpdateGridLoc;
begin
   FGridLoc := sgPoint(round(self.X / TILE_SIZE.x), round(self.y / TILE_SIZE.y));
end;

{ TEventTile }

procedure TEventTile.assign(data: TEventTile);
begin
   inherited assign(TTile(data));
end;

constructor TEventTile.Create(baseEvent: TRpgMapObject; const AParent: TSpriteEngine);
begin
   inherited Create(AParent, '');
   if assigned(baseEvent) then
   begin
      self.X := baseEvent.location.X * TILE_SIZE.X;
      self.Y := baseEvent.location.Y * TILE_SIZE.Y;
      FEvent := baseEvent;
      update(baseEvent.currentPage);
   end;
end;

procedure TEventTile.PrepareShader(shaders: TdmShaders);
var
   handle: integer;
begin
   handle := shaders.ShaderProgram('default', 'flash');
   shaders.UseShaderProgram(handle);
   shaders.SetUniformValue(handle, 'flashColor', FOnGetFlashColor());
   glClearStencil(0);
end;

procedure TEventTile.DrawSelf(SpriteRect: TRect);
var
   top, left: integer;
begin
   left := trunc(self.X + OffsetX - FEngine.WorldX);
   top := trunc(self.Y + OffsetY - FEngine.WorldY);

   glBindTexture(GL_TEXTURE_RECTANGLE_ARB, Self.Image.surface.handle);
   glEnable(GL_BLEND);
   glEnable(GL_ALPHA_TEST);
   glBegin(GL_QUADS);
      glTexCoord2i(spriteRect.Left,                    spriteRect.Top);
      glVertex2f(left,                                 top);
      glTexCoord2i(spriteRect.Left,                    spriteRect.Top + spriteRect.Bottom);
      glVertex2f(left,                                 top + self.Height);
      glTexCoord2i(spriteRect.Left + spriteRect.Right, spriteRect.Top + spriteRect.Bottom);
      glVertex2f(left + self.Width,                    top + self.Height);
      glTexCoord2i(spriteRect.Left + spriteRect.Right, spriteRect.Top);
      glVertex2f(left + self.Width,                    top);
   glEnd;
end;

procedure TEventTile.DrawFlash;
var
   current: integer;
begin
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   glPushAttrib(GL_CURRENT_BIT);
   PrepareShader(GSpriteEngine.ShaderEngine);
   DrawSelf(self.Image.spriteRect[self.ImageIndex]);
   glUseProgram(current);
   glPopAttrib;
end;

function TEventTile.MustFlash: boolean;
begin
   result := assigned(FOnMustFlash) and assigned(FOnGetFlashColor) and FOnMustFlash();
end;

procedure TEventTile.Draw;
begin
   if MustFlash then
      DrawFlash
   else inherited Draw;
end;

procedure TEventTile.update(newPage: TRpgEventPage);
const
   Z_TABLE: array [0..2] of integer = (3, 4, 8);
var
   x, y, z: integer;
   engine: T2kSpriteEngine;
   name: string;
begin
   engine := FEngine as T2kSpriteEngine;
   if assigned(newpage) then
   begin
      self.Visible := true;
      z := Z_TABLE[newpage.zOrder];
      x := trunc(self.X / TILE_SIZE.X);
      y := trunc(self.Y / TILE_SIZE.Y);
      self.z := max(z, engine.GetTopTile(x, y).Z + 1);
      FGridLoc := SgPoint(x, y);
      self.Visible := true;
   end
   else begin
      self.z := 10;
      self.visible := false;
   end;
   if assigned(FEvent) and assigned(FEvent.currentPage) then
   begin
      self.ImageIndex := FEvent.currentPage.whichTile;
      if FEvent.currentPage.TileGroup <> -1 then
         name := engine.tileset.Records[FEvent.currentPage.TileGroup].group.name
      else name := FEvent.CurrentPage.Name;
      engine.EnsureImage(name);
   end
   else name := '';
   self.ImageName := name;
end;

{ TBackgroundSprite }

constructor TBackgroundSprite.Create(parent: TSpriteEngine; input: TRpgMap);
begin
   inherited Create(parent);
   FScroll := TScrollData.Create(input);
end;

constructor TBackgroundSprite.Create(parent: TSpriteEngine; x, y: shortint; autoX, autoY: boolean);
begin
   inherited Create(parent);
   FScroll := TScrollData.Create(x, y, autoX, autoY);
end;

destructor TBackgroundSprite.Destroy;
begin
   FScroll.Free;
   inherited;
end;

function TBackgroundSprite.InVisibleRect: boolean;
begin
   result := true;
end;

procedure TBackgroundSprite.scroll;
begin
   if (FScroll.FAutoX) and (engine.WorldX <> FSavedOrigin.x) then
      self.OffsetX := self.OffsetX + ((engine.worldX - FSavedOrigin.x) / 2)
   else self.OffsetX := self.OffsetX + (FScroll.FX * BG_SCROLL_RATE);

   if (FScroll.FAutoY) and (engine.WorldY <> FSavedOrigin.Y) then
      self.OffsetY := self.OffsetY + ((engine.worldY - FSavedOrigin.Y) / 2)
   else Self.OffsetY := Self.OffsetY + (FScroll.FY * BG_SCROLL_RATE);

   FSavedOrigin := sgPointF(engine.WorldX, engine.WorldY);
   while self.OffsetX > 0 do
      self.offsetX := self.offsetX - self.PatternWidth;

   while self.OffsetX < -self.patternWidth do
      self.offsetX := self.offsetX + self.PatternWidth;

   while self.OffsetY > 0 do
      self.OffsetY := self.OffsetY - self.PatternHeight;

   while self.OffsetY < -self.PatternHeight do
      self.OffsetY := self.OffsetY + self.PatternHeight;
end;

{ TScrollData }

constructor TScrollData.Create(input: TRpgMap);
begin
   FX := input.scrollSpeed.x;
   FY := input.scrollSpeed.y;
   FAutoX := input.hScroll = stAutoscroll;
   FAutoY := input.vScroll = stAutoscroll;
end;

constructor TScrollData.Create(x, y: shortint; autoX, autoY: boolean);
begin
   FX := x;
   FY := y;
   FAutoX := autoX;
   FAutoY := autoY;
end;

end.
