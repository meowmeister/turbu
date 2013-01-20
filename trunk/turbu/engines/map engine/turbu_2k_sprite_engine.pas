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
unit turbu_2k_sprite_engine;

interface
uses
   types, Generics.Collections,
   commons, timing, tiles, dm_shaders,
   turbu_2k_sprite_list, turbu_2k_frames,
   turbu_maps, turbu_map_engine, turbu_2k_map_tiles, turbu_tilesets,
   turbu_map_objects, turbu_map_sprites, turbu_defs,
   sdl_sprite, sdl_canvas, SDL_ImageManager, SG_defs;

type
   TTileMatrix = TMatrix<TMapTile>;
   TTileMatrixList = class(TObjectList<TTileMatrix>);

   T2kSpriteEngine = class(TSpriteEngine)
   private
      FMap: TRpgMap;
      FBgImage: TBackgroundSprite;
      FTiles: TTileMatrixList;
      FTileset: TTileset;
      FOverlapping: TFacingSet;
      FViewport: TRect;
      FMapRect: TRect;
      FCurrentLayer: integer;
      FBlank: boolean;
      FMapObjects: TMapSpriteList;
      FSpriteLocations: TSpriteLocations;
      FCurrentParty: TCharSprite;

      //visual effects
      FShaderEngine: TdmShaders;
      FFadeColor: TGLColor;
      FFadeTarget: TGLColor;
      FFadeTime: TRpgTimestamp;
      FSystemGraphic: TSystemImages;
      FFlashColor: TGLColor;
      FFlashTime: TRpgTimestamp;
      FFlashDuration: integer;

      //displacement control
      FPanSpeed: single;
      FDisplacing: boolean;
      FReturning: boolean;
      FDestination: TPoint;
      FDispGoalX, FDispGoalY: single;
      FDisplacementX, FDisplacementY: single;
      FDisplacementSpeed: single;

      //shake control
      FShakePower: byte;
      FShakeSpeed: byte;
      FShakeCounter: byte;
      FShakeTime: integer;
      FBaseX: single;

      procedure SetViewport(const viewport: TRect);
      procedure loadTileMatrix(const value: TTileList; const index: integer; const viewport: TRect);
      function CreateNewTile(value: TTileRef): TMapTile;
      function FullCreateNewTile(x, y, layer: integer): TMapTile;
      function GetMaxLayer: integer; inline;
      function GetDefTile(layer, x, y: integer): TMapTile; inline;
      procedure DrawBG;
      function GetMapID: integer;
      function outOfBounds(x, y: integer): boolean; inline;

      procedure Tint();
      procedure Flash;
      procedure FlashTint;
      procedure adjustCoords(var x, y: integer);
      function IsBlank: boolean;
      procedure SetCurrentParty(const Value: TCharSprite);

      procedure CheckDisplacement;
      procedure clearDisplacement;
      procedure moveTo(x, y: integer);
      procedure ApplyDisplacement;
   protected
      function GetHeight: integer; override;
      function GetWidth: integer; override;
   public
      constructor Create(map: TRpgMap; const viewport: TRect; shaderEngine: TdmShaders;
                         canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
      destructor Destroy; override;

      procedure assignTile(const x, y, layer: integer; const tile: TTileRef);
      function updateBorders(x, y, layer: integer): boolean;
      procedure Process(Sender: TObject);
      procedure AdvanceFrame;
      function GetTile(x, y, layer: integer): TMapTile;
      function GetTopTile(x, y: integer): TMapTile;
      function tileInFrontOf(var location: TSgPoint; direction: TFacing): TMapTile;
      procedure RecreateTileMatrix;
      function AddMapObject(obj: TRpgMapObject): TMapSprite;
      procedure DeleteMapObject(obj: TMapSprite);
      procedure SwapMapSprite(old, new: TMapSprite);
      function Passable(x, y: integer; direction: TFacing): boolean; overload;
      function Passable(x, y: integer): boolean; overload;
      function Passable(location: TSgPoint; direction: TFacing; character: TMapSprite): boolean; overload;
      function Passable(location: TSgPoint; direction: TFacing): boolean; overload;
      function edgeCheck(const x, y: integer; const direction: TFacing): boolean;
      procedure EnsureImage(const filename: string);
      procedure Draw; override;
      function canExit(const x, y: integer; direction: TFacing; character: TMapSprite): boolean;
      function SpritesAt(location: TSgPoint): TArray<TMapSprite>;
      procedure AddLocation(const position: TSgPoint; character: TMapSprite);
      procedure LeaveLocation(const position: TSgPoint; character: TMapSprite);
      function normalizePoint(var x, y: integer): boolean;
      procedure ResizeCanvas;
      function onMap(where: TSgPoint): boolean;
      procedure centerOn(x, y: integer);

      //visual effects
      procedure CopyState(base: T2kSpriteEngine);
      procedure fadeTo(r, g, b, s: integer; time: integer);
      property SystemGraphic: TSystemImages read FSystemGraphic;
      function Fade: boolean;
      procedure FlashScreen(r, g, b, power, duration: integer);

      //displacement
      procedure displaceTo(x, y: integer);
      procedure setDispSpeed(speed: byte);
      procedure shakeScreen(power, speed: integer; duration: integer);

      property overlapping: TFacingSet read FOverlapping;
      property viewport: TRect read FViewport write SetViewport;
      property mapRect: TRect read FMapRect;
      property tileset: TTileSet read FTileset;
      property currentLayer: integer read FCurrentLayer write FCurrentLayer;
      property maxLayer: integer read GetMaxLayer;
      property mapObj: TRpgMap read FMap;
      property blank: boolean read IsBlank;
      property mapObjects: TMapSpriteList read FMapObjects;
      property CurrentParty: TCharSprite read FCurrentParty write SetCurrentParty;
      property Tile[layer, x, y: integer]: TMapTile read GetDefTile; default;
      property MapID: integer read GetMapID;
      property height: integer read GetHeight;
      property width: integer read GetWidth;
      property ShaderEngine: TdmShaders read FShaderEngine;
      property Displacing: boolean read FDisplacing;
      property Returning: boolean read FReturning write FReturning;
   end;

var
   GSpriteEngine: T2kSpriteEngine;

implementation
uses
   SysUtils, OpenGL, Math,
   turbu_constants, archiveInterface, charset_data, turbu_mapchars, turbu_OpenGL,
   turbu_2k_environment,
   sdl_13;

const
   BASESPEED = 3.8;
   MOVESPEED: array[1..6] of single = (BASESPEED / 8, BASESPEED / 4, BASESPEED / 2, BASESPEED, BASESPEED * 2, BASESPEED * 4);
   SHAKE_MAX = 23;
var
   LSineTable: array[0..SHAKE_MAX - 1] of extended;

{ T2kSpriteEngine }

procedure T2kSpriteEngine.AdvanceFrame;
begin
   TTile.Heartbeat;
end;

procedure T2kSpriteEngine.assignTile(const x, y, layer: integer;
  const tile: TTileRef);
var
   newTile: TMapTile;
begin
   if (x >= FMap.size.X) or (y >= FMap.size.Y) then
      Exit;
   FMap.AssignTile(x, y, layer, tile);
   if assigned(FTiles[layer][x, y]) then
      FTiles[layer][x, y].Dead;
   newTile := CreateNewTile(tile);
   FTiles[layer][x, y] := newTile;
   newTile.place(x, y, layer, tile, FTileset);
end;

function T2kSpriteEngine.canExit(const x, y: integer; direction: TFacing; character: TMapSprite): boolean;
var
   opposite: TFacing;
begin
   result := false;
   opposite := opposite_facing(direction);

//you have to be able to leave the tile you're on
   if passable(x, y, direction) then
   begin

//check to see if you're moving off the edge of the map, and
//that the tile you're moving into will let you enter from
//that direction
      if edgeCheck(x, y, direction) then
         result := passable(character.inFront, opposite, character);
      //end if
   end;
end;

function T2kSpriteEngine.normalizePoint(var x, y: integer): boolean;
var
   newX, newY: integer;
begin
   result := true;
   newX := safeMod(x, fTiles[0].width);
   newY := safeMod(y, fTiles[0].height);
   result := result and ((newX = x) or (wrHorizontal in FMap.wraparound));
   result := result and ((newY = y) or (wrVertical in FMap.wraparound));
   x := newX;
   y := newY;
end;

procedure T2kSpriteEngine.adjustCoords(var x, y: integer);
var
   halfwidth, halfheight, maxwidth, maxheight: integer;
begin
   halfwidth := min(round(canvas.width / 2), (width + 1) * 8);
   halfheight := min(round(canvas.height / 2), (height + 1) * 8);
   maxwidth := ((width + 1) * TILE_SIZE.x) - halfwidth;
   maxheight := ((height + 1) * TILE_SIZE.y) - halfheight;
   if x < halfwidth then
      x := halfwidth;
   if y < halfheight then
      y := halfheight;
   if x > maxwidth then
      x := maxwidth;
   if y > maxheight then
      y := maxheight;

   dec(x, halfwidth);
   dec(x, x mod TILE_SIZE.x);
   dec(y, halfheight);
   dec(y, y mod TILE_SIZE.y);
end;

function T2kSpriteEngine.updateBorders(x, y, layer: integer): boolean;
var
   tile: TBorderTile;
   neighbors: TNeighborSet;

   procedure TestInclude(x, y: integer; neighbor: TDirs8);
   begin
      if normalizePoint(x, y) then
      begin
         if assigned(FTiles[layer][x, y]) and (not tile.sharesBorder(FTiles[layer][x, y])) then
            include(neighbors, neighbor)
      end
      else include(neighbors, neighbor);
   end;

var
   tileRef: TTileRef;
   newTile: TMapTile;
begin
   result := false;
   if not normalizePoint(x, y) then
      Exit;
   if not (FTiles[layer][x, y] is TBorderTile) then
      Exit;

   neighbors := [];
   tile := TBorderTile(FTiles[layer][x, y]);
   testInclude(x, y - 1, n);
   testInclude(x + 1, y - 1, ne);
   testInclude(x + 1, y, e);
   testInclude(x + 1, y + 1, se);
   testInclude(x, y + 1, s);
   testInclude(x - 1, y + 1, sw);
   testInclude(x - 1, y, w);
   testInclude(x - 1, y - 1, nw);

   tileRef := FMap.GetTile(x, y, layer);
   if byte(neighbors) <> tileRef.tile then
   begin
      result := true;
      tileRef.tile := byte(neighbors);
      FMap.assignTile(x, y, layer, tileRef);
      FTiles[layer][x, y].Dead;
      newTile := self.CreateNewTile(tileRef);
      FTiles[layer][x, y] := newTile;
      newTile.place(X, Y, layer, tileRef, FTileset);
   end;
end;

procedure T2kSpriteEngine.AddLocation(const position: TSgPoint; character: TMapSprite);
begin
   FSpriteLocations.Add(position, character);
end;

procedure T2kSpriteEngine.LeaveLocation(const position: TSgPoint; character: TMapSprite);
begin
   if FSpriteLocations.KeyHasValue(position, character) then
      FSpriteLocations.RemovePair(position, character);
end;

function T2kSpriteEngine.AddMapObject(obj: TRpgMapObject): TMapSprite;
begin
   if obj.id = 0 then
      Exit(nil);
   if obj.isTile then
      result := TEventSprite.Create(obj, self)
   else result := TCharSprite.Create(obj, self);
   GEnvironment.AddEvent(result);
   TMonitor.Enter(FMapObjects);
   try
      FMapObjects.Add(result);
   finally
      TMonitor.Exit(FMapObjects);
   end;
end;

procedure T2kSpriteEngine.DeleteMapObject(obj: TMapSprite);
begin
   TMonitor.Enter(FMapObjects);
   try
      FSpriteLocations.RemovePair(obj.location, obj);
      FMapObjects.Remove(obj);
   finally
      TMonitor.Exit(FMapObjects);
   end;
end;

procedure T2kSpriteEngine.SwapMapSprite(old, new: TMapSprite);
var
   index: integer;
begin
   TMonitor.Enter(FMapObjects);
   try
      index := FMapObjects.IndexOf(old);
      if FSpriteLocations.KeyHasValue(old.location, old) then
         FSpriteLocations.RemovePair(old.location, old);
      FMapObjects[index] := new;
   finally
      TMonitor.Exit(FMapObjects);
   end;
end;

procedure T2kSpriteEngine.CopyState(base: T2kSpriteEngine);
begin
   FFadeColor := base.FFadeColor;
   //TODO: Add more here when necessary
end;

constructor T2kSpriteEngine.Create(map: TRpgMap; const viewport: TRect; shaderEngine: TdmShaders;
            canvas: TSdlCanvas; tileset: TTileset; images: TSdlImages);
var
   i: integer;
   size: TSgPoint;
   mapObj: TRpgMapObject;
begin
   //initial setup
   inherited Create(nil, canvas);
   FShaderEngine := shaderEngine;
   self.Images := images;
   FTiles := TTileMatrixList.Create;
   FTileset := tileset;
   FMap := map;
   size := FMap.size;
   self.VisibleWidth := canvas.Width;
   self.VisibleHeight := canvas.Height;
   FMapRect := rect(0, 0, size.x, size.y);
   FDisplacementSpeed := BASESPEED;
   FPanSpeed := BASESPEED;

   //create layers
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      FTiles.add(TTileMatrix.Create(size));

   //set viewport and populate it with initial tiles
   self.SetViewport(viewport);

   //load background
   if map.bgName <> '' then
   begin
      FBgImage := TBackgroundSprite.Create(self, map);
      FBgImage.Image := images.EnsureBGImage('Backgrounds/' + map.bgName + '.png', map.bgName);
   end;

   //populate events
   FMapObjects := TMapSpriteList.Create;
   for mapObj in map.mapObjects do
      addMapObject(mapObj);
   FSpriteLocations := TSpriteLocations.Create;
   for i := 1 to 4 do
      FFadeColor[i] := 1;
end;

destructor T2kSpriteEngine.Destroy;
begin
   FSpriteLocations.Free;
   FMapObjects.Free;
   FTiles.Free;
   FMap.Free;
   inherited;
 end;

procedure T2kSpriteEngine.Draw;
var
   current: integer;
begin
   glGetIntegerv(GL_CURRENT_PROGRAM, @current);
   try
      FShaderEngine.UseShaderProgram(FShaderEngine.ShaderProgram('default', 'defaultF'));
      if assigned(FBgImage) then
         DrawBG;
      inherited Draw;
   finally
      glUseProgram(current);
   end;
end;

procedure T2kSpriteEngine.DrawBG;
begin
   FBgImage.scroll;
   FBgImage.X := WorldX;
   repeat
      FBgImage.Y := WorldY;
      repeat
         FBgImage.Draw;
         FBgImage.Y := FBgImage.Y + FBgImage.PatternHeight;
      until FBgImage.y + FBgImage.OffsetY > WorldY + Canvas.Height;
      FBgImage.X := FBgImage.X + FBgImage.PatternWidth;
   until FBgImage.X + FBgImage.OffsetX > WorldX + Canvas.Width;
end;

function T2kSpriteEngine.edgeCheck(const x, y: integer; const direction: TFacing): boolean;
begin
   case direction of
      facing_up: result := (y > 0) or (wrVertical in FMap.wraparound);
      facing_right: result := (x < FMap.width) or (wrHorizontal in FMap.wraparound);
      facing_down: result := (y < FMap.height) or (wrVertical in FMap.wraparound);
      facing_left: result := (x > 0) or (wrHorizontal in FMap.wraparound);
      else raise ESpriteError.CreateFmt('Bad Direction value: %d is out of bounds for TFacing', [ord(direction)]);
   end;
end;

function T2kSpriteEngine.onMap(where: TSgPoint): boolean;
begin
   result := (clamp(where.x, 0, width) = where.x) and (clamp(where.y, 0, height) = where.y);
end;

procedure T2kSpriteEngine.centerOn(x, y: integer);
var
   pX, pY: integer; //pixel coordinates
   aX, aY: integer; //adjusted coordinates
   dX, dY: integer; //delta
begin
   px := x * TILE_SIZE.x;
   py := y * TILE_SIZE.y;
   adjustCoords(pX, pY);
   FDestination.X := pX;
   FDestination.Y := pY;
   aX := px div TILE_SIZE.x;
   aY := py div TILE_SIZE.y;
   dx := x - aX;
   dy := y - aY;
   self.SetViewport(rect(aX, aY, x + dX, y + dY));
end;

procedure T2kSpriteEngine.EnsureImage(const filename: string);
begin
   if (filename <> '') and (not self.Images.Contains(filename)) then
      self.Images.AddSpriteFromArchive(format('mapsprite\%s.png', [filename]), filename, SPRITE_SIZE);
end;

procedure T2kSpriteEngine.fadeTo(r, g, b, s, time: integer);
begin
   FFadeTarget[1] := r / 255;
   FFadeTarget[2] := g / 255;
   FFadeTarget[3] := b / 255;
   FFadeTarget[4] := s / 255;
   FFadeTime := TRpgTimestamp.Create(time * 100);
end;

procedure T2kSpriteEngine.FlashScreen(r, g, b, power, duration: integer);
begin
   FFlashColor[1] := r / 255;
   FFlashColor[2] := g / 255;
   FFlashColor[3] := b / 255;
   FFlashColor[4] := power / 255;
   duration := duration * 100;
   FFlashTime := TRpgTimestamp.Create(duration);
   FFlashDuration := duration;
end;

function T2kSpriteEngine.FullCreateNewTile(x, y, layer: integer): TMapTile;
var
   tile: TTileRef;
begin
   tile := FMap.getTile(x, y, layer);
   result := CreateNewTile(tile);
   if assigned(result) then
      result.place(x, y, layer, tile, FTileset);
end;

function T2kSpriteEngine.GetDefTile(layer, x, y: integer): TMapTile;
begin
   result := GetTile(x, y, layer);
end;

function T2kSpriteEngine.GetHeight: integer;
begin
   result := MapObj.size.y;
end;

function T2kSpriteEngine.GetMapID: integer;
begin
   result := MapObj.ID;
end;

function T2kSpriteEngine.GetMaxLayer: integer;
begin
   result := FTiles.Count - 1;
end;

function T2kSpriteEngine.GetTile(x, y, layer: integer): TMapTile;
var
   i: integer;
begin
   normalizePoint(x, y);
   if outOfBounds(x, y) then
      Exit(nil);
   if not assigned(FTiles.First[x, y]) then
      for I := 0 to FTiles.Count - 1 do
         FTiles[i][x, y] := FullCreateNewTile(x, y, i);
   result := FTiles[layer][x, y];
end;

function T2kSpriteEngine.GetTopTile(x, y: integer): TMapTile;
var
   i: integer;
begin
   result := GetTile(x, y, GetMaxLayer);
   if assigned(result) then
      Exit;
   for i := self.MaxLayer - 1 downto 0 do
   begin
      result := FTiles[i][x, y];
      if assigned(result) then
         Exit;
   end;
   assert(false); //should not reach this point
end;

function T2kSpriteEngine.GetWidth: integer;
begin
   result := MapObj.size.x;
end;

function T2kSpriteEngine.IsBlank: boolean;
begin
   result := (FFadeColor[1] = 0) and (FFadeColor[2] = 0) and (FFadeColor[3] = 0);
   result := true;
end;

function T2kSpriteEngine.CreateNewTile(value: TTileRef): TMapTile;
var
   tileType: TTileType;
   tileClass: TMapTileClass;
   filename: string;
   tileGroup: TTileGroup;
begin
   //don't create anything for the "blank tile"
   if smallint(value.value) = -1 then
      Exit(nil);

   tileGroup := FTileset.Records[value.group].group;
   tileType := tileGroup.tileType;
   filename := tileGroup.filename;
   if tileType = [] then
      tileClass := TMapTile
   else if tileType = [tsBordered] then
      tileClass := TBorderTile
   else if tileType = [tsAnimated] then
      tileClass := TAnimTile
   else if tileType = [tsBordered, tsAnimated] then
   begin
      if tileGroup.ocean then
         tileClass := TOceanTile
      else tileClass := TShoreTile;
   end
   else raise ESpriteError.Create('Unknown tile type.');
   result := tileClass.Create(Self, filename);
end;

{$Q+R+}
procedure T2kSpriteEngine.loadTileMatrix(const value: TTileList; const index: integer; const viewport: TRect);
var
   size: TSgPoint;

   procedure EquivalizeCoords(const x, y: integer; out equivX, equivY: integer);
   var
      adjustedCoords: TSgPoint;
   begin
      adjustedCoords := sgPoint(x, y);
      while (adjustedCoords.x < 0) or (adjustedCoords.y < 0) do
         adjustedCoords := adjustedCoords + size;
      adjustedCoords := adjustedCoords mod size;
      equivX := adjustedCoords.x;
      equivY := adjustedCoords.y;
   end;

   function GetIndex(x, y: integer): integer;
   begin
      result := (y * size.x) + x;
   end;

var
   x, y: integer;
   equivX, equivY: integer;
   newTile: TMapTile;
   tileRef: TTileRef;
   matrix: TTileMatrix;
begin
   matrix := FTiles[index];
   size := FMap.size;
   for y := viewport.top - 1 to viewport.top + viewport.bottom + 1 do
      for x := viewport.left - 1 to viewport.left + viewport.Right + 1 do
      begin
         EquivalizeCoords(x, y, equivX, equivY);
         if assigned(matrix[equivX, equivY]) then
            Continue;

         tileRef := value[getIndex(equivX, equivY)];
         newTile := CreateNewTile(tileRef);
         matrix[equivX, equivY] := newTile;
         if assigned(newTile) then
            newTile.place(equivX, equivY, index, tileRef, FTileset);
      end;
end;

function T2kSpriteEngine.outOfBounds(x, y: integer): boolean;
begin
   result := (x < 0) or (y < 0) or (x >= FMap.width) or (y >= FMap.height);
end;

function T2kSpriteEngine.Passable(x, y: integer; direction: TFacing): boolean;
const TRANSLATE: array[TFacing] of TTileAttribute = (taUp, taRight, taDown, taLeft);
var
   tile: TTile;
   i: integer;
begin
   result := true;
   for I := 0 to MaxLayer do
   begin
      tile := GetTile(x, y, i);
      if assigned(tile) then
         result := result and (TRANSLATE[direction] in tile.attributes);
   end;
end;

function T2kSpriteEngine.Passable(x, y: integer): boolean;
var
   dir: TFacing;
begin
   result := false;
   for dir := low(TFacing) to high(TFacing) do
      result := result or Passable(x, y, dir);
end;

function T2kSpriteEngine.Passable(location: TSgPoint; direction: TFacing; character: TMapSprite): boolean;
var
   sprites: TArray<TMapSprite>;
   sprite: TMapSprite;
begin
   sprites := self.SpritesAt(location);
   result := passable(location, direction) and ((sprites = nil) or
     ((length(sprites) = 1) and (sprites[0] = character)));
   if (result = false) and (length(sprites) > 0) then
   begin
      result := true;
      for sprite in sprites do
         result := result and (sprite.baseTile.z <> character.baseTile.z);
   end;
end;

function T2kSpriteEngine.Passable(location: TSgPoint; direction: TFacing): boolean;
begin
   result := passable(location.x, location.y, direction);
end;

procedure T2kSpriteEngine.Tint();
var
   handle: integer;
   gla: TGlArrayF4;
begin
   handle := FShaderEngine.ShaderProgram('default', 'tint', 'shift');
   FShaderEngine.UseShaderProgram(handle);
   FShaderEngine.SetUniformValue(handle, 'hShift', 0);
   FShaderEngine.SetUniformValue(handle, 'valMult', 1.0);
   system.move(FFadeColor[1], gla[0], sizeof(gla));
   gla[3] := 1;
   FShaderEngine.SetUniformValue(handle, 'rgbValues', gla);
   FShaderEngine.SetUniformValue(handle, 'satMult', FFadeColor[4]);
end;

procedure T2kSpriteEngine.Flash();
var
   handle: integer;
   gla: TGlArrayF4;
begin
   handle := FShaderEngine.ShaderProgram('default', 'flash');
   FShaderEngine.UseShaderProgram(handle);
   gla[0] := FFlashColor[1];
   gla[1] := FFlashColor[2];
   gla[2] := FFlashColor[3];
   gla[3] := FFlashColor[4] * (FFlashTime.timeRemaining / FFlashDuration);
   FShaderEngine.SetUniformValue(handle, 'flashColor', gla);
end;

procedure T2kSpriteEngine.FlashTint();
var
   handle: integer;
   gla: TGlArrayF4;
begin
   handle := FShaderEngine.ShaderProgram('default', 'flashtint', 'shift');
   FShaderEngine.UseShaderProgram(handle);
   FShaderEngine.SetUniformValue(handle, 'hShift', 0);
   FShaderEngine.SetUniformValue(handle, 'valMult', 1.0);
   system.move(FFadeColor[1], gla[0], sizeof(gla));
   gla[3] := 1;
   FShaderEngine.SetUniformValue(handle, 'rgbValues', gla);
   FShaderEngine.SetUniformValue(handle, 'satMult', FFadeColor[4]);
   gla[0] := (FFlashColor[1] / 255);
   gla[1] := (FFlashColor[2] / 255);
   gla[2] := (FFlashColor[3] / 255);
   gla[3] := (FFlashColor[4] / 255) * (FFlashTime.timeRemaining / FFlashDuration);
   FShaderEngine.SetUniformValue(handle, 'flashColor', gla);
end;

function T2kSpriteEngine.Fade: boolean;
var
   i: integer;
   time: cardinal;
   fade, flash: boolean;
begin
   result := false;
   flash := false;
   if assigned(FFadeTime) then
   begin
      time := FFadeTime.timeRemaining;
      for i := 1 to 4 do
         moveTowards(time, FFadeColor[i], FFadeTarget[i]);
      if time = 0 then
         freeAndNil(FFadeTime);
   end;
   for i := 1 to 4 do
      result := result or (FFadeColor[i] <> 1);
   fade := result;
   if assigned(FFlashTime) then
   begin
      if FFlashTime.timeRemaining > 0 then
      begin
         flash := true;
         result := true;
      end
      else FreeAndNil(FFlashTime);
   end;
   if fade and flash then
      FlashTint()
   else if fade then
      Tint
   else if flash then
      self.Flash;
end;

procedure T2kSpriteEngine.moveTo(x, y: integer);
begin
   adjustCoords(x, y);
   FDestination.X := x;
   FDestination.Y := y;
end;

procedure T2kSpriteEngine.clearDisplacement;
begin
   inc(FDestination.X, commons.round(FDisplacementX));
   FDisplacementX := 0;
   inc(FDestination.Y, commons.round(FDisplacementY));
   FDisplacementY := 0;
   worldX := commons.round(worldX);
   worldY := commons.round(worldY);
   moveTo(trunc(FCurrentParty.baseTile.X), trunc(FCurrentParty.baseTile.Y));
   FBaseX := worldX;
end;

procedure T2kSpriteEngine.ApplyDisplacement;
const
   SHAKE_AMP = 1.8;
var
   delta: single;
   i: integer;
   shakeBias: single;
begin
   if FShakeTime > 0 then
   begin
      FShakeCounter := (FShakeCounter + FShakeSpeed) mod SHAKE_MAX;
      shakeBias := FShakePower * LSineTable[FShakeCounter] * SHAKE_AMP;
      i := max(round(FShakeTime/32), 1);
      dec(FShakeTime, FShakeTime div i);
   end else shakeBias := 0;

   if trunc(FDestination.x + FDisplacementX) <> trunc(self.worldX) then
   begin
      delta := min(abs(FDestination.x + FDisplacementX - worldX), FPanSpeed);
      if FDestination.x + FDisplacementX < self.worldX then
         delta := -delta;
   end
   else delta := 0;
   FBaseX := FBaseX + delta;
   WorldX := FBaseX - shakeBias;
   if trunc(FDestination.y + FDisplacementY) <> trunc(self.worldY) then
   begin
      delta := min(abs(FDestination.Y + FDisplacementY - worldY), FPanSpeed);
      if FDestination.Y + FDisplacementY < worldY then
         delta := -delta;
      worldY := worldY + delta;
   end;
end;

procedure T2kSpriteEngine.CheckDisplacement;
var
   panned: boolean;
   delta: single;
begin
   panned := false;
   if FDispGoalX <> 0 then
   begin
      delta := min(abs(FDispGoalX), FDisplacementSpeed);
      if FDispGoalX < 0 then
         delta := delta * -1;
      FBaseX := FBaseX + delta;
      FDispGoalX := FDispGoalX - delta;
      FDisplacementX := FDisplacementX + delta;
      panned := true;
   end;
   if FDispGoalY <> 0 then
   begin
      delta := min(abs(FDispGoalY), FDisplacementSpeed);
      if FDispGoalY < 0 then
         delta := delta * -1;
      worldY := worldY + delta;
      FDispGoalY := FDispGoalY - delta;
      FDisplacementY := FDisplacementY + delta;
      panned := true;
   end;
   FDisplacing := panned;
   if (not FDisplacing) and (FReturning) then
   begin
      FReturning := false;
      self.clearDisplacement;
   end;
end;

procedure T2kSpriteEngine.Process(Sender: TObject);
var
   sprite: TMapSprite;
begin
   self.Dead;
   TMonitor.Enter(FMapObjects);
   try
      for sprite in FMapObjects do
      begin
         sprite.place;
         sprite.MoveTick;
      end;
   finally
      TMonitor.Exit(FMapObjects);
   end;
   if assigned(FCurrentParty) then
   begin
      FCurrentParty.place;
      FCurrentParty.MoveTick;
   end;

   CheckDisplacement;
   ApplyDisplacement;
//TODO: calculate map shaking, fading, and overlay colors
end;

procedure T2kSpriteEngine.RecreateTileMatrix;
var
   i: integer;
begin
   FTiles.Free;
   FTiles := TTileMatrixList.Create;
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      FTiles.add(TTileMatrix.Create(FMap.size));
   self.SetViewport(FViewport);
end;

procedure T2kSpriteEngine.ResizeCanvas;
begin
   self.Canvas.Resize;
   self.VisibleWidth := canvas.Width;
   self.VisibleHeight := canvas.Height;
end;

procedure T2kSpriteEngine.SetCurrentParty(const Value: TCharSprite);
begin
   FCurrentParty := Value;
   if assigned(value) then
   begin
      value.tiles[1].Parent := self;
      value.tiles[2].Parent := self;
      value.tiles[1].Engine := self;
      value.tiles[2].Engine := self;
   end;
end;

procedure T2kSpriteEngine.SetViewport(const viewport: TRect);
var
   i: integer;
begin
   assert(assigned(FMap));
   FViewport := viewport;
   self.WorldX := viewport.Left * TILE_SIZE.X;
   self.WorldY := viewport.Top * TILE_SIZE.Y;
   for i := low(FMap.tileMap) to high(FMap.tileMap) do
      loadTileMatrix(FMap.tileMap[i], i, viewport);
   FOverlapping := [];
   if (viewport.Left < 0) then
      include(FOverlapping, facing_left)
   else if (viewport.Right > self.Width)  then
      include(FOverlapping, facing_right);
   if (viewport.Top < 0) then
      include(FOverlapping, facing_up)
   else if viewport.Top > self.Height then
      include(FOverlapping, facing_down);
   ResizeCanvas;
   FBaseX := worldX;
end;

procedure T2kSpriteEngine.shakeScreen(power, speed, duration: integer);
begin
   FShakePower := power;
   FShakeSpeed := speed;
   FShakeTime := duration;
end;

function T2kSpriteEngine.SpritesAt(location: TSgPoint): TArray<TMapSprite>;
begin
   if not FSpriteLocations.ContainsKey(location) then
      result := nil
   else result := FSpriteLocations[location].ToArray;
end;

function T2kSpriteEngine.tileInFrontOf(var location: TSgPoint; direction: TFacing): TMapTile;
begin
   case direction of
      facing_up: location := point(location.x, location.y - 1);
      facing_right: location := point(location.x + 1, location.y);
      facing_down: location := point(location.x, location.y + 1);
      facing_left: location := point(location.x - 1, location.y);
   end;
   if normalizePoint(location.x, location.y) then
      result := TMapTile(self[0, location.x, location.y])
   else result := nil;
end;

procedure T2kSpriteEngine.displaceTo(x, y: integer);
begin
   adjustCoords(x, y);
   FDispGoalX := FDispGoalX + x - worldX;
   FDispGoalY := FDispGoalY + y - worldY;
end;

procedure T2kSpriteEngine.setDispSpeed(speed: byte);
begin
   if speed in [low(MOVESPEED)..high(MOVESPEED)] then
      FDisplacementSpeed := MOVESPEED[speed];
end;

var
   i: integer;
initialization
   for i := 0 to SHAKE_MAX - 1 do
      LSineTable[i] := sin(2 * pi * i / SHAKE_MAX);
end.
