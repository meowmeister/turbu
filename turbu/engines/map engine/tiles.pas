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
   types, classes, contnrs,
   commons, timing, charset_data, turbu_maps, turbu_tilesets,
   SDL_sprite, SG_Defs;

type
   TTile = class;
   TTileClass = class of TTile;
   TNeighborSet = set of TDirs8;

   TTile = class abstract(TParentSprite) //ASPHYRE version of TTileData
   private
      FTileID: word; // which tile from the overall x,y grid of the chipset
      FGridLoc: TSgPoint; //where this tile is located on the map
      FTerrainID: word;
      FAttributes: TTileAttributes;
      FSavedFx: integer;
      FSavedColor: TRpgColor;
      FFlashColor: TRpgColor;
      FFlashTimer: TRpgTimestamp;

      procedure saveColor;
      procedure restoreColor(which: TRpgColor);
      procedure setEngine(newEngine: TSpriteEngine); virtual;
   private
      class function DecodeZOrder(const value: TTileAttributes): byte; static;
   protected
      function InVisibleRect: boolean; override;
      procedure DoDraw; override;
   public
      constructor Create(const AParent: TSpriteEngine; tileset: string; const addself: boolean = false); reintroduce; overload; virtual;
      destructor Destroy; override;
      procedure Draw; override;
      procedure Assign(const value: TTile); reintroduce;
      function place(const xCoord,yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet): TTileAttributes; virtual;
      function open(exceptFor: TObject): boolean; virtual;
      procedure flash(r, g, b, a: byte; time: cardinal);
      function canEnter: boolean; virtual;

      property id: word read FTileID write FTileID;
      property location: TSgPoint read FGridLoc write FGridLoc;
      property terrain: word read FTerrainID write FTerrainID;
      property attributes: TTileAttributes read FAttributes write FAttributes;
   end;

   TUpperTile = class(TTile);

   TLowerTile = class(TTile)
   private
      FOccupied: TObjectList;
      FUpperTile: TUpperTile;

      function isOccupied: boolean;
      function hasCountertop: boolean; inline;
   protected
      FNeighbors: TNeighborSet;
   public
      constructor Create(const AParent: TSpriteEngine; tileset: string; const addself: boolean = false); override;
      destructor Destroy; override;
      procedure placeUpper(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet);
      function open(exceptFor: TObject): boolean; override;
      function canEnter: boolean; override;
//      procedure bump(character: TObject);

      property upperTile: TUpperTile read FUpperTile write FUpperTile;
      property occupied: boolean read isOccupied;
      property event: TObjectList read FOccupied write FOccupied;
      property countertop: boolean read hasCountertop;
   end;

   TAnimTile = class(TLowerTile)
   private
   class var
      FHeartbeat: byte;
      FDisplacement: byte;
   public
      procedure Draw; override;
      function place(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet): TTileAttributes; override;
      class procedure heartbeat;
   end;

   TBorderTile = class;

   TMiniTile = class(TTile)
   public
      constructor Create(const AParent: TBorderTile; tileset: string);
   end;

   TBorderTile = class(TLowerTile)
   private
      minitiles: array[1..4] of TMiniTile;
      function FindNeighbors: TNeighborSet; //determines which of
                                            //the 46 patterns it fits
      procedure setEngine(newEngine: TSpriteEngine); override;
   protected
      procedure doPlace; virtual;
   public
      constructor Create(const AParent: TSpriteEngine; tileset: string; const addself: boolean = false); override;
      destructor Destroy; override;
      procedure Draw; override;
      function place(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet): TTileAttributes; override;
      property neighbors: TNeighborSet read FindNeighbors write FNeighbors;
   end;

   TWaterTile = class abstract(TBorderTile)
   private
      FLinkedFilename: string;
{      function FindNeighbors: ANeighbors; //determines which of
                                          //the 46 patterns it fits}
   public
      function place(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet): TTileAttributes; override;
      procedure Draw; override;
   end;

   TShoreTile = class(TWaterTile)
   protected
      procedure doPlace; override;
   end;

   TOceanTile = class(TWaterTile)
   protected
      procedure doPlace; override;
   end;

{   TEventTile = class(TTile)
   private
      FEvent: TEvent;
   public
      constructor create(baseEvent: TEvent; const AParent: TSpriteEngine; map: TMapUnit); reintroduce;
      procedure assign(data: TEventTile); reintroduce;
      procedure update(newPage: TEventPage);

      property event: TEvent read FEvent write FEvent;
   end;}

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
   public
      constructor Create(parent: TSpriteEngine; input: TRpgMap); reintroduce; overload;
      constructor Create(parent: TSpriteEngine; x, y: shortint; autoX, autoY: boolean); reintroduce; overload;
      destructor Destroy; override;
      procedure scroll;

      property scrollData: TScrollData read FScroll write FScroll;
   end;

const
   ANIM_RATE = 5;
   ANIM_STEP = 3;
   ANIM_MAX = 9;
   BG_SCROLL_RATE = 0.1;

implementation

uses
   sysUtils,
   turbu_constants, turbu_2k_sprite_engine,
   SDL_ImageManager;

class function TTile.DecodeZOrder(const value: TTileAttributes): byte;
begin
   if taOverhang in value then
      result := 10
   else if taCeiling in value then
      result := 6
   else result := 1;
end;

constructor TTile.Create(const AParent: TSpriteEngine; tileset: string; const addself: boolean = false);
begin
   assert(AParent is T2kSpriteEngine);
   inherited create(AParent);
   imageName := tileset;
end;

destructor TTile.Destroy;
begin
   FFlashTimer.Free;
   inherited;
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

{$WARN USE_BEFORE_DEF OFF}
procedure TTile.DoDraw;
var
   lX, lY: single;
   overlap: TFacingSet;
   viewport: TRect;
   mapSize: TSgFloatPoint;
begin
   overlap := T2kSpriteEngine(FEngine).overlapping;
   if overlap <> [] then
   begin
      viewport := T2kSpriteEngine(FEngine).viewport;
      mapSize := TSgPoint(T2kSpriteEngine(FEngine).mapRect.BottomRight) * TILE_SIZE;
      lX := self.X;
      lY := self.Y;
      if facing_left in overlap then
      begin
         if FGridLoc.X > viewport.Right then
            self.X := lX - mapSize.x;
      end
      else if facing_right in overlap then
      begin
         if FGridLoc.X < viewport.Left then
            self.X := lX + mapSize.x;
      end;
      if facing_up in overlap then
      begin
         if FGridLoc.Y > viewport.Bottom then
            self.Y := lY - mapSize.Y;
      end
      else if facing_down in overlap then
      begin
         if FGridLoc.Y < viewport.Top then
            self.Y := lY + mapSize.Y;
      end;
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
      corrected := NormalizePoint(FGridLoc, T2kSpriteEngine(FEngine).mapRect);

      result := (corrected.X > FEngine.WorldX - Width ) and
      (corrected.Y > FEngine.WorldY - Height)    and
      (corrected.X < FEngine.WorldX + FEngine.VisibleWidth)  and
      (corrected.Y < FEngine.WorldY + FEngine.VisibleHeight);
   end;
end;

procedure TTile.Draw;
var
   i: byte;
   timer: cardinal;
begin
   inherited Draw;
   if assigned(FFlashTimer) then
   begin
      timer := FFlashTimer.timeRemaining;
      if timer > 0 then
      begin
         saveColor;
         FSavedFx := self.DrawFx;
         restoreColor(FFlashColor);
         self.DrawFx := fxOneColor;
         inherited Draw;
         for i := 1 to 4 do
            moveTowards(timer, FFlashColor.rgba[i], 0);
         restoreColor(FSavedColor);
         self.DrawFx := FSavedFx;
      end
      else freeAndNil(FFlashTimer);
   end;
end;

procedure TTile.flash(r, g, b, a: byte; time: cardinal);
begin
   with FFlashColor do
   begin
      rgba[1] := r;
      rgba[2] := g;
      rgba[3] := b;
      rgba[4] := a;
   end;
   if Assigned(FFlashTimer) then
      FFlashTimer.Free;
   FFlashTimer := TRpgTimestamp.Create(time);
end;

function TTile.open(exceptFor: TObject): boolean;
begin
   result := Self.canEnter;
end;

function TTile.place(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet): TTileAttributes;
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
   z := decodeZOrder(result);
end;

procedure TTile.restoreColor(which: TRpgColor);
begin
   with which do
   begin
//fixme
      Red := rgba[1];
      Green := rgba[2];
      Blue := rgba[3];
      Alpha := rgba[4];
   end;
end;

procedure TTile.saveColor;
begin
   with FSavedColor do
   begin
//fixme
      rgba[1] := lo(Red);
      rgba[2] := lo(Green);
      rgba[3] := lo(Blue);
      rgba[4] := lo(Alpha);
   end;
end;

procedure TTile.setEngine(newEngine: TSpriteEngine);
begin
   FEngine := newEngine;
end;

{ TBorderTile }

constructor TBorderTile.Create(const AParent: TSpriteEngine; tileset: string; const addself: boolean = false);
var i: byte;
begin
   inherited create(AParent, tileset, true);
   for i := 1 to 4 do
   begin
      minitiles[i] := TMiniTile.Create(self, tileset);
      miniTiles[i].Width := TILE_SIZE.X div 2;
      miniTiles[i].Height := TILE_SIZE.Y div 2;
   end;
end;

destructor TBorderTile.destroy;
var x: byte;
begin
   for x := 1 to 4 do
         minitiles[x].free;
   inherited destroy;
end;

function TBorderTile.FindNeighbors(): TNeighborSet;
begin
   //fix this
   //owner's internal name is FEngine
   result := FNeighbors;
end;

function TBorderTile.place(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet): TTileAttributes;
var
   tileRef: TTileRef;
begin
   tileRef.group := tileData.group;
   tileRef.tile := 0;
   result := inherited place(xCoord, yCoord, layer, tileRef, chip_data);
   FNeighbors := TNeighborSet(tiledata.tile);
   doPlace;
end;

procedure TBorderTile.setEngine(newEngine: TSpriteEngine);
var
  I: Integer;
begin
   inherited setEngine(newEngine);
   for I := 1 to 4 do
      minitiles[i].engine := newEngine;
end;

procedure TBorderTile.doPlace;
var
   minis, base: array[1..4] of word;
   i: byte;
begin
   minis[1] := 26;
   minis[2] := minis[1] + 1;
   minis[3] := minis[1] + 6;
   minis[4] := minis[3] + 1;
   miniTiles[1].X := self.X;
   miniTiles[1].Y := self.Y;
   miniTiles[2].X := self.X + 8;
   miniTiles[2].Y := self.Y;
   miniTiles[3].X := self.X;
   miniTiles[3].Y := self.Y + 8;
   miniTiles[4].X := self.X + 8;
   miniTiles[4].Y := self.Y + 8;
   for i := 1 to 4 do
   begin
      miniTiles[i].ImageName := self.ImageName;
      miniTiles[i].location := self.location;
   end;
//now comes the fun part
//skip if there's nothing to change
   if neighbors <> [] then
   begin
   //otherwise:
   //first: case 46: one-tile island
      if neighbors * [n, e, w, s] = [n, e, w, s] then
         for i := 1 to 4 do
            dec(minis[i], 26)
         //end for
      else
   //3 wall cases
   //42:
      if neighbors * [n, e, w] = [n, e, w] then
      begin
         dec(minis[1], 14);
         dec(minis[2], 10);
         dec(minis[3], 14);
         dec(minis[4], 10);
      end else
   //43:
      if neighbors * [n, e, s] = [n, e, s] then
      begin
         dec(minis[1], 10);
         dec(minis[2], 10);
         inc(minis[3], 14);
         inc(minis[4], 14);
      end else
   //44
      if neighbors * [e, w, s] = [e, w, s] then
      begin
         inc(minis[1], 10);
         inc(minis[2], 14);
         inc(minis[3], 10);
         inc(minis[4], 14);
      end else
   //45:
      if neighbors * [n, w, s] = [n, w, s] then
      begin
         dec(minis[1], 14);
         dec(minis[2], 14);
         inc(minis[3], 10);
         inc(minis[4], 10);
      end else
   //full-corner cases
   //34:
      if neighbors * [n, nw] = [n, nw] then
      begin
         dec(minis[1], 14);
         dec(minis[2], 14);
         dec(minis[3], 14);
         if se in neighbors then
            dec(minis[4], 22);
      end else
   //36:
      if neighbors * [n, ne] = [n, ne] then
      begin
         dec(minis[1], 10);
         dec(minis[2], 10);
         dec(minis[4], 10);
         if sw in neighbors then
            dec(minis[3], 22);
      end else
   //38:
      if neighbors * [e, se] = [e, se] then
      begin
         inc(minis[2], 14);
         inc(minis[3], 14);
         inc(minis[4], 14);
         if nw in neighbors then
            dec(minis[1], 22);
      end else
   //40:
      if neighbors * [s, sw] = [s, sw] then
      begin
         inc(minis[1], 10);
         inc(minis[3], 10);
         inc(minis[4], 10);
         if ne in neighbors then
            dec(minis[2], 22);
      end else
   //miscellaneous cases, which can be handled individually
      begin
   //store values for override
         for i := 1 to 4 do
            base[i] := minis[i];
   //set lone corners
         if nw in neighbors then
            dec(base[1], 22);
         if ne in neighbors then
            dec(base[2], 22);
         if sw in neighbors then
            dec(base[3], 22);
         if se in neighbors then
            dec(base[4], 22);
   //override values for walls
         if n in neighbors then
         begin
            base[1] := minis[1] - 12;
            base[2] := minis[2] - 12;
         end;
         if s in neighbors then
         begin
            base[3] := minis[3] + 12;
            base[4] := minis[4] + 12;
         end;
         if w in neighbors then
         begin
            base[1] := minis[1] - 2;
            base[3] := minis[3] - 2;
         end;
         if e in neighbors then
         begin
            base[2] := minis[2] + 2;
            base[4] := minis[4] + 2;
         end;
         for i := 1 to 4 do
            minis[i] := base[i];
      end;//end of misc block
   end;//end of big skip
   for i := 1 to 4 do
      miniTiles[i].ImageIndex := minis[i];
end;

procedure TBorderTile.Draw;
var i: byte;
begin
   for I := 1 to 4 do
      miniTiles[i].Draw;
end;

{ TWaterTile }

procedure TWaterTile.Draw;
begin
   doPlace;
   inherited;
end;

function TWaterTile.place(const xCoord, yCoord: word; const layer: byte;
  const tileData: TTileRef; chip_data: TTileSet): TTileAttributes;
begin
   FLinkedFilename := chip_data.Records[tileData.group].group.linkedFilename;
   result := inherited place(xCoord, yCoord, layer, tileData, chip_data);
end;

{function TWaterTile.FindNeighbors: ANeighbors;
begin
   //do later
   //other types of water tiles count as homogenous
end;}

{ TShoreTile }

{******************************************************************************
* Complicated routine to place all the minitiles for a water map.  Bordered
* tiles are pretty simple.  Deep ocean is a bit tougher, since it needs a
* linked image for shores.
******************************************************************************}
procedure TShoreTile.doPlace;
var
   minis, base: array[1..4] of word;
   i: integer;
   changed: array[1..4] of boolean;

   procedure ChangeBase(index, length: integer);
   begin
      inc(base[index], length);
      changed[index] := false;
   end;

   procedure OffsetBase(index, length: integer);
   begin
      base[index] := minis[index] + length;
      changed[index] := false;
   end;

begin
   minis[1] := 0;
   minis[2] := minis[1] + 1;
   minis[3] := minis[1] + 6;
   minis[4] := minis[3] + 1;
   miniTiles[1].X := self.X;
   miniTiles[1].Y := self.Y;
   miniTiles[2].X := self.X + 8;
   miniTiles[2].Y := self.Y;
   miniTiles[3].X := self.X;
   miniTiles[3].Y := self.Y + 8;
   miniTiles[4].X := self.X + 8;
   miniTiles[4].Y := self.Y + 8;
   for i := 1 to 4 do
   begin
      miniTiles[i].location := self.location;
      miniTiles[i].ImageName := FLinkedFilename;
      changed[i] := true;
   end;
//now comes the fun part
//skip if there's nothing to change
   if neighbors <> [] then
   begin
   //otherwise:
   //first: case 46: one-tile island
      if neighbors * [n, e, w, s] = [n, e, w, s] then
      begin
         for i := 1 to 4 do
            changed[i] := false;
      end
      else
   //3 wall cases
   //42:
      if neighbors * [n, e, w] = [n, e, w] then
      begin
         inc(minis[3], 36);
         inc(minis[4], 36);
      end else
   //43:
      if neighbors * [n, e, s] = [n, e, s] then
      begin
         inc(minis[2], 36);
         inc(minis[4], 36);
//         changeAll;
      end else
   //44:
      if neighbors * [e, w, s] = [e, w, s] then
      begin
         inc(minis[1], 36);
         inc(minis[2], 36);
//         changeAll;
      end else
   //45:
      if neighbors * [n, w, s] = [n, w, s] then
      begin
         inc(minis[2], 36);
         inc(minis[3], 36);
//         changeAll;
      end else
   //full-corner cases
   //34:
      if neighbors * [n, nw] = [n, nw] then
      begin
         inc(minis[1], 24);
         inc(minis[2], 12);
         for i := 1 to 3 do
            changed[i] := false;
         if se in neighbors then
         begin
            inc(minis[4], 30);
            changed[4] := false;
         end;
      end else
   //36:
      if neighbors * [n, ne] = [n, ne] then
      begin
         inc(minis[1], 24);
         inc(minis[4], 12);
         changed[1] := false;
         changed[2] := false;
         changed[4] := false;
         if sw in neighbors then
         begin
            inc(minis[3], 30);
            changed[3] := false;
         end;
      end else
   //38:
      if neighbors * [s, se] = [s, se] then
      begin
         inc(minis[2], 12);
         inc(minis[3], 24);
         changed[2] := false;
         changed[3] := false;
         changed[4] := false;
         if nw in neighbors then
         begin
            inc(minis[1], 30);
            changed[1] := false;
         end;
      end else
   //40:
      if neighbors * [s, sw] = [s, sw] then
      begin
         inc(minis[1], 12);
         inc(minis[4], 24);
         changed[1] := false;
         changed[3] := false;
         changed[4] := false;
         if ne in neighbors then
         begin
            inc(minis[3], 30);
            changed[3] := false;
         end;
      end else
   //miscellaneous cases, which can be handled individually
      begin
   //store values for override
         for i := 1 to 4 do
            base[i] := minis[i];
   //set lone corners
         if nw in neighbors then
            changeBase(1, 36);
         if ne in neighbors then
            changeBase(2, 36);
         if sw in neighbors then
            changeBase(3, 36);
         if se in neighbors then
            changeBase(4, 36);
   //override values for walls
         if n in neighbors then
         begin
            offsetBase(1, 24);
            offsetBase(2, 24);
         end;
         if s in neighbors then
         begin
            offsetBase(3, 24);
            offsetBase(4, 24);
         end;
         if w in neighbors then
         begin
            offsetBase(1, 12);
            offsetBase(3, 12);
         end;
         if e in neighbors then
         begin
            offsetBase(2, 12);
            offsetBase(4, 12);
         end;
         for i := 1 to 4 do
            minis[i] := base[i];
      end; //end of misc block
   end; //end of skip

{   case TGameMap(Engine).animFrame of
   1:;
   2, 4:
      for i := 1 to 4 do
         inc(minis[i], 2);
      //end for
   3:
      for i := 1 to 4 do
         inc(minis[i], 4);
      //end for
   end; }
   for i := 1 to 4 do
   begin
      miniTiles[i].ImageIndex := minis[i];
      if not changed[i] then
         miniTiles[i].ImageName := self.ImageName;
   end;
end;

{ TOceanTile }

procedure TOceanTile.doPlace;
var
   minis, base: array[1..4] of word;
   i: byte;
   changed: array[1..4] of boolean;

   procedure ChangeAll;
   var i: integer;
   begin
      for I := 1 to 4 do
         changed[i] := true;
   end;

begin
   minis[1] := 0;
   minis[2] := minis[1] + 1;
   minis[3] := minis[1] + 6;
   minis[4] := minis[3] + 1;
   miniTiles[1].X := self.X;
   miniTiles[1].Y := self.Y;
   miniTiles[2].X := self.X + 8;
   miniTiles[2].Y := self.Y;
   miniTiles[3].X := self.X;
   miniTiles[3].Y := self.Y + 8;
   miniTiles[4].X := self.X + 8;
   miniTiles[4].Y := self.Y + 8;
   for i := 1 to 4 do
   begin
      miniTiles[i].location := self.location;
      changed[i] := false;
   end;
//now comes the fun part
//skip if there's nothing to change
   if neighbors <> [] then
   begin
//otherwise:
//first: case 46: one-tile island
      if neighbors * [n, e, w, s] = [n, e, w, s] then
         changeAll
      else
   //3 wall cases
   //42:
      if neighbors * [n, e, w] = [n, e, w] then
      begin
         inc(minis[3], 6);
         inc(minis[4], 6);
         for i := 1 to 4 do
         begin
            miniTiles[i].ImageName := self.ImageName;
            changed[i] := true;
         end
      end else
   //43:
      if neighbors * [n, e, s] = [n, e, s] then
      begin
         inc(minis[2], 6);
         inc(minis[4], 6);
         changeAll;
      end else
   //44:
      if neighbors * [s, e, w] = [s, e, w] then
      begin
         inc(minis[1], 6);
         inc(minis[3], 6);
         changeAll;
      end else
   //45:
      if neighbors * [n, s, w] = [n, s, w] then
      begin
         inc(minis[1], 6);
         inc(minis[3], 6);
         changeAll;
      end else
   //full-corner cases
   //34:
      if neighbors * [n, nw] = [n, nw] then
      begin
         inc(minis[2], 24);
         inc(minis[3], 12);
         for i := 1 to 3 do
            changed[i] := true;
         if se in neighbors then
         begin
            inc(minis[4], 36);
            changed[4] := true;
         end;
      end else
   //36:
      if neighbors * [n, ne] = [n, ne] then
      begin
         inc(minis[1], 24);
         changed[1] := true;
         changed[2] := true;
         inc(minis[4], 12);
         changed[4] := true;
         if sw in neighbors then
         begin
            inc(minis[3], 36);
            changed[3] := true;
         end;
      end else
   //38:
      if neighbors * [s, se] = [s, se] then
      begin
         inc(minis[2], 12);
         inc(minis[3], 24);
         for i := 2 to 4 do
            changed[i] := true;
         if nw in neighbors then
         begin
            inc(minis[1], 36);
            changed[1] := true;
         end;
      end else
   //40:
      if neighbors * [s, sw] = [s, sw] then
      begin
         inc(minis[1], 12);
         changed[1] := true;
         changed[3] := true;
         inc(minis[4], 24);
         changed[4] := true;
         if ne in neighbors then
         begin
            inc(minis[2], 36);
            changed[2] := true;
         end;
      end else
   //miscellaneous cases, which can be handled individually
      begin
   //store values for override
         for i := 1 to 4 do
            base[i] := minis[i];
   //set lone corners
         if nw in neighbors then
         begin
            inc(base[1], 36);
            changed[1] := true;
         end;
         if ne in neighbors then
         begin
            inc(base[2], 36);
            changed[2] := true;
         end;
         if sw in neighbors then
         begin
            inc(base[3], 36);
            changed[3] := true;
         end;
         if se in neighbors then
         begin
            inc(base[4], 36);
            changed[4] := true;
         end;
   //override values for walls
         if n in neighbors then
         begin
            base[1] := minis[1] + 24;
            base[2] := minis[2] + 24;
            changed[1] := true;
            changed[2] := true;
         end;
         if s in neighbors then
         begin
            base[3] := minis[3] + 24;
            base[4] := minis[4] + 24;
            changed[3] := true;
            changed[4] := true;
         end;
         if w in neighbors then
         begin
            base[1] := minis[1] + 12;
            base[3] := minis[3] + 12;
            changed[1] := true;
            changed[3] := true;
         end;
         if e in neighbors then
         begin
            base[2] := minis[2] + 12;
            base[4] := minis[4] + 12;
            changed[2] := true;
            changed[4] := true;
         end;
         for i := 1 to 4 do
            minis[i] := base[i];
      end; //end of misc block
   end; //end of skip

{   case TGameMap(Engine).animFrame of
   1:;
   2, 4:
      for i := 1 to 4 do
         inc(minis[i], 2);
      //end for
   3:
      for i := 1 to 4 do
         inc(minis[i], 4);
      //end for
   end;}

   for i := 1 to 4 do
   begin
      if not changed[i] then
      begin
         inc(minis[i], 36);
         miniTiles[i].ImageName := self.ImageName;
      end
      else miniTiles[i].ImageName := FLinkedFilename;
      miniTiles[i].ImageIndex := minis[i];
   end;
end;

{ TAnimTile }

procedure TAnimTile.draw;
begin
   if FHeartbeat = 0 then
      ImageIndex := FTileID + FDisplacement;
   inherited;
end;

class procedure TAnimTile.heartbeat;
begin
   inc(FHeartbeat);
   if FHeartbeat >= ANIM_RATE then
   begin
      FHeartbeat := 0;
      inc(FDisplacement, ANIM_STEP);
      if FDisplacement > ANIM_MAX then
         FDisplacement := 0;
   end;
end;

function TAnimTile.place(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet): TTileAttributes;
var
   tileRef: TTileRef;
begin
   tileRef.group := tileData.group;
   tileRef.tile := 0;
   result := inherited place(xCoord, yCoord, layer, tileRef, chip_data);
   FTileID := ImageIndex;
end;

(*
{ TEventTile }

procedure TEventTile.assign(data: TEventTile);
begin
   inherited assign(TTile(data));
end;

constructor TEventTile.create(baseEvent: TEvent; const AParent: TSpriteEngine; map: TMapUnit);
begin
   inherited Create(AParent, map);
   update(baseEvent.lastCurrentPage);
   self.X := baseEvent.location.X * TILESIZE;
   self.Y := baseEvent.location.Y * TILESIZE;
   FEvent := baseEvent;
end;

procedure TEventTile.update(newPage: TEventPage);
var
   data: TDecodeResult;
   x, y: integer;
begin
   if newpage <> nil then
   begin
      data := decode(newpage.whichChip + 10000);
      case newpage.zOrder of
         0: z := 3;
         1: z := 4;
         2: z := 8;
      end;
      x := trunc(self.X / TILESIZE);
      y := trunc(self.Y / TILESIZE);
      if assigned(TGameMap(FEngine)[upper, x, y]) then
      begin
         if (TGameMap(FEngine)[upper, x, y].Z > z) then
            TGameMap(FEngine)[upper, x, y].Z := z - 1;
         if TGameMap(FEngine)[lower, x, y].Z > TGameMap(FEngine)[upper, x, y].Z then
            TGameMap(FEngine)[lower, x, y].Z := TGameMap(FEngine)[upper, x, y].Z - 1;
      end;
   end
   else begin
      data := decode(10000);
      self.z := 10;
      self.visible := false;
   end;
   self.ImageIndex := data.baseTile;
   self.ImageName := FBaseMap.chipsetName + ' ' + data.group;
end;
*)

{ TLowerTile }

{procedure TLowerTile.bump(character: TObject);
var
   bumper, dummy: TAdditionSprite;
   I: Integer;
begin
   bumper := character as TAdditionSprite;
   if bumper is THeroSprite then
      for I := 0 to FOccupied.Count - 1 do
      begin
         dummy := FOccupied[i] as TAdditionSprite;
         if dummy.hasPage and (dummy.event.lastCurrentPage.startCondition in [by_touch, by_collision]) then
            GScriptEngine.executeEvent(dummy.event, dummy);
         //end if
      end
   else if bumper.hasPage and (bumper.event.lastCurrentPage.startCondition = by_collision) then
      for I := 0 to FOccupied.Count - 1 do
         if FOccupied[i] = GGameEngine.currentParty then
            GScriptEngine.executeEvent(bumper.event, bumper);
         //end if
      //end for
   //end if
end;}

function TLowerTile.canEnter: boolean;
begin
   result := inherited canEnter;
   if assigned(FUpperTile) then
      result := result and FUpperTile.canEnter;
end;

constructor TLowerTile.Create(const AParent: TSpriteEngine; tileset: string; const addself: boolean = false);
begin
   inherited Create(AParent, tileset, addself);
   FOccupied := TObjectList.Create(false);
end;

destructor TLowerTile.Destroy;
begin
   FOccupied.Clear;
   FOccupied.Free;
   upperTile.Free;
   inherited;
end;

function TLowerTile.hasCountertop: boolean;
begin
   result := assigned(FUpperTile) and  (taCountertop in FUpperTile.attributes);
end;

function TLowerTile.isOccupied: boolean;
begin
   result := FOccupied.Count > 0;
end;

function TLowerTile.open(exceptFor: TObject): boolean;
var
   I: Integer;
begin
   result := (inherited open(exceptFor));
   for I := 0 to FOccupied.Count - 1 do
      result := result and (FOccupied[i] = exceptFor);
   if assigned(FUpperTile) then
      result := result and FUpperTile.open(exceptFor);
   //end if
end;

procedure TLowerTile.placeUpper(const xCoord, yCoord: word; const layer: byte;
                    const tileData: TTileRef; chip_data: TTileSet);
begin
   assert (tileData.group > 18);
   upperTile := TUpperTile.Create(Engine, imageName, true);
   upperTile.place(xCoord, yCoord, layer, tileData, chip_data);
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

{ TMiniTile }

constructor TMiniTile.Create(const AParent: TBorderTile; tileset: string);
begin
   inherited Create(AParent);
   ImageName := tileset;
end;

end.
