unit turbu_2k_map_tiles;

interface
uses
   Generics.Collections,
   tiles, turbu_tilesets, turbu_map_sprites,
   SDL_sprite;

type
   TMapTile = class(TTile)
   private
      FOccupied: TMapSpriteList;

      function isOccupied: boolean;
      function IsCountertop: boolean; inline;
   protected
      FNeighbors: TNeighborSet;
   public
      constructor Create(const AParent: TSpriteEngine; tileset: string); override;
      destructor Destroy; override;
      function open(exceptFor: TObject): boolean; override;
      procedure bump(character: TObject);

      property occupied: boolean read isOccupied;
      property event: TMapSpriteList read FOccupied write FOccupied;
      property countertop: boolean read IsCountertop;
   end;

   TAnimTile = class(TMapTile)
   private
      class var
      FDisplacement: word;
      FStepFlag: boolean;
      class constructor Create;
      class procedure OnHeartbeat;
   public
      procedure Draw; override;
      function place(const xCoord, yCoord, layer: word; const tileData: TTileRef;
                    chip_data: TTileSet): TTileAttributes; override;
   end;

   TBorderTile = class;

   TMiniTile = class(TTile)
   public
      constructor Create(const AParent: TBorderTile; tileset: string);
   end;

   TBorderTile = class(TMapTile)
   private
      minitiles: array[1..4] of TMiniTile;
   protected
      procedure doPlace; virtual;
      procedure setEngine(newEngine: TSpriteEngine); override;
   public
      constructor Create(const AParent: TSpriteEngine; tileset: string); override;
      destructor Destroy; override;
      procedure Draw; override;
      function place(const xCoord, yCoord, layer: word; const tileData: TTileRef;
                    chip_data: TTileSet): TTileAttributes; override;
      function sharesBorder(neighbor: TTile): boolean; virtual;

      property neighbors: TNeighborSet read FNeighbors;
   end;

   TWaterTile = class abstract(TBorderTile)
   private
      class var
      FDisplacement: word;
      class constructor Create;
      class procedure OnHeartbeat;
   private
      FLinkedFilename: string;
   public
      function place(const xCoord, yCoord, layer: word; const tileData: TTileRef;
                    chip_data: TTileSet): TTileAttributes; override;
      procedure Draw; override;
      function sharesBorder(neighbor: TTile): boolean; override;
   end;

   TShoreTile = class(TWaterTile)
   protected
      procedure doPlace; override;
   end;

   TOceanTile = class(TWaterTile)
   protected
      procedure doPlace; override;
   end;

   TMapTileClass = class of TMapTile;

implementation
uses
   turbu_constants, turbu_maps, turbu_map_objects;

{ TMapTile }

procedure TMapTile.bump(character: TObject);
var
   bumper, dummy: TMapSprite;
   I: Integer;
begin
{$MESSAGE WARN 'Commented-out code in live unit'}
   bumper := character as TMapSprite;
   if bumper is TCharSprite then
      for I := 0 to FOccupied.Count - 1 do
      begin
         dummy := FOccupied[i] as TMapSprite;
         if dummy.hasPage and (dummy.event.currentPage.startCondition in [by_touch, by_collision]) then
{            GScriptEngine.executeEvent(dummy.event, dummy); };
      end
   else if bumper.hasPage and (bumper.event.currentPage.startCondition = by_collision) then
      for I := 0 to FOccupied.Count - 1 do
 {        if FOccupied[i] = GGameEngine.currentParty then
            GScriptEngine.executeEvent(bumper.event, bumper); };
end;

constructor TMapTile.Create(const AParent: TSpriteEngine; tileset: string);
begin
   inherited Create(AParent, tileset);
   FOccupied := TMapSpriteList.Create;
end;

destructor TMapTile.Destroy;
begin
   FOccupied.Clear;
   FOccupied.Free;
   inherited;
end;

function TMapTile.IsCountertop: boolean;
begin
   result := taCountertop in self.attributes;
end;

function TMapTile.isOccupied: boolean;
begin
   result := FOccupied.Count > 0;
end;

function TMapTile.open(exceptFor: TObject): boolean;
var
   I: Integer;
begin
   result := (inherited open(exceptFor));
   for I := 0 to FOccupied.Count - 1 do
      result := result and (FOccupied[i] = exceptFor);
end;

{ TBorderTile }

constructor TBorderTile.Create(const AParent: TSpriteEngine; tileset: string);
var i: byte;
begin
   inherited create(AParent, tileset);
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

function TBorderTile.place(const xCoord, yCoord, layer: word; const tileData:
                    TTileRef; chip_data: TTileSet): TTileAttributes;
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

function TBorderTile.sharesBorder(neighbor: TTile): boolean;
begin
   result := (neighbor.ClassType = self.ClassType) and //to keep unnecessary string comparisons down
             (neighbor.ImageName = self.ImageName);
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
      if neighbors * [n, w] = [n, w] then
      begin
         dec(minis[1], 14);
         dec(minis[2], 14);
         dec(minis[3], 14);
         if se in neighbors then
            dec(minis[4], 22);
      end else
   //36:
      if neighbors * [n, e] = [n, e] then
      begin
         dec(minis[1], 10);
         dec(minis[2], 10);
         dec(minis[4], 10);
         if sw in neighbors then
            dec(minis[3], 22);
      end else
   //38:
      if neighbors * [e, s] = [e, s] then
      begin
         inc(minis[2], 14);
         inc(minis[3], 14);
         inc(minis[4], 14);
         if nw in neighbors then
            dec(minis[1], 22);
      end else
   //40:
      if neighbors * [s, w] = [s, w] then
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

class constructor TWaterTile.Create;
begin
   EnsureBroadcastList;
   FBroadcastList.Add(TWaterTile.OnHeartbeat);
end;

procedure TWaterTile.Draw;
begin
   doPlace;
   inherited;
end;

function TWaterTile.place(const xCoord, yCoord, layer: word; const tileData: TTileRef;
                          chip_data: TTileSet): TTileAttributes;
begin
   FLinkedFilename := chip_data.Records[tileData.group].group.linkedFilename;
   result := inherited place(xCoord, yCoord, layer, tileData, chip_data);
end;

function TWaterTile.sharesBorder(neighbor: TTile): boolean;
begin
   result := neighbor is TWaterTile;
end;

class procedure TWaterTile.OnHeartbeat;
begin
   if FHeartbeat mod ANIM_RATE = 0 then
   begin
      FDisplacement := (FDisplacement + 1) mod ANIM_LCM;
   end;
end;

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

   procedure UnchangeAll;
   var i: integer;
   begin
      for I := 1 to 4 do
         changed[i] := false;
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
         unchangeAll
      else
   //3 wall cases
   //42:
      if neighbors * [n, e, w] = [n, e, w] then
      begin
         inc(minis[3], 12);
         inc(minis[4], 12);
         UnchangeAll;
      end else
   //43:
      if neighbors * [n, e, s] = [n, e, s] then
      begin
         inc(minis[1], 24);
         inc(minis[3], 24);
         UnchangeAll;
      end else
   //44:
      if neighbors * [e, w, s] = [e, w, s] then
      begin
         inc(minis[1], 12);
         inc(minis[2], 12);
         UnchangeAll;
      end else
   //45:
      if neighbors * [n, w, s] = [n, w, s] then
      begin
         inc(minis[2], 24);
         inc(minis[4], 24);
         UnchangeAll;
      end else
   //full-corner cases
   //34:
      if neighbors * [n, w] = [n, w] then
      begin
         inc(minis[2], 24);
         inc(minis[3], 12);
         for i := 1 to 3 do
            changed[i] := false;
         if se in neighbors then
         begin
            inc(minis[4], 36);
            changed[4] := false;
         end;
      end else
   //36:
      if neighbors * [n, e] = [n, e] then
      begin
         inc(minis[1], 24);
         inc(minis[4], 12);
         changed[1] := false;
         changed[2] := false;
         changed[4] := false;
         if sw in neighbors then
         begin
            inc(minis[3], 36);
            changed[3] := false;
         end;
      end else
   //38:
      if neighbors * [s, e] = [s, e] then
      begin
         inc(minis[2], 12);
         inc(minis[3], 24);
         changed[2] := false;
         changed[3] := false;
         changed[4] := false;
         if nw in neighbors then
         begin
            inc(minis[1], 36);
            changed[1] := false;
         end;
      end else
   //40:
      if neighbors * [s, w] = [s, w] then
      begin
         inc(minis[1], 12);
         inc(minis[4], 24);
         changed[1] := false;
         changed[3] := false;
         changed[4] := false;
         if ne in neighbors then
         begin
            inc(minis[2], 36);
            changed[2] := false;
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

   case FDisplacement mod 4 of
   0:;
   1, 3:
      for i := 1 to 4 do
         inc(minis[i], 2);
      //end for
   2:
      for i := 1 to 4 do
         inc(minis[i], 4);
      //end for
   end;

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
         inc(minis[3], 12);
         inc(minis[4], 12);
         ChangeAll;
      end else
   //43:
      if neighbors * [n, e, s] = [n, e, s] then
      begin
         inc(minis[1], 24);
         inc(minis[3], 24);
         changeAll;
      end else
   //44:
      if neighbors * [s, e, w] = [s, e, w] then
      begin
         inc(minis[1], 12);
         inc(minis[2], 12);
         changeAll;
      end else
   //45:
      if neighbors * [n, s, w] = [n, s, w] then
      begin
         inc(minis[2], 24);
         inc(minis[4], 24);
         changeAll;
      end else
   //full-corner cases
   //34:
      if neighbors * [n, w] = [n, w] then
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
      if neighbors * [n, e] = [n, e] then
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
      if neighbors * [s, e] = [s, e] then
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
      if neighbors * [s, w] = [s, w] then
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

   case FDisplacement mod 4 of
   0:;
   1, 3:
      for i := 1 to 4 do
         inc(minis[i], 2);
      //end for
   2:
      for i := 1 to 4 do
         inc(minis[i], 4);
      //end for
   end;

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

class constructor TAnimTile.Create;
begin
   EnsureBroadcastList;
   FBroadcastList.Add(TAnimTile.OnHeartbeat);
end;

procedure TAnimTile.draw;
begin
   if FStepFlag then
      ImageIndex := FTileID + ((FDisplacement mod 4) * 3);
   inherited Draw;
end;

class procedure TAnimTile.OnHeartbeat;
begin
   if FHeartbeat mod ANIM_RATE2 = 0 then
   begin
      FDisplacement := (FDisplacement + 1) mod ANIM_LCM;
      FStepFlag := true;
   end
   else FStepFlag := false;
end;

function TAnimTile.place(const xCoord, yCoord, layer: word; const tileData: TTileRef;
                         chip_data: TTileSet): TTileAttributes;
begin
   result := inherited place(xCoord, yCoord, layer, tileData, chip_data);
   FTileID := ImageIndex;
end;

{ TMiniTile }

constructor TMiniTile.Create(const AParent: TBorderTile; tileset: string);
begin
   inherited Create(AParent);
   ImageName := tileset;
end;

end.
