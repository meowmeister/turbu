unit turbu_2k_map_engine_D;
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
   Generics.Collections, types, classes, SqlExpr, sysUtils,
   turbu_map_engine, turbu_2k_map_engine, turbu_database_interface,
   turbu_map_interface, turbu_tilesets, turbu_maps, turbu_2k_sprite_engine,
   turbu_map_sprites, mapobject_container,
   sdl_13, SG_defs, sdl_canvas;

type
   T2kMapEngineD = class(T2kMapEngine, IDesignMapEngine, IBreakable)
   private type
      TTriple = record
         x, y, z: integer;
         constructor Create(const coords: TsgPoint; layer: smallint); overload;
         constructor Create(x, y: integer; layer: smallint); overload;
      end;

      TUndoFrame = TArray<TPair<TTriple, TTileRef>>;

      TCell = array[0..1] of TTileRef;

      TSelection = class
      private
         FParent: T2kMapEngineD;
         FArea: types.TRect;
         FTiles: TArray<TCell>;
         FHook: TsgPoint;
         FHooked: boolean;
         FDirty: boolean;
         FSnapshot: TSdlRenderTarget;
        function GetPixArea: TRect;
      public
         constructor Create(parent: T2kMapEngineD);
         destructor Destroy; override;
         function Contains(const point: TsgPoint): boolean;
         procedure hook(const point: TsgPoint);
         procedure drag(const point: TsgPoint);
         procedure unhook;
         function Copy: string;
         constructor Paste(const json: string; origin: TsgPoint);

         property tiles: TArray<TCell> read FTiles;
         property area: TRect read FArea;
         property pixArea: TRect read GetPixArea;
         property hooked: boolean read FHooked;
         property dirty: boolean read FDirty;
      end;

   private
      FTilesetListD: TList<TTileGroupPair>;
      FTileSize: TSgPoint;
      FDrawFrom: TSgPoint;
      FDrawTo: TSgPoint;
      FPaintMode: TPaintMode;
      FDrawGuide: boolean;
      FPaletteList: TArray<integer>;
      FCurrentLayer: shortint;
      FAutosaveMaps: boolean;
      FExactDrawMode: boolean;
      FObjectContainers: TMapObjectContainerList;
      FCursorPosition: TSgPoint;
      FHookedObject: TMapSprite;
      FTopLeft: TSgPoint;
      FDBFilename: string;
      FController: ITurbuController;

      //undo data
      FBeingDrawn: TDictionary<TTriple, TTileRef>;
      FUndoStack: TStack<TUndoFrame>;

      FSelection: TSelection;

      function loadTilesetD(const value: TTileSet): TList<TTileGroupPair>;
      procedure saveMap(value: TRpgMap);
      procedure saveAndClearMapCache;
      procedure DrawGrid;
      procedure DrawCursor;
      procedure PrepareContainers;
      procedure ClearContainers;
      procedure ArrowKey(key: word);
      function NewMapObjectID(engine: T2kSpriteEngine): integer;
      function GetCurrentMapObject: TMapSprite;
      procedure DoDelete;
      function DoResize(map: TRpgMap; viewport: TRect): TRect;
      procedure UploadMapObjects;
      procedure Validate(query: TSqlQuery);

      procedure FillRect(bounds: TRect; const predicate: TPredicate<TsgPoint>);
      procedure InternalDraw(const position: TSgPoint; new: boolean);
      procedure InternalDrawRect(const position: TSgPoint; new: boolean);
      procedure InternalDrawPen(const position: TsgPoint; new: boolean);
      procedure InternalDrawFlood(const position: TSgPoint; new: boolean);
      procedure InternalDrawSelect(const position: TSgPoint; new: boolean);
      procedure DetermineFlood(const position: TsgPoint; out bounds: TRect;
         out floodSet: TDictionary<TsgPoint, boolean>);
      function FloodCompatible(const position: TsgPoint; tv: TTileRef): boolean;
      procedure TryFlood(const position: TsgPoint; var bounds: TRect;
         floodSet: TDictionary<TsgPoint, boolean>; open: TList<TsgPoint>;
         tv: TTileRef);
      function GetDelayDrawRect: TRect;
      procedure NewUndoFrame;
      procedure beingDrawn(x, y: integer); overload;
      procedure BeingDrawn(x, y: integer; tileRef: TTileRef); overload;
      procedure DrawRow(y: integer; const drawRect: TRect;
        const predicate: TPredicate<TsgPoint>);
   private //IBreakable
      procedure BreakSomething;
   private //IDesignMapEngine
      function initializeDesigner(window: TSdlWindow; const database: string): TSdlWindow;
      function GetTilesetImageSize(const index: byte): TSgPoint;
      function GetTilesetImage(const index: byte): PSdlSurface;
      function GetTileSize: TsgPoint;
      procedure DesignLoadMap(map: IMapMetadata);
      function mapPosition: TSgPoint;
      procedure SetController(const value: ITurbuController);
      procedure ResizeWindow(rect: TRect);
      procedure scrollMap(const newPosition: TSgPoint);
      procedure setPaletteList(value: TArray<integer>);
      procedure SetPaintMode(value: TPaintMode);
      procedure SetExactDrawMode(value: boolean);
      procedure draw(const position: TSgPoint; new: boolean);
      procedure Undo;
      procedure doneDrawing;
      procedure doubleClick;
      procedure rightClick(const position: TSgPoint);
      procedure KeyDown(key: word; Shift: TShiftState);
      procedure KeyUp(key: word; Shift: TShiftState);
      procedure SetCurrentLayer(const value: shortint);
      function GetCurrentLayer: shortint;
      function getAutosaveMaps: boolean;
      procedure setAutosaveMaps(const value: boolean);
      procedure saveCurrent;
      procedure saveAll;
      function addNewMap(parentID: integer): IMapMetadata;
      procedure editMapProperties(mapID: integer);
      procedure DeleteMap(mapID: integer; deleteMode: TDeleteMapMode);
      procedure ClearButtons;
      procedure Reset;
      procedure Pause;
      procedure Stop;
      procedure EditDatabase;

      procedure IDesignMapEngine.loadMap = DesignLoadMap;
      function IDesignMapEngine.Initialize = InitializeDesigner;
      procedure ClearSelection;
      procedure PasteSelection;
      procedure DrawSelection;
   protected
      procedure cleanup; override;
      procedure AfterPaint; override;
      function GetValidateProc: TProc<TSqlQuery>; override;
   public
      function IsDesign: boolean; override;
   end;

implementation
uses
   commons, math, windows, Dialogs, Controls, Forms, clipbrd,
   logs, database,
   turbu_map_metadata, archiveInterface, turbu_constants, turbu_sdl_image, turbu_database,
   turbu_functional, turbu_plugin_interface, turbu_containers, turbu_map_objects,
   turbu_2k_transitions_graphics,
   eval, MapObject_Editor, dm_database, db_upgrade,
   sdl, sg_utils, sdlstreams,
   dwsJSON;

const
   MAX_WIDTH: byte = 6; //assumed to be 6, but we can't be sure beforetime

{ T2kMapEngineD }

function T2kMapEngineD.IsDesign: boolean;
begin
   result := true;
end;

procedure T2kMapEngineD.PasteSelection;
var
   sel: TSelection;
   center: TsgPoint;
begin
   try
      center := CenterPoint(FCurrentMap.viewport);
      sel := TSelection.paste(clipboard.AsText, center);
   except
      Exit; //swallow exceptions from the paste not working; this means it was invalid
   end;
   ClearSelection;
   FSelection := sel;
   FDrawFrom := FSelection.area.TopLeft;
   FDrawTo := FSelection.area.BottomRight;
   repaint;
end;

procedure T2kMapEngineD.KeyDown(key: word; Shift: TShiftState);
const
   VK_C = 67;
   VK_X = 88;
   VK_V = 86;
begin
   if FTimer.Enabled or (FCurrentLayer >= 0) then
      Exit;
   case key of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: ArrowKey(key);
      VK_RETURN: DoubleClick;
      VK_BACK, VK_DELETE: DoDelete;
   end;
   if (FPaintMode = pmSelect) and (shift = [ssCtrl]) then
   begin
      case key of
         VK_C: if assigned(FSelection) then
            Clipboard.AsText := FSelection.Copy;
         VK_V: PasteSelection;
      end;
   end;
end;

procedure T2kMapEngineD.KeyUp(key: word; Shift: TShiftState);
begin
   //not doing anything here
end;

procedure T2kMapEngineD.ArrowKey(key: word);
var
   position: TSgPoint;
begin
   position := FCursorPosition;
   case key of
      VK_UP: dec(position.y);
      VK_DOWN: inc(position.y);
      VK_LEFT: dec(position.x);
      VK_RIGHT: inc(position.x);
   end;
   position.x := safeMod(position.x, FCurrentMap.MapObj.Size.x);
   position.y := safeMod(position.y, FCurrentMap.MapObj.Size.y);
   draw(position, true);
end;

procedure T2kMapEngineD.BeingDrawn(x, y: integer; tileRef: TTileRef);
var
   loc: TTriple;
begin
   loc := TTriple.Create(x, y, FCurrentLayer);
   if not FBeingDrawn.ContainsKey(loc) then
      FBeingDrawn.Add(loc, tileRef);
end;

procedure T2kMapEngineD.beingDrawn(x, y: integer);
var
   loc: TTriple;
begin
   loc := TTriple.Create(x, y, FCurrentLayer);
   if not FBeingDrawn.ContainsKey(loc) then
      FBeingDrawn.Add(loc, FCurrentMap.mapObj.getTile(x, y, FCurrentLayer));
end;

function T2kMapEngineD.InitializeDesigner(window: TSdlWindow; const database: string): TSdlWindow;
begin
   if FInitialized then
      Exit(window);
   try
      FDBFilename := database;
      result := inherited initialize(window, database);
      FObjectContainers.Free;
      FObjectContainers := TMapObjectContainerList.Create;

      FBeingDrawn := TDictionary<TTriple, TTileRef>.Create;
      FUndoStack := TStack<TUndoFrame>.Create;
      //do more
   except
      if FInitialized then
         cleanup;
      raise;
   end;
end;

procedure T2kMapEngineD.AfterPaint;
var
   guideRect: TRect;
begin
   if (FCurrentLayer < 0) and (not FTimer.Enabled) then
   begin
      DrawGrid;
      DrawCursor;
   end
   else if FDrawGuide then
   begin
      if assigned(FSelection) then
         DrawSelection;
      guideRect := GetDelayDrawRect;
      inc(guideRect.Right);
      inc(guideRect.Bottom);
      guideRect := multiplyRect(guideRect, TILE_SIZE);
      dec(guideRect.Right);
      dec(guideRect.Bottom);
      guideRect := TRectToSdlRect(guideRect);
      dec(guideRect.Left, FTopLeft.x);
      dec(guideRect.Right, FTopLeft.y);
      FCanvas.DrawBox(guideRect, SDL_WHITE);
   end;
end;

procedure T2kMapEngineD.BreakSomething;
begin
   raise Exception.Create('Error Message');
end;

procedure T2kMapEngineD.cleanup;
begin
   //FObjectContainers needs to be cleaned up before the sprite engine is
   //destroyed in the call to inherited
   self.ClearContainers;
   inherited cleanup;
   FreeAndNil(FObjectContainers);
   FreeAndNil(FTilesetListD);
   FBeingDrawn.Free;
   FUndoStack.Free;
end;

procedure T2kMapEngineD.ClearButtons;
begin
   //nothing to do here just yet
end;

procedure T2kMapEngineD.ClearContainers;
begin
   if assigned(FObjectContainers) then
      FObjectContainers.Clear;
end;

procedure T2kMapEngineD.Reset;
begin
   self.cleanup;
   self.FDatabase := nil;
   self.FCanvas := nil;
   self.FCurrentMap := nil;
   self.FWaitingMap := nil;
   self.FImages := nil;
   self.FDefaultBattleEngine := nil;
   self.FInitialized := false;
end;

procedure T2kMapEngineD.ResizeWindow(rect: TRect);
var
   viewport: TRect;
   topleft: TSgPoint;
begin
   topleft := rect.TopLeft;
   viewport.TopLeft := FCurrentMap.viewport.TopLeft;
   viewport.BottomRight := (TSgPoint(rect.BottomRight) - topLeft) / TILE_SIZE;
   FCurrentMap.viewport := DoResize(FCurrentMap.mapObj, viewport);
end;

procedure T2kMapEngineD.rightClick(const position: TSgPoint);
begin
   if not FTimer.Enabled then
      Exit;

   if FCurrentMap.passable(position.x, position.y) then
   begin
      if FPartySprite = nil then
         initializeParty;
      FPartySprite.leaveTile;
      if not assigned(FCurrentMap.CurrentParty) then
         FCurrentMap.CurrentParty := FPartySprite;
      FPartySprite.location := position;
   end;
end;

procedure T2kMapEngineD.Undo;
var
   frame: TUndoFrame;
   pair: TPair<TTriple, TTileRef>;
begin
   if FUndoStack.Count = 0 then
   begin
      Application.MessageBox('Nothing to undo!', 'TURBU Editor');
      exit;
   end;
   frame := FUndoStack.Pop;
   for pair in frame do
      FCurrentMap.assignTile(pair.Key.x, pair.Key.y, pair.key.z, pair.Value);
   Repaint;
end;

procedure T2kMapEngineD.UploadMapObjects;
var
   obj: TRpgMapObject;
begin
   if FDatabaseOwner then
   begin
      dmDatabase.MapObjects.Active := false;
      dmDatabase.MapObjects.CreateDataset;
      for obj in FWaitingMap.mapObjects do
         dmDatabase.MapObjects.AppendRecord([obj.id, obj.name]);
   end;
end;

procedure T2kMapEngineD.DesignLoadMap(map: IMapMetadata);
var
   viewport: TRect;
begin
   if assigned(FCurrentMap) then
   begin
      self.ClearContainers;
      FUndoStack.Clear;
      FreeAndNil(FPartySprite);
      if not FCurrentMap.mapObj.modified then
         freeAndNil(FMaps[FCurrentMap.mapObj.id])
      else if FAutosaveMaps then
      begin
         saveMap(FCurrentMap.mapObj);
         freeAndNil(FMaps[FCurrentMap.mapObj.id])
      end;
   end;

   prepareMap(map);
   FCurrentMap := nil;
   viewport := createViewport(FWaitingMap, FScrollPosition);
   FTopLeft := viewport.TopLeft;
   viewport := DoResize(FWaitingMap, viewport);
   if not assigned(FMaps[FWaitingMap.id]) then
   begin
      FTilesetListD.Free;
      FTilesetListD := loadTilesetD(FDatabase.tileset[FWaitingMap.tileset]);
      FMaps[FWaitingMap.id] := T2kSpriteEngine.Create(FWaitingMap, viewport,
                                FShaderEngine, FCanvas, FDatabase.tileset[FWaitingMap.tileset],
                                FImages);
   end;
   FCurrentLayer := 0;
   UploadMapObjects;
   if doneLoadingMap then
      Repaint
   else raise Exception.Create('Error loading map');
end;

procedure T2kMapEngineD.DoDelete;
var
   sprite: TMapSprite;
   index: integer;
begin
   sprite := GetCurrentMapObject;
   if not assigned(sprite) then
      Exit;
   index := FObjectContainers.firstIndexWhere(
      function(value: TMapObjectContainer): boolean
      begin
         result := value.base = sprite;
      end);
   assert(index <> -1);
   FObjectContainers.Delete(index);
   FCurrentMap.MapObjects.Remove(sprite);
   FHookedObject := nil;
   FCurrentMap.Dead;
   self.repaint;
end;

procedure T2kMapEngineD.saveAll;
begin
   saveAndClearMapCache;
   saveCurrent;
end;

procedure T2kMapEngineD.saveAndClearMapCache;
var
   i: integer;
   map: T2kSpriteEngine;
begin
   for i := low(FMaps) to high(FMaps) do
   begin
      map := FMaps[i];
      if assigned(map) and (map <> FCurrentMap) then
      begin
         saveMap(map.mapObj);
         FreeAndNil(FMaps[i]);
      end;
   end;
end;

procedure T2kMapEngineD.saveCurrent;
begin
   saveMap(FCurrentMap.mapObj);
end;

procedure T2kMapEngineD.saveMap(value: TRpgMap);
var
   metadata: TMapMetadata;
   saveStream: TMemoryStream;
begin
   if not value.modified then
      Exit;

   saveStream := TMemoryStream.Create;
   try
      value.save(saveStream);
      metadata := FDatabase.mapTree.item[value.id];
      assert(metadata.name = value.Name);
      GArchives[MAP_ARCHIVE].writeFile(metadata.internalFilename.name, saveStream);
      value.modified := false;
   finally
      saveStream.Free;
   end;
end;

procedure T2kMapEngineD.setAutosaveMaps(const value: boolean);
begin
   FAutosaveMaps := value;
   if value then
      self.saveAndClearMapCache;
end;

procedure T2kMapEngineD.SetCurrentLayer(const value: shortint);
var
   enumerator: TTileGroupRecord;
   image: TRpgSdlImage;
   texture: TSdlTexture;
begin
   if (value < 0) and (FCurrentLayer >= 0) then
      PrepareContainers
   else if (value >= 0) and (FCurrentLayer < 0) then
      ClearContainers;
   FCurrentLayer := value;
   for enumerator in FCurrentMap.tileset.Records do
   begin
      image := retrieveImage('tileset', enumerator.group.filename);
      assert(assigned(image));
      texture := image.Texture;
      //TODO: this is a horrible hack and will need to be fixed eventually
      if assigned(texture.ptr) then
      begin
         if (value < 0) or (value in enumerator.layers) then
            texture.alpha := $FF
         else texture.alpha := $A0;
      end;
   end;
   self.repaint;
end;

procedure T2kMapEngineD.SetExactDrawMode(value: boolean);
begin
   FExactDrawMode := value;
end;

procedure T2kMapEngineD.SetPaintMode(value: TPaintMode);
begin
   FPaintMode := value;
end;

procedure T2kMapEngineD.SetController(const value: ITurbuController);
begin
   FController := value;
end;

procedure T2kMapEngineD.setPaletteList(value: TArray<integer>);
begin
   assert((value[0] = 1) or (length(value) mod value[0] = 1));
   FPaletteList := value;
end;

procedure T2kMapEngineD.Stop;
begin
   Pause;
   Assert(false, 'not implemented yet');
end;

procedure T2kMapEngineD.InternalDrawPen(const position: TsgPoint; new: boolean);
var
   drawRect: TRect;
begin
   drawRect.TopLeft := position;
   drawRect.Right := drawRect.Left + FPaletteList[0] - 1;
   drawRect.Bottom := drawRect.Top + (high(FPaletteList) div FPaletteList[0]) - 1;

   if new then
      FDrawFrom := position;

   FillRect(drawRect, nil);
end;

procedure T2kMapEngineD.InternalDrawRect(const position: TSgPoint; new: boolean);
begin
   if new then
      FDrawFrom := position;
   FDrawTo := position;
   FDrawGuide := true;
end;

procedure T2kMapEngineD.DrawRow(y: integer; const drawRect: TRect; const predicate: TPredicate<TsgPoint>);
var
   x, index: integer;
   offset, drawSize: TsgPoint;
   tileValue: TTileRef;
begin
   drawSize := sgPoint(FPaletteList[0], (length(FPaletteList) - 1) div FPaletteList[0]);
   for x := max(0, drawRect.Left) to min(drawRect.Right, FCurrentMap.Width - 1) do
   begin
      if assigned(predicate) and (predicate(sgPoint(x, y)) = false) then
         Continue;
      //(((a mod b) + b) mod b) to compensate for negative numbers
      offset := (((sgPoint(x, y) - FDrawFrom) mod drawSize) + drawSize) mod drawSize;
      index := (offset.y * FPaletteList[0]) + offset.x + 1;

      tileValue := FCurrentMap.tileset.tile(FPaletteList[index], FCurrentLayer);
      BeingDrawn(x,y);
      FCurrentMap.assignTile(x, y, FCurrentLayer, tileValue);
   end;
end;

procedure T2kMapEngineD.FillRect(bounds: TRect; const predicate: TPredicate<TsgPoint>);
var
   i, j: integer;
   tileRef: TTileRef;
begin
   for i := bounds.Top to bounds.Bottom do
      DrawRow(i, bounds, predicate);
   bounds := sg_utils.expandRect(bounds, 1);
   if not FExactDrawMode then
      for j := bounds.Top to bounds.Bottom do
         for I := bounds.Left to bounds.Right do
         begin
            tileRef := FCurrentMap.mapObj.GetTile(i, j, FCurrentLayer);
            if FCurrentMap.updateBorders(i, j, FCurrentLayer) then
               BeingDrawn(i, j, tileRef);
         end;
   FCurrentMap.Dead;
end;

function T2kMapEngineD.FloodCompatible(const position: TsgPoint; tv: TTileRef): boolean;
var
   group, tile, group2, tile2: integer;
   tv2: TTileRef;
   tileGroup: TTileGroupRecord;
begin
   tv2 := FCurrentMap.mapObj.getTile(position.x, position.y, FCurrentLayer);

   group := tv.group;
   tile := tv.tile;
   group2 := tv2.group;
   tile2 := tv2.tile;

   result := group = group2;
   tileGroup := FCurrentMap.tileset.Records[group];
   if result and not (tsBordered in tileGroup.group.tileType) then
      result := tile = tile2;
end;

procedure T2kMapEngineD.TryFlood(const position: TsgPoint; var bounds: TRect;
  floodSet: TDictionary<TsgPoint, boolean>; open: TList<TsgPoint>; tv: TTileRef);
begin
   if floodSet.ContainsKey(position) then
      Exit;
   if not FloodCompatible(position, tv) then
      Exit;
   floodSet.Add(position, true);
   if position.x > 0 then
      open.add(sgPoint(position.x - 1, position.y));
   if position.y > 0 then
      open.add(sgPoint(position.x, position.y - 1));
   if position.x < FCurrentMap.Width then
      open.add(sgPoint(position.x + 1, position.y));
   if position.y < FCurrentMap.Height then
      open.add(sgPoint(position.x, position.y + 1));
   bounds.Left := min(bounds.Left, position.x);
   bounds.Right := max(bounds.Right, position.x);
   bounds.Top := min(bounds.Top, position.y);
   bounds.Bottom := max(bounds.bottom, position.y);
end;

procedure T2kMapEngineD.DetermineFlood(const position: TsgPoint; out bounds: TRect;
  out floodSet: TDictionary<TsgPoint, boolean>);
var
   tv: TTileRef;
   open: TList<TsgPoint>;
   lastOpen: TArray<TsgPoint>;
   point: TsgPoint;
begin
   tv := FCurrentMap.mapObj.getTile(position.x, position.y, FCurrentLayer);
   bounds := rect(position, position);
   floodSet := TDictionary<TsgPoint, boolean>.Create;
   open := TList<TsgPoint>.Create;
   try
      try
         TryFlood(position, bounds, floodSet, open, tv);
         repeat
            lastOpen := open.ToArray;
            open.Clear;
            for point in lastOpen do
               TryFlood(point, bounds, floodSet, open, tv)
         until open.Count = 0;
      except
         floodSet.Free;
         raise;
      end;
   finally
      open.Free;
   end;
end;

procedure T2kMapEngineD.InternalDrawFlood(const position: TSgPoint; new: boolean);
var
   bounds: TRect;
   floodSet: TDictionary<TsgPoint, boolean>;
begin
   if not new then
      Exit;
   DetermineFlood(position, bounds, floodSet);
   try
      FillRect(bounds,
        function (position: TsgPoint): boolean
        begin result := floodSet.ContainsKey(position) end);
   finally
      floodSet.Free;
   end;
end;

procedure T2kMapEngineD.ClearSelection;
var
   x, y, z, counter: integer;
   cell: TCell;
   area: TRect;
begin
   if assigned(FSelection) then
   begin
      if FSelection.dirty then
      begin
         counter := 0;
         for y := FSelection.area.Top to FSelection.area.Bottom do
            for x := FSelection.area.Left to FSelection.area.Right do
            begin
               if (x >= 0) and (y >= 0) and (x < FCurrentMap.Width) and (y < FCurrentMap.Height) then
               begin
                  cell := FSelection.tiles[counter];
                  for z := Low(TCell) to High(TCell) do
                     FCurrentMap.assignTile(x, y, z, cell[z]);
               end;
               inc(counter);
            end;

         area := sg_utils.expandRect(FSelection.area, 1);
         if not FExactDrawMode then
            for y := area.Top to area.Bottom do
               for x := area.Left to area.Right do
               begin
                  FCurrentMap.updateBorders(x, y, 0);
                  FCurrentMap.updateBorders(x, y, 1);
               end;
      end;
      FreeAndNil(FSelection);
   end;
end;

procedure T2kMapEngineD.InternalDrawSelect(const position: TSgPoint; new: boolean);
begin
   if assigned(FSelection) and FSelection.contains(position) and not FSelection.hooked then
      FSelection.hook(position)
   else if assigned(FSelection) and FSelection.hooked then
   begin
      FSelection.drag(position);
      FDrawFrom := FSelection.area.TopLeft;
      FDrawTo := FSelection.area.BottomRight;
   end
   else begin
      if new then
         ClearSelection;
      InternalDrawRect(position, new);
   end;
end;

procedure T2kMapEngineD.InternalDraw(const position: TSgPoint; new: boolean);
begin
   case FPaintMode of
      pmPen: InternalDrawPen(position, new);
      pmFlood: InternalDrawFlood(position, new);
      pmRect: InternalDrawRect(position, new);
//      pmEllipse: InternalDrawEllipse(position, new);
      pmSelect: InternalDrawSelect(position, new);
   end;
end;

procedure T2kMapEngineD.draw(const position: TSgPoint; new: boolean);
begin
   if FTimer.Enabled then
      Exit;

   if FCurrentLayer >= 0 then
      InternalDraw(position, new)
   else begin
      FCursorPosition := position;
      if not assigned(FHookedObject) then
         FHookedObject := self.GetCurrentMapObject;
   end;
   self.repaint;
end;

procedure T2kMapEngineD.DrawCursor;
var
   drawRect: TRect;
begin
   drawRect.TopLeft := FCursorPosition;
   drawRect.Right := drawRect.Left + 1;
   drawRect.Bottom := drawRect.Top + 1;
   drawRect := TRectToSdlRect(constrictRect(multiplyRect(drawRect, TILE_SIZE.x), 1));
   dec(drawrect.Left, trunc(FCurrentMap.WorldX));
   dec(drawrect.Top, trunc(FCurrentMap.WorldY));
   FCanvas.DrawBox(drawRect, SDL_WHITE);
end;

procedure T2kMapEngineD.DrawGrid;
var
   x, y: integer;
   renderer: TSdlRenderer;
begin
   renderer := FCanvas.Renderer;
   x := -(trunc(FCurrentMap.WorldX) mod TILE_SIZE.x);
   y := -(trunc(FCurrentMap.WorldY) mod TILE_SIZE.y);
   SDL_SetRenderDrawColor(renderer, SDL_BLACK);
   while x < FCanvas.Width do
   begin
      SDL_RenderDrawLine(renderer, x, 0, x, FCanvas.Height);
      inc(x, TILE_SIZE.x);
   end;
   while y < FCanvas.Height do
   begin
      SDL_RenderDrawLine(renderer, 0, y, FCanvas.Width, y);
      inc(y, TILE_SIZE.y);
   end;
end;

procedure T2kMapEngineD.EditDatabase;
var
   frmDatabase: TfrmDatabase;
begin
   frmDatabase := TFrmDatabase.Create(nil);
   try
      frmDatabase.ShowModal;
   finally
      frmDatabase.Free;
   end;
end;

function T2kMapEngineD.addNewMap(parentID: integer): IMapMetadata;
var
   meta: TMapMetadata;
   newmap: TRpgMap;
begin
   meta := FDatabase.mapTree.AddNewMetadata(parentID);
   newmap := TRpgMap.Create(meta);
   if eval.TfrmMapProperties.EditMap(meta, newMap) then
   begin
      result := meta;
      meta.internalFilename := GArchives[MAP_ARCHIVE].MakeValidFilename(format('%s.tmf', [meta.name]));
      saveMap(newMap);
      designLoadMap(result);
      FDatabase.save;
   end
   else begin
      result := nil;
      FDatabase.mapTree.Remove(meta);
      newmap.Free;
   end;
end;

procedure T2kMapEngineD.editMapProperties(mapID: integer);
var
   map: T2kSpriteEngine;
   oldsize: TSgPoint;
begin
   map := FMaps[mapID];
   assert(assigned(map));
   oldsize := map.mapObj.size;
   if eval.TfrmMapProperties.EditMap(FDatabase.mapTree[mapID], map.mapObj) then
   begin
      if oldsize <> map.mapObj.size then
      begin
         map.RecreateTileMatrix;
         FCurrentMap.viewport := DoResize(FCurrentMap.mapObj, FCurrentMap.viewport);
      end;
      self.repaint;
      FUndoStack.Clear;
   end;
end;

procedure T2kMapEngineD.DeleteMap(mapID: integer; deleteMode: TDeleteMapMode);

   procedure InternalDeleteMap(mapID: integer; deleteMode: TDeleteMapMode);
   var
      map, child: TMapMetadata;
      filename: string;
      children: TList<TMapMetadata>;
   begin
      map := FDatabase.mapTree[mapID];
      if not assigned(map) then
         raise ERpgPlugin.Create('Invalid map ID.');
      filename := map.internalFilename.name;
      GArchives[MAP_ARCHIVE].deleteFile(filename);
      mapID := map.ID;
      children := FDatabase.mapTree.ChildrenOf(mapID);
      try
         for child in children do
            case deleteMode of
               dmTree: deleteMap(child.id, dmTree);
               dmSibling: child.parent := map.parent;
               dmTop: child.parent := 0;
               dmNone: assert(false);
            end;

         FDatabase.mapTree.remove(map);
      finally
         children.Free;
      end;
   end;

begin
   internalDeleteMap(mapID, deleteMode);
//   FDatabase.mapTree.
end;

function T2kMapEngineD.getAutosaveMaps: boolean;
begin
   result := FAutosaveMaps;
end;

procedure T2kMapEngineD.doneDrawing;
begin
   FCurrentMap.Dead;
   FCurrentMap.mapObj.modified := true;
   if assigned(FHookedObject) then
   begin
      FObjectContainers.Delete(FObjectContainers.FirstIndexWhere(
         function(arg1: TMapObjectContainer): boolean
         begin
            result := (arg1.base = FHookedObject);
         end));
      FHookedObject.location := FCursorPosition;
      FHookedObject.event.location := FCursorPosition;
      FObjectContainers.Add(TMapObjectContainer.Create(FHookedObject, FCanvas));
      FHookedObject := nil;
      self.repaint;
   end;
   if FCurrentLayer >= 0 then
   begin
      FDrawGuide := false;
      if FPaintMode = pmRect then
      begin
         FillRect(GetDelayDrawRect, nil);
         repaint;
      end
      else if FPaintMode = pmSelect then
      begin
         FDrawGuide := true;
         if FSelection = nil then
            FSelection := TSelection.Create(self)
         else FSelection.Unhook;
         repaint;
      end;
      NewUndoFrame;
   end;
end;

function T2kMapEngineD.DoResize(map: TRpgMap; viewport: TRect): TRect;
begin
   if assigned(FController) then
      result := rect(viewport.TopLeft, FController.MapResize(map.size * TILE_SIZE) / TILE_SIZE)
   else result := viewport;
end;

procedure T2kMapEngineD.doubleClick;
var
   sprite: TMapSprite;
   obj: TRpgMapObject;
begin
   if FCurrentLayer >= 0 then
      Exit;

   sprite := self.GetCurrentMapObject;
   FHookedObject := nil;
   if assigned(sprite) then
   begin
      TfrmObjectEditor.EditMapObject(sprite.event, FCurrentMap.MapObj, FCurrentMap.tileset.name);
      sprite.updatePage(sprite.event.currentPage);
      self.repaint;
   end
   else begin
      obj := TFrmObjectEditor.NewMapObject(self.NewMapObjectID(FCurrentMap),
                                           FCurrentMap.MapObj, FCurrentMap.tileset.name);
      if assigned(obj) then
      begin
         sprite := FCurrentMap.AddMapObject(obj);
         FCurrentMap.mapObj.mapObjects.Add(obj);
         sprite.location := FCursorPosition;
         obj.location := FCursorPosition;
         FObjectContainers.Add(TMapObjectContainer.Create(sprite, FCanvas));
         self.repaint;
      end;
   end;
end;

function T2kMapEngineD.GetTilesetImageSize(const index: byte): TSgPoint;
var
   tileCount: integer;
begin

   //first calculate size
   //TODO: we shouldn't have to do this at load time.  It should be calculated
   //already and saved with the tileset
   FTilesize := FTilesetListD.last.Key.group.dimensions;
   MAX_WIDTH := FTilesetListD.Last.Value.Width div FTilesize.x;

   tileCount := TFunctional.reduce2<TTileGroupPair, integer>(FTilesetListD,
      function(const input1: TTileGroupPair; input2: integer): integer
      var
         kind: TTileType;
      begin
         if not (index in input1.Key.layers) then
            Exit(input2);

         kind := input1.Key.group.tileType;
         if tsBordered in kind then
            result := 1
         else
         begin
            result := input1.Value.Width div FTilesize.x;
            if kind = [] then
               result := (input1.Value.Height div FTilesize.y) * result;
         end;
         result := result + input2;
      end);

   result := FTilesize * sgPoint(MAX_WIDTH, Ceil(tileCount / MAX_WIDTH));
end;

function T2kMapEngineD.GetTileSize: TsgPoint;
const SIZE: TsgPoint = (x: 16; y: 16);
begin
   result := SIZE;
end;

function T2kMapEngineD.GetValidateProc: TProc<TSqlQuery>;
begin
   result := self.Validate;
end;

function T2kMapEngineD.GetCurrentLayer: shortint;
begin
   result := FCurrentLayer;
end;

function T2kMapEngineD.GetCurrentMapObject: TMapSprite;
begin
   result := FCurrentMap.mapObjects.firstWhere(
   function(obj: TMapSprite): boolean
   begin
      result := obj.location = FCursorPosition;
   end);
end;

function T2kMapEngineD.GetDelayDrawRect: TRect;
begin
   result := rect(min(FDrawFrom.x, FDrawTo.x), min(FDrawFrom.y, FDrawTo.y),
           max(FDrawFrom.x, FDrawTo.x), max(FDrawFrom.y, FDrawTo.y));
end;

procedure T2kMapEngineD.DrawSelection;
begin
   FCanvas.Draw(FSelection.FSnapshot, FSelection.GetPixArea.TopLeft);
end;

function T2kMapEngineD.GetTilesetImage(const index: byte): PSdlSurface;
var
   pair: TTileGroupPair;
   surfaceSize: TSgPoint;
   srcRect, dstRect: TRect;
   kind: TTileType;
begin
   assert(assigned(FTilesetListD));
   surfaceSize := GetTilesetImageSize(index);

   result := TSdlSurface.Create(surfaceSize.x, surfaceSize.y, 8);
   result.Fill(nil, 0);
   result.CopyPaletteFrom(FTilesetListD.Last.Value);
   dstRect := rect(point(0, 0), FTileSize);
   for pair in FTilesetListD do
   begin
      if not (index in pair.Key.layers) then
         Continue;

      kind := pair.key.group.tileType;
      if tsBordered in kind then
         srcRect := rect(point(0,0), FTileSize)
      else if kind = [tsAnimated] then
         srcRect := rect(0, 0, pair.Value.Width, FTileSize.y)
      else srcRect := rect(0, 0, pair.value.width, pair.value.height);
      if srcRect.Right + dstRect.Left > result.Width then
      begin
         dstRect.Left := 0;
         inc(dstRect.Top, FTileSize.y);
      end;
      dstRect.BottomRight := srcRect.BottomRight;
      result.BlitFrom(pair.Value, @srcRect, @dstRect);
      inc(dstRect.Left, srcRect.Right);
      inc(dstRect.Top, srcRect.bottom - FTileSize.y);
   end;
end;

function T2kMapEngineD.loadTilesetD(const value: TTileSet): TList<TTileGroupPair>;
begin
   result := value.Records.mapFP<TTileGroupPair>(
      function(const input: TTileGroupRecord): TTileGroupPair
      var
         filename: string;
         rw: PSDL_RWops;
      begin
         filename := 'tileset\' + input.group.filename + '.png';
         if not FImages.contains(input.group.filename) then
         begin
            rw := sdlstreams.SDLStreamSetup(GArchives[IMAGE_ARCHIVE].getFile(filename));
            result {newItem} := TTileGroupPair.Create(input,
              TRpgSdlImage.CreateSprite(FCanvas.Renderer, rw, '.png', input.group.filename, FImages).surface);
            TStream(rw.unknown).Free;
            SDLStreamCloseRWops(rw);
         end
         else
         begin
            result := TTileGroupPair.Create(input,
              (FImages.Image[input.group.filename] as TRpgSdlImage).surface);
         end;
      end);
end;

function T2kMapEngineD.mapPosition: TSgPoint;
begin
   if assigned(FCurrentMap) then
      result := sgPoint(trunc(FCurrentMap.WorldX), trunc(FCurrentMap.WorldY))
   else result := FTopleft;
end;

function T2kMapEngineD.NewMapObjectID(engine: T2kSpriteEngine): integer;
begin
   result := engine.mapObjects.reduce<integer>(
      function(const input1: TMapSprite; input2: integer): integer
      begin
         result := max(engine.mapObjects[0].event.id, input2);
      end) + 1;
end;

procedure T2kMapEngineD.NewUndoFrame;
var
   frame: TUndoFrame;
begin
   if (FBeingDrawn = nil) or (FBeingDrawn.Count = 0) then
      Exit;
   frame := FBeingDrawn.ToArray;
   FUndoStack.Push(frame);
   FBeingDrawn.Clear;
end;

procedure T2kMapEngineD.Pause;
begin
   FTimer.Enabled := false;
   FPlaying := false;
end;

procedure T2kMapEngineD.PrepareContainers;
var
   obj: TMapSprite;
begin
   for obj in FCurrentMap.mapobjects do
      FObjectContainers.Add(TMapObjectContainer.Create(obj, FCanvas));
end;

procedure T2kMapEngineD.scrollMap(const newPosition: TSgPoint);
begin
   FCurrentMap.ScrollMap(newPosition);
   self.repaint;
end;

procedure T2kMapEngineD.Validate(query: TSqlQuery);
var
   version: integer;
begin
   query.SQL.Text := 'select ID from DBDATA';
   query.Open;
   if query.RecordCount <> 1 then
      raise EBadDB.CreateFmt('Invalid DBDATA record count: %d. The database may be corrupted', [query.RecordCount]);
   version := query.FieldByName('ID').AsInteger;
   query.Close;
   if version < DBVERSION then
   begin
      if version >= MIN_DBVERSION then
      begin
         if (MessageDlg('This project is using an out-of-date database and can''t be loaded with the current map engine.  Would you like to upgrade the project now?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
            db_upgrade.UpgradeDatabase(dmDatabase, FDBFilename)
         else Abort;
      end
      else MessageDlg('This project is using an out-of-date database and can''t be loaded.', mtError, [mbOK], 0);
   end;
end;

{ T2kMapEngineD.TTriple }

constructor T2kMapEngineD.TTriple.Create(const coords: TsgPoint;
  layer: smallint);
begin
   x := coords.x;
   y := coords.y;
   z := layer;
end;

constructor T2kMapEngineD.TTriple.Create(x, y: integer; layer: smallint);
begin
   self.x := x;
   self.y := y;
   self.z := layer;
end;

{ T2kMapEngineD.TSelection }

constructor T2kMapEngineD.TSelection.Create(parent: T2kMapEngineD);
var
   size: TsgPoint;
   x, y, z, counter: integer;
   pixArea: TRect;
begin
   FArea := parent.GetDelayDrawRect;
   FParent := parent;
   size := sgPoint((FArea.Right + 1) - FArea.Left, (FArea.Bottom + 1) - FArea.Top);
   SetLength(FTiles, size.x * size.y);
   counter := 0;
   for y := FArea.Top to FArea.Bottom do
      for x := FArea.Left to FArea.Right do
      begin
         for z := Low(TCell) to High(TCell) do
            FTiles[counter][z] := parent.FCurrentMap.mapObj.getTile(x, y, z);
         inc(counter);
      end;
   pixArea := self.GetPixArea;
   FSnapshot := TSdlRenderTarget.Create(pixArea.BottomRight);
   FSnapshot.parent.pushRenderTarget;
   FSnapshot.SetRenderer;
   try
      FSnapshot.parent.DrawRect(GRenderTargets[RENDERER_MAIN], ORIGIN, pixArea);
   finally
      FSnapshot.parent.popRenderTarget;
   end;
end;

destructor T2kMapEngineD.TSelection.Destroy;
begin
   FSnapshot.Free;
   inherited;
end;

function T2kMapEngineD.TSelection.GetPixArea: TRect;
begin
   result := TRectToSdlRect(multiplyRect(FArea, SPRITE_SIZE));
   result.TopLeft := result.TopLeft - FParent.FScrollPosition;
end;

function T2kMapEngineD.TSelection.Contains(const point: TsgPoint): boolean;
begin
   Result := (point.x >= FArea.Left) and (point.x <= FArea.Right)
     and (point.y >= FArea.Top) and (point.y <= FArea.Bottom);
end;

procedure T2kMapEngineD.TSelection.drag(const point: TsgPoint);
var
   offset: TsgPoint;
begin
   offset := point - FArea.TopLeft;
   if offset <> FHook then
   begin
      offset := offset - FHook;
      inc(FArea.Left, offset.x);
      inc(FArea.Right, offset.x);
      inc(FArea.Top, offset.y);
      inc(FArea.Bottom, offset.y);
      FDirty := true;
   end;
end;

procedure T2kMapEngineD.TSelection.hook(const point: TsgPoint);
begin
   FHook := point - FArea.TopLeft;
   FHooked := true;
end;

procedure T2kMapEngineD.TSelection.unhook;
begin
   FHooked := false;
end;

function T2kMapEngineD.TSelection.Copy: string;
var
   writer: TdwsJSONWriter;
   cell: TCell;
   tile: TTileRef;
begin
   writer := TdwsJSONWriter.Create(nil);
   try
      writer.BeginObject;
         writer.WriteName('CLASS');
         writer.WriteString('Selection');
         writer.WriteName('WIDTH');
         writer.WriteInteger((FArea.Right - FArea.Left) + 1);
         writer.WriteName('HEIGHT');
         writer.WriteInteger((FArea.Bottom - FArea.Top) + 1);
         writer.WriteName('TILES');
         writer.BeginArray;
            for cell in FTiles do
            begin
               writer.BeginArray;
                  for tile in cell do
                     writer.WriteInteger(tile.value);
               writer.EndArray;
            end;
         writer.EndArray;
      writer.EndObject;

      result := writer.ToString;
   finally
      writer.Free;
   end;
end;

constructor T2kMapEngineD.TSelection.Paste(const json: string;
  origin: TsgPoint);
var
   obj: TdwsJSONObject;
   val: TdwsJSONValue;
   tile, tiles: TdwsJSONArray;
   i, w, h, z: integer;
begin
   obj := nil; //shut up, compiler warnings...
   if json = '' then
      Abort;
   try
      obj := TdwsJSONObject.ParseString(json) as TdwsJSONObject;
   except
      Abort;
   end;
   try
      val := obj.Items['CLASS'];
      if (val = nil) or (val.Value.AsString <> 'Selection') then
         Abort;
      w := obj.Items['WIDTH'].Value.AsInteger;
      h := obj.Items['HEIGHT'].Value.AsInteger;
      dec(origin.x, w div 2);
      dec(origin.y, h div 2);
      FArea.TopLeft := origin;
      FArea.Right := origin.x + w - 1;
      FArea.Bottom := origin.y + h - 1;
      tiles := obj.Items['TILES'] as TdwsJSONArray;
      setLength(FTiles, tiles.ElementCount);
      for i := 0 to high(FTiles) do
      begin
         tile := tiles.Elements[i] as TdwsJSONArray;
         for z := Low(TCell) to High(TCell) do
            FTiles[i][z].value := tile.Elements[z].Value.AsInteger;
      end;
   finally
      obj.Free;
   end;
   FDirty := true;
end;

end.
