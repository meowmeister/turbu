unit turbu_map_engine;
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
   Generics.Collections, Types, Classes, ComCtrls, Menus,
   turbu_plugin_interface, turbu_versioning, turbu_battle_engine,
   turbu_database_interface, turbu_map_interface,
   sg_defs,
   sdl_13;

type
   TMapEngineData = class(TRpgMetadata);

   TDeleteMapMode = (dmTree, dmSibling, dmTop, dmNone);

   TMapResizeEvent = function(const size: TSgPoint): TSgPoint of object;

   IBreakable = interface
   ['{70501C2F-5A15-4033-B3FF-17946D27A0E1}']
      procedure BreakSomething;
   end;

   ITurbuController = interface(IInterface)
   ['{020CD243-BE17-492B-898B-FACAC0DDDB10}']
      function MapResize(const size: TSgPoint): TSgPoint;
   end;

   TButtonPosition = (bpLayer, bpSave, bpPlay, bpCommand);

   ITurbuControllerEx = interface(ITurbuController)
   ['{236C7AAC-2AEF-4853-8962-37F909FCF090}']
      procedure TilesetChanged;
      procedure SetButton(button: TToolButton; position: TButtonPosition);
      procedure setLayer(const value: integer);
      procedure UpdateEngine(const filename: string);
   end;

   IMapEngine = interface(IInterface)
   ['{A5FDC982-D72E-448E-8E37-7865094C5B5E}']
      function initialize(window: TSdlWindow; const database: string): TSdlWindow;
      procedure registerBattleEngine(value: IBattleEngine);
      function setDefaultBattleEngine(name: string): boolean;
      procedure loadMap(map: IMapMetadata);
      procedure Play;
      function Playing: boolean;
      function mapTree: IMapTree;
      procedure NewGame;

      function getData: TMapEngineData;
      property data: TMapEngineData read getData;
   end;

   IDesignMapEngine = interface(IMapEngine)
   ['{B68B1D70-D95E-4CEB-B009-2197D7EC7642}']
      function GetTilesetImage(const index: byte): PSdlSurface;
      property tilesetImage[const index: byte]: PSdlSurface read GetTilesetImage;
      procedure SetCurrentLayer(const value: shortint);
      function GetCurrentLayer: shortint;
      function GetTileSize: TsgPoint;
      property CurrentLayer: shortint read GetCurrentLayer write SetCurrentLayer;
      function mapPosition: TSgPoint;
      procedure SetController(const value: ITurbuController);
      procedure ResizeWindow(rect: TRect);
      procedure scrollMap(const newPosition: TSgPoint);
      procedure setPaletteList(value: TArray<integer>);
      procedure draw(const position: TSgPoint; new: boolean);
      procedure doneDrawing;
      procedure Repaint;
      procedure doubleClick;
      procedure rightClick(const position: TSgPoint);
      procedure KeyDown(key: word; Shift: TShiftState);
      procedure KeyUp(key: word; Shift: TShiftState);
      function getAutosaveMaps: boolean;
      procedure setAutosaveMaps(const value: boolean);
      property autosaveMaps: boolean read getAutosaveMaps write setAutosaveMaps;
      procedure saveCurrent;
      procedure saveAll;
      function AddNewMap(parentID: integer): IMapMetadata;
      procedure EditMapProperties(mapID: integer);
      procedure DeleteMap(mapID: integer; deleteResult: TDeleteMapMode);
      procedure ClearButtons;
      procedure Reset;
      procedure Pause;
      procedure Stop;
      procedure EditDatabase;
   end;

   TMapEngine = class abstract (TRpgPlugBase, IMapEngine)
   private
      FData: TMapEngineData;
      function GetData: TMapEngineData;
   protected
      FBattleEngines: TDictionary<string, IBattleEngine>;
      FDefaultBattleEngine: IBattleEngine;
      fWindow: TSdlWindow;
      FInitialized: boolean;
      procedure cleanup; virtual;
   public
      destructor Destroy; override;
      procedure AfterConstruction; override;
      function initialize(window: TSdlWindow; const database: string): TSdlWindow; virtual;
      procedure registerBattleEngine(value: IBattleEngine);
      function setDefaultBattleEngine(name: string): boolean;
      procedure loadMap(map: IMapMetadata); virtual; abstract;
      procedure Play; virtual; abstract;
      function Playing: boolean; virtual; abstract;
      function MapTree: IMapTree; virtual; abstract;
      procedure NewGame; virtual; abstract;

      property data: TMapEngineData read GetData write FData;
   end;

   TMatrix<T> = class(TObject)
   private
      FMatrix: array of T;
      FWidth, FHeight: integer;
      function GetValue(X, Y: integer): T; inline;
      procedure SetValue(X, Y: integer; const Value: T); inline;

      procedure VerticalExpand(base: TMatrix<T>; position: integer);
      procedure VerticalContract(base: TMatrix<T>; position: integer);
      procedure HorizontalContract(base: TMatrix<T>; fromRow, toRow,
        position: integer);
      procedure HorizontalExpand(base: TMatrix<T>; fromRow, toRow,
        position: integer);
   public
      constructor Create(size: TSgPoint); overload;
      constructor Create(size: TSgPoint; base: TMatrix<T>; position: integer); overload;
      property value[X, Y: integer]: T read GetValue write SetValue; default;
      property width: integer read FWidth;
      property height:integer read FHeight;
   end;

implementation
uses
   sysUtils, TypInfo;

{ TMapEngine }

procedure TMapEngine.AfterConstruction;
begin
  inherited;
   assert(assigned(data));
   assert(data.name <> '');
   assert(data.version > TVersion.create(0, 0, 0));
end;

procedure TMapEngine.cleanup;
begin
   FreeAndNil(FBattleEngines);
end;

destructor TMapEngine.Destroy;
begin
   if FInitialized then
      self.cleanup;
   FData.Free;
   inherited Destroy;
end;

function TMapEngine.GetData: TMapEngineData;
begin
   Result := FData;
end;

function TMapEngine.initialize(window: TSdlWindow; const database: string): TSdlWindow;
begin
   FBattleEngines := TDictionary<string, IBattleEngine>.Create;
end;

procedure TMapEngine.registerBattleEngine(value: IBattleEngine);
begin
   if not FInitialized then
      raise ERpgPlugin.Create('Map engine not initialized!');
   if not FBattleEngines.ContainsValue(value) then
   begin
      FBattleEngines.Add(value.data.name, value);
      if FBattleEngines.Count = 1 then
         setDefaultBattleEngine(value.data.name);
   end;
end;

function TMapEngine.setDefaultBattleEngine(name: string): boolean;
var
   newEngine: IBattleEngine;
begin
   result := FBattleEngines.TryGetValue(name, newEngine);
   if result then
      FDefaultBattleEngine := newEngine;
end;

{ TMatrix<T> }

constructor TMatrix<T>.Create(size: TSgPoint);
begin
   inherited Create;
   setLength(FMatrix, size.X * size.Y);
   FWidth := size.x;
   FHeight := size.y;
end;

constructor TMatrix<T>.Create(size: TSgPoint; base: TMatrix<T>; position: integer);
begin
   if (position < 1) or (position > 9) then
      raise Exception.CreateFmt('Invalid position value: %d; valid values are 1..9', [position]);
   Create(size);
   try
      if base.FHeight <= FHeight then
         VerticalExpand(base, position)
      else verticalContract(base, position);
   except
      on E: ERangeError do
      begin
         E.Message := format('TMatrix<%s>: Range check error resizing a %dX%d matrix to %dX%d, position %d',
                             [PTypeInfo(TypeInfo(T)).Name, base.width, base.height, size.x, size.y, position]);
         raise;
      end;
   end;
end;

procedure TMatrix<T>.HorizontalExpand(base: TMatrix<T>; fromRow, toRow, position: integer);
var
   start, i: integer;
begin
   case position mod 3 of
      0: start := FWidth - base.Width;
      1: start := 0;
      2: start := (FWidth - base.Width) div 2;
   end;
   for i := 0 to base.Width - 1 do
      self[i + start, toRow] := base[i, fromRow];
end;

procedure TMatrix<T>.HorizontalContract(base: TMatrix<T>; fromRow, toRow, position: integer);
var
   start, i: integer;
begin
   case position mod 3 of
      0: start := 0;
      1: start := (base.Width - FWidth) div 2;
      2: start := base.Width - FWidth;
   end;
   for i := 0 to FWidth - 1 do
      self[i, toRow] := base[i + start, fromRow];
end;

procedure TMatrix<T>.VerticalExpand(base: TMatrix<T>; position: integer);
var
   start, i: integer;
begin
   case position div 3 of
      0: start := 0;
      1: start := (FHeight - base.Height) div 2;
      2: start := FHeight - base.Height;
   end;
   for i := 0 to base.Height - 1 do
      if base.width <= self.width then
         HorizontalExpand(base, i, i + start, position)
      else HorizontalContract(base, i, i + start, position);
end;

procedure TMatrix<T>.VerticalContract(base: TMatrix<T>; position: integer);
var
   start, i: integer;
begin
   case position div 3 of
      0: start := 0;
      1: start := (base.Height - FHeight) div 2;
      2: start := base.Height - FHeight;
   end;
   for i := 0 to FHeight - 1 do
      if base.width <= self.width then
         HorizontalExpand(base, i, i + start, position)
      else HorizontalContract(base, i, i + start, position);
end;

{$o-}
function TMatrix<T>.GetValue(X, Y: integer): T;
begin
   result := FMatrix[(Y * FWidth) + x];
end;

procedure TMatrix<T>.SetValue(X, Y: integer; const Value: T);
begin
   FMatrix[(Y * FWidth) + x] := Value;
end;

end.
