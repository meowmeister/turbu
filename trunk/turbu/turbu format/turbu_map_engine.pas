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
   Generics.Collections, Types, Classes,
   turbu_plugin_interface, turbu_versioning, turbu_battle_engine,
   turbu_database_interface, turbu_map_interface,
   sg_defs,
   sdl_13;

type
   TMapEngineData = class(TRpgMetadata);

   TDeleteMapMode = (dmTree, dmSibling, dmTop, dmNone);

   IBreakable = interface
   ['{70501C2F-5A15-4033-B3FF-17946D27A0E1}']
      procedure BreakSomething;
   end;

   IMapEngine = interface(IInterface)
   ['{A5FDC982-D72E-448E-8E37-7865094C5B5E}']
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase);
      procedure registerBattleEngine(value: IBattleEngine);
      function setDefaultBattleEngine(name: string): boolean;
      function loadMap(map: IMapMetadata): IRpgMap;
      procedure Play;
      function Playing: boolean;
      procedure KeyDown(key: word; Shift: TShiftState);
      procedure KeyUp(key: word; Shift: TShiftState);

      function getData: TMapEngineData;
      property data: TMapEngineData read getData;
   end;

   IDesignMapEngine = interface(IMapEngine)
   ['{B68B1D70-D95E-4CEB-B009-2197D7EC7642}']
      function GetTilesetImage(const index: byte): PSdlSurface;
      property tilesetImage[const index: byte]: PSdlSurface read GetTilesetImage;
      procedure SetCurrentLayer(const value: shortint);
      function GetCurrentLayer: shortint;
      property CurrentLayer: shortint read GetCurrentLayer write SetCurrentLayer;
      function mapSize: TSgPoint;
      function mapPosition: TSgPoint;
      procedure scrollMap(const newPosition: TSgPoint);
      procedure setPaletteList(value: TList<integer>);
      procedure draw(const position: TSgPoint; new: boolean);
      procedure doneDrawing;
      procedure doubleClick;
      procedure rightClick(const position: TSgPoint);
      function getAutosaveMaps: boolean;
      procedure setAutosaveMaps(const value: boolean);
      property autosaveMaps: boolean read getAutosaveMaps write setAutosaveMaps;
      procedure saveCurrent;
      procedure saveAll;
      function AddNewMap(parentID: integer): IMapMetadata;
      procedure EditMapProperties(mapID: integer);
      procedure DeleteMap(mapID: integer; deleteResult: TDeleteMapMode);
      procedure Reset;
      procedure Pause;
      procedure Stop;
   end;

   TMapEngine = class abstract (TRpgPlugBase, IMapEngine)
   private
      FData: TMapEngineData;
      function GetData: TMapEngineData;
   protected
      FBattleEngines: TDictionary<string, IBattleEngine>;
      FDefaultBattleEngine: IBattleEngine;
      fWindow: TSdlWindowId;
      FInitialized: boolean;
      procedure cleanup; virtual;
   public
      destructor Destroy; override;
      procedure AfterConstruction; override;
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase); virtual;
      procedure registerBattleEngine(value: IBattleEngine);
      function setDefaultBattleEngine(name: string): boolean;
      function loadMap(map: IMapMetadata): IRpgMap; virtual; abstract;
      procedure Play; virtual; abstract;
      function Playing: boolean; virtual; abstract;
      procedure KeyDown(key: word; Shift: TShiftState); virtual; abstract;
      procedure KeyUp(key: word; Shift: TShiftState); virtual; abstract;

      property data: TMapEngineData read GetData write FData;
   end;

   TMatrix<T> = class(TObject)
   private
      FMatrix: array of array of T;
      FWidth, FHeight: integer;
      function GetValue(X, Y: integer): T; inline;
      procedure SetValue(X, Y: integer; const Value: T); inline;
   public
      constructor Create(size: TSgPoint);
      property value[X, Y: integer]: T read GetValue write SetValue; default;
      property width: integer read FWidth;
      property height:integer read FHeight;
   end;

implementation
uses
   sysUtils;

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

procedure TMapEngine.initialize(window: TSdlWindowId; database: IRpgDatabase);
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
   setLength(FMatrix, size.X, size.Y);
   FWidth := size.x;
   FHeight := size.y;
end;

{$Q-}{$R-}
function TMatrix<T>.GetValue(X, Y: integer): T;
begin
   if (x < 0) or (y < 0) or (x >= FWidth) or (Y >= FHeight) then
      raise ERangeError.Create('Matrix bounds out of range');
   result := FMatrix[X, Y];
end;

procedure TMatrix<T>.SetValue(X, Y: integer; const Value: T);
begin
   if (x < 0) or (y < 0) or (x >= FWidth) or (Y >= FHeight) then
      raise ERangeError.Create('Matrix bounds out of range');
   FMatrix[X, Y] := Value;
end;

end.
