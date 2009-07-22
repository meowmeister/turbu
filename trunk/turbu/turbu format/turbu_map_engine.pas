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
   Generics.Collections, types,
   turbu_plugin_interface, turbu_versioning, turbu_battle_engine,
   turbu_database_interface, turbu_map_interface,
   sg_defs,
   sdl_13;

type
   IMapEngine = interface(IInterface)
   ['{A5FDC982-D72E-448E-8E37-7865094C5B5E}']
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase);
      procedure registerBattleEngine(value: IBattleEngine);
      function setDefaultBattleEngine(name: string): boolean;
      function loadMap(map: IRpgMap; startPosition: TSgPoint): boolean;
   end;

   TMapEngineData = class(TRpgMetadata);

   TMapEngine = class abstract (TRpgPlugBase, IMapEngine)
   private
      FData: TMapEngineData;
   protected
      FBattleEngines: TDictionary<string, IBattleEngine>;
      FDefaultBattleEngine: IBattleEngine;
      fWindow: TSdlWindowId;
      FInitialized: boolean;
      procedure cleanup; virtual; abstract;
   public
      destructor Destroy; override;
      procedure AfterConstruction; override;
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase); virtual;
      procedure registerBattleEngine(value: IBattleEngine);
      function setDefaultBattleEngine(name: string): boolean;
      function loadMap(map: IRpgMap; startPosition: TSgPoint): boolean; virtual; abstract;

      property data: TMapEngineData read FData write FData;
   end;

   TMatrix<T> = class(TObject)
   private
      FMatrix: array of array of T;
      FWidth, FHeight: integer;
      function GetValue(X, Y: integer): T; inline;
      procedure SetValue(X, Y: integer; const Value: T); inline;
   public
      constructor Create(size: TSgPoint);
      property Value[X, Y: integer]: T read GetValue write SetValue; default;
   end;

implementation
uses
   sysUtils;

type
   ERpgPlugin = class(Exception); //hack; remove when compiler's fixed

{ TMapEngine }

procedure TMapEngine.AfterConstruction;
begin
  inherited;
   assert(assigned(data));
   assert(data.name <> '');
   assert(data.version > TVersion.create(0, 0, 0));
end;

destructor TMapEngine.Destroy;
begin
   if FInitialized then
      self.cleanup;
   FBattleEngines.Free;
   FData.Free;
   inherited Destroy;
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
   if (x < 0) or (y < 0) or (x > FWidth) or (Y > FHeight) then
      raise ERangeError.Create('Matrix bounds out of range');
   result := FMatrix[X, Y];
end;

procedure TMatrix<T>.SetValue(X, Y: integer; const Value: T);
begin
   if (x < 0) or (y < 0) or (x > FWidth) or (Y > FHeight) then
      raise ERangeError.Create('Matrix bounds out of range');
   FMatrix[X, Y] := Value;
end;

end.
