unit turbu_2k_map_engine;
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
   turbu_map_engine, turbu_versioning, turbu_database_interface,
   turbu_database,
   SG_defs,
   sdl_13;

type
   T2kMapEngine = class(TMapEngine)
   private
      FDatabase: TRpgDatabase;
      FWindowHandle: TSdlWindowId;
      FStretchRatio: TSgFloatPoint;
   protected
      procedure cleanup; override;
   public
      constructor Create; override;
      procedure initialize(window: TSdlWindowId; database: IRpgDatabase); override;
   end;

implementation
uses
   sysUtils,
   commons, turbu_plugin_interface, turbu_game_data;

{ T2kMapEngine }

procedure T2kMapEngine.cleanup;
begin
  assert(FInitialized);
   //do something eventually
end;

constructor T2kMapEngine.Create;
begin
  inherited;
  self.data := TMapEngineData.Create('TURBU basic map engine', TVersion.Create(0, 1, 0));
end;

procedure T2kMapEngine.initialize(window: TSdlWindowId; database: IRpgDatabase);
var
   db: I2kDatabase;
   layout: TGameLayout;
begin
   inherited initialize(window, database);
   if not supports(database, I2kDatabase, db) then
      raise ERpgPlugin.Create('Incompatible project database');
   FDatabase := db.dbObject;
   layout := FDatabase.layout;
   if window = 0 then
   begin
      FWindowHandle :=  SDL_CreateWindow(format('TURBU engine - %s', [FDatabase.mapTree[0].name]),
                        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                        layout.physWidth, layout.physHeight,
                        [sdlwOpenGl, sdlwShown, {sdlwResizable,} sdlwInputGrabbed]);
      //In RM2K, project title is stored in the LMT.  I'll need to convert that
      //to grab a project title. This will do for now.

      //add sdlwResizable if Sam's able to add in a "logical size" concept, or
      //if I end up doing it on this end

      if FWindowHandle = 0 then
         raise ERpgPlugin.Create('Unable to initialize SDL window: ' + LFCR + string(SDL_GetError));
   end
   else FWindowHandle := window;
   FStretchRatio.x := layout.physWidth / layout.width;
   FStretchRatio.y := layout.physHeight / layout.height;
end;

end.
