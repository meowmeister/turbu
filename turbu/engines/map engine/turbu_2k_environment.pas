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
*****************************************************************************}unit turbu_2k_environment;

interface
uses
   Generics.Collections,
   turbu_heroes, turbu_database;

type
   T2kEnvironment = class
   private
      FDatabase: TRpgDatabase;
      FHeroes: TList<TRpgHero>;
   public
      constructor Create(database: TRpgDatabase);
      destructor Destroy; override;

      property heroes: TList<TRpgHero> read FHeroes;
   end;

var
   GSwitches: TArray<boolean>;

implementation
uses
   turbu_characters;

type
   THeroList = class(TObjectList<TRpgHero>);

var
   GEnvironment: T2kEnvironment;

{ T2kEnvironment }

constructor T2kEnvironment.Create(database: TRpgDatabase);
var
   hero: THeroTemplate;
   list: THeroList;
begin
   assert(GEnvironment = nil);
   FDatabase := database;
   list := THeroList.Create;
   list.OwnsObjects := true;
   FHeroes := list;
   database.hero.download;
   for hero in database.hero.Values do
      FHeroes.Add(TRpgHero.Create(hero));
   setLength(GSwitches, database.switch.count + 1);
end;

destructor T2kEnvironment.Destroy;
begin
   FHeroes.Free;
   GEnvironment := nil;
   inherited Destroy;
end;

end.
