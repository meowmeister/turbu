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
unit turbu_2k_environment;

interface
uses
   Generics.Collections,
   turbu_heroes, turbu_database;

type
   T2kEnvironment = class
   private
      FDatabase: TRpgDatabase;
      FHeroes: TList<TRpgHero>;
      FSwitches: TArray<boolean>;
      function GetSwitch(const i: integer): boolean;
      procedure SetSwitch(const i: integer; const Value: boolean);
      function GetHero(const i: integer): TRpgHero;
      function GetHeroCount: integer;
   public
      constructor Create(database: TRpgDatabase);
      destructor Destroy; override;

      property Heroes[const i: integer]: TRpgHero read GetHero;
      property HeroCount: integer read GetHeroCount;
      property Switch[const i: integer]: boolean read GetSwitch write SetSwitch;
   end;

var
   GEnvironment: T2kEnvironment;

implementation
uses
   Commons,
   turbu_characters;

type
   THeroList = class(TObjectList<TRpgHero>);

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
   setLength(FSwitches, database.switch.count + 1);
end;

destructor T2kEnvironment.Destroy;
begin
   FHeroes.Free;
   GEnvironment := nil;
   inherited Destroy;
end;

function T2kEnvironment.GetHero(const i: integer): TRpgHero;
begin
   if clamp(i, 0, FHeroes.Count) = i then
      result := FHeroes[i]
   else result := FHeroes[0];
end;

function T2kEnvironment.GetHeroCount: integer;
begin
   result := FHeroes.Count - 1;
end;

function T2kEnvironment.GetSwitch(const i: integer): boolean;
begin
   if clamp(i, 0, high(FSwitches)) = i then
      result := FSwitches[i]
   else result := false;
end;

procedure T2kEnvironment.SetSwitch(const i: integer; const Value: boolean);
begin
   if clamp(i, 0, high(FSwitches)) = i then
      FSwitches[i] := value;
end;

end.
