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
unit turbu_2k_monster_party;

interface
uses
   Generics.Collections,
   turbu_monsters, turbu_classes,
   sg_defs, SDL_ImageManager;

type
   T2KMonster = class(TRpgObject)
   private
      FHP: integer;
      FMP: integer;
      FMaxHP: integer;
      FMaxMP: integer;
      FSprite: TSdlImage;
      FVisible: boolean;
      FPosition: TsgPoint;
      function getTemplate: TRpgMonster; inline;
      function IsDead: boolean; inline;
   protected
      class function templateClass: TDatafileClass; override;
   public
      constructor Create(template: TRpgMonster; element: TRpgMonsterElement; images: TSdlImages);
      procedure Draw;

      property template: TRpgMonster read getTemplate;
      property HP: integer read FHP write FHP;
      property MP: integer read FMP write FMP;
      property maxHP: integer read FMaxHP;
      property maxMP: integer read FMaxMP;
      property sprite: TSdlImage read FSprite;
      property visible: boolean read FVisible write FVisible;
      property dead: boolean read IsDead;
   end;

   T2kMonsterParty = class(TRpgObject)
   private
      FMonsters: TObjectList<T2kMonster>;
      function getTemplate: TRpgMonsterParty;
   protected
      class function templateClass: TDatafileClass; override;
   public
      constructor Create(template: TRpgMonsterParty; images: TSdlImages);
      destructor Destroy; override;
      procedure Draw;

      property template: TRpgMonsterParty read getTemplate;
      property monsters: TObjectList<T2kMonster> read FMonsters;
   end;

implementation
uses
   SysUtils,
   turbu_database;

{ T2KMonster }

constructor T2KMonster.Create(template: TRpgMonster; element: TRpgMonsterElement; images: TSdlImages);
begin
   inherited Create(template);
   FVisible := not element.invisible;
   FHP := template.stat[1];
   FMaxHP := FHP;
   FMP := template.stat[2];
   FMaxMP := FMP;
   FSprite := images.Image[format('MONSTER*%s', [template.filename])];
   FPosition := element.position;
end;

procedure T2KMonster.Draw;
begin
   if (not self.visible) or self.dead then
      Exit;
   FSprite.Draw(self.FPosition, []);
end;

function T2KMonster.getTemplate: TRpgMonster;
begin
   Result := inherited template as TRpgMonster;
end;

function T2KMonster.IsDead: boolean;
begin
   result := FHp <= 0;
end;

class function T2KMonster.templateClass: TDatafileClass;
begin
   result := TRpgMonster;
end;

{ T2kMonsterParty }

constructor T2kMonsterParty.Create(template: TRpgMonsterParty; images: TSdlImages);
var
   element: TRpgMonsterElement;
begin
   inherited Create(template);
   FMonsters := TObjectList<T2kMonster>.Create;
   for element in template.monsters do
      FMonsters.Add(T2kMonster.Create(GDatabase.monsters[element.monster], element, images));
end;

destructor T2kMonsterParty.Destroy;
begin
   FMonsters.Free;
   inherited Destroy;
end;

procedure T2kMonsterParty.Draw;
var
   monster: T2kMonster;
begin
   for monster in FMonsters do
      monster.draw;
end;

function T2kMonsterParty.getTemplate: TRpgMonsterParty;
begin
   result := inherited template as TRpgMonsterParty;
end;

class function T2kMonsterParty.templateClass: TDatafileClass;
begin
   result := TRpgMonsterParty;
end;

end.
