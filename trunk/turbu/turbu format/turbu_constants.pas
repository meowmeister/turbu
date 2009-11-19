unit turbu_constants;
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
   types;

const
   //game "constant variables"
   STAT_COUNT = 6;
   COMMAND_COUNT = 7;
   WEAPON_SLOTS = 2;
   ARMOR_SLOTS = 3;
   TOTAL_SLOTS = WEAPON_SLOTS + ARMOR_SLOTS;
   PORTRAITS_PER_SHEET = 16;
   SPRITES_PER_SHEET = 8;

   //sizes
   PORTRAIT_SIZE: TPoint  = (X: 48;  Y: 48);
   SPRITE_SIZE: TPoint    = (X: 24;  Y: 32);
   LOGICAL_SIZE: TPoint   = (X: 320; Y: 240);
   PHYSICAL_SIZE: TPoint  = (X: 640; Y: 480);
   TILE_SIZE: TPoint = (X: 16; Y: 16);

   //archive constants
   BASE_ARCHIVE      = 0;
   DATABASE_ARCHIVE  = 1;
   MAP_ARCHIVE       = 2;
   IMAGE_ARCHIVE     = 3;
   SCRIPT_ARCHIVE    = 4;
   MUSIC_ARCHIVE     = 5;

   //variable types
   vt_none: integer  = -1;
   vt_U8             = 0;
   vt_U16            = 1;
   vt_U32            = 2;
   vt_U64            = 3;
   vt_S8             = 4;
   vt_S16            = 5;
   vt_S32            = 6;
   vt_S64            = 7;
   vt_boolean        = 8;
   vt_float          = 9;
   vt_string         = 10;
   vt_char           = 11;
   vt_object         = 12;
   vt_rpgHero        = 13;
   vt_rpgCharacter   = 14;
   vt_rpgParty       = 15;
   vt_rpgVehicle     = 16;
   vt_rpgMapobj      = 17;

   VT_ADDRESSES = [12..17]; //set of parameters that are pointers

   TYPENAMES: array[0..17] of string =
   ('byte', 'word', 'cardinal', 'UInt64', 'shortint', 'smallint', 'integer', 'Int64',
    'boolean', 'float', 'string', 'char', 'TObject', 'TRpgHero', 'TRpgCharacter',
    'TRpgParty', 'TRpgVehicle', 'TRpgMapObject');

resourcestring
   DESIGN_DB = 'design';
   DBNAME = 'project.tdb';
   PROJECT_DB = 'database';
   MAP_DB = 'maps';
   IMAGE_DB = 'images';
   SCRIPT_DB = 'scripts';

implementation

end.
