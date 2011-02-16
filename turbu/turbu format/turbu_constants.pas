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
   Messages,
   sg_defs;

{$J+}
const
   //game "constant variables"
   STAT_COUNT = 6;
   COMMAND_COUNT = 7;
   WEAPON_SLOTS = 2;
   ARMOR_SLOTS = 3;
   TOTAL_SLOTS = WEAPON_SLOTS + ARMOR_SLOTS;
   PORTRAITS_PER_SHEET = 16;
   SPRITES_PER_SHEET = 8;
   LAYERS: byte = 2;
   ANIM_RATE: byte = 44;  //for water tiles
   ANIM_RATE2: byte = 12; //for animated tiles
   MAXPARTYSIZE = 4;

   //sizes
   PORTRAIT_SIZE: TSgPoint    = (X: 48;  Y: 48);
   SPRITE_SIZE: TSgPoint      = (X: 24;  Y: 16);
   LOGICAL_SIZE: TSgPoint     = (X: 320; Y: 240);
   PHYSICAL_SIZE: TSgPoint    = (X: 640; Y: 480);
   TILE_SIZE: TSgPoint        = (X: 16; Y: 16);

   //variable types
   vt_none: integer  = -1;
   vt_integer        = 0;
   vt_boolean        = 1;
   vt_float          = 2;
   vt_string         = 3;
   vt_char           = 4;
   vt_object         = 5;
   vt_rpgHero        = 6;
   vt_rpgCharacter   = 7;
   vt_rpgParty       = 8;
   vt_rpgVehicle     = 9;
   vt_rpgMapobj      = 10;

   VT_ADDRESSES = [5..10]; //set of parameters that are pointers

   TYPENAMES: array[0..10] of string =
   ('integer', 'boolean', 'float', 'string', 'char', 'TObject', 'TRpgHero', 'TRpgCharacter',
    'TRpgParty', 'TRpgVehicle', 'TRpgMapObject');

    //custom message types
    WM_RENDER = WM_USER + 1;

resourcestring
   DESIGN_DB = 'design';
   DBNAME = 'project.tdb';
   PROJECT_DB = 'database';
   MAP_DB = 'maps';
   IMAGE_DB = 'images';
   SCRIPT_DB = 'scripts';

implementation

end.
