unit turbu_map_interface;
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

type
   IRpgMap = interface(IInterface)
   ['{8B9CDCC2-AFB6-408C-88C3-2E50D145C901}']
      function GetTileset: integer;
      procedure SetTileset(const Value: integer);
      property tileset: integer read GetTileset write SetTileset;
   end;

   IMapMetadata = interface(IInterface)
   ['{2028AFBA-6E31-4B5D-92A0-8375DA9AEC53}']
      function GetID: integer;
      function GetName: string;
      function GetParent: integer;
      function GetTreeOpen: boolean;
      function GetMapEngine: string;

      property ID: integer read GetID;
      property name: string read GetName;
      property parent: integer read GetParent;
      property treeOpen: boolean read GetTreeOpen;
      property mapEngine: string read GetMapEngine;
   end;

   IMapTree = interface;

   IMapMetadataEnumerator = interface
   ['{4FD5CC25-13B8-4127-A67E-668909C3B977}']
      function GetCurrent: IMapMetadata;
      function MoveNext: boolean;
      property Current: IMapMetadata read GetCurrent;
   end;

   IMapTree = interface(IInterface)
   ['{ABB814B1-412E-4520-A6B0-A53C04200DFD}']
      function GetEnumerator: IMapMetadataEnumerator;
      function CurrentMap: integer;
      function Get(x: integer): IMapMetadata;
      function Count: integer;

      property Items[x: integer]: IMapMetadata read Get; default;
   end;

implementation

end.
