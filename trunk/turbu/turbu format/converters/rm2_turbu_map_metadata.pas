unit rm2_turbu_map_metadata;
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
   LMT, turbu_map_metadata;

type
   T2k2MapMetadata = class helper for TMapMetadata
   public
      constructor Convert(base: TMapTreeData; id: smallint);
   end;

   T2k2MapTree = class helper for TMapTree
   public
      constructor Convert(base: TFullTree; compact: boolean);
   end;

implementation
uses
   types, generics.Collections,
   turbu_sounds, rm2_turbu_sounds;

type
   TMapTreeConversionAid = class(TMapTree); //hack to allow access to protected members

{ T2k2MapMetadata }

constructor T2k2MapMetadata.Convert(base: TMapTreeData; id: smallint);
begin
   inherited Create;
   self.name := string(base.name);
   self.id := id;
   self.parent := base.parent;
   self.generation := base.generation;
   self.scrollPosition := point(base.unk05, base.unk06);
   self.treeOpen := base.treeOpen;
   self.bgmState := TInheritedDecision(base.bgmState);
   self.bgmData := TRpgMusic.Convert(base.bgmData);
   self.battleBgState := TInheritedDecision(base.battleBgState);
   self.canPort := TInheritedDecision(base.canPort);
   self.canEscape := TInheritedDecision(base.canEscape);
   self.canSave := TInheritedDecision(base.canSave);
end;

{ T2k2MapTree }

constructor T2k2MapTree.Convert(base: TFullTree; compact: boolean);
var
   i, j: integer;
begin
   self.Create;
   addLookup(0);
   self.Capacity := base.getSize;
   i := 0;
   j := 0;
   while I <= base.getMax do
   begin
      if (not base.getMapData(i).isArea) or (i = 0) then
      begin
         if (i > 0) then
            inc(j)
         else j := base.lookup(i);
         self.Add(TMapMetadata.Convert(base.getMapData(i), i));
         if i > 0 then
            self.addLookup(j);
      end
      else self.AddLookup(-1);
      inc(i);
   end;
   self.currentMap := base.currentMap;
   setLength(TMapTreeConversionAid(self).FNodeSet, length(base.nodeSet));

end;

end.
