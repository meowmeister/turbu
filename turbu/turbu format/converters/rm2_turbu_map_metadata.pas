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
   Generics.Collections,
   LMT, turbu_map_metadata, conversion_report;

type
   T2k2MapMetadata = class helper for TMapMetadata
   public
      constructor Convert(base: TMapTreeData; basetree: TFullTree; id: smallint);
   end;

   T2k2MapTree = class helper for TMapTree
   private
      function FindByID(list: TObjectList<TMapMetadata>; id: smallint): TMapMetadata;
      procedure verifyLookups;
   public
      constructor Convert(base: TFullTree; compact: boolean);
      procedure fixTree(base: TFullTree; conversionReport: IConversionReport);
   end;

implementation
uses
   Types, Math, SysUtils,
   turbu_sounds, rm2_turbu_sounds, turbu_containers;

{ T2k2MapMetadata }

constructor T2k2MapMetadata.Convert(base: TMapTreeData; basetree: TFullTree; id: smallint);
begin
   inherited Create;
   self.name := string(base.name);
   self.id := id;
   self.parent := base.parent;
   self.scrollPosition := point(base.unk05, base.unk06);
   self.treeOpen := base.treeOpen;
   self.bgmState := TInheritedDecision(base.bgmState);
   self.bgmData := TRpgMusic.Convert(base.bgmData);
   self.battleBgState := TInheritedDecision(base.battleBgState);
   self.canPort := TInheritedDecision(base.canPort);
   self.canEscape := TInheritedDecision(base.canEscape);
   self.canSave := TInheritedDecision(base.canSave);
   self.mapEngine := 'TURBU basic map engine';
end;

{ T2k2MapTree }

constructor T2k2MapTree.Convert(base: TFullTree; compact: boolean);
var
   i, j: integer;
   current: TMapTreeData;
begin
   self.Create;
   self.FMapEngines.Add('TURBU basic map engine');
   self.Capacity := base.getSize;
   i := 0;
   j := 0;
   while I <= base.getMax do
   begin
      current := base.getMapData(i);
      if assigned(current) and ((not current.isArea) or (i = 0)) then
      begin
         self.Add(TMapMetadata.Convert(base.getMapData(i), base, i));
         self.addLookup(j);
         inc(j)
      end
      else self.AddLookup(-1);
      inc(i);
   end;
   self.currentMap := base.currentMap;
   verifyLookups;
end;

function T2k2MapTree.FindByID(list: TObjectList<TMapMetadata>; id: smallint): TMapMetadata;
var
   enumerator: TMapMetadata;
begin
   result := nil;
   for enumerator in list do
      if enumerator.id = id then
         Exit(enumerator);
end;

procedure T2k2MapTree.fixTree(base: TFullTree; conversionReport: IConversionReport);
resourcestring
   MAP_PARENT_ERROR = 'Map "%s"''s ID is lower than its parent''s, and has been ' +
   'reordered.  This may cause script errors.';
var
   i, j: integer;
   tempList: TList<TMapMetadata>;
begin
   conversionReport.setCurrentTask('Resolving map tree conflicts');
   FTranslationTable.Free;
   FTranslationTable := TList<smallint>.Create;
   tempList := TList<TMapMetadata>.Create;
   try
      self.AddLookup(0);
      tempList.add(self[0]);
      for I := 1 to system.high(base.nodeSet) do
      begin
         j := base.nodeSet[i];
         if not base[j].isArea then
         begin
            tempList.Add(FindByID(self, j));
            self.AddLookup(tempList.Last.id);
         end
         else self.AddLookup(-1);
      end;
      self.OwnsObjects := false;
      self.Clear;
      self.AddRange(tempList);
      self.OwnsObjects := true;
   finally
      tempList.Free;
   end;
end;

procedure T2k2MapTree.verifyLookups;
var
   i: integer;
begin
   for I := 0 to self.Count - 1 do
      if self.lookup[i] <> -1 then
         assert(self[lookup[i]].id = i);
end;

end.
