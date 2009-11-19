unit map_tree_controller;
{ *****************************************************************************
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
  ***************************************************************************** }

interface
uses
   ComCtrls,
   turbu_map_metadata;

procedure buildMapTree(const data: TMapTree; tree: TTreeView);

implementation
uses
   windows, Generics.Collections, sysUtils;

procedure buildMapTree(const data: TMapTree; tree: TTreeView);
var
   nodeDic: TDictionary<word,TTreeNode>;
   enumerator: TMapMetadata;
   parent: TTreeNode;
//   i: integer;

   function findParentNode(const value: word): TTreeNode;
   begin
      nodeDic.TryGetValue(value, result);
   end;

begin
   nodeDic := TDictionary<word, TTreeNode>.Create;
   tree.Items.BeginUpdate;
   try
      tree.Items.Clear;
//      i := 0;
      for enumerator in data do
      begin
//         inc(i); //index variable for debugging
         parent := findParentNode(enumerator.parent);
         if assigned(parent) or (enumerator = data[0]) then
         begin
            nodeDic.Add(enumerator.id, tree.Items.AddChildObject(parent,
                        enumerator.name, enumerator));
            if assigned(parent) then
               parent.expanded := TMapMetadata(parent.data).treeOpen
//            else OutputDebugString(pWideChar(format('Map %s, id %d missing parent id %d.',[enumerator.name, enumerator.id, enumerator.parent])));
         end
         else
            assert(false);
      end;
      tree.Select(findParentNode(data.currentMap));
   finally
      tree.Items.EndUpdate;
      nodeDic.Free;
   end;
   tree.Selections[0].MakeVisible;
end;

end.
