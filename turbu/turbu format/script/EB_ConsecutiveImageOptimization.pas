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

unit EB_ConsecutiveImageOptimization;

interface
uses
   EventBuilder;

procedure ConsecutiveImageOptimization(value: TEBRoutine);

implementation
uses
   Generics.Collections,
   EB_Optimizations, EB_Maps;

function CheckImage(image: TEBNewImage): boolean;
var
   idx: integer;
   obj: TEBObject;
begin
   result := false;
   idx := image.Owner.children.IndexOf(image);
   if idx = 0 then
      Exit;
   obj := image.Owner.children[idx - 1];
   if obj.ClassType = TEBNewImage then
   begin
      image.Owner.Insert(idx - 1, TEBRenderPause.Create(nil));
      result := true;
   end;
end;

procedure ConsecutiveImageOptimization(value: TEBRoutine);
var
   images: TList<TEBNewImage>;
   i: integer;
begin
   images := TList<TEBNewImage>.Create;
   try
      TCollector.CollectExactRecursive<TEBNewImage>(value, images);
      if images.Count < 2 then
         Exit;
      for i := 0 to images.Count - 1 do
         if CheckImage(images[i]) then
            Exit;
   finally
      images.Free;
   end;
end;

end.
