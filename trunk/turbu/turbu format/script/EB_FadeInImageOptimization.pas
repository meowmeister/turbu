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

unit EB_FadeInImageOptimization;

interface
uses
   EventBuilder;

procedure FadeInImageOptimization(value: TEBRoutine);

implementation
uses
   Generics.Collections,
   EB_Optimizations, EB_Maps;

procedure CheckFade(fade: TEBShowScreen);
var
   idx: integer;
   obj: TEBObject;
begin
   idx := fade.Owner.children.IndexOf(fade);
   if idx = 0 then
      Exit;
   obj := fade.Owner.children[idx - 1];
   if obj.ClassType = TEBNewImage then
      fade.Owner.Insert(idx - 1, TEBRenderPause.Create(nil));
end;

procedure FadeInImageOptimization(value: TEBRoutine);
var
   fades: TList<TEBShowScreen>;
   i: integer;
begin
   fades := TList<TEBShowScreen>.Create;
   try
      TCollector.CollectExactRecursive<TEBShowScreen>(value, fades);
      for i := 0 to fades.Count - 1 do
         CheckFade(fades[i]);
   finally
      fades.Free;
   end;
end;

end.
