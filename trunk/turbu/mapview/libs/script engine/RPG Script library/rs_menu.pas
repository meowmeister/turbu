unit rs_menu;
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
   shop_data;

   function prepareStore(merchandise: string): word;
   function shop(style: word; messageStyle: byte; store: word): boolean;

var
   GShops: array of TStoreInventory;

implementation
uses
   LDB, chipset_graphics, script_engine, commons;

function prepareStore(merchandise: string): word;
var
   i: smallint;
begin
   //is this a copy of a shop we've already created?
   for i := 0 to high(GShops) do
      if merchandise = GShops[i].base then
      begin
         result := i;
         Exit;
      end;

   //if not, create a new shop...
   setLength(GShops, length(GShops) + 1);
   GShops[high(GShops)] := TStoreInventory.Create(merchandise);
   result := high(GShops);
end;

function shop(style: word; messageStyle: byte; store: word): boolean;
begin
   GCurrentEngine.setCurrentShop(store);
   if messageStyle >= SHOP_STYLES then
      messageStyle := 0;
   if style > 2 then
      style := 0;
   GGameEngine.menuInt := ((messageStyle + 1) shl 4) or (style);
   GGameEngine.systemMenu(mnuShop);
   assert(GGameEngine.menuInt in [0, 1]);
   if GGameEngine.menuInt = 1 then
      result := true
   else result := false;
end;

function inputText(start: string; heroId: word): string;
begin
   GGameEngine.menuInt := heroID;
   GGameEngine.menuStr := start;
   GGameEngine.systemMenu(mnuName);
   result := GGameEngine.menuStr;
end;

var
   destroyer: smallint;

initialization

{$WARN FOR_VARIABLE OFF} //no other way to do it if I can't
                         //declare variables in this section
finalization
begin
   for destroyer := low(GShops) to high(GShops) do
      GShops[destroyer].free;
   finalize(GShops);
end;

end.
