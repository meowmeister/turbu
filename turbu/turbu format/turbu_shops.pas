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
unit turbu_shops;

interface
uses
   turbu_defs;

type
   TShopData = class
   private
      FShopType: TShopTypes;
      FMessageStyle: integer;
      FInventory: TIntArray;
   public
      constructor Create(shopType: TShopTypes; messageStyle: integer; inventory: TIntArray);
      property shopType: TShopTypes read FShopType;
      property messageStyle: integer read FMessageStyle;
      property inventory: TIntArray read FInventory;
   end;

implementation

{ TShopData }

constructor TShopData.Create(shopType: TShopTypes; messageStyle: integer;
  inventory: TIntArray);
begin
   FShopType := shopType;
   FMessageStyle := messageStyle;
   FInventory := inventory;
end;

end.
