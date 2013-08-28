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
unit turbu_2k_inventory_menu;

interface
uses
   Types,
   turbu_defs,
   turbu_2k_menu_basis, turbu_2k_menu_components, turbu_2k_frames;

type
   TGameItemMenu = class(TCustomGameItemMenu)
   private
      procedure ItemButton(which: TButtonCode; theMenu: TGameMenuBox;
        theOwner: TMenuPage);
      procedure itemCursor(position: smallint; theMenu: TGameMenuBox;
        theOwner: TMenuPage);
      procedure itemSetup(position: integer; theMenu: TGameMenuBox;
        theOwner: TMenuPage);
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
   end;

implementation

uses
   turbu_2k_item_types;

{ TGameItemMenu }

constructor TGameItemMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
   self.onButton := ItemButton;
   self.onCursor := itemCursor;
   self.onSetup := itemSetup;
end;

procedure TGameItemMenu.ItemButton(which: TButtonCode; theMenu: TGameMenuBox;
  theOwner: TMenuPage);
begin
   if inventory.count = 0 then
      Exit;

   if (which = btn_enter) and (optionEnabled[cursorPosition]) then
   begin
      if inventory[cursorPosition] is TAppliedItem then
      begin
         focusPage('PartyTarget', cursorPosition * -1);
         if TAppliedItem(inventory[cursorPosition]).areaItem then
            parent.placeCursor(-1)
         else parent.placeCursor(0);
      end else if inventory[cursorPosition] is TSwitchItem then
      begin
         TSwitchItem(inventory[cursorPosition]).use;
         parent.leave(false);
      end;
   end;
end;

procedure TGameItemMenu.itemCursor(position: smallint; theMenu: TGameMenuBox; theOwner: TMenuPage);
begin
   if inventory.Count > 0 then
      (theOwner.menu('Desc') as TOnelineLabelBox).text := inventory[position].desc;
end;

procedure TGameItemMenu.itemSetup(position: integer; theMenu: TGameMenuBox; theOwner: TMenuPage);
var
   i: integer;
begin
   for i := 0 to inventory.Count - 1 do
      optionEnabled[i] := inventory[i].usableOnField;
end;

const INVENTORY_LAYOUT =
  '[{"Name": "Inventory", "Class": "TGameItemMenu",    "Coords": [0, 32, 320, 240]},' +
   '{"Name": "Desc",      "Class": "TOnelineLabelBox", "Coords": [0, 0,  320, 32 ]}]';

initialization
   TMenuEngine.RegisterMenuPage('Inventory', INVENTORY_LAYOUT);
   TMenuEngine.RegisterMenuBoxClass(TGameItemMenu);
end.
