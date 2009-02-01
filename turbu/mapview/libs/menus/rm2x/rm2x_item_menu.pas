unit rm2x_item_menu;
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
   windows,
   chipset_data, menu_basis, rm2X_menu_components, rpg_list,
   {asphyreSprite} SDL_sprite;

type
   TItemMenuPage = class(TMenuPage)
   private
      FItemMenu: TGameItemMenu;
      FDescBox: TOnelineLabelBox;

      function bottomRect(input: TRect): TRect; inline;
      function topRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine); override;

      property descBox: TOnelineLabelBox read FDescBox;
      property itemMenu: TGameItemMenu read FItemMenu;
   end;

implementation

uses
   types, sysUtils, forms,
   commons, LDB, item_code, chipset_graphics, frames, script_engine,
   script_interface, text_graphics, rm2X_menu_engine;

{ TItemMenuPage }

function TItemMenuPage.topRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top, input.Right, 32);
end;

function TItemMenuPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 32, input.Right, input.Bottom - 32);
end;

constructor TItemMenuPage.Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine);
begin
   inherited Create(parent, coords, main);
   FItemMenu := TGameItemMenu.Create(parent, bottomRect(coords), main, self);
   FItemMenu.inventory := GParty.inventory;
   FItemMenu.onButton := TButtonFunc(GScriptExec.GetProcAsMethodN('standard_item_button'));
   FItemMenu.onCursor := TCursorFunc(GScriptExec.GetProcAsMethodN('standard_item_cursor'));
   FItemMenu.onSetup := TSetupFunc(GScriptExec.GetProcAsMethodN('standard_item_setup'));
   self.registerComponent(FItemMenu);
   FDescBox := TOnelineLabelBox.Create(parent, topRect(coords));
   self.registerComponent(FDescBox);
end;

end.
