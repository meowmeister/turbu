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
unit turbu_2k_title_menu;

interface
uses
   Types,
   turbu_defs,
   turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_menu_components;

type
   TTitleMenu = class(TGameMenuBox)
   protected
      procedure DrawText; override;
      procedure doCursor(position: smallint); override;
      procedure doButton(const input: TButtonCode); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
   end;

   TTitleScreen = class(TGameMenuBox)
   protected
      procedure DrawText; override;
   end;

implementation
uses
   SysUtils, Forms,
   turbu_constants, turbu_database, turbu_text_utils,
   turbu_2k_map_engine,
   rs_message,
   sdl_13, SDL_ImageManager, sg_defs, sg_utils;

{ TTitleMenu }

constructor TTitleMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
var
   i: integer;
begin
   inherited Create(parent, coords, main, owner);
   SetLength(FOptionEnabled, 3);
   for i := 0 to High(FOptionEnabled) do
      FOptionEnabled[i] := true;
end;

procedure TTitleMenu.DoButton(const input: TButtonCode);
begin
   if input <> btn_cancel then
      inherited DoButton(input);
   if input = btn_enter then
   begin
      case FCursorPosition of
         0:
         begin
            GGameEngine.NewGame;
            return;
         end;
         1: focusPage('Save', 1);
         2: Application.Terminate;
         else assert(false);
      end;
   end;
end;

procedure TTitleMenu.DoCursor(position: smallint);
var
   coords: TRect;
begin
   FCursorPosition := position;
   if self.focused then
   begin
      coords := rect(FBounds.left + 4, FBounds.Top + 6 + (16 * position), 56, 18);
      GMenuEngine.cursor.visible := true;
      GMenuEngine.cursor.layout(SdlRectToTRect(coords));
   end;
end;

procedure TTitleMenu.DrawText;
begin
   GFontEngine.drawText(GDatabase.vocab[V_MENU_NEW],  0, 0,  1);
   GFontEngine.drawText(GDatabase.vocab[V_MENU_LOAD], 0, 16, 1);
   GFontEngine.drawText(GDatabase.vocab[V_MENU_QUIT], 0, 32, 1);
end;

{ TTitleScreen }

procedure TTitleScreen.DrawText;
var
   imageName: string;
   image: TSdlImage;
   cls: TSdlImageClass;
begin
   cls := FEngine.Images.SpriteClass;
   FEngine.Images.SpriteClass := TSdlOpaqueImage;
   try
      imagename := format('Special Images\%s.png', [GDatabase.layout.titleScreen]);
      image := FEngine.Images.EnsureImage(imagename,
                                          '*TitleScreen',
                                          sgPoint(GDatabase.layout.width, GDatabase.layout.height));
   finally
      FEngine.Images.SpriteClass := cls;
   end;
   SDL_SetTextureBlendMode(image.surface, []);
   image.Draw;
end;

const
   TITLE_LAYOUT =
  '[{"Name": "Main", "Class": "TTitleMenu",   "Coords": [128, 148, 192, 212]},' +
   '{"Name": "BG",   "Class": "TTitleScreen", "Coords": [-8,  -8,  328, 248], "Background": true}]';

initialization
   TMenuEngine.RegisterMenuBoxClass(TTitleMenu);
   TMenuEngine.RegisterMenuBoxClass(TTitleScreen);
   TMenuEngine.RegisterMenuPage('Title', TITLE_LAYOUT);
end.
