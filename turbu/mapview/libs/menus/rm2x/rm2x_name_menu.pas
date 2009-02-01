unit rm2x_name_menu;
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
   chipset_data, frames, menu_basis, rm2x_menu_components,
   {asphyreSprite} SDL_sprite;

type
   TPortraitPanel = class(TSystemFrame)
   private
      FPortrait: TSystemMiniTile;
      procedure setID(const Value: word);
   public
      procedure Draw; override;
      procedure moveTo(coords: TRect); override;

      property id: word write setID;
   end;

   TNameDisplay = class(TSystemFrame)
   private
      FName: string;
   public
      procedure Draw; override;

      property name: string read FName write FName;
   end;

   TLetterSelectorBox = class(TCustomScrollBox)
   private
      procedure backspace; inline;
      procedure evaluate;
      const
         EXTRAS = '0123456789 -''!@#$%^&*()~_\|+=/?:;,.<>"`';
         OPTIONS: array[1..4] of string = ('BACK', 'DEFAULT', 'CLEAR', 'DONE');
   protected
      procedure drawItem(id, x, y: word; color: byte); override;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure doSetup(value: integer); override;
      procedure doButton(const input: TButtonCode); override;
   end;

   TNameMenuPage = class(TMenuPage)
   private
      FPortraitDisplay: TPortraitPanel;
      FInfo: TOnelineLabelBox;
      FNameDisplay: TNameDisplay;
      FWorkBox: TLetterSelectorBox;

      function topLeftRect(input: TRect): TRect; inline;
      function topRightRect(input: TRect): TRect; inline;
      function bottomRightRect(input: TRect): TRect; inline;
      function bottomRect(input: TRect): TRect; inline;
   public
      constructor Create(parent: TSpriteEngine; coords: TRect; main: TMenuEngine); override;
      procedure setup(value: integer); override;
   end;

implementation
uses
   classes,
   commons, LDB, chipset_graphics, script_engine, text_graphics;

{ TPortraitPanel }

procedure TPortraitPanel.Draw;
begin
   if assigned(FPortrait) then //don't draw anything, even the
   begin            //frame, if there's no portrait to display.
      inherited Draw;
      FPortrait.Draw;
   end;
end;

procedure TPortraitPanel.moveTo(coords: TRect);
begin
   inherited moveTo(coords);
   if assigned(FPortrait) then
   begin
      FPortrait.X := FBounds.left + 8;
      FPortrait.Y := FBounds.top + 8;
   end;
end;

procedure TPortraitPanel.setID(const Value: word);
begin
   FPortrait.Free;
   if value <> 0 then
      FPortrait := loadPortrait(GParty[value].template.portrait, GParty[value].template.portraitIndex);
   FPortrait.X := FBounds.left + 8;
   FPortrait.Y := FBounds.top + 8;
end;

{ TNameMenuPage }

function TNameMenuPage.topLeftRect(input: TRect): TRect;
begin
   result := rect(input.Left, input.Top + 16, 64, 64);
end;

function TNameMenuPage.topRightRect(input: TRect): TRect;
begin
   result := rect(input.left + 64, input.Top + 16, input.right - 64, 32);
end;

function TNameMenuPage.bottomRightRect(input: TRect): TRect;
begin
   result := rect(input.left + 64, input.Top + 48, input.right - 64, 32);
end;

function TNameMenuPage.bottomRect(input: TRect): TRect;
begin
   result := rect(input.left, input.Top + 80, input.right, input.bottom - 92);
end;

constructor TNameMenuPage.Create(parent: TSpriteEngine; coords: TRect;  main: TMenuEngine);
begin
   inherited create(parent, coords, main);
   FWorkBox := TLetterSelectorBox.Create(parent, bottomRect(coords), main, self);
   registerComponent(FWorkBox);
   FPortraitDisplay := TPortraitPanel.Create(parent, topLeftRect(coords));
   registerComponent(FPortraitDisplay);
   FInfo := TOnelineLabelBox.Create(parent, topRightRect(coords));
   registerComponent(FInfo);
   FNameDisplay := TNameDisplay.Create(parent, bottomRightRect(coords));
   registerComponent(FNameDisplay);
end;

procedure TNameMenuPage.setup(value: integer);
begin
   inherited setup(value);
   FNameDisplay.name := GGameEngine.menuStr;
   FPortraitDisplay.id := GGameEngine.menuInt;
   if value = 0 then
      FInfo.text := 'Input text here'
   else FInfo.text := 'Enter a name';
end;

{ TNameDisplay }

procedure TNameDisplay.Draw;
begin
   inherited Draw;
   drawText(name, origin.x + 16, origin.Y + 10, 0);
end;

{ TLetterSelectorBox }

constructor TLetterSelectorBox.Create(parent: TSpriteEngine; coords: TRect;  main: TMenuEngine; owner: TMenuPage);
var
   I: word;
begin
   inherited Create(parent, coords, main, owner);
   FColumns := 13;
   FLastLineColumns := high(OPTIONS);
   FDisplayCapacity := ((coords.bottom - 16) div 16) * FColumns;
   for I := ord('A') to ord('Z') do
      FParsedText.Add(char(i));
   for I := ord('a') to ord('z') do
      FParsedText.Add(char(i));
   for I := 1 to length(EXTRAS) do
      FParsedText.add(EXTRAS[i]);
   for i := low(OPTIONS) to high(OPTIONS) do
      FParsedText.Add(OPTIONS[i]);
   setLength(FOptionEnabled, FParsedText.Count);
   for I := low(FOptionEnabled) to high(FOptionEnabled) do
      FOptionEnabled[i] := true;
   //end FOR
end;

procedure TLetterSelectorBox.backspace;
begin
   with TNameMenuPage(FOwner).FNameDisplay do
   begin
      Delete(FName, length(FName), 1);
   end;
end;

procedure TLetterSelectorBox.doButton(const input: TButtonCode);
var
   owner: TNameMenuPage;
   max, maxletter: smallint;
begin
   if input <> btn_cancel then
      inherited doButton(input);
   owner := TNameMenuPage(FOwner);
   max := high(FOptionEnabled);
   maxletter := max - high(OPTIONS);

   if input = btn_cancel then
   begin
      self.backspace;
      GCurrentEngine.mediaPlayer.playSystemSound(sfxCancel);
      self.evaluate;
   end;
   if (input = btn_enter) and (FOptionEnabled[FCursorPosition]) then
   begin
      if FCursorPosition in [0..maxletter] then
         with owner.FNameDisplay do
            name := name + FParsedText[FCursorPosition]
         //end WITH
      else if FCursorPosition = max - 3 then //BACK
         self.backspace
      else if FCursorPosition = max - 2 then //DEFAULT
         owner.FNameDisplay.name := GGameEngine.menuStr
      else if FCursorPosition = max - 1 then //CLEAR
         owner.FNameDisplay.name := ''
      else if FCursorPosition = max then     //DONE
      begin
         GGameEngine.menuStr := owner.FNameDisplay.name;
         self.return;
      end else raise EParseMessage.Create('Unimplemented TLetterSelectorBox option!');
      self.evaluate;
   end;
end;

procedure TLetterSelectorBox.doSetup(value: integer);
begin
   inherited doSetup(value);
   self.evaluate;
end;

procedure TLetterSelectorBox.drawItem(id, x, y: word; color: byte);
begin
   drawText(FParsedText[id], x, y, color);
end;

procedure TLetterSelectorBox.evaluate;
begin
   FOptionEnabled[high(FOptionEnabled) - 3] := TNameMenuPage(FOwner).FNameDisplay.name <> '';
   FOptionEnabled[high(FOptionEnabled) - 1] := FOptionEnabled[high(FOptionEnabled) - 3];
   FOptionEnabled[high(FOptionEnabled)] := FOptionEnabled[high(FOptionEnabled) - 3];
end;

end.
