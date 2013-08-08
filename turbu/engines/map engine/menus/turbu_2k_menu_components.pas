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
unit turbu_2k_menu_components;

interface
uses
   Types,
   tiles,
   turbu_2k_frames, turbu_2k_menu_basis, turbu_2k_items,
   sdl_sprite;

type
   TRm2kGameMenuBox = class(TGameMenuBox)
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
   end;

   TGameCashMenu = class(TRm2kGameMenuBox)
   public
      procedure Draw; override;
   end;

   TCustomScrollBox = class abstract(TRm2kGameMenuBox)
   private
      FNextArrow: TSystemTile;
      FPrevArrow: TSystemTile;
      FTimer: byte;
   protected
      FDisplayCapacity: byte;
      FTopPosition: smallint;

      procedure drawItem(id, x, y: word; color: byte); virtual; abstract;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      destructor Destroy; override;

      procedure doCursor(position: smallint); override;
      procedure Draw; override;
      procedure moveTo(coords: TRect); override;
   end;

   TCustomOnelineBox = class abstract(TRm2kGameMenuBox)
   end;

   TOnelineLabelBox = class(TCustomOnelineBox)
   private
      FText: string;
   public
      procedure moveTo(coords: TRect); override;
      procedure Draw; override;
      property text: string write FText;
   end;

   TOnelineCharReadout = class(TCustomOnelineBox)
   private
      FChar: word;
   public
      procedure Draw; override;
      property character: word write FChar;
   end;

   TCustomPartyPanel = class(TRm2kGameMenuBox)
   protected
      FPortrait: array[1..4] of TSprite;
      FCount: byte;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
      procedure moveTo(coords: TRect); override;
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;
   end;

   TGameItemMenu = class(TCustomScrollBox)
   private
      FInventory: TRpgInventory;

      procedure setInventory(const Value: TRpgInventory);
   protected
      procedure drawItem(id, x, y: word; color: byte); override;
   public
      constructor Create(parent: TmenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage);
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;

      property inventory: TRpgInventory read FInventory write setInventory;
   end;

function loadPortrait(const filename: string; const index: byte): TSprite;

implementation

uses
   SysUtils, Math,
   commons,
   turbu_text_utils, turbu_2k_environment, turbu_database, turbu_constants,
   turbu_heroes, turbu_characters,
   SDL_ImageManager,
   SDL_13;

{ TRm2kGameMenuBox }

constructor TRm2kGameMenuBox.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   inherited Create(parent, coords, main, owner);
end;

{ TGameCashMenu }

procedure TGameCashMenu.Draw;
var
   money: string;
   xPos, yPos: single;
begin
   inherited Draw;
   money := IntToStr(GEnvironment.money);
   yPos := origin.y + 10;
   xPos := GFontEngine.drawTextRightAligned(GDatabase.vocab['Money'],
                                            origin.X + FBounds.Right - 10,
                                            yPos, 1).x;
   GFontEngine.drawTextRightAligned(money, xPos - 4, yPos, 0);
end;

{ TCustomScrollBox }

constructor TCustomScrollBox.Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage);
begin
   FNextArrow := TSystemTile.Create(parent, parent.SystemGraphic.rects[srArrowD], ORIGIN, 0);
   FPrevArrow := TSystemTile.Create(parent, parent.SystemGraphic.rects[srArrowU], ORIGIN, 0);
   FNextArrow.ImageName := parent.SystemGraphic.filename;
   FPrevArrow.ImageName := parent.SystemGraphic.filename;
   FPrevArrow.Visible := false;
   FNextArrow.Visible := false;
   inherited Create(parent, coords, main, owner);
end;

destructor TCustomScrollBox.Destroy;
begin
   FNextArrow.free;
   FPrevArrow.free;
   inherited;
end;

procedure TCustomScrollBox.Draw;
var
  I: Integer;
  j, color: byte;
  max: smallint;
  origin: tpoint;
begin
   inherited Draw;
   origin := point(round(FCorners[topLeft].x - engine.WorldX), round(fcorners[topLeft].Y - engine.WorldY));
   max := FParsedText.count - (FLastLineColumns + 1);
   for I := FTopPosition to min(max, FTopPosition + FDisplayCapacity - 1) do
   begin
      j := i - FTopPosition;
      if FOptionEnabled[i] then
         color := 0
      else color := 3;
      drawItem(i, 13 + (j mod FColumns) * (columnWidth + SEPARATOR),
               (j div FColumns) * 15 + origin.y + 12, color)
   end;
   if FLastLineColumns > 0 then
      for I := max + 1 to FParsedText.count - 1 do
      begin
         j := i - (max + 1);
         if FOptionEnabled[i] then
            color := 0
         else color := 3;
         GFontEngine.drawTextCentered(FParsedText[i],
                          13 + (j mod FLastLineColumns) * (lastColumnWidth + SEPARATOR),
                          ((j div FLastLineColumns) + (i div FColumns)) * 15 + origin.y + 12,
                          color, lastColumnWidth);
      end;
   inc(FTimer);
   if FTimer > 9 then
   begin
      FPrevArrow.Draw;
      FNextArrow.Draw;
   end;
   if FTimer > 18 then
      FTimer := 0;
end;

procedure TCustomScrollBox.moveTo(coords: TRect);
begin
   inherited moveTo(coords);
   FPrevArrow.Y := FBounds.Top;
   FPrevArrow.X := FBounds.Left + trunc(FBounds.Right / 2) - trunc(FPrevArrow.PatternWidth / 2);
   FNextArrow.Y := FBounds.Top + FBounds.Bottom - 8;
   FNextArrow.X := FPrevArrow.X;
end;

procedure TCustomScrollBox.doCursor(position: smallint);
begin
   if FParsedText.Count = 0 then
      position := 0
   else if position >= FParsedText.Count then
      position := FParsedText.Count - 1;
   if position < FTopPosition then
      FTopPosition := position - (position mod FColumns)
   else if position >= FTopPosition + FDisplayCapacity then
      FTopPosition := (position - (position mod FColumns) + FColumns - FDisplayCapacity);
   inherited doCursor(position - FTopPosition);
   FCursorPosition := position;
   FPrevArrow.Visible := (FTopPosition > 0);
   FNextArrow.Visible := (FTopPosition + FDisplayCapacity < FParsedText.Count);
end;

{ TOnelineLabelBox }

procedure TOnelineLabelBox.Draw;
begin
   inherited Draw;
   if origin.x < 0 then
      assert(false);
   GFontEngine.drawText(FText, origin.x + 10, origin.y + 10, 0);
end;

procedure TOnelineLabelBox.moveTo(coords: TRect);
begin
   assert(coords.bottom = 32);
   inherited moveTo(coords);
end;

{ TOnelineCharReadout }

procedure TOnelineCharReadout.Draw;
var
   dummy: TRpgHero;
   yPos: integer;
begin
   inherited;
   dummy := GEnvironment.Heroes[FChar];
   yPos := origin.Y + 10;
   GFontEngine.drawText(dummy.name, origin.x + 8, yPos, 0);
   GFontEngine.drawText(GDatabase.vocab['StatShort-Lv'], origin.x + 86, yPos, 1);
   GFontEngine.drawTextRightAligned(IntToStr(dummy.level), origin.x + 116, yPos, 0);
   if dummy.highCondition = 0 then
      GFontEngine.drawText(GDatabase.vocab['Normal Status'], origin.X + 126, yPos, 0)
   else with GDatabase.conditions[dummy.highCondition] do
      GFontEngine.drawText(name, origin.X + 126, yPos, color);
   GFontEngine.drawText(GDatabase.vocab['StatShort-HP'], origin.X + 186, yPos, 1);
   GFontEngine.drawTextRightAligned(intToStr(dummy.hp), origin.x + 222, yPos, 0);
   GFontEngine.drawText('/', origin.x + 222, yPos, 0);
   GFontEngine.drawTextRightAligned(intToStr(dummy.maxHp), origin.X + 246, yPos, 0);
   GFontEngine.drawText(GDatabase.vocab['StatShort-MP'], origin.X + 254, yPos, 1);
   GFontEngine.drawTextRightAligned(intToStr(dummy.mp), origin.x + 290, yPos, 0);
   GFontEngine.drawText('/', origin.x + 290, yPos, 0);
   GFontEngine.drawTextRightAligned(intToStr(dummy.maxMp), origin.X + 314, yPos, 0);
end;

{ TCustomPartyPanel }

constructor TCustomPartyPanel.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
var
   i: Integer;
begin
   inherited Create(parent, coords, main, owner);
   for i := 1 to 4 do
      FPortrait[i] := nil;
   //end if
   setLength(FOptionEnabled, MAXPARTYSIZE);
end;

procedure TCustomPartyPanel.moveTo(coords: TRect);
var
  I: Integer;
begin
   inherited moveTo(coords);
   for I := 1 to 4 do
   if assigned(FPortrait[i]) then
   with FPortrait[i] do begin
      x := self.X + 8;
      y := self.Y + 8 + ((i - 1) * 56);
   end;
end;

procedure TCustomPartyPanel.doCursor(position: smallint);
var
   coords: TRect;
   origin2: TPoint;
   dummy: word;
   cursor: TSysFrame;
begin
   if self.FDontChangeCursor then
      position := self.FCursorPosition;

   origin2.X := round(FCorners[topLeft].x - engine.WorldX) + 58;
   if position <> -1 then
   begin
      origin2.Y := trunc(FCorners[topLeft].Y - engine.worldY + (position * 56)) + 6;
      coords := rect(origin2.x, origin2.Y, self.width - 66, 52)
   end
   else begin
      origin2.Y := trunc(FCorners[topLeft].Y - engine.worldY) + 6;
      dummy := GEnvironment.Party.openSlot - 1;
      dummy := 56 * dummy;
      coords := rect(origin2.x, origin2.Y, self.width - 66, dummy);
   end;
   cursor := (Engine as TMenuSpriteEngine).cursor;
   cursor.Visible := true;
   cursor.layout(coords);
   FCursorPosition := position;
   FDontChangeCursor := false;
end;

procedure TCustomPartyPanel.doSetup(value: integer);
var
   i: byte;
   template: TClassTemplate;
begin
   inherited doSetup(value);
   i := 1;
   FParsedText.Clear;
   while GEnvironment.Party[i] <> GEnvironment.Heroes[0] do
   begin
      template := TClassTemplate(GEnvironment.Party[i].template);
      FPortrait[i].Free;
      FPortrait[i] := loadPortrait(template.portrait, template.portraitIndex);
      FPortrait[i].x := self.X + 8;
      FPortrait[i].Y := self.Y + 8 + ((i - 1) * 56);
      FParsedText.Add(GEnvironment.Party[i].name);
      FOptionEnabled[i - 1] := true;
      inc(i);
   end;
   FCount := i - 1;
   for I := i to 4 do
   begin
      freeAndNil(FPortrait[i]);
      FOptionEnabled[i - 1] := false;
   end;
end;

{ TGameItemMenu }

constructor TGameItemMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
begin
   assert(coords.bottom mod 16 = 0);
   inherited Create(parent, coords, main, owner);
   FDisplayCapacity := trunc((coords.bottom - 16) / 8);
   FColumns := 2;
end;

procedure TGameItemMenu.doCursor(position: smallint);
begin
   if self.FDontChangeCursor then
      position := self.FCursorPosition;
   inherited doCursor(position);
   FDontChangeCursor := false;
end;

procedure TGameItemMenu.drawItem(id, x, y: word; color: byte);
begin
   GFontEngine.drawText(FParsedText[id], x, y, color);
   GFontEngine.drawText(':', x + 120, y, color);
   GFontEngine.drawTextRightAligned(intToStr((FInventory[id] as TRpgItem).quantity), x + 136, y, color)
end;

procedure TGameItemMenu.setInventory(const Value: TRpgInventory);
begin
   FInventory := Value;
   self.setup(0);
end;

procedure TGameItemMenu.doSetup(value: integer);
var
  i: Integer;
begin
   inherited doSetup(value);
   self.Visible := true;
   FParsedText.Clear;
   if assigned(FInventory) then
   begin
      SetLength(FOptionEnabled, FInventory.Count);
      FInventory.sort;
      for i := 0 to FInventory.Count - 1 do
         FParsedText.Add(TRpgItem(FInventory[i]).template.name);
      self.placeCursor(FSetupValue);
   end;
end;

{ Classless}

function loadPortrait(const filename: string; const index: byte): TSprite;
var
   engine: TSpriteEngine;
begin
   engine := GMenuEngine;
   Engine.Images.EnsureImage(format('portrait\%s', [filename]), filename);
   result := TSprite.Create(Engine);
   result.Visible := true;
   result.ImageName := filename;
   result.ImageIndex := index;
end;

end.
