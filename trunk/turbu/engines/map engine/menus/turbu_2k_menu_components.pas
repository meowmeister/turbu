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
   TGameCashMenu = class(TGameMenuBox)
   public
      procedure DrawText; override;
   end;

   TCustomScrollBox = class abstract(TGameMenuBox)
   private
      FNextArrow: TSystemTile;
      FPrevArrow: TSystemTile;
      FTimer: byte;
   protected
      FDisplayCapacity: byte;
      FTopPosition: smallint;

      procedure drawItem(id, x, y, color: integer); virtual; abstract;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;

      procedure doCursor(position: smallint); override;
      procedure DrawText; override;
      procedure moveTo(coords: TRect); override;
   end;

   TCustomOnelineBox = class abstract(TGameMenuBox)
   end;

   TOnelineLabelBox = class(TCustomOnelineBox)
   private
      FText: string;
      procedure SetText(const Value: string);
   public
      procedure DrawText; override;
      property text: string write SetText;
   end;

   TOnelineCharReadout = class(TCustomOnelineBox)
   private
      FChar: word;
   public
      procedure DrawText; override;
      property character: word write FChar;
   end;

   TCustomPartyPanel = class(TGameMenuBox)
   protected
      FPortrait: array[1..4] of TSprite;
      FCount: byte;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect; main: TMenuEngine; owner: TMenuPage); override;
      procedure moveTo(coords: TRect); override;
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;
   end;

   TCustomGameItemMenu = class(TCustomScrollBox)
   private
      FInventory: TRpgInventory;

      procedure setInventory(const Value: TRpgInventory);
   protected
      procedure drawItem(id, x, y, color: integer); override;
   public
      constructor Create(parent: TMenuSpriteEngine; coords: TRect;
        main: TMenuEngine; owner: TMenuPage); override;
      procedure doSetup(value: integer); override;
      procedure doCursor(position: smallint); override;

      property inventory: TRpgInventory read FInventory write setInventory;
   end;

function loadPortrait(filename: string; const index: byte): TSprite;

implementation

uses
   SysUtils, Math,
   commons, ArchiveUtils,
   turbu_text_utils, turbu_2k_environment, turbu_database, turbu_constants,
   turbu_heroes, turbu_characters,
   SDL_ImageManager, SG_defs, sg_utils,
   SDL_13;

{ TGameCashMenu }

procedure TGameCashMenu.DrawText;
var
   money: string;
   xPos, yPos: single;
begin
   money := IntToStr(GEnvironment.money);
   yPos := 2;
   xPos := GFontEngine.drawTextRightAligned(GDatabase.vocab[V_MONEY_NAME],
                                            getRightSide,
                                            yPos, 2).x;
   GFontEngine.drawTextRightAligned(money, xPos - 4, yPos, 2);
   //not sure why this has to be drawn twice before it will draw right.  Some
   //arcane GL state issue, no doubt.
   xPos := GFontEngine.drawTextRightAligned(GDatabase.vocab[V_MONEY_NAME],
                                            getRightSide,
                                            yPos, 2).x;
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

procedure TCustomScrollBox.DrawText;
var
   i, j, color, max: Integer;
begin
   max := FParsedText.count - (FLastLineColumns + 1);
   for i := FTopPosition to min(max, FTopPosition + FDisplayCapacity - 1) do
   begin
      j := i - FTopPosition;
      if FOptionEnabled[i] then
         color := 1
      else color := 4;
      drawItem(i, 5 + (j mod FColumns) * (columnWidth + SEPARATOR),
               (j div FColumns) * 15 + origin.y + 4, color)
   end;
   if FLastLineColumns > 0 then
      for i := max + 1 to FParsedText.count - 1 do
      begin
         j := i - (max + 1);
         if FOptionEnabled[i] then
            color := 1
         else color := 4;
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

procedure TOnelineLabelBox.DrawText;
begin
   if origin.x < 0 then
      assert(false);
   GFontEngine.drawText(FText, origin.x + 2, origin.y + 2, 1);
end;

procedure TOnelineLabelBox.SetText(const Value: string);
begin
   FText := value;
   InvalidateText;
end;

{ TOnelineCharReadout }

procedure TOnelineCharReadout.DrawText;
var
   dummy: TRpgHero;
   yPos: integer;
begin
   dummy := GEnvironment.Heroes[FChar];
   yPos := 2;
   GFontEngine.drawText(dummy.name, 0, yPos, 1);
   GFontEngine.drawText(GDatabase.vocab['StatShort-Lv'], 78, yPos, 2);
   GFontEngine.drawTextRightAligned(IntToStr(dummy.level), 108, yPos, 1);
   if dummy.highCondition = 0 then
      GFontEngine.drawText(GDatabase.vocab['Normal Status'], 118, yPos, 1)
   else GFontEngine.drawText(name, 118, yPos, GDatabase.conditions[dummy.highCondition].color);
   GFontEngine.drawText(GDatabase.vocab['StatShort-HP'], 178, yPos, 2);
   GFontEngine.drawTextRightAligned(intToStr(dummy.hp), 216, yPos, 1);
   GFontEngine.drawText('/', 216, yPos, 1);
   GFontEngine.drawTextRightAligned(intToStr(dummy.maxHp), 240, yPos, 1);
   GFontEngine.drawText(GDatabase.vocab['StatShort-MP'], 246, yPos, 2);
   GFontEngine.drawTextRightAligned(intToStr(dummy.mp), 280, yPos, 1);
   GFontEngine.drawText('/', 280, yPos, 1);
   GFontEngine.drawTextRightAligned(intToStr(dummy.maxMp), 304, yPos, 1);
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
   setLength(FOptionEnabled, MAXPARTYSIZE);
end;

procedure TCustomPartyPanel.moveTo(coords: TRect);
var
  I: Integer;
begin
   inherited moveTo(coords);
   for I := 1 to 4 do
      if assigned(FPortrait[i]) then
      begin
         FPortrait[i].x := 0;
         FPortrait[i].y := (i - 1) * 56;
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

   origin2.X := FOrigin.x + 4;
   if position <> -1 then
   begin
      origin2.Y := FOrigin.y + (position * 56) + 4;
      coords := rect(origin2.x, origin2.Y, self.width - 8, 56)
   end
   else begin
      origin2.Y := FOrigin.Y + 4;
      dummy := GEnvironment.Party.openSlot - 1;
      dummy := 56 * dummy;
      coords := rect(origin2.x, origin2.Y, self.width - 66, dummy);
   end;
   cursor := (Engine as TMenuSpriteEngine).cursor;
   cursor.Visible := true;
   cursor.layout(SdlRectToTRect(coords));
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
   SetLength(FOptionEnabled, GEnvironment.Party.size);
   while GEnvironment.Party[i] <> GEnvironment.Heroes[0] do
   begin
      template := TClassTemplate(GEnvironment.Party[i].template);
      FPortrait[i].Free;
      FPortrait[i] := loadPortrait(template.portrait, template.portraitIndex);
      FPortrait[i].x := 0;
      FPortrait[i].Y := (i - 1) * 56;
      FPortrait[i].SetSpecialRender;
      FParsedText.Add(GEnvironment.Party[i].name);
      FOptionEnabled[i - 1] := true;
      inc(i);
   end;
   InvalidateText;
   FCount := i - 1;
   for I := i to 4 do
   begin
      freeAndNil(FPortrait[i]);
      if i < length(FOptionEnabled) then
         FOptionEnabled[i - 1] := false;
   end;
end;

{ TCustomGameItemMenu }

constructor TCustomGameItemMenu.Create(parent: TMenuSpriteEngine; coords: TRect;
  main: TMenuEngine; owner: TMenuPage);
begin
   assert(coords.bottom mod 16 = 0);
   inherited Create(parent, coords, main, owner);
   FDisplayCapacity := trunc((coords.bottom - 16) / 8);
   FColumns := 2;
end;

procedure TCustomGameItemMenu.doCursor(position: smallint);
begin
   if self.FDontChangeCursor then
      position := self.FCursorPosition;
   inherited doCursor(position);
   FDontChangeCursor := false;
end;

procedure TCustomGameItemMenu.drawItem(id, x, y, color: integer);
begin
   GFontEngine.drawText(FParsedText[id], x, y, color);
   GFontEngine.drawText(':', x + 120, y, color);
   GFontEngine.drawTextRightAligned(intToStr((FInventory[id] as TRpgItem).quantity), x + 136, y, color)
end;

procedure TCustomGameItemMenu.setInventory(const Value: TRpgInventory);
begin
   if FInventory <> Value then
   begin
      FInventory := Value;
      self.setup(0);
   end;
end;

procedure TCustomGameItemMenu.doSetup(value: integer);
var
  i: Integer;
begin
   inherited doSetup(value);
   FParsedText.Clear;
   if assigned(FInventory) then
   begin
      SetLength(FOptionEnabled, FInventory.Count);
      FInventory.sort;
      for i := 0 to FInventory.Count - 1 do
         FParsedText.Add(TRpgItem(FInventory[i]).template.name);
   end
   else SetLength(FOptionEnabled, 0);
   self.placeCursor(FSetupValue);
   InvalidateText;
end;

{ Classless}

function loadPortrait(filename: string; const index: byte): TSprite;
var
   engine: TSpriteEngine;
begin
   engine := GMenuEngine;
   if not ArchiveUtils.GraphicExists(filename, 'portrait') then
      Exit(nil);
   Engine.Images.EnsureImage(format('portrait\%s', [filename]), filename);
   result := TSprite.Create(Engine);
   result.Visible := true;
   result.ImageName := filename;
   result.ImageIndex := index;
end;

initialization
   TMenuEngine.RegisterMenuBoxClass(TGameCashMenu);
   TMenuEngine.RegisterMenuBoxClass(TOnelineLabelBox);
   TMenuEngine.RegisterMenuBoxClass(TOnelineCharReadout);
end.
