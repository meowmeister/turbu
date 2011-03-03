unit frames;
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
   types, classes,
   commons, charset_data, chipset_data, tiles, timing,
   SDL_ImageManager, sdl_sprite, SG_defs;
   {asphyreSprite, asphyreImages, asphyreDef;}

type
   TMboxLocation = (mb_top, mb_middle, mb_bottom);

   TSystemTile = class abstract(TTiledAreaSprite)
   private
      FLocation: TSgPoint;
   public
      constructor Assign(Value: TTiledAreaSprite); reintroduce;
      property location: TSgPoint read FLocation write FLocation;
   end;

   TSystemMiniTile = class(TMiniTile);

   TAnimSystemTile = class(TSystemTile);

   TCorners = (topLeft, topRight, bottomLeft, bottomRight);

   TAnimSysFrame = class(TAnimSystemTile)
   private
      FTimer: byte;
   protected
      FCorners: array[TCorners] of TAnimSystemTile;
      FBorders: array[TFacing] of TTiledAreaSprite;
      FBackground: TTiledAreaSprite;
      FBounds: TRect;
      procedure DoDraw; override;
   public
      constructor Create(parent: TParentSprite; displacement: TSgPoint; length: integer); reintroduce;
      procedure layout(const coords: TRect);
      procedure moveTo(coords: TRect); virtual;
      procedure Move(const movecount: single); override;

      property bounds: TRect read FBounds;
   end;

   TStaticSysFrame = class(TAnimSysFrame)
   public
      constructor Create(base: TAnimSysFrame);
   end;

   TSystemRects = (srWallpaper, srFrameTL, srFrameTR, srFrameBL, srFrameBR,
                  srFrameT, srFrameB, srFrameL, srFrameR, srArrowU, srArrowD,
                  srMerchU, srMerchDot, srMerchD, srMerchEq, srCursorTL,
                  srCursorTR, srCursorBL, srCursorBR, srCursorT, srCursorB,
                  srCursorL, srCursorR, srCursorBG, srTimer, srShadows,
                  srColors, srBackground);

   TSystemImages = class(TObject)
   private
      FRects: array[TSystemRects] of TRect;
      FFilename: string;
      FImages: TSdlImages;
      FBetterArrow: TAnimSystemTile;
      FDotArrow: TAnimSystemTile;
      FWorseArrow: TAnimSystemTile;
      FEqualValue: TAnimSystemTile;
      FBackground: cardinal;
   public
      constructor Create(parent: TSpriteEngine; const filename: string; images: TSdlImages);
      destructor Destroy; override;
      procedure unload;
      procedure reload;

      property filename: string read FFilename;
      property bgColor: cardinal read FBackground;
   end;

   TSystemFrame = class abstract (TAnimSysFrame)
   private
      function getRightSide: integer; inline;
      function getOrigin: TRpgPoint; inline;
   protected
      FOrigin: TPoint;
      FWidth: word;
      FHeight: word;
      FScreenPos: TRect;

      property origin: TRpgPoint read getOrigin;
      property rightside: integer read getRightSide;
   public
      procedure realign; inline;
      constructor Create(parent: TSpriteEngine; const coords: TRect);

      property width: word read FWidth;
      property height: word read FHeight;
   end;

   TMessageState = (mb_display, mb_choice, mb_input, mb_prompt);

   TCustomMessageBox = class abstract(TSystemFrame)
   private
      FInputResult: array of byte;
      FMessageText: string;
      FBoxVisible: boolean;

      procedure parseText(input: string); virtual;
   protected
      FCursorPosition: smallint;
      FParsedText: TStringList;
      FOptionEnabled: array of boolean;
      FColumns: byte;
      FPromptLines: byte;
      FDontChangeCursor: boolean;
      FButtonLock: TRpgTimestamp;
      FLastLineColumns: byte;

      function columnWidth: word;
      function lastColumnWidth: word;
   public
      constructor Create(parent: TSpriteEngine; const coords: TRect); virtual;
      destructor Destroy; override;
      procedure button(const input: TButtonCode); virtual;
      procedure placeCursor(position: smallint); virtual;

      property text: string read FMessageText write parseText;
      property boxVisible: boolean read FBoxVisible write FBoxVisible;
   end;

   TMessageBox = class(TCustomMessageBox)
   private
      FPortrait: TSystemMiniTile;
      FNextArrow: TSystemMiniTile;
      FPosition: TMboxLocation;
      FRightPortrait: boolean;
      FTimer: byte;
      FContinuing: boolean;
      FState: TMessageState;
      FAcceptCancel: boolean;

      procedure setRightside(const value: boolean);
      procedure setPosition(const value: TMboxLocation);
      procedure changeInputResult(digit, value: byte);
      procedure setState(value: TMessageState);
      function getInputResult(digit: byte): byte;
      function computeInputResult: integer;
      procedure parseText(input: string); override;
   public
      constructor Create(parent: TSpriteEngine; const coords: TRect); override;
      destructor Destroy; override;
      procedure Draw; override;
      procedure realign; inline;
      procedure moveTo(coords: TRect); reintroduce;
      procedure placeCursor(position: smallint); override;
      procedure button(const input: TButtonCode); override;
      procedure tick;
      procedure continue;
      procedure setPortrait(const filename: string; const index: byte);
      procedure setupInput(const digits: byte);

      property position: TMboxLocation read FPosition write setPosition;
      property portrait: TSystemMiniTile read FPortrait write FPortrait;
      property continuing: boolean read FContinuing;
      property rightside: boolean write setRightside;
      property state: TMessageState read FState write setState;
      property canCancel: boolean  read FAcceptCancel write FAcceptCancel;
      property inputResult[x: byte]: byte read getInputResult write changeInputResult;
   end;

   TSystemTimer = class(TSprite)
   private
      FTime: word;
      FPrevTime: word;
      FTiles: array[1..5] of TSystemMiniTile;
      FPrevState: TGameState;

      procedure updateTime;
      procedure updatePosition(location: TRpgPoint);
   public
      constructor Create(parent: TSpriteEngine); reintroduce;
      destructor Destroy; override;
      procedure Draw; override;
   end;

const
   CURSOR_UNCHANGED = 9999;
   SEPARATOR = 8;
   ARROW_DISPLACEMENT: TSgPoint = (x: 8; y: 0);
   FRAME_DISPLACEMENT: TSgPoint = (x: 32; y: 0);

implementation

uses
   windows, sysUtils, contnrs, math,
   chipset_graphics, locate_files, script_engine, strtok,
   text_graphics, turbu_defs;

const
   CORNER: TPoint = (x: 8; y: 8);
   FRAMEHORIZ: TPoint = (x: 16; y: 8);
   FRAMEVERT: TPoint = (x: 8; y: 16);
   SPRITEBAR: TPoint = (x:32; y:8);
   SPRITEBLOCK: TPoint = (x: 16; y: 16);
   CURSORBG: TPoint = (x: 16; y: 16);
   V_SHADOW: TPoint = (x: 16; y: 16);
   V_SHADOW_BLOCK: TPoint = (x: 32; y: 16);
   CURSORBGBLOCK: TPoint = (x: 32; y: 16);
   CLOCKTIMEBLOCK: TPoint = (x: 32; y: 192);

{ TSystemImages }

constructor TSystemImages.Create(parent: TSpriteEngine; const filename: string; images: TSdlImages);
begin
   inherited create;
   assert(TGameMap(parent).loadedImages.IndexOf(filename) = -1);
   FFilename := filename;
   locate_files.findGraphic(FFilename, 'System');
//   GXyzHack := false;

   GFileLoader.Enter;
try
   images.AddFromFile(FFilename, 'System');

   FRects[srWallpaper] := rect(0, 0, 32, 32);
   FRects[srFrameTL] := rect(32, 0, 8, 8);
   FRects[srFrameTR] := rect(56, 0, 8, 8);
   FRects[srFrameBL] := rect(32, 24, 8, 8);
   FRects[srFrameBR] := rect(56, 24, 8, 8);
   FRects[srFrameT] := rect(40, 0, 16, 8);
   FRects[srFrameB] := rect(40, 24, 16, 8);
   FRects[srFrameL] := rect(32, 8, 8, 16);
   FRects[srFrameR] := rect(56, 8, 8, 16);
   FRects[srArrowU] := rect(40, 8, 16, 8);
   FRects[srArrowD] := rect(40, 16, 16, 8);
   FRects[srMerchU] := rect(128, 0, 8, 8);
   FBetterArrow := TAnimSystemTile.Create(parent, FRects[srMerchU], ARROW_DISPLACEMENT, 4);
   FBetterArrow.ImageName := 'System';
   FBetterArrow.DrawRect := FRects[srMerchU];

   FRects[srMerchDot] := rect(128, 8, 8, 8);
   FDotArrow := TAnimSystemTile.Create(parent, FRects[srMerchDot], ARROW_DISPLACEMENT, 4);
   FDotArrow.ImageName := 'System';
   FDotArrow.DrawRect := FRects[srMerchDot];

   FRects[srMerchD] := rect(128, 16, 8, 8);
   FWorseArrow := TAnimSystemTile.Create(parent, FRects[srMerchD], ARROW_DISPLACEMENT, 4);
   FWorseArrow.ImageName := 'System';
   FWorseArrow.DrawRect := FRects[srMerchD];

   FRects[srMerchEq] := rect(128, 24, 8, 8);
   FEqualValue := TAnimSystemTile.Create(parent, FRects[srMerchEq], ARROW_DISPLACEMENT, 4);
   FEqualValue.ImageName := 'System';
   FEqualValue.DrawRect := FRects[srMerchEq];

   FRects[srCursorTL] := rect(64, 0, 8, 8);
   FRects[srCursorBL] := rect(64, 24, 8, 8);
   FRects[srCursorTR] := rect(88, 0, 8, 8);
   FRects[srCursorBR] := rect(88, 24, 8, 8);
   FRects[srCursorT] := rect(72, 0, 16, 8);
   FRects[srCursorB] := rect(72, 24, 16, 8);
   FRects[srCursorL] := rect(64, 8, 8, 16);
   FRects[srCursorR] := rect(88, 8, 8, 16);
   FRects[srCursorBG] := rect(72, 8, 16, 16);

   FRects[srTimer] := rect(32, 32, 8, 16);
   TGameMap(parent).systemTimer := TSystemTimer.Create(parent);
   TGameMap(parent).timer2 := TSystemTimer.Create(parent);

   FRects[srShadows] := rect(128, 32, 32, 16);
   FRects[srColors] := rect(0, 48, 8, 8);
   FRects[srBackground] := rect(0, 32, 16, 16);

   TGameMap(parent).loadedImages.Add(filename);
finally
   GFileLoader.Leave;
end;
end;

destructor TSystemImages.Destroy;
begin
   FBetterArrow.free;
   FDotArrow.free;
   FWorseArrow.free;
   FEqualValue.free;
   inherited;
end;

procedure TSystemImages.reload;

   procedure reveal(image: string);
      var index: integer;
   begin
      index := FImages.IndexOf(FFilename + ' ' + image);
      FImages[index].Name := image;
   end;

begin
   reveal('System Wallpaper');
   reveal('SysFrame TL');
   reveal('SysFrame TR');
   reveal('SysFrame BL');
   reveal('SysFrame BR');
   reveal('SysFrame T');
   reveal('SysFrame B');
   reveal('SysFrame L');
   reveal('SysFrame R');
   reveal('SysArrow U');
   reveal('SysArrow D');
   reveal('SysMerch U');
   reveal('SysMerch DOT');
   reveal('SysMerch D');
   reveal('SysMerch EQUAL');
   reveal('SysCursor TL');
   reveal('SysCursor BL');
   reveal('SysCursor TR');
   reveal('SysCursor BR');
   reveal('SysCursor T');
   reveal('SysCursor B');
   reveal('SysCursor L');
   reveal('SysCursor R');
   reveal('SysCursor BG');
   reveal('SysTimer');
end;

procedure TSystemImages.unload;

   procedure hide(image: string);
      var index: integer;
   begin
      index := FImages.IndexOf(image);
      FImages[index].Name := FFilename + ' ' + FImages[index].Name;
   end;

begin
   hide('System Wallpaper');
   hide('SysFrame TL');
   hide('SysFrame TR');
   hide('SysFrame BL');
   hide('SysFrame BR');
   hide('SysFrame T');
   hide('SysFrame B');
   hide('SysFrame L');
   hide('SysFrame R');
   hide('SysArrow U');
   hide('SysArrow D');
   hide('SysMerch U');
   hide('SysMerch DOT');
   hide('SysMerch D');
   hide('SysMerch EQUAL');
   hide('SysCursor TL');
   hide('SysCursor BL');
   hide('SysCursor TR');
   hide('SysCursor BR');
   hide('SysCursor T');
   hide('SysCursor B');
   hide('SysCursor L');
   hide('SysCursor R');
   hide('SysCursor BG');
   hide('SysTimer');
end;

{ TAnimSysFrame }

constructor TAnimSysFrame.Create(parent: TParentSprite; displacement: TSgPoint; length: integer);
var
   i: integer;
begin
   assert(parent is TGameMap);
   inherited create(parent, rect(0, 0, 0, 0), point(0, 0), 0);
   FBackground := TTiledAreaSprite.create(self, GGameEngine.systemGraphic.FRects[srCursorBG], displacement, length);
   FBackground.Z := 1;
   FBorders[facing_up] := TTiledAreaSprite.Create(self, GGameEngine.systemGraphic.FRects[srCursorT], displacement, length);
   FBorders[facing_down] := TTiledAreaSprite.Create(self, GGameEngine.systemGraphic.FRects[srCursorB], displacement, length);
   FBorders[facing_left] := TTiledAreaSprite.Create(self, GGameEngine.systemGraphic.FRects[srCursorL], displacement, length);
   FBorders[facing_right] := TTiledAreaSprite.Create(self, GGameEngine.systemGraphic.FRects[srCursorR], displacement, length);
   FCorners[topLeft] := TAnimSystemTile.create(self, GGameEngine.systemGraphic.FRects[srCursorTL], displacement, length);
   FCorners[bottomLeft] := TAnimSystemTile.create(self, GGameEngine.systemGraphic.FRects[srCursorBL], displacement, length);
   FCorners[topRight] := TAnimSystemTile.create(self, GGameEngine.systemGraphic.FRects[srCursorTR], displacement, length);
   FCorners[bottomRight] := TAnimSystemTile.create(self, GGameEngine.systemGraphic.FRects[srCursorBR], displacement, length);
   for I := 0 to ord(high(TFacing)) do
      FBorders[TFacing(i)].Z := 2;
   for I := 0 to ord(high(TCorners)) do
      FCorners[TCorners(i)].Z := 3;
   for i := 0 to FList.count - 1 do
      FList[i].ImageName := 'System';
   visible := false;
end;

procedure TAnimSysFrame.layout(const coords: TRect);
var
   tl, br: TSgPoint;
   dummy: TRect;
begin
   if (self.location.x <> coords.right) or (self.location.Y <> coords.Bottom) then
   begin
      tl := TSgPoint(coords.TopLeft) + FCorners[topLeft].DrawRect.BottomRight;
      br := TSgPoint(coords.BottomRight) - FCorners[bottomRight].DrawRect.BottomRight;
      dummy := FBorders[facing_up].FillArea;
      dummy.Right := br.x - tl.x;
      FBorders[facing_up].FillArea := dummy;

      dummy := FBorders[facing_down].FillArea;
      dummy.Right := br.x - tl.x;
      FBorders[facing_down].FillArea := dummy;

      dummy := FBorders[facing_left].FillArea;
      dummy.bottom := br.y - tl.y;
      FBorders[facing_left].FillArea := dummy;

      dummy := FBorders[facing_right].FillArea;
      dummy.bottom := br.y - tl.y;
      FBorders[facing_right].FillArea := dummy;

      FBackground.FillArea := rect(tl, br - tl);
{      FBackground.ScaleX := coords.Right / 16;
      FBackground.ScaleY := coords.Bottom / 16;}
//fixme
//      FBackground.DoCenter := false;
      self.location := coords.BottomRight; //location in this case represents
                                           //size, not position
   end;
   moveTo(coords);
end;

procedure TAnimSysFrame.moveTo(coords: TRect);
var
   tl: TSgPoint;
begin
   tl := coords.TopLeft;
   FCorners[topLeft].X := tl.x;
   FBorders[facing_left].x := tl.x;
   FCorners[bottomLeft].X := tl.x;

   inc(tl.x, FCorners[topLeft].DrawRect.Right);
   FBorders[facing_up].x := tl.x;
   FBackground.X := tl.x;
   FBorders[facing_down].x := tl.x;

   inc(tl.x, FBorders[facing_up].FillArea.Right);
   FCorners[topRight].X := tl.x;
   FBorders[facing_right].X := tl.x;
   FCorners[bottomRight].x := tl.x;

   FCorners[topLeft].Y := tl.y;
   FBorders[facing_up].Y := tl.y;
   FCorners[topRight].Y := tl.y;

   inc(tl.y, FCorners[topRight].DrawRect.Bottom);
   FBorders[facing_left].y := tl.y;
   FBackground.y := tl.y;
   FBorders[facing_right].y := tl.y;

   inc(tl.y, FBorders[facing_right].FillArea.Bottom);
   FCorners[bottomLeft].y := tl.y;
   FBorders[facing_down].y := tl.y;
   FCorners[bottomRight].y := tl.y;

   FBounds := coords;
end;

procedure TAnimSysFrame.Move(const movecount: single);
begin
   inc(FTimer);
   if FTimer = 15 then
   begin
      FTimer := 0;
      inherited Move(movecount);
   end;
end;

procedure TAnimSysFrame.DoDraw;
begin
   self.Move(1);
   inherited DoDraw;
end;

{ TSystemFrame }

function TSystemFrame.getOrigin: TRpgPoint;
begin
   result := point(trunc(FBounds.Left - FEngine.WorldX), trunc(FBounds.Top - FEngine.WorldY));
end;

function TSystemFrame.getRightSide: integer;
begin
   result := self.origin.x + FBounds.Right - 8;
end;

procedure TSystemFrame.realign;
var dummy: TRect;
begin
   dummy := FScreenPos;
   self.moveTo(dummy);
end;

constructor TSystemFrame.Create(parent: TSpriteEngine; const coords: TRect);
begin
   assert(parent is TGameMap);
   inherited create(parent, point(0,0), 1);
   FCorners[topLeft].DrawRect := GGameEngine.systemGraphic.FRects[srFrameTL];
   FCorners[bottomLeft].DrawRect := GGameEngine.systemGraphic.FRects[srFrameBL];
   FCorners[topRight].DrawRect := GGameEngine.systemGraphic.FRects[srFrameTR];
   FCorners[bottomRight].DrawRect := GGameEngine.systemGraphic.FRects[srFrameBR];
   FBorders[facing_up].DrawRect := GGameEngine.systemGraphic.FRects[srFrameT];
   FBorders[facing_down].DrawRect := GGameEngine.systemGraphic.FRects[srFrameB];
   FBorders[facing_left].DrawRect := GGameEngine.systemGraphic.FRects[srFrameL];
   FBorders[facing_right].DrawRect := GGameEngine.systemGraphic.FRects[srFrameR];
   FBackground.DrawRect := GGameEngine.systemGraphic.FRects[srWallpaper];
   self.FWidth := coords.right;
   self.FHeight := coords.bottom;
   layout(coords);
end;

{ TMessageBox }

constructor TMessageBox.Create(parent: TSpriteEngine; const coords: TRect);
begin
   assert(parent is TGameMap);
   //these three go before the inherited constructor because they're
   //needed (the first, at least) by the virtual MoveTo function that gets
   //called from the inherited constructor
{   FNextArrow := TSystemMiniTile.Create(parent, nil);
   FNextArrow.ImageName := 'SysArrow D';
   FPortrait := TSystemMiniTile.Create(parent, nil); }

   inherited;
   FMessageText := '';

   FPortrait.Visible := false;
   FRightPortrait := false;
end;

destructor TMessageBox.Destroy;
begin
   FPortrait.Free;
   FNextArrow.free;
   inherited;
end;

procedure TMessageBox.Draw;
var
  I: Integer;
  xVal, yVal: smallint;
const FUDGEFACTOR: array[0..3] of shortint = (0, 1, 2, 4);
begin
   if boxVisible then
      inherited Draw;

   if FState = mb_display  then
   begin
      if FPortrait.Visible then
         FPortrait.Draw;
      if FPortrait.Visible and not FRightPortrait then
         xVal := 72
      else
         xVal := 10;
   if FState = mb_display then
      FNextArrow.Draw;
   end
   else
   begin
      TGameMap(Engine).cursor.Draw;
      xVal := 12;
   end;
   for I := 0 to min(FParsedText.count - 1, 3) do
   begin
//      yVal := ord(FPosition) * 80 + 10 + (i * trunc(TGameMap(Engine).fontEngine[0].TextHeight('A') + 2));
      inc(yVal, FUDGEFACTOR[i]);
      if (FState = mb_prompt) and (i = 2) and (TGameMap(Engine).menuInt > GParty.money) then
         drawText(FParsedText[i], xVal, yVal, 3)
      else
         drawText(FParsedText[i], xVal, yVal, 0)
      //end if
   end
end;

function TMessageBox.getInputResult(digit: byte): byte;
begin
   assert(digit < length(FInputResult));
   result := FInputResult[digit];
end;

procedure TMessageBox.moveTo(coords: TRect);
begin
   inherited moveTo(coords);
   FNextArrow.X := 152 + fcorners[topLeft].X;
   FNextArrow.Y := fcorners[bottomLeft].Y;
   FPortrait.Y := fcorners[topLeft].Y + 16;
   setRightside(FRightPortrait);
end;

procedure TMessageBox.parseText(input: string);
var
   i: word;
   maxWidth: word;
   tempList: TStringList;
   dummy: string;
begin
   FMessageText := input;
   tempList := TStringList.Create;
   FParsedText.Clear;
   if inString(input, #3) = -1 then
   begin
      strtok.Split(input, ' ', tempList);
      i := 0;
      case FPortrait.Visible of
         true: maxWidth := 240;
         false: maxWidth := 300
         else maxWidth := 0;
      end;
      repeat
         dummy := '';
{         while (i < tempList.count) and
               (TGameMap(Engine).fontEngine[whichFont].TextWidth(dummy) +
                TGameMap(Engine).fontEngine[whichFont].TextWidth(tempList[i]) <= maxWidth) do}
         begin
            dummy := strtok.AddToken(tempList[i], dummy, ' ', MAXINT);
            inc(i);
         end;
         FParsedText.Add(dummy);
      until i >= tempList.count;
      FContinuing := (FParsedText.Count > 4);
   end
   else begin
      inherited;
      if (FParsedText[FParsedText.Count - 1] = ' ') and (FAcceptCancel) then
         FParsedText.Delete(FParsedText.count - 1);
      //end if
   end;
   SetLength(FOptionEnabled, FParsedText.Count);
   for I := 0 to high(FOptionEnabled) do
      FOptionEnabled[i] := true;
   //end if
   tempList.free;
end;

procedure TMessageBox.placeCursor(position: smallint);
var coords: trect;
begin
   case FState of
      mb_display: raise EParseMessage.create('Tried to place a cursor in a message-only box!');
      mb_choice, mb_prompt: inherited placeCursor(position);
      mb_input:
      begin
{         coords := rect(7 + trunc((position * TGameMap(Engine).fontEngine[0].TextWidth('  0'))),
                   trunc(FCorners[topLeft].Y - engine.worldY) + 6,
                   trunc(TGameMap(Engine).fontEngine[0].TextWidth('0') + 9),
                   trunc(TGameMap(Engine).fontEngine[0].TextHeight('J')) + 7);}
         with TGameMap(Engine).cursor do
         begin
            Visible := true;
            layout(coords);
         end;
         FCursorPosition := position;
      end;
   end;
end;

procedure TMessageBox.realign;
begin
   self.moveTo(FScreenPos);
end;

procedure TMessageBox.setPortrait(const filename: string; const index: byte);
begin
   if Engine.Images.IndexOf('portrait ' + filename) = -1 then
      TGameMap(Engine).loadPortrait(filename);
   FPortrait.Visible := true;
   FPortrait.ImageName := 'portrait ' + filename;
   FPortrait.ImageIndex := index;
end;

procedure TMessageBox.setPosition(const value: TMboxLocation);
begin
   FPosition := value;
   moveTo(rect(0, ord(value) * 80, 320, 80));
end;

procedure TMessageBox.setRightside(const value: boolean);
begin
   FRightPortrait := value;
   case value of
      false: FPortrait.X := FBackground.X + 16;
      true: FPortrait.X := FCorners[topRight].x - 56;
   end;
end;

procedure TMessageBox.setState(value: TMessageState);
begin
   FState := value;
   setPosition(FPosition);
end;

procedure TMessageBox.setupInput(const digits: byte);
var
   dummy: string;
   i: byte;
begin
   dummy := '0';
   for I := 2 to digits do
      dummy := dummy + '  0';
   dummy := dummy + #3;
   setLength(FInputResult, digits);
   for i := 0 to high(FInputResult) do
      FInputResult[i] := 0;
   text := dummy;
end;

procedure TMessageBox.tick;
begin
   inc(FTimer);
   if FTimer >= 3 then
   begin
      FNextArrow.Visible := not FNextArrow.Visible;
      FTimer := 0;
   end;
end;

procedure TMessageBox.button(const input: TButtonCode);
begin
   if assigned(FButtonLock) then
      if (FButtonLock.timeRemaining = 0) then
         freeAndNil(FButtonLock)
      else Exit;
   //end if

   case FState of
      mb_display:
         case input of
            btn_enter, btn_cancel: TGameMap(Engine).endMessage;
            else ;
         end;
      mb_choice:
         case input of
            btn_cancel:
            begin
               if FAcceptCancel then
               begin
                  TGameMap(Engine).menuInt := -1;
                  TGameMap(Engine).endMessage;
                  GScriptEngine.mediaPlayer.playSystemSound(sfxCancel);
               end;
            end;
            else begin
               inherited button(input);
               if input = btn_enter then
                  TGameMap(Engine).endMessage;
               //end if
            end;
         end;
         mb_prompt:
         case input of
            btn_enter:
            begin
               if (FCursorPosition = 3) or ((FCursorPosition = 2) and (GParty.money >= TGameMap(Engine).menuInt)) then
               begin
                  TGameMap(Engine).menuInt := FCursorPosition;
                  TGameMap(Engine).endMessage;
                  GScriptEngine.mediaPlayer.playSystemSound(sfxAccept);
               end
               else GScriptEngine.mediaPlayer.playSystemSound(sfxBuzzer);;
            end;
            btn_cancel:
            begin
               if FAcceptCancel then
               begin
                  TGameMap(Engine).menuInt := 3;
                  TGameMap(Engine).endMessage;
                  GScriptEngine.mediaPlayer.playSystemSound(sfxCancel);
               end;
            end;
            btn_down, btn_up:
            begin
               if FCursorPosition = 2 then
               begin
                  placeCursor(3);
               end
               else placeCursor(2);
               GScriptEngine.mediaPlayer.playSystemSound(sfxCursor);
            end
            else ;
         end;
         mb_input:
         begin
         case input of
            btn_enter:
            begin
               TGameMap(Engine).menuInt := computeInputResult;
               TGameMap(Engine).endMessage;
               GScriptEngine.mediaPlayer.playSystemSound(sfxAccept);
            end;
            btn_down:
            begin
               if inputResult[FCursorPosition] = 0 then
                  inputResult[FCursorPosition] := 9
               else inputResult[FCursorPosition] := inputResult[FCursorPosition] - 1;
               GScriptEngine.mediaPlayer.playSystemSound(sfxCursor);
            end;
            btn_up:
            begin
               if inputResult[FCursorPosition] = 9 then
                  inputResult[FCursorPosition] := 0
               else inputResult[FCursorPosition] := inputResult[FCursorPosition] + 1;
               GScriptEngine.mediaPlayer.playSystemSound(sfxCursor);
            end;
            btn_left:
            begin
               if FCursorPosition > 0 then
                  placeCursor(FCursorPosition - 1);
               GScriptEngine.mediaPlayer.playSystemSound(sfxCursor);
            end;
            btn_right:
            begin
               if FCursorPosition < high(FInputResult) then
                  placeCursor(FCursorPosition + 1);
               GScriptEngine.mediaPlayer.playSystemSound(sfxCursor);
            end
            else ;
         end;
      end;
      //end cases
   end;
   if (input in [btn_up, btn_down, btn_left, btn_right]) then
      FButtonLock := TRpgTimestamp.Create(180);
end;

procedure TMessageBox.changeInputResult(digit, value: byte);
var
   dummy: string;
   i: byte;
begin
   assert(digit <= high(FInputResult));
   assert(value < 10);
   if FInputResult[digit] = value then
      Exit;

   FInputResult[digit] := value;
   dummy := intToStr(FInputResult[0]);
   for I := 1 to high(FInputResult) do
      dummy := dummy + '  ' + intToStr(FInputResult[i]);
   dummy := dummy + #3;
   text := dummy;
end;

function TMessageBox.computeInputResult: integer;
var dummy: byte;
begin
   result := 0;
   for dummy := high(FInputResult) downto 0 do
      result := result + powerWrap(10, high(FInputResult) - dummy) * FInputResult[dummy];
end;

procedure TMessageBox.continue;
var
  I: Integer;
begin
   assert(FParsedText.count > 4);
   for I := 1 to 4 do
      FParsedText.Delete(0);
   FContinuing := (FParsedText.Count > 4);
end;

{ TSystemTimer }

constructor TSystemTimer.Create(parent: TSpriteEngine);
var
  I: Integer;
begin
   assert(parent is TGameMap);
   inherited Create(parent);
   for I := 1 to 5 do
   begin
//      FTiles[i] := TSystemMiniTile.Create(parent, nil);
      FTiles[i].ImageName := 'SysTimer';
   end;
   FTiles[1].ImageIndex := 11;
   FTiles[3].ImageIndex := 10;
   FTime := 0;
   Visible := false;
   FPrevTime := 0;
   z := 12;
   FPrevState := on_map;
end;

destructor TSystemTimer.Destroy;
var
   I: Integer;
begin
   for I := 1 to 5 do
      FTiles[i].free;
   inherited;
end;

procedure TSystemTimer.Draw;
var i: byte;
begin
   FTime := TGameMap(Engine).scriptEngine.timer.time;
   if FTime <> FPrevTime then
      updateTime;
   FPrevTime := FTime;
   if (Engine.WorldX <> X) or (Engine.WorldY <> Y)
      or (TGameMap(Engine).state <> FPrevState) then
   begin
      X := Engine.WorldX;
      Y := Engine.WorldY;
      updatePosition(point(round(x), round(y)));
   end;
   for i := 1 to 5 do
      FTiles[i].Draw;
end;

procedure TSystemTimer.updatePosition(location: TRpgPoint);
var
  I: Integer;
begin
   if FPrevState <> TGameMap(Engine).state then
   begin
      FPrevState := TGameMap(Engine).state;
      case FPrevState of
         on_map: ;
         in_message:
            if TGameMap(Engine).currentMBox.position = mb_top then
               inc(location.Y, 160);
            //end if
         //end in_message case

{         in_menu: ;
         in_battle: ;
         trapping_keys: ;}
         else
            raise EFatalError.create('Unable to set timer for current game state!');
      end;
   end;

   inc(location.X, 1);
   inc(location.Y, 10);
   for I := 1 to 5 do
   begin
      FTiles[i].X := location.X + (i * 9);
      FTiles[i].Y := location.Y;
   end;
end;

procedure TSystemTimer.updateTime;
var min, sec: byte;
begin
   min := FTime div 60;
   sec := FTime mod 60;
   if min > 10 then
      FTiles[1].ImageIndex := min div 10
   else
      FTiles[1].ImageIndex := 11;
   //end if
   FTiles[2].ImageIndex := min mod 10;
   FTiles[4].ImageIndex := sec div 10;
   FTiles[5].ImageIndex := sec mod 10;
end;

{ TCustomMessageBox }

procedure TCustomMessageBox.button(const input: TButtonCode);
var
   max, absMax, dummy: smallint;
   ratio: byte;
begin
   if (FCursorPosition = -1) and (input in [btn_up, btn_down, btn_left, btn_right]) then
      Exit;
   if FParsedText.Count = 0 then
      Exit;
   if assigned(FButtonLock) then
   begin
      if FButtonLock.timeRemaining = 0 then
         freeAndNil(FButtonLock);
      Exit;
   end;

   dummy := FCursorPosition; //to suppress a compiler warning
   max := (FParsedText.Count - 1) - (FPromptLines + FLastLineColumns);
   absMax := max + FLastLineColumns;
   case input of
      btn_enter:
      begin
         if FOptionEnabled[FCursorPosition] then
         begin
            TGameMap(Engine).menuInt := FCursorPosition;
            GScriptEngine.mediaPlayer.playSystemSound(sfxAccept);
         end else
            GScriptEngine.mediaPlayer.playSystemSound(sfxBuzzer);
         //end if
      end;
      btn_down:
      begin
         if FCursorPosition <= max - FColumns then
            dummy := FCursorPosition + FColumns
         else if FColumns = 1 then
            dummy := 0
         else if (FLastLineColumns > 0) and (FCursorPosition <= max) then
         begin
            dummy := FCursorPosition mod FColumns;
            ratio := FColumns div FLastLineColumns;
            dummy := (dummy div ratio) + max + 1;
         end;
      end;
      btn_up:
      begin
         if FCursorPosition > max then
         begin
            ratio := FColumns div FLastLineColumns;
            dummy := FCursorPosition - (max + 1);
            dummy := (max + 1 - FColumns) + (dummy * ratio) + (ratio div 2);
         end else if FCursorPosition >= FColumns then
            dummy := FCursorPosition - FColumns
         else if FColumns = 1 then
            dummy := FParsedText.Count - 1;
         //end if
      end;
      btn_right:
      begin
         if (FColumns > 1) and (FCursorPosition < absMax) then
            dummy := FCursorPosition + 1;
         //end if
      end;
      btn_left:
      begin
         if (FColumns > 1) and (FCursorPosition > 0) then
            dummy := FCursorPosition - 1;
         //end if
      end;
      else ;
   end;
   if (input in [btn_up, btn_down, btn_left, btn_right]) and (dummy <> FCursorPosition) then
   begin
      FButtonLock := TRpgTimestamp.Create(180);
      placeCursor(dummy);
      GScriptEngine.mediaPlayer.playSystemSound(sfxCursor);
   end;
end;

constructor TCustomMessageBox.Create(parent: TSpriteEngine; const coords: TRect);
begin
   inherited;
   FColumns := 1;
   FPromptLines := 0;
   FParsedText := TStringList.Create;
   FParsedText.Duplicates := dupAccept;
end;

destructor TCustomMessageBox.Destroy;
begin
   FParsedText.Clear;
   FParsedText.free;
   inherited;
end;

procedure TCustomMessageBox.parseText(input: string);
begin
   strtok.Split(input, #3, FParsedText);
end;

function TCustomMessageBox.columnWidth: word;
begin
   result := ((self.FBounds.Right - 8) div FColumns) - SEPARATOR;
end;

function TCustomMessageBox.lastColumnWidth: word;
begin
   result := ((self.FBounds.Right - 8) div FLastLineColumns) - SEPARATOR;
end;

procedure TCustomMessageBox.placeCursor(position: smallint);
var
   coords: TRect;
   column, columns: byte;
   width: word;
   max: smallint;
begin
   if self.FDontChangeCursor then
      position := (self.FCursorPosition);
   FCursorPosition := position;
   max := (FParsedText.Count - 1) - (FPromptLines + FLastLineColumns);
   if (position > max) and (FLastLineColumns > 0) then
   begin
      columns := FLastLineColumns;
      width := lastColumnWidth;
   end else
   begin
      columns := FColumns;
      width := columnWidth;
   end;
   if FParsedText.Count = 0 then
      position := 0
   else if position >= FParsedText.Count then
      position := FParsedText.Count - 1;
   if position > max then
      dec(position, max + 1);
   column := position mod columns;
   inc(position, FPromptLines * columns);
   coords := rect(8 + (column * (width + SEPARATOR)),
                  (position div columns) * 15 + origin.y + 8,
                  width, 18);
   if FCursorPosition > max then
      inc(coords.top, (FCursorPosition div FColumns) * 15);

   with TGameMap(Engine).cursor do
   begin
      Visible := true;
      layout(coords);
   end;
   FDontChangeCursor := false;
end;

{ TStaticSysFrame }

constructor TStaticSysFrame.Create(base: TAnimSysFrame);
var
   i: TFacing;
   j: TCorners;
begin
   self.Assign(base);
   for i := low(TFacing) to high(TFacing) do
   begin
      FBorders[i] := TAnimSystemTile.assign(base.FBorders[i]);
      self.Add(FBorders[i]);
   end;
   for j := topLeft to bottomRight do
   begin
      FCorners[j] := TAnimSystemTile.assign(base.FCorners[j]);
      self.Add(FCorners[j]);
   end;
   FBackground := TAnimSystemTile.Assign(base.FBackground);
   self.Add(FBackground);
end;

{ TSystemTile }

constructor TSystemTile.Assign(Value: TTiledAreaSprite);
begin
   inherited Create(value.Parent, value.DrawRect, value.Displacement, value.SeriesLength);
   inherited Assign(value);
end;

end.
