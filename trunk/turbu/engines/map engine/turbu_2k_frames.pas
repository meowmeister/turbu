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
unit turbu_2k_frames;

interface
uses
   Types, Classes, SyncObjs,
   turbu_defs, timing,
   sg_defs, sdl_sprite, sdl_ImageManager, sdl_canvas;

type
   TSystemRects = (srWallpaper, srFrameTL, srFrameTR, srFrameBL, srFrameBR,
                  srFrameT, srFrameB, srFrameL, srFrameR, srArrowU, srArrowD,
                  srMerchU, srMerchDot, srMerchD, srMerchEq, srCursorTL,
                  srCursorTR, srCursorBL, srCursorBR, srCursorT, srCursorB,
                  srCursorL, srCursorR, srCursorBG, srTimer, srShadows,
                  srColors, srBackground);
   TMenuState = (msNone, msShared, msExclusiveShared, msFull);

   TSystemTile = class abstract(TTiledAreaSprite)
   private
      FLocation: TSgPoint;
   public
      constructor Create(parent: TParentSprite; region: TRect; displacement: TSgPoint; length: integer);
      constructor Assign(Value: TTiledAreaSprite); reintroduce;
      property location: TSgPoint read FLocation write FLocation;
   end;

   TMenuSpriteEngine = class;
   TSysFrame = class;
   TMessageBox = class;

   TSystemImages = class(TObject)
   private
      FRects: array[TSystemRects] of TRect;
      FFilename: string;
      FBetterArrow: TSystemTile;
      FDotArrow: TSystemTile;
      FWorseArrow: TSystemTile;
      FEqualValue: TSystemTile;
      FStretch: boolean;
      procedure Setup(parent: TMenuSpriteEngine);
      function GetHandle: integer;
      function GetDrawRect(value: integer): TRect;
   public
      constructor Create(images: TSdlImages; const filename: string; stretch: boolean);
      destructor Destroy; override;

      property filename: string read FFilename;
   end;

   TMenuSpriteEngine = class(TSpriteEngine)
   private
      FSystemGraphic: TSystemImages;
      FMenuInt: integer;
      FCursor: TSysFrame;
      FMessageBox: TMessageBox;
      FMenuState: TMenuState;
      FTerminated: boolean;

      procedure EndMessage;
   public
      constructor Create(graphic: TSystemImages; canvas: TSdlCanvas; images: TSdlImages);
      destructor Destroy; override;
      procedure ShowMessage(const msg: string; modal: boolean);
      procedure Terminate;

      property MenuInt: integer read FMenuInt;
      property Cursor: TSysFrame read FCursor;
      property MessageBox: TMessageBox read FMessageBox;
      property State: TMenuState read FMenuState;
   end;

   TCorners = (topLeft, topRight, bottomLeft, bottomRight);

   TSysFrame = class(TSystemTile)
   private
      FFrameSize: TSgPoint;
   protected
      FCorners: array[TCorners] of TSystemTile;
      FBorders: array[TFacing] of TTiledAreaSprite;
      FBackground: TTiledAreaSprite;
      FBounds: TRect;
   public
      constructor Create(parent: TMenuSpriteEngine; displacement: TSgPoint; length: integer; const coords: TRect); reintroduce;
      procedure layout(const coords: TRect);
      procedure moveTo(coords: TRect); virtual;

      property bounds: TRect read FBounds;
   end;

   TMessageState = (mb_display, mb_choice, mb_input, mb_prompt);
   TPlaySoundEvent = reference to procedure(which: TSfxTypes);
   TValidateEvent = reference to function(const text: string): boolean;

   TCustomMessageBox = class abstract(TSysFrame)
   private
      FInputResult: array of byte;
      FMessageText: string;
      FBoxVisible: boolean;
      FPlaySound: TPlaySoundEvent;
      FSignal: TSimpleEvent;

      procedure PlaySound(which: TSfxTypes);
      procedure parseText(const input: string); virtual;
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
      constructor Create(parent: TMenuSpriteEngine; const coords: TRect); virtual;
      destructor Destroy; override;
      procedure button(const input: TButtonCode); virtual;
      procedure placeCursor(position: smallint); virtual;

      property text: string read FMessageText write parseText;
      property boxVisible: boolean read FBoxVisible write FBoxVisible;
      property OnPlaySound: TPlaySoundEvent read FPlaySound write FPlaySound;
      property Signal: TSimpleEvent read FSignal;
   end;

   TMessageBox = class(TCustomMessageBox)
   private
      FPortrait: TSprite;
      FNextArrow: TTiledAreaSprite;
      FPosition: TMboxLocation;
      FRightPortrait: boolean;
      FState: TMessageState;
      FAcceptCancel: boolean;
      FOnValidate: TValidateEvent;
      
      procedure setRightside(const value: boolean);
      procedure changeInputResult(digit, value: byte);
      function getInputResult(digit: byte): byte;
      function computeInputResult: integer;
      procedure parseText(const input: string); override;
      function Validate(const text: string): boolean;
   private //drawing section
      FFrameTarget: TSdlRenderTarget;
      FTextTarget: TsdlRenderTarget;
      FFrameDrawn: boolean;
      FTextRate: single;
      FRemainder: single;
      FTextCounter: integer;
      FTextPosX: single;
      FTextPosY: single;
      FTextColor: integer;
      FTextLine: integer;
      FSpecialText: string;
      FSpecialIndex: integer;
      FImmediate: boolean;

      procedure DrawChar(value: char); overload;
      procedure DrawChar(index: integer); overload;
      procedure DrawNextChar;
      procedure DrawFrame;
      procedure ParseToken(const input: string; var counter: integer);
      procedure ParseParamToken(const input: string; var counter: integer);
      procedure ParseInt(const input: string; var counter: integer);
      procedure NewLine;
      procedure DrawSpecialChar(const line: string);
      procedure SetTextRate(value: integer);
      function GetHeroName(value: integer): string;
      function GetIntegerValue: integer;
      procedure ClearTarget(target: TSdlRenderTarget);
      procedure PromptButton(const input: TButtonCode);
      procedure InputButton(const input: TButtonCode);
      function GetDrawCoords: TRect;
      procedure ResetText;
   public
      constructor Create(parent: TMenuSpriteEngine; const coords: TRect); override;
      destructor Destroy; override;
      procedure Draw; override;
      procedure button(const input: TButtonCode); override;
      procedure moveTo(coords: TRect); override;
{      procedure realign; inline;
      procedure placeCursor(position: smallint); override;
      procedure tick;}
      procedure setPortrait(const filename: string; const index: byte);
//      procedure setupInput(const digits: byte);

      property position: TMboxLocation read FPosition write FPosition;
      property portrait: TSprite read FPortrait;
      property rightside: boolean write setRightside;
      property state: TMessageState read FState write FState;
{      property canCancel: boolean  read FAcceptCancel write FAcceptCancel;}
      property inputResult[x: byte]: byte read getInputResult write changeInputResult;
      property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
   end;

var
   GMenuEngine: TMenuSpriteEngine;

implementation
uses
   SysUtils, Math, Character, OpenGL,
   commons, turbu_text_utils, turbu_2k_environment, turbu_OpenGL,
   sdl_13;

const
   ARROW_DISPLACEMENT: TSgPoint = (x: 8; y: 0);
   SEPARATOR = 8;
   FRAME_DISPLACEMENT: TSgPoint = (x: 32; y: 0);

{ TSystemTile }

constructor TSystemTile.Assign(Value: TTiledAreaSprite);
begin
   inherited Create(value.Parent, value.DrawRect, value.Displacement, value.SeriesLength);
   inherited Assign(value);
end;

constructor TSystemTile.Create(parent: TParentSprite; region: TRect;
  displacement: TSgPoint; length: integer);
begin
   inherited Create(parent, region, displacement, length);
   FRenderSpecial := true;
end;

{ TSysFrame }

constructor TSysFrame.Create(parent: TMenuSpriteEngine; displacement: TSgPoint;
  length: integer; const coords: TRect);
var
   i: integer;
   Graphic: TSystemImages;
begin
   inherited create(parent, NULLRECT, ORIGIN, 0);
   self.Z := 1;
   graphic := parent.FSystemGraphic;
   FBackground := TTiledAreaSprite.create(self, graphic.FRects[srWallpaper], displacement, length);
   FBackground.Z := 1;
   FBackground.name := 'background';
   FBackground.stretch := graphic.FStretch;
//   FBackground.Visible := false;
   FBorders[facing_up] := TTiledAreaSprite.Create(self, graphic.FRects[srFrameT], displacement, length);
   FBorders[facing_down] := TTiledAreaSprite.Create(self, graphic.FRects[srFrameB], displacement, length);
   FBorders[facing_left] := TTiledAreaSprite.Create(self, graphic.FRects[srFrameL], displacement, length);
   FBorders[facing_right] := TTiledAreaSprite.Create(self, graphic.FRects[srFrameR], displacement, length);
   FCorners[topLeft] := TSystemTile.create(self, graphic.FRects[srFrameTL], displacement, length);
   FCorners[bottomLeft] := TSystemTile.create(self, graphic.FRects[srFrameBL], displacement, length);
   FCorners[topRight] := TSystemTile.create(self, graphic.FRects[srFrameTR], displacement, length);
   FCorners[bottomRight] := TSystemTile.create(self, graphic.FRects[srFrameBR], displacement, length);
   for I := 0 to ord(high(TFacing)) do
   begin
      FBorders[TFacing(i)].Z := 2;
//      FBorders[TFacing(i)].visible := false;
   end;
   for I := 0 to ord(high(TCorners)) do
   begin
      FCorners[TCorners(i)].Z := 3;
      FCorners[TCorners(i)].FillArea := NULLRECT;
//      FCorners[TCorners(i)].visible := false;
   end;
   for i := 0 to FList.count - 1 do
   begin
      FList[i].ImageName := graphic.filename;
      FList[i].SetSpecialRender;
   end;
   visible := false;
   layout(coords);
end;

procedure TSysFrame.layout(const coords: TRect);
var
   tl, br: TSgPoint;
   fill: TRect;
begin
   if (FFrameSize.x <> coords.Right) or (FFrameSize.Y <> coords.Bottom) then
   begin
      tl := TSgPoint(coords.TopLeft) + FCorners[topLeft].DrawRect.BottomRight;
      br := TSgPoint(coords.BottomRight) - FCorners[bottomRight].DrawRect.BottomRight;
      fill := FBorders[facing_up].FillArea;
      fill.Right := br.x - tl.x;
      FBorders[facing_up].FillArea := fill;

      fill := FBorders[facing_down].FillArea;
      fill.Right := br.x - tl.x;
      FBorders[facing_down].FillArea := fill;

      fill := FBorders[facing_left].FillArea;
      fill.bottom := br.y - tl.y;
      FBorders[facing_left].FillArea := fill;

      fill := FBorders[facing_right].FillArea;
      fill.bottom := br.y - tl.y;
      FBorders[facing_right].FillArea := fill;

      FBackground.FillArea := coords;
      FBackground.ScaleX := coords.Right / 16;
      FBackground.ScaleY := coords.Bottom / 16;
      FFrameSize := coords.BottomRight;
   end;
   moveTo(coords);
end;

procedure SetX(sprite: TTiledAreaSprite; x: integer);
var
   area: TRect;
begin
   area := sprite.FillArea;
   area.Left := x;
   sprite.FillArea := area;
end;

procedure SetY(sprite: TTiledAreaSprite; y: integer);
var
   area: TRect;
begin
   area := sprite.FillArea;
   area.top := y;
   sprite.FillArea := area;
end;

procedure TSysFrame.moveTo(coords: TRect);
var
   tl: TSgPoint;
begin
   tl := coords.TopLeft;
   FCorners[topLeft].x := tl.x;
   SetX(FBorders[facing_left], tl.x);
   FCorners[bottomLeft].x := tl.x;

   inc(tl.x, FCorners[topLeft].DrawRect.Right);
   SetX(FBorders[facing_up], tl.x);
   SetX(FBorders[facing_down], tl.x);
   FBackground.X := tl.x;

   inc(tl.x, FBorders[facing_up].FillArea.Right);
   FCorners[topRight].x := tl.x;
   SetX(FBorders[facing_right], tl.x);
   FCorners[bottomRight].x := tl.x;

   FCorners[topLeft].y := tl.y;
   SetY(FBorders[facing_up], tl.y);
   FCorners[topRight].y := tl.y;

   inc(tl.y, FCorners[topRight].DrawRect.Bottom);
   SetY(FBorders[facing_left], tl.y);
   SetY(FBorders[facing_right], tl.y);
   FBackground.y := tl.y;

   inc(tl.y, FBorders[facing_right].FillArea.Bottom);
   FCorners[bottomLeft].y := tl.y;
   SetY(FBorders[facing_down], tl.y);
   FCorners[bottomRight].y := tl.y;

   FBounds := coords;
end;

{ TMenuSpriteEngine }

constructor TMenuSpriteEngine.Create(graphic: TSystemImages; canvas: TSdlCanvas; images: TSdlImages);
begin
   assert(GMenuEngine = nil);
   GMenuEngine := self;
   inherited Create(nil, canvas);
   self.Images := images;
   FSystemGraphic := graphic;
   graphic.Setup(self);
   FCursor := TSysFrame.Create(self, FRAME_DISPLACEMENT, 2, NULLRECT);
   FMessageBox := TMessageBox.Create(self, rect(0, 0, 320, 80));
end;

destructor TMenuSpriteEngine.Destroy;
begin
   GMenuEngine := nil;
   GFontEngine.OnGetColor := nil;
   GFontEngine.OnGetDrawRect := nil;
   FSystemGraphic.Free;
   EndMessage;
   inherited Destroy;
end;

procedure TMenuSpriteEngine.EndMessage;
begin
//   FEnterLock := true;
   FMenuState := msNone;
   FMessageBox.visible := false;
   FMessageBox.FSignal.SetEvent;
end;

procedure TMenuSpriteEngine.ShowMessage(const msg: string; modal: boolean);
begin
   if FTerminated then
      Exit;
{   while FState = gs_fading do
      windows.sleep(GFrameLength);}
   FMessageBox.Visible := true;
   if modal then
      FMenuState := msExclusiveShared
   else FMenuState := msShared;
   FMessageBox.text := msg;
   FMessageBox.state := mb_display;
   FMessageBox.FSignal.WaitFor;
end;

procedure TMenuSpriteEngine.Terminate;
begin
   FTerminated := true;
   EndMessage;
end;

{ TSystemImages }

constructor TSystemImages.Create(images: TSdlImages; const filename: string; stretch: boolean);
var
   cls: TSdlImageClass;
begin
   inherited Create;
   FFilename := filename;
   FStretch := stretch;
   cls := images.SpriteClass;
   images.SpriteClass := TSdlImage;
   try
      images.EnsureImage(format('system\%s.png', [filename]), filename);
   finally
      images.SpriteClass := cls;
   end;

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
   FRects[srMerchDot] := rect(128, 8, 8, 8);
   FRects[srMerchD] := rect(128, 16, 8, 8);
   FRects[srMerchEq] := rect(128, 24, 8, 8);

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
{   TGameMap(parent).systemTimer := TSystemTimer.Create(parent);
   TGameMap(parent).timer2 := TSystemTimer.Create(parent);}

   FRects[srShadows] := rect(128, 32, 32, 16);
   FRects[srColors] := rect(0, 48, 16, 16);
   FRects[srBackground] := rect(0, 32, 16, 16);
end;

destructor TSystemImages.Destroy;
begin
   FBetterArrow.free;
   FDotArrow.free;
   FWorseArrow.free;
   FEqualValue.free;
   inherited;
end;

function TSystemImages.GetDrawRect(value: integer): TRect;
begin
   assert(value in [1..20]);
   result := FRects[srColors];
   if value > 10 then
      inc(result.Top, result.Bottom);
   inc(result.left, result.Right * ((value - 1) mod 10));
end;

function TSystemImages.GetHandle: integer;
begin
   result := FBetterArrow.Image.handle;
end;

procedure TSystemImages.Setup(parent: TMenuSpriteEngine);
begin
   FBetterArrow := TSystemTile.Create(parent, FRects[srMerchU], ARROW_DISPLACEMENT, 4);
   FBetterArrow.ImageName := FFilename;
   FBetterArrow.DrawRect := FRects[srMerchU];
   FDotArrow := TSystemTile.Create(parent, FRects[srMerchDot], ARROW_DISPLACEMENT, 4);
   FDotArrow.ImageName := FFilename;
   FDotArrow.DrawRect := FRects[srMerchDot];
   FWorseArrow := TSystemTile.Create(parent, FRects[srMerchD], ARROW_DISPLACEMENT, 4);
   FWorseArrow.ImageName := FFilename;
   FWorseArrow.DrawRect := FRects[srMerchD];
   FEqualValue := TSystemTile.Create(parent, FRects[srMerchEq], ARROW_DISPLACEMENT, 4);
   FEqualValue.ImageName := FFilename;
   FEqualValue.DrawRect := FRects[srMerchEq];
   if not assigned(GFontEngine) then
      asm int 3 end;
   GFontEngine.OnGetColor := self.GetHandle;
   GFontEngine.OnGetDrawRect := self.GetDrawRect;
end;

{ TCustomMessageBox }

constructor TCustomMessageBox.Create(parent: TMenuSpriteEngine; const coords: TRect);
begin
   inherited Create(parent, ORIGIN, 1, coords);
   FColumns := 1;
   FPromptLines := 0;
   FParsedText := TStringList.Create;
   FParsedText.Duplicates := dupAccept;
   FSignal := TSimpleEvent.Create;
   FBoxVisible := true;
end;

destructor TCustomMessageBox.Destroy;
begin
   FSignal.Free;
   FParsedText.free;
   inherited;
end;

function TCustomMessageBox.columnWidth: word;
begin
   result := ((self.FBounds.Right - 8) div FColumns) - SEPARATOR;
end;

function TCustomMessageBox.lastColumnWidth: word;
begin
   result := ((self.FBounds.Right - 8) div FLastLineColumns) - SEPARATOR;
end;

procedure TCustomMessageBox.parseText(const input: string);
begin
   FSignal.ResetEvent;
   FParsedText.Text := input;
   FMessageText := input;
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

   with TMenuSpriteEngine(FEngine).cursor do
   begin
      Visible := true;
      layout(coords);
   end;
   FDontChangeCursor := false;
end;

procedure TCustomMessageBox.PlaySound(which: TSfxTypes);
begin
   if assigned(FPlaySound) then
      FPlaySound(which);
end;

procedure TCustomMessageBox.button(const input: TButtonCode);
var
   max, absMax, lPosition: smallint;
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

   lPosition := FCursorPosition; //to suppress a compiler warning
   max := (FParsedText.Count - 1) - (FPromptLines + FLastLineColumns);
   absMax := max + FLastLineColumns;
   case input of
      btn_enter:
      begin
         if FOptionEnabled[FCursorPosition] then
         begin
            TMenuSpriteEngine(FEngine).FMenuInt := FCursorPosition;
            PlaySound(sfxAccept);
         end
         else PlaySound(sfxBuzzer);
      end;
      btn_down:
      begin
         if FCursorPosition <= max - FColumns then
            lPosition := FCursorPosition + FColumns
         else if FColumns = 1 then
            lPosition := 0
         else if (FLastLineColumns > 0) and (FCursorPosition <= max) then
         begin
            lPosition := FCursorPosition mod FColumns;
            ratio := FColumns div FLastLineColumns;
            lPosition := (lPosition div ratio) + max + 1;
         end;
      end;
      btn_up:
      begin
         if FCursorPosition > max then
         begin
            ratio := FColumns div FLastLineColumns;
            lPosition := FCursorPosition - (max + 1);
            lPosition := (max + 1 - FColumns) + (lPosition * ratio) + (ratio div 2);
         end else if FCursorPosition >= FColumns then
            lPosition := FCursorPosition - FColumns
         else if FColumns = 1 then
            lPosition := FParsedText.Count - 1;
      end;
      btn_right:
      begin
         if (FColumns > 1) and (FCursorPosition < absMax) then
            lPosition := FCursorPosition + 1;
      end;
      btn_left:
      begin
         if (FColumns > 1) and (FCursorPosition > 0) then
            lPosition := FCursorPosition - 1;
      end;
      else ;
   end;
   if (input in [btn_up, btn_down, btn_left, btn_right]) and (lPosition <> FCursorPosition) then
   begin
      FButtonLock := TRpgTimestamp.Create(180);
      placeCursor(lPosition);
      PlaySound(sfxCursor);
   end;
end;

{ TMessageBox }

constructor TMessageBox.Create(parent: TMenuSpriteEngine; const coords: TRect);
const BORDER_THICKNESS = 16;
begin
   //these three lines go before the inherited constructor because they're
   //needed (the first, at least) by the virtual MoveTo function that gets
   //called from the inherited constructor
   FNextArrow := TSystemTile.Create(parent, parent.FSystemGraphic.FRects[srArrowD], ORIGIN, 1);
   FNextArrow.ImageName := parent.FSystemGraphic.FFilename;
   FPortrait := TSprite.Create(parent);
   FPortrait.SetSpecialRender;
   inherited Create(parent, coords);
   FTextColor := 1;
   SetTextRate(1);

   FFrameTarget := TSdlRenderTarget.Create(sgPoint(self.Width, self.Height));
   FTextTarget := TSdlRenderTarget.Create(sgPoint(self.Width - BORDER_THICKNESS, self.Height - BORDER_THICKNESS));
   ClearTarget(FFrameTarget);
   ClearTarget(FTextTarget);
end;

destructor TMessageBox.Destroy;
begin
   FFrameTarget.Free;
   FTextTarget.Free;
   inherited Destroy;
end;

procedure TMessageBox.ClearTarget(target: TSdlRenderTarget);
var
   r, g, b, a: byte;
begin
   target.parent.pushRenderTarget;
   SDL_GetRenderDrawColor(FTextTarget.parent.Renderer, r, g, b, a);
   SDL_SetRenderDrawColor(FTextTarget.parent.Renderer, 0, 0, 0, 0);
   target.SetRenderer;
   glDisable(GL_BLEND);
   target.Clear;
   glEnable(GL_BLEND);
   SDL_SetRenderDrawColor(FTextTarget.parent.Renderer, r, g, b, a);
   target.parent.popRenderTarget;
end;

function TMessageBox.GetDrawCoords: TRect;
begin
   result.Left := 0;
   result.Right := FFrameTarget.parent.Width;
   result.Bottom := FFrameTarget.parent.Height div 3;
   result.Top := result.Bottom * ord(FPosition);
end;

procedure TMessageBox.Draw;
const
   TEXTV = 0.1;
   TEXTH = 0.025;
var
   xVal, yVal: single;
   dest: TRect;
begin
   if boxVisible then
      DrawFrame;

   if FState = mb_display then
   begin
      FTextTarget.parent.pushRenderTarget;
      FTextTarget.SetRenderer;
      try
         FPortrait.Draw;
         DrawNextChar;
      finally
         FTextTarget.parent.popRenderTarget;
      end;
      dest := GetDrawCoords;
      setX(FNextArrow, dest.Right div 2);
      xVal := dest.right * TEXTH;
      yVal := dest.Bottom * TEXTV;
      inc(dest.Left, round(xVal));
      dec(dest.right, round(xVal * 2));
      inc(dest.Top, round(yVal));
      dec(dest.Bottom, round(yVal * 2));
      SDL_RenderCopy(FTextTarget.parent.Renderer, FTextTarget.handle, nil, @dest);
      setY(FNextArrow, dest.Bottom + dest.top);
      if FTextCounter >= FParsedText.Count then
         FNextArrow.Draw;
   end
   else TMenuSpriteEngine(Engine).cursor.Draw;
end;

procedure TMessageBox.DrawFrame;
var
   coords: TRect;
begin
   if not FFrameDrawn then
   begin
      FFrameTarget.parent.pushRenderTarget;
      FFrameTarget.SetRenderer;
      SDL_SetRenderDrawColor(FFrameTarget.parent.Renderer, 0, 0, 0, 0);
      FFrameTarget.Clear;
      SDL_SetRenderDrawColor(FFrameTarget.parent.Renderer, 1, 1, 1, 1);
      inherited Draw;
      FFrameTarget.parent.popRenderTarget;
      FFrameDrawn := true;
   end;
   coords.Left := 0;
   coords.Right := FFrameTarget.parent.Width;
   coords.Bottom := FFrameTarget.parent.Height div 3;
   coords.Top := coords.Bottom * ord(FPosition);
//   FFrameTarget.DrawFull(coords.TopLeft);
   SDL_RenderCopy(FFrameTarget.parent.Renderer, FFrameTarget.handle, nil, @coords);
end;

procedure TMessageBox.DrawChar(value: char);
var
   newPos: TSgFloatPoint;
begin
   newPos := GFontEngine.drawChar(value, FTextPosX, FTextPosY, FTextColor);
   FTextPosX := newPos.x;
   FTextPosY := newPos.y;
end;

procedure TMessageBox.NewLine;
const 
   TOP_MARGIN = 3;
   LINE_HEIGHT = 16;
begin
   if FPortrait.Visible and not FRightPortrait then
      FTextPosX := 65
   else FTextPosX := 3;
   inc(FTextLine);
   FTextPosY := (LINE_HEIGHT * FTextLine) + TOP_MARGIN;   
end;

function TMessageBox.GetIntegerValue: integer;
begin
   inc(FTextCounter);
   if FParsedText[FTextCounter] = '\V' then
      result := GEnvironment.Ints[GetIntegerValue]
   else if not TryStrToInt(FParsedText[FTextCounter], result) then
      Abort;
end;

procedure TMessageBox.moveTo(coords: TRect);
begin
   inherited moveTo(coords);
   self.Width := coords.Right;
   self.Height := coords.Bottom;

   SetX(FNextArrow, 152 + trunc(FCorners[topLeft].X));
   SetY(FNextArrow, trunc(FCorners[bottomLeft].Y));
   FPortrait.Y := 8;
   setRightside(FRightPortrait);
end;

procedure TMessageBox.SetTextRate(value: integer);
begin
   FTextRate := value * 0.01;
end;

function TMessageBox.GetHeroName(value: integer): string;
begin
   if value = 0 then
   begin
      if GEnvironment.partySize = 0 then
         result := ''
      else result := GEnvironment.party.hero[1].name;
   end
   else if clamp(value, 1, GEnvironment.HeroCount) <> value then
      Abort
   else result := GEnvironment.Heroes[value].name;
end;

procedure TMessageBox.DrawSpecialChar(const line: string);
const HALF_CHAR = 3;
begin
   assert(line[1] = '\');
   try
      case line[2] of
         '$': FSpecialText := IntToStr(GEnvironment.money);
         '!':; //TODO: implement this
         '.': FRemainder := FRemainder - 0.25;  //quarter-second delay
         '|': FRemainder := FRemainder - 1;     //full-second delay
         '>': FImmediate := true;
         '<': FImmediate := false;
         '^': TMenuSpriteEngine(Engine).endMessage;
         '_': FTextPosX := FTextPosX + HALF_CHAR;
         'E': Abort; //TODO: implement error reporting
         'e': Abort; //TODO: implement error reporting
         'C': FTextColor := clamp(GetIntegerValue, 1, 20);
         'S': SetTextRate(clamp(GetIntegerValue, 1, 20));
         'N': FSpecialText := GetHeroName(GetIntegerValue);
         'V': FSpecialText := IntToStr(GEnvironment.Ints[GetIntegerValue]);
         'T': Abort; //TODO: implement string array in Environment
         'F': Abort; //TODO: implement float array in Environment
         'O': Abort; //TODO: implement vocab display
      end;   
   except
      on EAbort do ;
   end;
   if FSpecialText <> '' then
      FSpecialIndex := 1;
end;

procedure TMessageBox.DrawChar(index: integer);
var
   value: string;
begin
   value := FParsedText[index];
   if length(value) = 1 then
      drawChar(value[1])
   else if value = #13#10 then
      NewLine
   else DrawSpecialChar(value);
end;

procedure TMessageBox.DrawNextChar;
begin
   if FTextCounter >= FParsedText.Count then
      Exit;

   //to prevent deadlocking on the synchronized part of ResetText
   if TMonitor.TryEnter(self) then
      try
         FRemainder := FRemainder + (TRpgTimestamp.FrameLength / 1000);
         while (FTextCounter < FParsedText.Count) and (FImmediate or (FRemainder > FTextRate)) do
         begin
            if (FSpecialIndex > 0) and (FSpecialIndex <= length(FSpecialText)) then
            begin
               DrawChar(FSpecialText[FSpecialIndex]);
               inc(FSpecialIndex);
            end
            else begin
               if FSpecialIndex > 0 then
               begin
                  FSpecialIndex := 0;
                  FSpecialText := '';
               end;
               DrawChar(FTextCounter);
               inc(FTextCounter);
            end;
            if not FImmediate then
               FRemainder := FRemainder - FTextRate;
         end;
      finally
         TMonitor.Exit(self);
      end;
end;

procedure TMessageBox.PromptButton(const input: TButtonCode);
begin
   case input of
      btn_enter:
      begin
         if Validate(FParsedText[FCursorPosition]) then
         begin
            TMenuSpriteEngine(Engine).FMenuInt := FCursorPosition;
            TMenuSpriteEngine(Engine).endMessage;
            playSound(sfxAccept);
         end
         else playSound(sfxBuzzer);;
      end;
      btn_cancel:
      begin
         if FAcceptCancel then
         begin
            TMenuSpriteEngine(Engine).FMenuInt := 3;
            TMenuSpriteEngine(Engine).endMessage;
            playSound(sfxCancel);
         end;
      end;
      btn_down, btn_up:
      begin
         if FCursorPosition = 2 then
         begin
            placeCursor(3);
         end
         else placeCursor(2);
         playSound(sfxCursor);
      end
      else ;
   end;
end;

procedure TMessageBox.InputButton(const input: TButtonCode);
begin
   case input of
      btn_enter:
      begin
         TMenuSpriteEngine(Engine).FMenuInt := computeInputResult;
         TMenuSpriteEngine(Engine).endMessage;
         playSound(sfxAccept);
      end;
      btn_down:
      begin
         if inputResult[FCursorPosition] = 0 then
            inputResult[FCursorPosition] := 9
         else inputResult[FCursorPosition] := inputResult[FCursorPosition] - 1;
         playSound(sfxCursor);
      end;
      btn_up:
      begin
         if inputResult[FCursorPosition] = 9 then
            inputResult[FCursorPosition] := 0
         else inputResult[FCursorPosition] := inputResult[FCursorPosition] + 1;
         playSound(sfxCursor);
      end;
      btn_left:
      begin
         if FCursorPosition > 0 then
            placeCursor(FCursorPosition - 1);
         playSound(sfxCursor);
      end;
      btn_right:
      begin
         if FCursorPosition < high(FInputResult) then
            placeCursor(FCursorPosition + 1);
         playSound(sfxCursor);
      end
      else ;
   end;
end;

procedure TMessageBox.button(const input: TButtonCode);
begin
   if assigned(FButtonLock) then
      if (FButtonLock.timeRemaining = 0) then
         freeAndNil(FButtonLock)
      else Exit;

   case FState of
      mb_display:
         case input of
            btn_enter, btn_cancel: if FTextCounter >= FParsedText.Count then
              TMenuSpriteEngine(Engine).endMessage;
            else ;
         end;
      mb_choice:
         case input of
            btn_cancel:
            begin
               if FAcceptCancel then
               begin
                  TMenuSpriteEngine(Engine).FMenuInt := -1;
                  TMenuSpriteEngine(Engine).endMessage;
                  PlaySound(sfxCancel);
               end;
            end;
            else begin
               inherited button(input);
               if input = btn_enter then
                  TMenuSpriteEngine(Engine).endMessage;
            end;
         end;
      mb_prompt: PromptButton(input);
      mb_input: InputButton(input);
   end;
   FButtonLock := TRpgTimestamp.Create(180);
end;

function TMessageBox.computeInputResult: integer;
var
   i: integer;
begin
   result := 0;
   for i := high(FInputResult) downto 0 do
      result := (result * 10) + FInputResult[i];
end;

function TMessageBox.getInputResult(digit: byte): byte;
begin
   assert(digit < length(FInputResult));
   result := FInputResult[digit];
end;

procedure TMessageBox.changeInputResult(digit, value: byte);
var
   dummy: string;
   i: integer;
begin
   assert(digit <= high(FInputResult));
   assert(value < 10);
   if FInputResult[digit] = value then
      Exit;

   FInputResult[digit] := value;
   dummy := intToStr(FInputResult[0]);
   for I := 1 to high(FInputResult) do
      dummy := dummy + '  ' + intToStr(FInputResult[i]);
   text := dummy;
end;

procedure TMessageBox.ParseInt(const input: string; var counter: integer);
var
   start: integer;
begin
   inc(counter);
   start := counter;
   if UpperCase(copy(input, start, 3)) = '\V[' then
   begin
      FParsedText.Add('\V');
      inc(counter);
      ParseInt(input, counter);
   end
   else begin
      while (counter <= length(input)) and (TCharacter.IsDigit(input[counter])) do
         inc(counter);
      if (counter > length(input)) or (input[counter] <> ']') then
      begin
         FParsedText.Add('\e');
         counter := start;
      end
      else begin
         FParsedText.Add(copy(input, start, counter - start));
//         inc(counter);
      end;
   end;
end;

procedure TMessageBox.ParseParamToken(const input: string; var counter: integer);
var
   token: char;
begin
   token := UpCase(input[counter]);
   FParsedText.Add('\' + token);
   case token of
      'C','S','N','V','T','F':
      begin
         inc(counter);
         if input[counter] = '[' then
            ParseInt(input, counter)
         else FParsedText.Add('\E' + input[counter]);
      end;
      'O': FParsedText.Add('\E' + input[counter]); //TODO: support \O for vocab
      else assert(false);
   end;
end;

procedure TMessageBox.ParseToken(const input: string; var counter: integer);
var
   token: char;
begin
   assert(input[counter] = '\');
   inc(counter);
   token := UpCase(input[counter]);
   case token of
      '\': FParsedText.Add('\');
      '$','!','.','|','>','<','^','_': FParsedText.Add('\' + token);
      'C','S','N','V','T','F','O': ParseParamToken(input, counter);
      else FParsedText.Add('\E' + input[counter]);
   end;
end;

procedure TMessageBox.setPortrait(const filename: string; const index: byte);
var
   image: TSdlImage;
begin
   image := Engine.Images.EnsureImage(format('portrait\%s', [filename]), filename);
   FPortrait.Visible := true;
   FPortrait.ImageName := image.name;
   FPortrait.ImageIndex := index;
end;

procedure TMessageBox.setRightside(const value: boolean);
begin
   FRightPortrait := value;
   case value of
      false: FPortrait.X := 8;
      true: FPortrait.X := FCorners[topRight].x - 56;
   end;
end;

procedure TMessageBox.ResetText;
begin
   FSignal.ResetEvent;
   FParsedText.Clear;
   runThreadsafe(procedure begin ClearTarget(FTextTarget) end, true);
   FTextCounter := 0;
   FSpecialText := '';
   FSpecialIndex := 0;
   FImmediate := false;
   FRemainder := 0;
   FTextLine := -1;
   SetTextRate(1);
   NewLine;
end;

procedure TMessageBox.parseText(const input: string);
var
   counter: integer;
begin
   TMonitor.Enter(self);
   try
      ResetText;
      counter := 1;
      while counter <= length(input) do
      begin
         if input[counter] = #13 then
         begin
            FParsedText.Add(#13#10);
            if (counter < length(input)) and (input[counter + 1] = #10) then
               inc(counter);
         end
         else if input[counter] <> '\' then
            FParsedText.Add(input[counter])
         else ParseToken(input, counter);
         inc(counter);
      end;
   finally
      TMonitor.Exit(self);
   end;
end;

function TMessageBox.Validate(const text: string): boolean;
begin
   if assigned(FOnValidate) then
      result := FOnValidate(text)
   else result := true;
end;

end.
