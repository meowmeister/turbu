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
   sg_defs, sdl_sprite, sdl_ImageManager, sdl_canvas,
   sdl_13;

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
   TCustomMessageBox = class;

   TSystemImages = class(TObject)
   private type TSystemRectArray = array[TSystemRects] of TRect;
   private
      FRects: TSystemRectArray;
      FFilename: string;
      FBetterArrow: TSystemTile;
      FDotArrow: TSystemTile;
      FWorseArrow: TSystemTile;
      FEqualValue: TSystemTile;
      FStretch: boolean;
      FTranslucent: boolean;
      procedure Setup(parent: TMenuSpriteEngine);
      function GetDrawRect(value: integer): TRect;
      function GetHandle: TSdlTexture;
   public
      constructor Create(images: TSdlImages; const filename: string;
        stretch, translucent: boolean);
      destructor Destroy; override;

      property filename: string read FFilename;
      property rects: TSystemRectArray read FRects;
      property translucent: boolean read FTranslucent;
   end;

   TMessageBoxTypes = (mbtMessage, mbtChoice, mbtPrompt, mbtInput);

   TMenuSpriteEngine = class(TSpriteEngine)
   private
      FSystemGraphic: TSystemImages;
      FMenuInt: integer;
      FCursor: TSysFrame;
      FMenuState: TMenuState;
      FBoxes: array[TMessageBoxTypes] of TCustomMessageBox;
      FCurrentBox: TCustomMessageBox;
      FPosition: TMboxLocation;
      FBoxVisible: boolean;
      FEnding: boolean;
      procedure SetPosition(const Value: TMboxLocation);
      procedure SetBoxVisible(const Value: boolean);
      function GetPortrait: TSprite;
   protected
      procedure EndMessage;
      function InnVocab(style: integer; const name: string; value: integer = 0): string;
   public
      constructor Create(graphic: TSystemImages; canvas: TSdlCanvas; images: TSdlImages);
      destructor Destroy; override;
      procedure ShowMessage(const msg: string; modal: boolean);
      procedure inn(style, cost: integer);
      procedure ChoiceBox(const msg: string; const responses: TArray<string>; allowCancel: boolean);
      procedure InputNumber(digits: integer);
      procedure button(const input: TButtonCode);
      procedure SetPortrait(const filename: string; index: integer);
      procedure SetRightside(value: boolean);

      property MenuInt: integer read FMenuInt write FMenuInt;
      property Cursor: TSysFrame read FCursor;
      property State: TMenuState read FMenuState;
      property SystemGraphic: TSystemImages read FSystemGraphic;
      property position: TMboxLocation read FPosition write SetPosition;
      property boxVisible: boolean read FBoxVisible write SetBoxVisible;
      property portrait: TSprite read GetPortrait;
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
      FMessageText: string;
      FBoxVisible: boolean;
      FPlaySound: TPlaySoundEvent;
      FFrameTarget: TSdlRenderTarget;
      FFrameDrawn: boolean;
      FCoords: TRect;

      procedure DrawFrame;
   protected
      FSignal: TSimpleEvent;
      FParsedText: TStringList;
      FOptionEnabled: array of boolean;
      FColumns: byte;
      FDontChangeCursor: boolean;
      FButtonLock: TRpgTimestamp;
      FLastLineColumns: byte;
      FPosition: TMboxLocation;
      FTextTarget: TsdlRenderTarget;
      FTextPosX: single;
      FTextPosY: single;
      FTextCounter: integer;
      FTextColor: integer;
      FTextLine: integer;

      function columnWidth: word;
      function lastColumnWidth: word;
      procedure PlaySound(which: TSfxTypes);
      procedure parseText(const input: string); virtual;
      procedure ClearTarget(target: TSdlRenderTarget);
      procedure SetPosition(const Value: TMboxLocation);
      procedure EndMessage;

      procedure DoDraw; override;
      procedure DrawChar(value: char); overload;
      procedure DrawChar(const value: string); overload;
      procedure DrawLine(const value: string);
      procedure NewLine; virtual;
      procedure DrawSpecialChar(const line: string); virtual;
      function GetIntegerValue: integer;
      function GetDrawCoords: TRect;
      procedure ResetText; virtual;

      function ParseToken(const input: string; var counter: integer): string;
      function ParseParamToken(const input: string; var counter: integer): string;
      function ParseInt(const input: string; var counter: integer): string;
   public
      constructor Create(parent: TMenuSpriteEngine; const coords: TRect); virtual;
      destructor Destroy; override;
      procedure button(const input: TButtonCode); virtual; abstract;
      procedure Draw; override;
      procedure moveTo(coords: TRect); override;

      property text: string read FMessageText write parseText;
      property boxVisible: boolean read FBoxVisible write FBoxVisible;
      property position: TMboxLocation read FPosition write SetPosition;
      property OnPlaySound: TPlaySoundEvent read FPlaySound write FPlaySound;
      property Signal: TSimpleEvent read FSignal;
   end;

procedure SetX(sprite: TTiledAreaSprite; x: integer);
procedure SetY(sprite: TTiledAreaSprite; y: integer);

var
   GMenuEngine: TMenuSpriteEngine;
const
   SEPARATOR = 8;

implementation
uses
   Windows, SysUtils, Math, OpenGL, Character,
   commons, turbu_text_utils, turbu_2k_environment, turbu_OpenGL, turbu_database,
   turbu_2k_message_boxes, turbu_2k_sprite_engine, turbu_script_engine, rs_media;

const
   ARROW_DISPLACEMENT: TSgPoint = (x: 8; y: 0);
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
   end;
   for I := 0 to ord(high(TCorners)) do
   begin
      FCorners[TCorners(i)].Z := 3;
      FCorners[TCorners(i)].FillArea := NULLRECT;
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

      FBackground.FillArea := TRectToSdlRect(coords);
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
var
   size: TRect;
   boxtype: TMessageBoxTypes;
begin
   assert(GMenuEngine = nil);
   GMenuEngine := self;
   inherited Create(nil, canvas);
   self.Images := images;
   FSystemGraphic := graphic;
   graphic.Setup(self);
   FCursor := TSysFrame.Create(self, FRAME_DISPLACEMENT, 2, NULLRECT);
   size := rect(0, 0, 320, 80);
   FPosition := mb_bottom;
   FBoxes[mbtMessage] := TMessageBox.Create(self, size);
   FBoxes[mbtChoice] := TChoiceBox.Create(self, size);
   FBoxes[mbtPrompt] := TPromptBox.Create(self, size);
   FBoxes[mbtInput] := TInputBox.Create(self, size);
   for boxtype := Low(TMessageBoxTypes) to High(TMessageBoxTypes) do
      FBoxes[boxtype].OnPlaySound := rs_media.PlaySystemSound;
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
   FMenuState := msNone;
   if assigned(FCurrentBox) and not FEnding then
   begin
      FEnding := true;
      try
         FCurrentBox.EndMessage;
      finally
         FEnding := false;
      end;
      FCursor.Visible := false;
   end;
   FCurrentBox := nil;
end;

function TMenuSpriteEngine.GetPortrait: TSprite;
begin
   result := (FBoxes[mbtMessage] as TMessageBox).portrait;
end;

procedure TMenuSpriteEngine.SetPortrait(const filename: string; index: integer);
begin
   (FBoxes[mbtMessage] as TMessageBox).setPortrait(filename, index);
end;

function TMenuSpriteEngine.InnVocab(style: integer; const name: string; value: integer = 0): string;
var
   key: string;
begin
   key := format('Inn%d-%s', [style, name]);
   result := GDatabase.vocab[key];
   if result = '' then
   begin
      key := format('Inn%d-%s', [1, name]);
      result := GDatabase.vocab[key];
   end;
   if pos('\i', result) > 0 then
      result := StringReplace(result, '\i', IntToStr(value), [rfReplaceAll]);
   if pos('\$', result) > 0 then
      result := StringReplace(result, '\$', IntToStr(value), [rfReplaceAll]);
end;

procedure TMenuSpriteEngine.InputNumber(digits: integer);
begin
   FCurrentBox := FBoxes[mbtInput];
   TValueInputBox(FCurrentBox).setupInput(digits);
   TValueInputBox(FCurrentBox).placeCursor(0);
   TValueInputBox(FCurrentBox).canCancel := false;
   TValueInputBox(FCurrentBox).setupInput(digits);
   FBoxes[mbtInput].Visible := true;
   FMenuState := msExclusiveShared;
end;

procedure TMenuSpriteEngine.inn(style, cost: integer);
var
   greet1: string;
   choices: TArray<string>;
begin
   greet1 := innVocab(style, 'Greet', cost);
   choices := TArray<string>.Create(innVocab(style, 'Stay'), innVocab(style, 'Cancel'));
   choiceBox(greet1, choices, true);
end;

procedure TMenuSpriteEngine.button(const input: TButtonCode);
begin
   assert(assigned(FCurrentBox));
   FCurrentBox.button(input);
end;

procedure TMenuSpriteEngine.ChoiceBox(const msg: string; const responses: TArray<string>;
  allowCancel: boolean);
var
   box: TCustomMessageBox;
begin
   while GSpriteEngine.State = gs_fading do
      sleep(TRpgTimestamp.FrameLength);
   box := FBoxes[mbtChoice];
   box.text := msg;
   TChoiceBox(box).SetChoices(responses);
   TChoiceBox(box).canCancel := allowCancel;
   TChoiceBox(box).placeCursor(0);
   box.Visible := true;
   FCurrentBox := box;
   try
      FMenuState := msExclusiveShared;
      box.FSignal.WaitFor;
   finally
      endMessage;
   end;
end;

procedure TMenuSpriteEngine.SetBoxVisible(const Value: boolean);
var
   box: TCustomMessageBox;
begin
   FBoxVisible := Value;
   for box in FBoxes do
      box.boxVisible := value;
end;

procedure TMenuSpriteEngine.SetPosition(const Value: TMboxLocation);
var
   box: TCustomMessageBox;
begin
   FPosition := Value;
   for box in FBoxes do
      box.position := value;
end;

procedure TMenuSpriteEngine.SetRightside(value: boolean);
begin
   (FBoxes[mbtMessage] as TMessageBox).rightside := value;
end;

procedure TMenuSpriteEngine.ShowMessage(const msg: string; modal: boolean);
var
   box: TCustomMessageBox;
begin
   while GSpriteEngine.State = gs_fading do
      sleep(TRpgTimestamp.FrameLength);
   box := FBoxes[mbtMessage];
   box.text := msg;
   box.Visible := true;
   FCurrentBox := box;
   try
      if modal then
         FMenuState := msExclusiveShared
      else FMenuState := msShared;
      GScriptEngine.SetWaiting(
         function: boolean begin result := box.FSignal.WaitFor(0) = wrSignaled end);
      GScriptEngine.ThreadWait;
   finally
      endMessage;
   end;
end;

{ TSystemImages }

constructor TSystemImages.Create(images: TSdlImages; const filename: string;
  stretch, translucent: boolean);
var
   cls: TSdlImageClass;
begin
   inherited Create;
   FFilename := filename;
   FStretch := stretch;
   FTranslucent := translucent;
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
{$MESSAGE WARN 'Commented out code in live unit'}
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

function TSystemImages.GetHandle: TSdlTexture;
begin
   result := FBetterArrow.Image.surface;
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
const BORDER_THICKNESS = 16;
begin
   inherited Create(parent, ORIGIN, 1, coords);
   FColumns := 1;
   FParsedText := TStringList.Create;
   FParsedText.Duplicates := dupAccept;
   FSignal := TSimpleEvent.Create;
   FBoxVisible := true;
   FFrameTarget := TSdlRenderTarget.Create(sgPoint(self.Width, self.Height));
   ClearTarget(FFrameTarget);
   FTextTarget := TSdlRenderTarget.Create(sgPoint(self.Width - BORDER_THICKNESS, self.Height - BORDER_THICKNESS));
   ClearTarget(FTextTarget);
   FTextColor := 1;
end;

destructor TCustomMessageBox.Destroy;
begin
   FButtonLock.Free;
   FSignal.Free;
   FParsedText.free;
   FFrameTarget.Free;
   FTextTarget.Free;
   inherited;
end;

procedure TCustomMessageBox.DrawChar(value: char);
var
   newPos: TSgFloatPoint;
begin
   newPos := GFontEngine.drawChar(value, FTextPosX, FTextPosY, FTextColor);
   FTextPosX := newPos.x;
   FTextPosY := newPos.y;
end;

procedure TCustomMessageBox.DrawFrame;
begin
   if not FFrameDrawn then
   begin
      FFrameTarget.parent.pushRenderTarget;
      FFrameTarget.SetRenderer;
      SDL_SetRenderDrawColor(FFrameTarget.parent.Renderer, 0, 0, 0, 0);
      FFrameTarget.Clear;
      SDL_SetRenderDrawColor(FFrameTarget.parent.Renderer, 1, 1, 1, 1);
      inherited DoDraw;
      FFrameTarget.parent.popRenderTarget;
      FFrameDrawn := true;
      SDL_SetTextureBlendMode(FFrameTarget.handle, [sdlbBlend]);
   end;
//   FFrameTarget.DrawFull(FCoords.TopLeft);
   SDL_RenderCopy(FFrameTarget.parent.Renderer, FFrameTarget.handle, nil, @FCoords);
end;

function TCustomMessageBox.GetDrawCoords: TRect;
begin
   result.Left := 0;
   result.Right := FTextTarget.parent.Width;
   result.Bottom := FTextTarget.parent.Height div 3;
   result.Top := result.Bottom * ord(FPosition);
end;

procedure TCustomMessageBox.DrawLine(const value: string);
var
   ch: char;
begin
   for ch in value do
      DrawChar(ch);
end;

procedure TCustomMessageBox.EndMessage;
begin
   TMenuSpriteEngine(Engine).endMessage;
   visible := false;
   FSignal.SetEvent;
end;

function TCustomMessageBox.GetIntegerValue: integer;
begin
   inc(FTextCounter);
   if FParsedText[FTextCounter] = '\V' then
      result := GEnvironment.Ints[GetIntegerValue]
   else if not TryStrToInt(FParsedText[FTextCounter], result) then
      Abort;
end;

function TCustomMessageBox.ParseInt(const input: string; var counter: integer): string;
var
   start: integer;
begin
   inc(counter);
   start := counter;
   if UpperCase(copy(input, start, 3)) = '\V[' then
   begin
      inc(counter);
      result := '\V' + ParseInt(input, counter);
   end
   else begin
      while (counter <= length(input)) and (TCharacter.IsDigit(input[counter])) do
         inc(counter);
      if (counter > length(input)) or (input[counter] <> ']') then
      begin
         result := '\e';
         counter := start;
      end
      else begin
         result := copy(input, start, counter - start);
//         inc(counter);
      end;
   end;
end;

function TCustomMessageBox.ParseParamToken(const input: string; var counter: integer): string;
var
   token: char;
begin
   token := UpCase(input[counter]);
   result := '\' + token;
   case token of
      'C','S','N','V','T','F':
      begin
         inc(counter);
         if input[counter] = '[' then
            result := ParseInt(input, counter)
         else result := '\E' + input[counter];
      end;
      'O': result := '\E' + input[counter]; //TODO: support \O for vocab
      else assert(false);
   end;
end;

function TCustomMessageBox.ParseToken(const input: string; var counter: integer): string;
var
   token: char;
begin
   assert(input[counter] = '\');
   inc(counter);
   token := UpCase(input[counter]);
   case token of
      '\': result := '\';
      '$','!','.','|','>','<','^','_': result := '\' + token;
      'C','S','N','V','T','F','O': result := ParseParamToken(input, counter);
      else result := '\E' + input[counter];
   end;
end;

procedure TCustomMessageBox.DrawSpecialChar(const line: string);
const HALF_CHAR = 3;
begin
   assert(line[1] = '\');
   case line[2] of
      '$': DrawLine(IntToStr(GEnvironment.money));
      '_': FTextPosX := FTextPosX + HALF_CHAR;
      'C': FTextColor := clamp(GetIntegerValue, 1, 20);
      'V': DrawLine(IntToStr(GEnvironment.Ints[GetIntegerValue]));
   end;
end;

procedure TCustomMessageBox.Draw;
begin
   if self.Visible then
      DoDraw;
end;

procedure TCustomMessageBox.DrawChar(const value: string);
begin
   if length(value) = 1 then
      drawChar(value[1])
   else if value = #13#10 then
      NewLine
   else DrawSpecialChar(value);
end;
procedure TCustomMessageBox.DoDraw;
begin
   if boxVisible then
      DrawFrame;
end;

procedure TCustomMessageBox.ClearTarget(target: TSdlRenderTarget);
var
   r, g, b, a: byte;
begin
   target.parent.pushRenderTarget;
   SDL_GetRenderDrawColor(target.parent.Renderer, r, g, b, a);
   SDL_SetRenderDrawColor(target.parent.Renderer, 0, 0, 0, 0);
   target.SetRenderer;
   glDisable(GL_BLEND);
   target.Clear;
   glEnable(GL_BLEND);
   SDL_SetRenderDrawColor(target.parent.Renderer, r, g, b, a);
   target.parent.popRenderTarget;
end;

function TCustomMessageBox.columnWidth: word;
begin
   result := ((self.FBounds.Right - 8) div FColumns) - SEPARATOR;
end;

function TCustomMessageBox.lastColumnWidth: word;
begin
   result := ((self.FBounds.Right - 8) div FLastLineColumns) - SEPARATOR;
end;

procedure TCustomMessageBox.moveTo(coords: TRect);
begin
   inherited moveTo(coords);
   self.Width := coords.Right;
   self.Height := coords.Bottom;
end;

procedure TCustomMessageBox.NewLine;
const
   TOP_MARGIN = 3;
   LINE_HEIGHT = 16;
begin
   FTextPosX := 3;
   inc(FTextLine);
   FTextPosY := (LINE_HEIGHT * FTextLine) + TOP_MARGIN;
end;

procedure TCustomMessageBox.parseText(const input: string);
begin
   FSignal.ResetEvent;
   FParsedText.Text := input;
   FMessageText := input;
end;

procedure TCustomMessageBox.PlaySound(which: TSfxTypes);
begin
   if assigned(FPlaySound) then
      FPlaySound(which);
end;

procedure TCustomMessageBox.ResetText;
begin
   FSignal.ResetEvent;
   runThreadsafe(procedure begin ClearTarget(FTextTarget) end, true);
   FTextCounter := 0;
   FTextLine := -1;
   NewLine;
end;

procedure TCustomMessageBox.SetPosition(const Value: TMboxLocation);
begin
   FPosition := Value;
   FCoords.Left := 0;
   FCoords.Right := FFrameTarget.parent.Width;
   FCoords.Bottom := FFrameTarget.parent.Height div 3;
   FCoords.Top := FCoords.Bottom * ord(FPosition);
end;

end.
