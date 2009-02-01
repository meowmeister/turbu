unit chipset_graphics;
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
   types, classes, contnrs, syncObjs, //windows libs
   commons, chipset_data, LMU, LDB, LMT, charset_data, addition_sprite, charset_graphics,
   script_engine, tiles, frames, rm2X_menu_engine, weather, timing,
   rs_map, map_unit,//turbu libs
   sdl_canvas, sdl_sprite, SDL_ImageManager, SG_Defs,
   AsphyreDB, {AsphyreImages, AsphyreSprite,} AsphyreFonts, AsphyreDef,
   AsphyreTextures, AsphyreDevices; //asphyre libs

type
{MAP ENGINE}

   TTransProc = procedure(var location: integer);
   TRenderProc = procedure;
   TMapSet = array of TRpgMap;
   TTileGrid = array of array of TLowerTile;

   TGameMap = class(TSpriteEngine)
   private
      FCurrMap: TRpgMap;
      FMapSet: TMapSet;
      FTileGrid: TTileGrid;
      FLongAnim: boolean;
      FAnimStage: byte;
      FState: TGameState;
      FSavedState: TGameState;
      FSwitchState: TSwitchState;
      FCutscene: byte;
      FScriptEngine: TScriptEngine;
      FSystemGraphic: TSystemImages;
      FMenuInt: integer;
      FMenuStr: string;
      FCursor: TAnimSysFrame;

      FWeatherEngine: TWeatherSystem;
      FFontEngine: TAsphyreFonts;
      FDatabase: TLcfDataBase;
      FMapTree: TFullTree;
      FCurrentMBox: TMessageBox;
      FSystemTimer: TSystemTimer;
      FSystemTimer2: TSystemTimer;
      FSystemMenu: TGameMenu;
      FMenuEnabled: boolean;
      FEnterLock: boolean;
      FDontLockEnter: boolean;
      FCurrentParty: TCharSprite;
      FTransProc: TTransProc;
      FTransitionProgress: integer;
      FRenderProc: TRenderProc;
      FBlank: boolean;
      FInitialRender: boolean;

      FFadeColor: TRpgColor;
      FNegFadeColor: TRpgColor;
      FFadeTarget: TRpgColor;
      FNegFadeTarget: TRpgColor;
      FFadeTime: TRpgTimestamp;
      FGrayAlpha: byte;
      FGrayAlphaTarget: byte;
      FFlashColor: cardinal;
      FFlashTime: TRpgTimestamp;

      FShakePower: byte;
      FShakeSpeed: byte;
      FShakeCounter: byte;
      FShakeTime: integer;
      FShakeBias: shortint;
      FScreenLocked: boolean;
      FPanSpeed: double;
      FDisplacing: boolean;
      FReturning: boolean;
      FDestination: TPoint;
      FDisplX: double;
      FDisplY: double;
      FDisplacementX, FDisplacementY: double;
      FDisplacementSpeed: double;

      class var
      FLoadedImages: TStringList;
      FWallpapers: TObjectList;

      function getEvent(x: word): TAdditionSprite;
      procedure setEvent(x: word; data: TAdditionSprite);
      function getHeight: integer; inline;
      function getWidth: integer; inline;
      function getDisplacement: TRpgPoint; inline;
      function getFadeColor: cardinal;
      function getTile (layer: whichlayer; x,y: integer): TTile;
      procedure writeTile (layer: whichlayer; x,y: integer; const theTile: TTile);
      procedure transDrawSelf(sender: TObject);
      procedure transDrawProc(sender: TObject);
      procedure endTransition; inline;
      procedure fillScreen(const color: TRpgColor; drawFx: cardinal);
      procedure adjustCoords(var x, y: integer);
      function getCharacters: word;
   public
      constructor create (theMap: TMapUnit; theLDB: TLcfDataBase; theLMT: TFullTree;
                          canvas: TSdlCanvas; images: TSdlImages; const rtpLocation: string;
                          fontEngine: TAsphyreFonts); overload;
      destructor Destroy; override;
      procedure loadChipset(filename: string; chipList: TSdlImages);
      procedure loadBG(const filename, imageName: string);
      procedure loadCharset(const imagename, filename: string);
      procedure loadShopCharset(const filename: string);
      procedure loadRpgImage(filename: string; mask: boolean);
      procedure loadPortrait(filename: string);
      procedure loadAnim(const imagename: string);
      procedure moveTo(x,y: integer);
      procedure displaceTo(x,y: integer);
      procedure centerOn(x,y: integer);
      procedure setPanSpeed(speed: byte);
      procedure setDispSpeed(speed: byte);
      procedure returnFromDisplacement;
      procedure clearDisplacement;
      procedure Draw; reintroduce;
      procedure AdvanceFrame;
      procedure unlockEnter; inline;
      procedure endMessage;
      procedure button(const input: TButtonCode);
      procedure choiceBox(const msg: string; const acceptCancel: boolean);
      procedure inn(style: byte; cost: integer);
      procedure inputNumber(const digits: byte);
      procedure setSkin(const name: string);
      procedure fadeTo(r, g, b: smallint; time: integer);
      procedure fadeOut(time: integer);
      procedure fadeIn(time: integer);
      procedure flashScreen(r, g, b, a: byte; time: integer);
      procedure shakeScreen(power, speed: byte; duration: cardinal);
      procedure sleep;
      procedure wake;
      procedure beginTransition; inline;
      procedure endErase; inline;
      procedure endShow;
      procedure systemMenu(which: TSysMenuList);
      function messageBox(const input: string): boolean;
      function continueMessage: boolean;
      function overlapRect(const first, second: TRect): boolean;
      function passable(const x, y: integer; const direction: TFacing; character: TAdditionSprite): boolean; overload;
      function passable(const x, y: integer): boolean; overload; inline;
      function passable(const coords: TSgPoint; const direction: TFacing): boolean; overload;
      function passable(const x, y: integer; const direction: TFacing): boolean; overload;
      function edgeCheck(const x, y: integer; const direction: TFacing): boolean;
      function canExit(const x, y: integer; direction: TFacing; character: TAdditionSprite): boolean;
      function heroIn(const location: TMboxLocation): boolean;
      procedure process(sender: TObject);
      function tileInFrontOf(var location: TSgPoint; direction: TFacing): TLowerTile;
      function swapOutSpriteList(newList: TSpriteList): TSpriteList;
      procedure changeMaps(newmap: word; newLocation: TSgPoint);

      //alternate engine routines
      procedure adjustOwnership;
      procedure assignCurrentMap(map: TRpgMap);

      property tile [layer: whichlayer; x,y: integer]: TTile read getTile write writeTile; default;
      property map: TMapSet read FMapSet;
      property character[x: word]: TAdditionSprite read getEvent write setEvent;
      property characters: word read getCharacters;
      property currentMap: TRpgMap read FCurrMap;
      property height: integer read getHeight;
      property width: integer read getWidth;
      property animFrame: byte read FAnimStage;
      property event[x: word]: TAdditionSprite read getEvent write setEvent;
      property state: TGameState read FState;
      property scriptEngine: TScriptEngine read FScriptEngine;
      property database: TLcfDataBase read FDatabase;
      property mapTree: TFullTree read FMapTree;
      property currentMBox: TMessageBox read FCurrentMBox write FCurrentMBox;
      property menuInt: integer read FMenuInt write FMenuInt;
      property menuStr: string read FMenuStr write FMenuStr;
      property timer: TSystemTimer read FSystemTimer;
      property timer2: TSystemTimer read FSystemTimer2 write FSystemTimer2;
      property systemTimer: TSystemTimer read FSystemTimer write FSystemTimer;
      property systemGraphic: TSystemImages read FSystemGraphic write FSystemGraphic;
      property cursor: TAnimSysFrame read FCursor;
      property menu: TGameMenu read FSystemMenu;
      property menuEnabled: boolean read FMenuEnabled write FMenuEnabled;
      property fontEngine: TAsphyreFonts read FFontEngine;
      property currentParty: TCharSprite read FCurrentParty write FCurrentParty;
      property transProc: TTransProc read FTransProc write FTransProc;
      property renderProc: TRenderProc read FRenderProc write FRenderProc;
      property blank: boolean read FBlank write FBlank;
      property initialRender: boolean read FInitialRender write FInitialRender;
      property fadeColor: cardinal read getFadeColor;
      property grayAlpha: byte read FGrayAlpha write FGrayAlphaTarget;
      property screenLocked: boolean read FScreenLocked write FScreenLocked;
      property displacing: boolean read FDisplacing;
      property displacement: TRpgPoint read getDisplacement;
      property returning: boolean read FReturning write FReturning;
      property weatherEngine: TWeatherSystem read FWeatherEngine;
      property cutscene: byte read FCutscene write FCutscene;
      property enterLock: boolean read FEnterLock write FEnterLock;
      property noEnterLock: boolean write FDontLockEnter;

      class property loadedImages: TStringList read FLoadedImages;
   end;

var
   GFileLoader: TCriticalSection;
   GDataArchive: TASDb;
   GGameEngine: TGameMap;
   GFrameLength: word;

const
   BASESPEED = 3.8;
   MOVESPEED: array[1..6] of real = (BASESPEED / 8, BASESPEED / 4, BASESPEED / 2, BASESPEED, BASESPEED * 2, BASESPEED * 4);
   TILESIZE = 16;

implementation
uses
   windows, forms, graphics, sysUtils, math, //windows libs
   console, png_routines, locate_files, menu_basis, transition_graphics,
   transitions, preloader, fileIO, //turbu libs
   tempfile, pngimage,
   SDL;

const
   SHAKE_MAX = 23;
var
   Li: integer;
   LSineTable: array[0..SHAKE_MAX - 1] of extended;

{ TGameMap }

procedure TGameMap.beginTransition;
begin
   FSavedState := FState;
   FState := gs_fading;
   FBlank := false;
end;

procedure TGameMap.button(const input: TButtonCode);
begin
   if FEnterLock and (input in [btn_enter, btn_cancel]) then
      Exit;
   case FState of
      on_map:
         if FCutscene > 0 then
            Exit
         else if (input = btn_cancel) and FMenuEnabled then
         begin
            if not FDontLockEnter then
               FEnterLock := true;
            frmConsole.newScript := TConsoleEventThread.Create(SCRIPT_HEADER + 'openMenu;' + SCRIPT_FOOTER);
            FScriptEngine.registerConsoleThread(frmConsole.newScript);
            FScriptEngine.mediaPlayer.playSystemSound(sfxAccept);
         end
         else if assigned(FCurrentParty) then
         begin
            if input in [btn_up, btn_right, btn_down, btn_left] then
               GMoveLock.Enter;
            case input of
               btn_enter:
               begin
                  if not FDontLockEnter then
                     FEnterLock := true;
                  GEventLock.enter;
                  try
                     FCurrentParty.action;
                  finally
                     GEventLock.leave;
                  end;
               end;
               btn_up: FCurrentParty.move(facing_up);
               btn_down: FCurrentParty.move(facing_down);
               btn_left: FCurrentParty.move(facing_left);
               btn_right: FCurrentParty.move(facing_right);
            end;
            if input in [btn_up, btn_right, btn_down, btn_left] then
               GMoveLock.Leave;
         end;
      in_message: FCurrentMBox.button(input);
      in_menu: FSystemMenu.button(input);
      in_battle: ;
      gs_fading: ;
   end;
end;

procedure TGameMap.returnFromDisplacement;
begin
   FDisplX := -FDisplacementX;
   FDisplY := -FDisplacementY;
end;

function TGameMap.edgeCheck(const x, y: integer; const direction: TFacing): boolean;
begin
   result := true;
   case direction of
      facing_up: if y = 0 then result := false;
      facing_right: if x = currentMap.width then result := false;
      facing_down: if y = currentMap.height then result := false;
      facing_left: if x = 0 then result := false;
   end;
end;

function TGameMap.canExit(const x, y: integer; direction: TFacing; character: TAdditionSprite): boolean;
var
   opposite: TFacing;
begin
   result := false;
   opposite := opposite_facing(direction);

//you have to be able to leave the tile you're on
   if passable(x, y, direction, character) then
   begin

//check to see if you're moving off the edge of the map, and
//that the tile you're moving into will let you enter from
//that direction
      if edgeCheck(x, y, direction) then
         result := passable(character.inFront, opposite);
      //end if
   end;
end;

function TGameMap.messageBox(const input: string): boolean;
begin
   while FState = gs_fading do
      windows.sleep(GFrameLength);
   FCurrentMBox.Visible := true;
   FState := in_message;
   FCurrentMBox.text := input;
   FcurrentMbox.state := mb_display;
   result := FCurrentMBox.continuing;
end;

function TGameMap.continueMessage: boolean;
begin
   FCurrentMBox.continue;
   FCurrentMBox.Visible := true;
   FState := in_message;
   result := FCurrentMBox.continuing;
end;

procedure TGameMap.endMessage;
begin
   FEnterLock := true;
   FState := on_map;
   FCurrentMBox.visible := false;
end;

procedure TGameMap.endErase;
begin
   endTransition;
   FBlank := true;
end;

procedure TGameMap.endShow;
begin
   endTransition;
end;

procedure TGameMap.endTransition;
begin
   FState := FSavedState;
   GCurrentTarget := -1;
   FRenderProc := nil;
   FTransProc := nil;
end;

procedure TGameMap.fadeIn(time: integer);
begin
   fadeTo(0, 0, 0, time);
end;

procedure TGameMap.fadeOut(time: integer);
begin
   fadeTo(-255, -255, -255, time);
end;

procedure TGameMap.fadeTo(r, g, b: smallint; time: integer);
var
   pr, pg, pb, pa: byte;
   nr, ng, nb, na: byte;
begin
   pr := greaterOf(r, 0);
   pg := greaterOf(g, 0);
   pb := greaterOf(b, 0);
   pa := (pr + pg + pb) div 3;
   nr := abs(lesserOf(r, 0));
   ng := abs(lesserOf(g, 0));
   nb := abs(lesserOf(b, 0));
   na := (nr + ng + nb) div 3;
   FFadeTarget.color := cRGB1(pr, pg, pb, pa);
   FNegFadeTarget.color := cRGB1(nr, ng, nb, na);
   FFadeTime := TRpgTimestamp.Create(time);
end;

procedure TGameMap.flashScreen(r, g, b, a: byte; time: integer);
begin
   FFlashColor := cRGB1(r, g, b, a);
   FFlashTime := TRpgTimestamp.Create(time);
end;

procedure TGameMap.shakeScreen(power, speed: byte; duration: cardinal);
begin
   FShakePower := power;
   FShakeSpeed := speed;
   FShakeTime := duration;
end;

procedure TGameMap.changeMaps(newmap: word; newLocation: TSgPoint);
var
   hero: TAdditionSprite;
   oldmap: integer;
   dummy: word;
begin
   assert(assigned(GCurrentThread));
   if newmap = FCurrMap.mapID then
      Exit;

   FSwitchState := sw_ready;
   if not assigned(FMapSet[newmap]) then
   begin
      GPreloader.clear;
      GPreloader.push(TPreloaderStackFrame.Create(pl_map, '', newmap, nil));
   end;
   GCurrentEngine.killAll;
   GCurrentEngine.unregisterEvents(GCurrentThread);
   while not FBlank do
      windows.sleep(10);
   FSwitchState := sw_switching;
   if not assigned(FMapSet[newmap]) or (FMapSet[newmap] = preloader.PRELOADING) then
   begin
      GPreloader.haste;
      repeat
         windows.sleep(50);
      until assigned(FMapSet[newmap]) and (FMapSet[newmap] <> preloader.PRELOADING);
   end;
   hero := FCurrMap.event[0];
   if assigned(hero) then
      (hero as THeroSprite).packUp;
   FCurrMap.spriteList := self.swapOutSpriteList(FMapSet[newmap].spriteList);
   FCurrMap.clearOut;
   oldmap := FCurrMap.mapID;
   FCurrMap := FMapSet[newmap];
   FCurrMap.placeEvents;
   FCurrMap.event[0] := hero;
   if assigned(hero) then
   begin
      FCurrMap.event[0].location := newLocation;
      (hero as THeroSprite).settleDown;
   end;
   freeAndNil(FMapSet[oldmap]);
   centerOn(newLocation.x * TILESIZE, newLocation.y * TILESIZE);
   case FMapTree[newmap].bgmState of
      first:
      begin
         dummy := newmap;
         repeat
            dummy := FMapTree[dummy].parent;
         until (dummy = 0) or (FMapTree[dummy].bgmState <> first);
         if dummy = 0 then
            GCurrentEngine.mediaPlayer.stopMusic
         else if FMapTree[dummy].bgmState = third then
            GCurrentEngine.mediaPlayer.playMusic(FMapTree[dummy].bgmData);
      end;
      second:;
      third: GCurrentEngine.mediaPlayer.playMusic(FMapTree[newmap].bgmData);
   end;
   FSwitchState := sw_noSwitch;
   GCurrentEngine.resume;
end;

procedure TGameMap.choiceBox(const msg: string; const acceptCancel: boolean);
begin
   FCurrentMBox.Visible := true;
   FState := in_message;
   FCurrentMbox.state := mb_choice;
   FCurrentMBox.placeCursor(0);
   FCurrentMBox.canCancel := acceptCancel;
   FCurrentMBox.text := msg;
end;

procedure TGameMap.clearDisplacement;
begin
   inc(FDestination.X, commons.round(FDisplacementX));
   FDisplacementX := 0;
   inc(FDestination.Y, commons.round(FDisplacementY));
   FDisplacementY := 0;
   worldX := commons.round(worldX);
   worldY := commons.round(worldY);
   moveTo(trunc(FCurrentParty.baseTile.X), trunc(FCurrentParty.baseTile.Y));
end;

procedure TGameMap.inn(style: byte; cost: integer);
begin
   FCurrentMBox.Visible := true;
   FState := in_message;
   FCurrentMbox.state := mb_prompt;
   FCurrentMBox.canCancel := true;
   FMenuInt := cost;
   with GDatabase do
      FCurrentMBox.text := innVocab[style, inn_greet1] + ' ' + intToStr(cost)
                   + ' ' + vocabulary[moneyUnit] + ' ' + innVocab[style, inn_greet2]
                   + #3 + innVocab[style, inn_greet3] + #3 + innVocab[style, inn_stay]
                   + #3 + innVocab[style, inn_cancel];
   FCurrentMBox.placeCursor(2);
end;

procedure TGameMap.inputNumber(const digits: byte);
begin
   FCurrentMBox.Visible := true;
   FState := in_message;
   FCurrentMBox.state := mb_input;
   FCurrentMBox.placeCursor(0);
   FCurrentMBox.canCancel := false;
   FCurrentMBox.setupInput(digits);
end;

constructor TGameMap.create(theMap: TMapUnit; theLDB: TLcfDataBase; theLMT: TFullTree;
                            canvas: TSdlCanvas; images: TSdlImages; const rtpLocation: string;
                            fontEngine: TAsphyreFonts);
var
   sysName: string;
begin
   inherited create(nil);
   GGameEngine := self;
   GFileLoader := TCriticalSection.Create;
   GDatabase := theLDB;
   FMapTree := theLMT;
   setLength(FMapSet, theLMT.getMax + 1);
   FCurrMap := TRpgMap.Create(theMap, self);
   FMapSet[FCurrMap.mapID] := FCurrMap;
   FCurrMap.spriteList := FSpriteList;
   self.Images := images;
   FLoadedImages := TStringList.Create;
   FDatabase := theLDB;
   FLongAnim := not theLdb.getChipset(theMap.terrain).animation;
//if "animation" is TRUE then it's not a long animation
   FAnimStage := 1;
   FState := on_map;

   sysName := theLDB.SystemData.systemGraphic;
   FSystemGraphic := TSystemImages.Create(self, sysName, images);
   FWallpapers := TObjectList.Create;
   FWallpapers.Add(FSystemGraphic);
   FScriptEngine := TScriptEngine.create(self);
   FFontEngine := fontEngine;
   FFontEngine[0].Interleave := 1.5;
   FFontEngine[0].ShadowIntensity := 1.3;
   FFontEngine[1].Interleave := 1.5;
   FFontEngine[1].ShadowIntensity := 1.3;
   FCurrentMBox := TMessageBox.Create(self, rect(0, 160, 320, 80));
   FCurrentMbox.Visible := false;
   FCurrentMBox.boxVisible := true;
   FCurrentMBox.position := mb_bottom;
   FFadeColor.color := 0;
   FNegFadeColor.color := 0;
   FCursor := TAnimSysFrame.Create(self, FRAME_DISPLACEMENT, 2);
   FSystemMenu := TGameMenu.Create(self);
   FMenuEnabled := true;
   FDisplacementSpeed := BASESPEED;
   FPanSpeed := BASESPEED;
   FWeatherEngine := TWeatherSystem.Create(images);
   prepareRoute('', false);
end;

destructor TGameMap.Destroy;
begin
   FCurrMap.free;
   FScriptEngine.Free;
   FCurrentMBox.Free;
   FSystemMenu.free;
   FCursor.free;
   FWeatherEngine.free;
   freeAndNil(GFileLoader);
   freeAndNil(FWallpapers);
   freeAndNil(FLoadedImages);
   inherited Destroy;
end;

procedure TGameMap.loadAnim(const imagename: string);
var
   theImage: TPngObject;
   texSize: TPoint;
   bgcolor: cardinal;
   dummy: boolean;
   filename: string;
const
   ANIM_BLOCK: TPoint = (x: 96; y: 96);
begin
   filename := imagename;
   locate_files.findGraphic(filename, 'battle');
   GXyzHack := false;
   if filename = '' then
      raise EParseMessage.create('Image file "' + imageName + '" not found!');
   if FLoadedImages.IndexOf(filename) <> -1 then
      Exit;

   theImage := TPNGObject.Create;
   try
      theImage.LoadFromFile(filename);
      texSize := point(theImage.Width, theImage.Height);
      bgcolor := getPaletteColor(theImage, 0, dummy);
   finally
      theImage.Free;
   end;
   texSize := point(ceilPowerOfTwo(texSize.x), ceilPowerOfTwo(texSize.y));
   GFileLoader.Enter;
   try
//fixme
{      assert(image.AddFromFile(filename, ANIM_BLOCK, ANIM_BLOCK, texSize, aqHigh, alMask, true, bgColor, 0));}
      images[images.Count - 1].Name := 'Anim ' + imagename;
      FLoadedImages.Add(filename);
   finally
      GFileLoader.Leave;
   end;
end;

procedure TGameMap.loadBG(const filename, imageName: string);
var
   bigsize: tpoint;
   realsize: tpoint;
   checkmap: TPngObject;
begin
   GXyzHack := false;
   if FLoadedImages.IndexOf(filename) <> -1 then
      Exit;

   GFileLoader.Enter;
   checkmap := TPngObject.Create;   
try
   //get the BG image's size, since Asphyre wants it up-front
   checkmap.LoadFromFile(filename);
   bigsize.X := CeilPowerOfTwo(checkmap.Width);
   bigsize.Y := CeilPowerOfTwo(checkmap.Height);
   realsize.X := checkmap.Width;
   realsize.Y := checkmap.Height;

//fixme
{   Image.AddFromFile(filename, realsize, realsize, bigsize, aqHigh, alNone, false, 0, 0);}
   assert (Images[Images.count - 1].TexPerRow = 1);
   assert (Images[Images.count - 1].TexRows = 1);
   Images[Images.count - 1].Name := 'Background ' + imageName;

   FLoadedImages.Add(filename);
finally
   GFileLoader.Leave;
   checkmap.Free;
end;
end;

procedure TGameMap.loadCharset(const imagename, filename: string);
var
   bgColor: Tcolor;
   theImage: TPngObject;
begin
   if FLoadedImages.IndexOf(filename) <> -1 then
   begin
      GXyzHack := false;
      Exit;
   end;

   GFileLoader.Enter;
try
   if GXyzHack = true then
   begin
      GXyzHack := false;
      theImage := TPngObject.Create;
      theImage.LoadFromFile(filename);
      bgColor := theImage.Pixels[0, 0];
      theImage.Free;
   end else bgColor := getBGColor(filename);
   //load files
//fixme
{   image.AddFromFile(filename, SPRITE, SPRITE, SPRITE_SET, aqHigh, alMask, true, bgColor, 1);}
   images[images.Count - 1].Name := 'Charset ' + imagename;

   FLoadedImages.Add(filename);
finally
   GFileLoader.leave;
end;
end;

procedure TGameMap.loadChipset(filename: string; chipList: TSdlImages);
var
   bgColor: Tcolor;
   fileList: TThirteenTemps;
   theImage: TPngObject;
   i, currentSize: byte;
   tempFilename: string;
   imagename: string;
begin
   imagename := filename;
   findGraphic(filename, 'chipset');
   if filename = '' then
      raise EParseMessage.create('Chipset graphic file "' + fileName + '" not found!');
   if FLoadedImages.IndexOf(filename) <> -1 then
      Exit;

   GFileLoader.enter;
   try
      chiplist.AddFromFile(filename, imagename);
      FLoadedImages.Add(filename);
   finally
      GFileLoader.Leave;
   end;
end;

procedure TGameMap.loadPortrait(filename: string);
const
   PORTRAIT: TPoint = (X: 48; Y: 48);
   //FACESET: TPoint = (X: 256; Y: 256);
   FACESET: TPoint = (X: 192; Y: 192);
var
   facesetName: string;
   index: word;
begin
   facesetName := filename;
   findGraphic(facesetName, 'faceset');
   GXyzHack := false;
   if facesetName = '' then
      raise EParseMessage.create('FaceSet graphic file "' + filename + '" not found!');
   if FLoadedImages.IndexOf(facesetName) <> -1 then
      Exit;

   GFileLoader.Enter;
try
//fixme
{   image.AddFromFile(facesetName, PORTRAIT, PORTRAIT, FACESET, aqHigh, alNone, false, 0, 0);}
   index := images.Count - 1;
   assert(images[index].texPerRow = 4);
   assert(images[index].texRows = 4);
   images[index].Name := 'portrait ' + filename;
   FLoadedImages.Add(facesetName);
finally
   GFileLoader.Leave;
end;
end;

procedure TGameMap.loadRpgImage(filename: string; mask: boolean);
var
   theImage: TPngObject;
   imageSize, texSize: TPoint;
   bgcolor: cardinal;
   dummy: boolean;
   alpha: TALphaLevel;
   inName, codeName: string;
begin
   inName := filename;
   if mask then
      codeName := filename + 'M'
   else codeName := filename + 'NM';
   if FLoadedImages.IndexOf(codeName) <> -1 then
      Exit;
   locate_files.findGraphic(filename, 'picture');
   GXyzHack := false;
   if filename = '' then
      raise EParseMessage.create('Image file "' + inName + '" not found!');

   GFileLoader.Enter;
   try
      theImage := TPNGObject.Create;
      try
         theImage.LoadFromFile(filename);
         imageSize := point(theImage.Width, theImage.Height);
         bgcolor := getPaletteColor(theImage, 0, dummy);
      finally
         theImage.Free;
      end;
      texSize := point(ceilPowerOfTwo(imageSize.x), ceilPowerOfTwo(imageSize.y));
      if mask then
         alpha := alMask
      else alpha := alNone;
//fixme
{      assert(image.AddFromFile(filename, imageSize, imageSize, texSize, aqHigh, alpha, mask, bgColor, 0));}
      images[images.Count - 1].Name := codename;
      FLoadedImages.Add(codeName);
   finally
      GFileLoader.Leave;
   end;
end;

procedure TGameMap.loadShopCharset(const filename: string);
const
   CHAR: TPoint = (x: 24; y: 32);
   SHOPSET: TPoint = (x: 1024; y: 32);
var
   theImage: TPngObject;
   temp1, temp2: TTempFileStream;
   tempImage: TPngObject;
   imagename, charsetName, tempFilename, tempFileName2: string;
   dummy: boolean;
   bgcolor: cardinal;
   i: byte;
   x, y: word;
begin
   charsetName := filename;
   imageName := filename;
   findGraphic(charsetName, 'charset');
   if charsetName = '' then
   begin
      GXyzHack := false;
      raise EParseMessage.create('CharSet graphic file "' + filename + '" not found!');
   end;
   if FLoadedImages.indexOf('shop ' + imagename) <> -1 then
   begin
      GXyzHack := false;
      Exit;
   end;

   GFileLoader.Enter;
try
   theImage := TPNGObject.Create;
   theImage.LoadFromFile(charsetName);
   if GXyzHack = true then
   begin
      GXyzHack := false;
      theImage := TPngObject.Create;
      theImage.LoadFromFile(filename);
      bgColor := theImage.Pixels[0, 0];
      theImage.Free;
   end else bgcolor := getPaletteColor(theImage, 0, dummy);
   temp1 := getPNGRect(theImage, rect(0, 64, 288, 32));
   tempFilename := temp1.FileName;
   temp1.Free;
   temp1 := getPNGRect(theImage, rect(0, 192, 288, 32));
   tempFilename2 := temp1.FileName;
   temp1.Free;
   temp1 := stitchPNGFiles(tempFilename, tempFileName2);
   DeleteFile(PAnsiChar(tempFilename));
   DeleteFile(PAnsiChar(tempFilename2));
   tempFilename := temp1.FileName;
   temp1.Destroy;
   for I := 0 to 7 do
   begin
      x := 24 + (72 * (i mod 4));
      y := 64 + (128 * (i div 4));
      temp1 := getPNGRect(theImage, rect(x, y, CHAR.X, CHAR.Y));
      tempImage := TPNGObject.Create;
      try
         temp1.Seek(0, 0);
         tempImage.LoadFromStream(temp1);
         png_routines.grayscale(tempImage);
         temp1.Seek(0, 0);
         tempImage.SaveToStream(temp1);
      finally
         tempImage.Free;
      end;
      tempFileName2 := temp1.FileName;
      temp1.Free;
      temp2 := stitchPNGFiles(tempFilename, tempFilename2);
      DeleteFile(PAnsiChar(tempFilename));
      DeleteFile(PAnsiChar(tempFilename2));
      tempFilename := temp2.FileName;
      temp2.Destroy;
   end;
//fixme
{   self.image.AddFromFile(tempFileName, CHAR, CHAR, SHOPSET, aqHigh, alMask, true, bgColor, 1);}
   images[images.Count - 1].Name := 'Shop ' + imagename;
   DeleteFile(PAnsiChar(tempFilename));
   FLoadedImages.Add('shop ' + imagename);
finally
   GFileLoader.Leave;
   theImage.free;
end;
end;

procedure TGameMap.adjustCoords(var x, y: integer);
var
   halfwidth, halfheight, maxwidth, maxheight: integer;
begin
   halfwidth := lesserOf(round(canvas.width / 2), (width + 1) * 8);
   halfheight := lesserOf(round(canvas.height / 2), (height + 1) * 8);
   maxwidth := ((width + 1) * TILESIZE) - halfwidth;
   maxheight := ((height + 1) * TILESIZE) - halfheight;
   if x < halfwidth then
      x := halfwidth;
   if y < halfheight then
      y := halfheight;
   if x > maxwidth then
      x := maxwidth;
   if y > maxheight then
      y := maxheight;

   dec(x, halfwidth);
   dec(x, x mod TILESIZE);
   dec(y, halfheight);
   dec(y, y mod TILESIZE);
end;

procedure TGameMap.moveTo(x, y: integer);
begin
   adjustCoords(x, y);
   FDestination.X := x;
   FDestination.Y := y;
end;

procedure TGameMap.displaceTo(x, y: integer);
begin
   adjustCoords(x, y);
   FDisplX := FDisplX + x - worldX;
   FDisplY := FDisplY + y - worldY;
end;

procedure TGameMap.centerOn(x, y: integer);
begin
   adjustCoords(x, y);
   FDestination.X := x;
   FDestination.Y := y;
   worldX := x;
   worldY := y;
end;

function TGameMap.overlapRect(const first, second: TRect): boolean;
begin
   result := PointInRect(first.TopLeft, second) or PointInRect(first.BottomRight, second)
          or PointInRect(second.TopLeft, first) or PointInRect(second.BottomRight, first);
end;

function TGameMap.passable(const x, y: integer; const direction: TFacing): boolean;
begin
   result := FCurrMap.canPass(x, y, direction) and (FCurrMap.blocked(x,y) = nil);
end;

function TGameMap.passable(const x, y: integer; const direction: TFacing; character: TAdditionSprite): boolean;
begin
   result := FCurrMap.canPass(x, y, direction) and (FCurrMap.blocked(x,y) <> character);
end;

function TGameMap.passable(const x, y: integer): boolean;
begin
   result := passable(x, y, facing_down) or passable(x, y, facing_up) or passable(x, y, facing_left) or passable(x, y, facing_right);
end;

function TGameMap.passable(const coords: TSgPoint; const direction: TFacing): boolean;
begin
   result := passable(coords.x, coords.y, direction);
end;

procedure TGameMap.setDispSpeed(speed: byte);
begin
   if speed in [low(MOVESPEED)..high(MOVESPEED)] then
      FDisplacementSpeed := MOVESPEED[speed];
end;

procedure TGameMap.setEvent(x: word; data: TAdditionSprite);
begin
   FCurrMap.event[x] := data;
end;

procedure TGameMap.setPanSpeed(speed: byte);
begin
   if speed in [low(MOVESPEED)..high(MOVESPEED)] then
      FPanSpeed := MOVESPEED[speed];
end;

procedure TGameMap.setSkin(const name: string);
var
   dummy: string;
   newPaper: TSystemImages;
   I: Integer;
begin
   if name = FSystemGraphic.filename then
      Exit;
   dummy := name;
   findGraphic(dummy, 'System');
   if dummy = '' then
      Exit;

   FSystemGraphic.unload;
   if FLoadedImages.IndexOf(name) = -1 then
   begin
      newPaper := TSystemImages.Create(self, name, Self.Images);
      FWallpapers.Add(newPaper);
      FSystemGraphic := newPaper;
   end
   else begin
      i := 0;
      while (i < FWallpapers.Count) and (TSystemImages(FWallpapers[i]).filename <> name) do
         inc(i);
      assert(i < FWallpapers.count);
      FSystemGraphic := TSystemImages(FWallpapers[i]);
      FSystemGraphic.reload;
   end;
end;

procedure TGameMap.sleep;
begin
   self.FState := sleeping;
end;

function TGameMap.swapOutSpriteList(newList: TSpriteList): TSpriteList;
begin
   result := FSpriteList;
   FSpriteList := newList;
end;

procedure TGameMap.systemMenu(which: TSysMenuList);
begin
   FState := sleeping;
   fadeOut(350);
   windows.sleep(350);
   case which of
      mnuMain: FSystemMenu.showMain;
      mnuShop: FSystemMenu.showShop;
      mnuName: FSystemMenu.showName;
   end;
   fadeIn(400);
   windows.Sleep(400);
   FState := in_menu;
   FSystemMenu.activate;
   repeat
      windows.sleep(32);
   until (FSystemMenu.state = ms_fading) or (application.terminated);
   FState := sleeping;
   fadeOut(350);
   windows.Sleep(350);
   FState := on_map;
   FSystemMenu.shutdown;
   fadeIn(400);
   windows.Sleep(400);
end;

procedure TGameMap.unlockEnter;
begin
   FEnterLock := false;
end;

procedure TGameMap.adjustOwnership;
var
   i: integer;
begin
   assert(self <> GGameEngine);
   for i := 0 to FSpriteList.Count - 1 do
      if FSpriteList[i] is TTile then
         FSpriteList[i].engine := GGameEngine
      else FSpriteList[i].engine := GGameEngine;
   //end FOR
end;

procedure TGameMap.AdvanceFrame;
begin
   if FState in [on_map, in_message] then
      inc(FAnimStage);
   if FAnimStage > 10 then //debugging
      FAnimStage := 0;     //code
   if FState = in_message then
      FCurrentMBox.tick
   else if FState = in_menu then
      GGameEngine.cursor.move(1);
end;

procedure TGameMap.assignCurrentMap(map: TRpgMap);
begin
   assert(self <> GGameEngine);
   FCurrMap := map;
end;

procedure TGameMap.fillScreen(const color: TRpgColor; drawFx: cardinal);
var
   easel: TRpgColor; //easel: an item for mixing colors
begin
   easel.color := 0;
   if color.rgba[1] > 0 then
   begin
      easel.rgba[4] := color.rgba[1];
      easel.rgba[1] := 255;
//fixme
//      canvas.FillRect(0, 0, Canvas.Device.Width, Canvas.Device.Height, easel.color, drawFx);
      easel.color := 0;
   end;
   if color.rgba[2] > 0 then
   begin
      easel.rgba[4] := color.rgba[2];
      easel.rgba[2] := 255;
//fixme
//      canvas.FillRect(0, 0, Canvas.Device.Width, Canvas.Device.Height, easel.color, drawFx);
      easel.color := 0;
   end;
   if color.rgba[3] > 0 then
   begin
      easel.rgba[4] := color.rgba[3];
      easel.rgba[3] := 255;
//fixme
//      canvas.FillRect(0, 0, Canvas.Device.Width, Canvas.Device.Height, easel.color, drawFx);
   end;
end;

{$Q-} {$R-} //warning. Serious Math Ahead. Hard hat recommended.
procedure TGameMap.process(sender: TObject);

   procedure doFade(var color: TRpgColor; const target: TRpgColor; const time: cardinal); inline;
   begin
      moveTowards(time, color.rgba[1], target.rgba[1]); //R transform
      moveTowards(time, color.rgba[2], target.rgba[2]); //G transform
      moveTowards(time, color.rgba[3], target.rgba[3]); //B transform
      moveTowards(time, color.rgba[4], target.rgba[4]); //A transform
   end;

const
   SHAKE_AMP = 1.8;
   SHAKE_PERIOD = 1.1;
var
   panned: boolean;
   diff: double;
   i: integer;
   time: cardinal;
   leftPoint: real;
begin
   TAnimTile.heartbeat; //to keep all animated tiles in synch
   self.dead;

   //place all characters
   FCurrMap.placeChars;
{Move/shake Map}
   leftPoint := worldX - FShakeBias;
   if FShakeTime > 0 then
   begin
      FShakeCounter := (FShakeCounter + FShakeSpeed) mod SHAKE_MAX;
      FShakeBias := round(FShakePower * LSineTable[FShakeCounter] * SHAKE_AMP);
      worldX := leftPoint + FShakeBias;
      i := greaterOf(round(FShakeTime/32), 1);
      dec(FShakeTime, FShakeTime div i);
   end else FShakeBias := 0;

   panned := false;
   if assigned(self.currentParty) then
   begin
      WorldX := FShakeBias + between(commons.round(currentParty.baseTile.X - 160), 0, ((width + 1) * TILESIZE) - self.Canvas.Width);
      WorldY := between(commons.round(currentParty.baseTile.Y - 120), 0, ((height + 1)  * TILESIZE) - self.Canvas.Height);
   end else begin
      if trunc(FDestination.x + FDisplacementX) <> trunc(self.worldX) then
      begin
         diff := min(abs(FDestination.x + FDisplacementX - worldX), FPanSpeed);
         if FDestination.x + FDisplacementX < self.worldX then
            diff := diff * -1;
         worldX := worldX + diff;
      end;
      if trunc(FDestination.y + FDisplacementY) <> trunc(self.worldY) then
      begin
         diff := min(abs(FDestination.Y + FDisplacementY - worldY), FPanSpeed);
         if FDestination.Y + FDisplacementY < worldY then
            diff := diff * -1;
         worldY := worldY + diff;
      end;
   end;
   if FDisplX <> 0 then
   begin
      diff := min(abs(FDisplX), FDisplacementSpeed);
      if FDisplX < 0 then
         diff := diff * -1;
      worldX := worldX + diff;
      FDisplX := FDisplX - diff;
      FDisplacementX := FDisplacementX + diff;
      panned := true;
   end;
   if FDisplY <> 0 then
   begin
      diff := min(abs(FDisplY), FDisplacementSpeed);
      if FDisplY < 0 then
         diff := diff * -1;
      worldY := worldY + diff;
      FDisplY := FDisplY - diff;
      FDisplacementY := FDisplacementY + diff;
      panned := true;
   end;
   FDisplacing := panned;
   if (not FDisplacing) and (FReturning) then
   begin
      FReturning := false;
      self.clearDisplacement;
   end;
{Fade map}
   if assigned(FFadeTime) then
   begin
      time := FFadeTime.timeRemaining;
      if FFadeColor.color <> FFadeTarget.color then
         doFade(FFadeColor, FFadeTarget, time);
      if FNegFadeColor.color <> FNegFadeTarget.color then
         doFade(FNegFadeColor, FNegFadeTarget, time);
      if FGrayAlphaTarget <> FGrayAlpha then
         moveTowards(time, FGrayAlpha, FGrayAlphaTarget);
      if time = 0 then
         freeAndNil(FFadeTime);
   end;
end;

procedure TGameMap.Draw;
var
   i: integer;
   fader: TRpgColor;
   worldRect, spriteRect: TRect;
   time: cardinal;
begin
   case FState of
      on_map, in_message:
      begin
         if FSwitchState = sw_switching then
            Exit;

         if (FLongAnim and (FAnimStage >= 5)) or
            (not FLongAnim and (FAnimStage >= 4)) then
            FAnimStage := 1;
{DRAW SCREEN}
{$REGION DRAWSCREEN}
         if FCurrMap.usesPano then
            FCurrMap.drawBG;
         worldRect := rect(trunc(worldX) - TILESIZE, trunc(worldY) - TILESIZE, trunc(worldX) + self.Canvas.Width + TILESIZE, trunc(worldY) + self.Canvas.height + TILESIZE);
         for i := 0 to FSpriteList.Count - 1 do
         begin
            with TSprite(FSpriteList[i]) do
               spriteRect := rect(Trunc(X), trunc(y), trunc(x) + 16, trunc(y) + 24);
            if (TSprite(FSpriteList[i]).Visible) and ((overlapRect(worldRect, spriteRect)) or TSprite(FSpriteList[i]).pinned) then
               TSprite(FSpriteList.Items[i]).Draw
         end;
         FWeatherEngine.Draw;
{$ENDREGION}

{SHADE SCREEN}
{$REGION SHADESCREEN}
         if FFadeColor.rgba[4] > 0 then
            self.fillScreen(FFadeColor, fxAdd);
         if FNegFadeColor.rgba[4] > 0 then
            self.fillScreen(FNegFadeColor, fxSub);
         fader.color := FFlashColor;
         if fader.rgba[4] > 0 then
         begin
//fixme
//            canvas.FillRect(0, 0, Canvas.Width, Canvas.Device.Height, fader.color, fxBlend);
            time := FFlashTime.timeRemaining;
            moveTowards(time, fader.rgba[4], 0);
            FFlashColor := fader.color;
         end;
         if (FSavedState = gs_fading) and (FFadeColor.color = FFadeTarget.color) and not FInitialRender then
            FBlank := true;
{$ENDREGION}

         if FState = in_message then
         begin
            FCurrentMbox.realign;
            FCurrentMBox.Draw;
         end;
      end;
      in_menu: FSystemMenu.draw;
      sleeping:
      begin
         if (FSystemMenu.state = ms_fading) then
            FSystemMenu.draw;
         //end IF
      end;
      in_battle: ;
      gs_fading:
      begin
         if assigned(FTransProc) then
         begin
            if GCurrentTarget = -1 then
            begin
               GCurrentTarget := 0;
               FTransitionProgress := 0;
            end;
            if FInitialRender then
            begin
               GRenderTargets.RenderOn(2, transDrawSelf, clBlack, false);
               FInitialRender := false;
            end else GRenderTargets.RenderOn(GCurrentTarget, self.transDrawProc, clBlack, false);
         end
         else transDrawSelf(self);
      end;
   end;
   if FSwitchState = sw_ready then
      FSwitchState := sw_switching;
end;
{$Q+} {$R+}

function TGameMap.tileInFrontOf(var location: TSgPoint; direction: TFacing): TLowerTile;
begin
   case direction of
      facing_up: location := point(location.x, location.y - 1);
      facing_right: location := point(location.x + 1, location.y);
      facing_down: location := point(location.x, location.y + 1);
      facing_left: location := point(location.x - 1, location.y);
   end;
   result := TLowerTile(self[lower, location.x, location.y]);
end;

procedure TGameMap.transDrawProc(sender: TObject);
begin
   FTransProc(FTransitionProgress);
end;

procedure TGameMap.transDrawSelf(sender: TObject);
begin
   FState := FSavedState;
   FSavedState := gs_fading;
   self.Draw;
   if not FBlank then
   begin
      FSavedState := FState;
      FState := gs_fading;
   end else FSavedState := FState;
end;

function TGameMap.getCharacters: word;
begin
   result := FCurrMap.characters;
end;

function TGameMap.getDisplacement: TRpgPoint;
begin
   result := point(commons.round(FDisplacementX), commons.round(FDisplacementY));
end;

function TGameMap.getEvent(x: word): TAdditionSprite;
begin
   result := FCurrMap.event[x];
end;

function TGameMap.getFadeColor: cardinal;
begin
   result := FFadeColor.color;
end;

function TGameMap.getTile(layer: whichlayer; x, y: integer): TTile;
begin
   result := FCurrMap[layer, x, y];
end;

function TGameMap.getHeight: integer;
begin
   result := FCurrMap.height;
end;

function TGameMap.getWidth: integer;
begin
   result := FCurrMap.width;
end;

function TGameMap.heroIn(const location: TMboxLocation): boolean;
var yPos: integer;
begin
   result := false;
   yPos := (FCurrentParty.location.Y * TILESIZE) - trunc(worldY);
   case location of
      mb_top: if yPos <= 96 then result := true;
      mb_middle: if (yPos <= 176) and (yPos > 80) then result := true;
      mb_bottom: if yPos > 160 then result := true;
   end;
end;

procedure TGameMap.wake;
begin
   FState := on_map;
end;

procedure TGameMap.writeTile(layer: whichlayer; x, y: integer; const theTile: TTile);
begin
   FCurrMap[layer,x,y] := theTile;
end;

initialization
begin
   for Li := 0 to SHAKE_MAX - 1 do
      LSineTable[Li] := sin(2 * pi * Li / SHAKE_MAX);
end;

end.
