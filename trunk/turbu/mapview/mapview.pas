unit mapview;
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
   Controls, Forms, Classes, contnrs, inifiles, //windows libs
   commons, console, LMU, LMT, LDB, chipset_graphics, script_engine, locate_files, //turbu libs
   SDL_ImageManager, SDL_canvas,
   {AsphyreImages, AsphyreCanvas, AsphyreDevices, AsphyreTimers, Asphyre2D,
   AsphyreFonts, AsphyreDb, AsphyreTextures, AsphyreSubsc,
   AsphyreKeyboard, AsphyreScreener, AsphyreImages,} //asphyre libs
   uPSCompiler; //pascalScript

type
   TInputThread = class(TRpgThread)
   private
      FScanVal: TSet16;
      FClear: boolean;
   public
      constructor Create;
      procedure Execute; override;

      property clear: boolean read FClear;
      property scan: TSet16 read FScanVal;
   end;

   TfrmGameForm = class(TForm)
      procedure FormDestroy(Sender: TObject);
      procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      procedure deviceRender(Sender: TObject);
      procedure standardRender(Sender: TObject);
      procedure timerTimer(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure FormCreate(Sender: TObject);
      procedure FormShow(Sender: TObject);
   private
      { Private declarations }
      FImages: TSdlImages;
      FCanvas: TSdlCanvas;
   public
      { Public declarations }
   end;

function init(filename: string): boolean;

procedure initializeHero;

var
   frmGameForm: TfrmGameForm;
   theMap: TFileStream = nil;
   mapTree: TFullTree;
   ldbData: TLcfDataBase;
   currentMap: TMapUnit;
   mapMax: word;
   chipsetMax: byte;
   mapID: word;
   mapEngine: TGameMap;
   frames: byte;
   heartbeat: byte;
   GInputReader: TInputThread;
   GInitializedHero: boolean;

implementation

uses
   Windows, strUtils, sysUtils, dialogs, math, {DirectInput,} //windows libs
   fileIO, chipset_data, chipset, charset_graphics, charset_data,
   rm_sound, tiles, rs_system, transitions, transition_graphics, {png_routines,}
   {preloader,} formats, //turbu libs
   openformThrowaway, //other window
   {asphyreDef, //asphyre lib} SG_defs,
   SDL; //SDL library

{$R *.dfm}
function init(filename: string): boolean;
var
   dummy: string;
   theLMT, theLDB: TFileStream;
   i, j: integer;
begin
result := false;
theLMT := nil;
theLDB := nil;
try
try
try
   GCurrentFolder := GetCurrentDir;
   theLMT := commons.openFile(GCurrentFolder + '\RPG_RT.lmt');
   theLDB := commons.openFile(GCurrentFolder + '\RPG_RT.ldb');
   if getString(theLMT) <> 'LcfMapTree' then
      raise EParseMessage.create('Error! RPG_RT.LMT file header is missing');
   mapTree := TFullTree.Create(theLMT);
   mapMax := mapTree.getMax;
   if getString(theLDB) <> 'LcfDataBase' then
      raise EParseMessage.create('Error! RPG_RT.LDB file header is missing');
   GProjectFormat := scanRmFormat(theLDB);
   case GProjectFormat of
      pf_2k: rtpLocation := GetRegistryValue('\Software\ASCII\RPG2000', 'RuntimePackagePath');
      pf_2k3: rtpLocation := GetRegistryValue('\Software\Enterbrain\RPG2003', 'RUNTIMEPACKAGEPATH');
   end;
   ldbData := TLcfDataBase.Create(theLDB);
   chipsetMax := ldbData.getMaxChipsets;
//   GDataArchive := frmGameForm.fontDB;
   if ExtractFileExt(filename) = '.lmt' then
   begin
      GInitializedHero := true;
      i := 0;
      j := mapTree.heroStartMap;
      repeat
         j := j div 10;
         inc(i)
      until j = 0;
      filename := GCurrentFolder + '/map';
      for j := 1 to 4 - i do
         filename := filename + '0';
      filename := filename + intToStr(mapTree.heroStartMap) + '.lmu';
   end;
   theMap := commons.openFile(FileName);
   if getString(theMap) <> 'LcfMapUnit' then
      raise EParseMessage.create('Error! LMU file header is missing');
   dummy := FileName;
   delete(dummy, LastDelimiter('.', FileName), 4);
   mapID := StrToInt(AnsiRightStr(dummy, 4));
   result := true;
except
   on E: EParseMessage do
   begin
      msgBox(E.Message, 'TopenForm.FormShow says:', MB_OK);
      raise EMessageAbort.Create
   end
end; // end of TRY block
except
   on EMessageAbort do
   begin
      ShowMessage('Aborting due to parse error.');
      application.Terminate
   end
end // end of second TRY block
finally
   theLMT.Free;
   theLDB.Free;
end;
end;

{$R-}
procedure TfrmGameForm.timerTimer(Sender: TObject);
var dummy: boolean;
begin
   inc(frames);
//   GFrameLength := max(commons.round(timer.latency), 1);
   if assigned(mapEngine.transProc) then
      mapEngine.Draw
   else if mapEngine.blank then
      //do nothing
   else begin
      GRenderTargets.RenderOn(2, standardRender, 0, true);
   end;
//   dummy := device.Render(0, true);
//   timer.Process;
   if dummy then
      screenCanvas.Flip;
   if frames > heartbeat then
   begin
      mapEngine.advanceFrame;
      frames := 0;
   end;
   GScriptEngine.eventTick;
   mapEngine.scriptEngine.timer.tick;
end;
{$R+}

procedure TfrmGameForm.standardRender(Sender: TObject);
begin
   mapEngine.draw;
end;

procedure TfrmGameForm.deviceRender(Sender: TObject);
begin
   if assigned(mapEngine.renderProc) then
      mapEngine.renderProc
   else if mapEngine.blank then
//fixme
//      canvas.FillRect(rect(0, 0, device.Width, device.Height), clBlack4, fxNone)
   else begin
//fixme
//      canvas.TexMap(GRenderTargets[2], pBounds4(0, 0, device.Width, device.Height), clWhite4, tcNull, fxNone);

//fixme
{      if mapEngine.grayAlpha > 0 then
      begin
         outputDebugString(pChar('Rendering grayscale with alpha of ' + intToStr(mapEngine.grayAlpha)));
         canvas.TexMap(GRenderTargets[2], pBounds4(0, 0, device.Width, device.Height), cAlpha4(mapEngine.grayAlpha), tcNull, fxGrayScale);
      end;}
   end;
end;

procedure TfrmGameForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   application.terminate;
end;

procedure TfrmGameForm.FormCreate(Sender: TObject);
begin
   frames := 0;
try
try
{   if not device.initialize then
      raise EParseMessage.create('Asphyre Device failed to initialize.');}
//   fontDB.FileName := ExtractFilePath(ParamStr(0)) + DIRMARK + fontDB.FileName;
   GInputReader := TInputThread.Create;
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TGameForm.FormShow says:', MB_OK);
      raise EMessageAbort.Create
   end
end; // end of TRY block
except
   on EMessageAbort do
   begin
      ShowMessage('Aborting due to fatal error.');
      application.Terminate
   end
end // end of second TRY block
end;

procedure TfrmGameForm.FormDestroy(Sender: TObject);
begin
   GInputReader.Free;
end;

procedure TfrmGameForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   multiplierX, multiplierY: single;
   whichTile: TPoint;
begin
   multiplierX := (clientWidth / ScreenCanvas.Width);
   multiplierY := (clientHeight / ScreenCanvas.Height);
   x := round(x / multiplierX);
   y := round(y / multiplierY);
   inc (x, round(mapEngine.WorldX));
   inc (y, round(mapEngine.worldY));
   mapEngine.moveTo(x, y);

   if button = mbRight then
   begin
      whichTile := point(X div TILESIZE, Y div TILESIZE);
      if mapEngine.passable(whichTile.x, whichTile.y) then
      begin
         if mapEngine.currentParty = nil then
            initializeHero
         else mapEngine.currentParty.leaveTile;
         mapEngine.currentParty.location := whichtile;
      end;
   end;
end;

procedure TfrmGameForm.FormShow(Sender: TObject);
var
   fileName: string;
   i, j: smallint;
   tileIndex: integer;
   currentChipset: TChipSet;
   bgm: TRmMusic;
begin
try
try
try
   self.Caption := mapTree[0].name + ' - TURBU Map Viewer';
   SDL_init(SDL_INIT_AUDIO);
   FImages := TSdlImages.Create;
   currentMap := TMapUnit.Create(theMap, ldbData, mapTree, mapID);
   FCanvas := TSdlCanvas.Create(cmHardware, false, rect(100, 100, 320, 240), 16);
   mapEngine := TGameMap.create(currentMap, ldbData, mapTree, FCanvas, FImages, rtpLocation);
   transitions.init;
   GRenderTargets := TSdlRenderTargets.Create;
//   GRenderTargets.AddRenderTargets(4, device.Width, device.Height, aqHigh, alMask);
   mapEngine.Images := FImages;
   currentMap.eventBlock.compiler := mapEngine.scriptEngine.compiler;
   GGlobalEvents.compiler := mapEngine.scriptEngine.compiler;
   GGlobalEvents.compileAll;
   if ldbData.getChipset(currentMap.terrain).hiSpeed then
      heartbeat := 6
   else heartbeat := 12;
   fileName := ldbData.getChipset(currentMap.terrain).filename;
   if filename <> '' then
      mapEngine.loadChipset(fileName, FImages);
//check for background image
   if (currentmap.usesPano) and (currentMap.panoName <> '') then
   begin
      filename := currentMap.panoName;
      findGraphic(filename, 'panorama');
      if filename = '' then
         raise EParseMessage.create('Panorama graphic file "' + fileName + '" not found!');
      mapEngine.loadBG(filename, currentMap.panoName);
      mapEngine.currentMap.setBG;
   end;
   for i := 0 to currentMap.eventCount - 1 do
   begin
      filename := currentMap.events[i].page[0].filename;
      if (currentMap.events[i].page[0].filename <> '') then
      begin
         findGraphic(filename, 'charset');
         if filename <> '' then
            mapEngine.loadCharset(currentMap.events[i].page[0].filename, filename);
      end;
   end;
   mapEngine.currentMap.placeEvents;
   for i := 1 to ldbData.heroes - 1 do
   begin
      filename := ldbData.hero[i].filename;
      if (ldbData.hero[i].filename <> '') and (FImages.Image[filename + intToStr(0)] = nil) then
      begin
         findGraphic(filename, 'charset');
         if filename <> '' then
            mapEngine.loadCharset(ldbData.hero[i].filename, filename);
      end;
   end;
   if GInitializedHero then
   begin
      if ldbData.SystemData.startingHeroes > 0 then
         mapEngine.character[0] := THeroSprite.create(mapEngine, GScriptEngine.hero[ldbData.SystemData.startingHero[1]], GParty)
      else mapEngine.character[0] := THeroSprite.create(mapEngine, nil, GParty);
      mapEngine.currentParty := mapEngine.character[0] as TCharSprite;
      for i := 1 to ldbData.SystemData.startingHeroes do
         rs_system.heroJoin(ldbData.SystemData.startingHero[i]);
   end;
   bgm := mapTree[mapID].bgmData;
   if bgm.filename <> '' then
      mapEngine.scriptEngine.playMusic(bgm);
   tileIndex := 0; //initial value
   currentChipset := ldbData.getChipset(currentMap.terrain);
   for j := 0 to currentMap.height - 1 do
   begin
      for i := 0 to currentMap.width - 1 do
      begin
         mapEngine[lower, i, j].place(i, j, lower, currentMap.lowChip[tileIndex], currentChipset);
         if currentMap.highChip[tileIndex] <> 10000 then
            TLowerTile(mapEngine[lower, i, j]).placeUpper(i, j, upper, currentMap.highChip[tileIndex], currentChipset);
         inc(tileIndex);
      end;
   end;
   mapEngine.currentMap.scanSquare;
{   timer.OnProcess := mapEngine.process;
   timer.Enabled := true;}
   GInputReader.Resume;
except
   on E: EParseMessage do
   begin
      msgBox(E.message, 'TGameForm.FormShow says:', MB_OK);
      raise EMessageAbort.Create
   end
end; // end of TRY block
except
   on EMessageAbort do
   begin
      ShowMessage('Aborting due to fatal error.');
      application.Terminate
   end
end // end of second TRY block
finally
   // anything need to be finalized?
end;
end;

procedure initializeHero;
begin
   mapEngine.character[0] := THeroSprite.create(mapEngine, GScriptEngine.hero[1], GParty);
   mapEngine.currentParty := mapEngine.character[0] as TCharSprite;
   rs_system.heroJoin(1);
end;

{ TInputThread }

constructor TInputThread.Create;
begin
//   frmGameForm.AsphyreKeyboard1.initialize;
   inherited Create(true);
end;

procedure TInputThread.Execute;
var
   buttonCode: TButtonCode;
   i: word;
   code: integer;
{const
   BUTTON_LIST: array[1..9] of integer = (DIK_DOWN, DIK_LEFT, DIK_RIGHT, DIK_UP, DIK_RETURN, DIK_NUMPADENTER, DIK_ESCAPE, DIK_INSERT, DIK_SPACE);}
begin
   inherited;
   GCurrentThread := self;
   FScanVal := [];
   repeat
      if GetForegroundWindow = frmGameForm.Handle then
{      with frmGameForm.AsphyreKeyboard1 do
      begin
         Update;
         code := -1;
         FClear := false;
         for I := 1 to high(BUTTON_LIST) do
         begin
            FScanVal.bits[i] := key[BUTTON_LIST[i]];
            if FScanVal.bits[i] then
               code := BUTTON_LIST[i];
         end;
         if code = -1 then
         begin
            FClear := true;
            mapEngine.unlockEnter;
            mapEngine.menu.buttonLock := false;
            sleep(12);
         end
         else begin
            buttonCode := TButtonCode(99);
            case code of
               DIK_UP: buttonCode := btn_up;
               DIK_DOWN: buttonCode := btn_down;
               DIK_LEFT: buttonCode := btn_left;
               DIK_RIGHT: buttonCode := btn_right;
               DIK_RETURN, DIK_NUMPADENTER: buttonCode := btn_enter;
               DIK_ESCAPE, DIK_INSERT: buttonCode := btn_cancel;
               DIK_SPACE: self.syncRun(frmConsole.Show);
            end;
            if ord(buttonCode) <> 99 then
               mapEngine.button(buttonCode);
            sleep(12);
         end;
      end
      else sleep(50);}
   until self.Terminated;
end;

initialization

finalization
begin
   theMap.free;
   mapTree.free;
   ldbData.free;
   currentMap.free;
   mapEngine.free;
   GThreadCleanLock.free;
end;
end.
