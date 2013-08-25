unit rm2_turbu_converter_thread;
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
   Types, classes, windows, Generics.Collections,
   commons, conversion_report, formats, turbu_map_metadata, archiveInterface,
   LDB, LMT, LMU, events;

type
   TConverterThread = class(TThread)
   private
      FReport: IConversionReport;
      FLdb: TLcfDataBase;
      FLmt: TFullTree;
      FFromLoc, FToLoc: string;
      FFormat: TProjectFormat;
      FMapSprites: TStringList;
      FPortraits: TStringList;
      FBackgrounds: TStringList;
      FBattleBackgrounds: TStringList;
      FSongs: TStringList;
      FSounds: TStringList;
      FSysTiles: TStringList;
      FPictures: TStringList;
      FMovies: TStringList;
      FMonsters: TStringList;
      FAnims: TStringList;
      FAnims2: TStringList;
      FBattleSprites: TStringList;
      FWeapons: TStringList;
      FFrames: TStringList;
      procedure VerifyFormat(datafile: TStream);
      procedure ConvertMap(const filename: string; database: TLcfDataBase; mapTree: TFullTree;
                     metadata: TMapTree; input, output: IArchive);
      procedure CopyResources(input, output: IArchive);
      procedure ScanCommands(list: TList<TEventCommand>);
      procedure ScanEventsForResources(block: TEventBlock);
      procedure ScanPage(page: TEventPage);
      procedure ScanMove(block: TEventMoveBlock);
      procedure ScanPageCommands(list: TEventCommandList);
      procedure setNewStep(value: string);
      procedure GatherResources;
      procedure UploadScriptCache;
   protected
      procedure Execute; override;
   public
      constructor Create(report: IConversionReport; fromLoc, toLoc: string; format: TProjectFormat);
      destructor Destroy; override;
   end;

var
   conversionArchive: integer;

implementation
uses
   SysUtils, StrUtils, DB,
   fileIO, discInterface, logs, locate_files, rm2_turbu_event_builder, dm_database,
   turbu_constants, turbu_database, turbu_engines, db_create, turbu_defs, EventBuilder,
   turbu_functional, turbu_maps, turbu_classes, turbu_pathing, turbu_tbi_lib,
   IOUtils,
   rm2_turbu_database, rm2_turbu_maps, rm2_turbu_map_metadata, rm2_turbu_map_objects,
   sdl, sdl_13, sdlStreams, sdl_image, sg_defs;

const
   CONVERSION_TASKS = 8;

type
   TImageProcessor = function (archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;

function convertSprite(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward; overload;
function convertSprite(name: string; id: integer; dirty: boolean): boolean; forward; overload;
function convertPortrait(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertBattleChar(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertWeapon(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertMusic(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertSFX(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertAnim(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertAnim2(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertMovies(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function ProcessImage(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;

{ TConverterThread }

procedure TConverterThread.ConvertMap(const filename: string;
  database: TLcfDataBase; mapTree: TFullTree; metadata: TMapTree; input,
  output: IArchive);

   function GetIdFromFilename: integer;
   var
      dummy: string;
   begin
      dummy := StringReplace(ExtractFileName(filename), 'map', '', [rfIgnoreCase]);
      result := StrToIntDef(stringReplace(dummy, '.lmu', '', [rfIgnoreCase]), -1);
   end;

var
   mapFile, outFile: TStream;
   map: TMapUnit;
   cMap: TRpgMap;
   id, dummy: integer;
begin
   map := nil;
   outFile := nil;
   cMap := nil;

   mapFile := input.getFile(filename);
   try
      if getString(mapFile) <> 'LcfMapUnit' then
         raise EParseMessage.createFmt('%s is corrupt!', [filename]);
      id := GetIdFromFilename;
	  //in case we have, for whatever reason, a map that's not in the map tree
      if (id = -1) or (not metadata.Containsmap(id)) then
      begin
         FReport.makeError(format('No entry for map %s exists in the map tree.', [filename]));
         Exit;
      end;
      map := TMapUnit.Create(mapFile, database, id);
      cMap := TRpgMap.Convert(map, mapTree.getMapData(id), database, id);
      outFile := TMemoryStream.Create;
      cMap.save(outFile);
      outFile.Seek(0, soFromBeginning);

      FBackgrounds.Add(cmap.bgName);
      FSongs.Add(metadata[id].bgmData.name);
      FBattleBackgrounds.Add(metadata[id].battleBgName);

      scanEventsForResources(map.eventBlock);

      dummy := 1;
      metadata[id].internalFilename := output.MakeValidFilename(format('%s.tmf', [cmap.name]), dummy);
      output.writeFile(format('maps\%s', [metadata[id].internalFilename.name]), outFile);
   finally
      mapFile.Free;
      map.Free;
      outFile.Free;
      cMap.Free;
   end;
end;

procedure TConverterThread.CopyResources(input, output: IArchive);
var
   rtpInput: IArchive;

   procedure processImageList(const folderName, outFolderName, stepname: string;
                              fileList: TStringList; processor: TImageProcessor;
                              rtpFunc: TLocateFileFunc = nil);
   const NOT_FOUND = 'File %s from %s not found!';
   var
      filename: string;
      lFilename: string;
      index: integer;
   begin
      if terminated then
         Exit;
      if not assigned(rtpFunc) then
         rtpFunc := locate_files.findGraphicF;
      if fileList.Find('', index) then
         filelist.Delete(index);
      if fileList.Find('(OFF)', index) then
         filelist.Delete(index);
      if stepname <> '' then
         FReport.newStep(stepname);
      try
         for filename in input.allFiles(folderName) do
         begin
            if terminated then
               Exit;
            lFilename := ChangeFileExt(ExtractFileName(filename), '');
            try
               if processor(input, outFolderName, format('%s\%s', [foldername, filename]), false) then
               begin
                  if fileList.Find(lFilename, index) then
                     fileList.Delete(index);
               end
               else FReport.makeError(format(NOT_FOUND, [filename, folderName]));
            except
               on E: EInvalidImage do
                  FReport.makeError(format('Unable to convert image %s\%s: %s', [filename, folderName, E.Message]));
            end;
         end;
      except
         on EDirectoryNotFoundException do ;
         //Project folder doesn't exist. Swallow the error and continue
      end;
      for filename in fileList do
      begin
         if terminated then
            Exit;
         lFilename := ExtractFilename(rtpFunc(filename, folderName));
         if processor(rtpInput, outFolderName, format('%s\%s', [folderName, lFilename]), true) then
            FReport.makeNotice(format('Copied RTP resource %s\%s.', [folderName, filename]))
         else FReport.makeError(format(NOT_FOUND, [filename, folderName]));
      end;
   end;

var
   specialsList: TStringList;
begin
   rtpInput := OpenFolder(locate_files.rtpLocation);
   processImageList('Backdrop', 'Battle BG', 'Battle backgrounds', FBattleBackgrounds, processImage);
   processImageList('Battle', 'Animation', 'Battle animations', FAnims, convertAnim);
   if GProjectFormat = pf_2k3 then
   begin
      processImageList('Battle2', 'Animation', '', FAnims2, convertAnim2);
      processImageList('BattleCharSet', 'BattleSprite', 'Battle Sprites', FBattleSprites, ConvertBattleChar);
      processImageList('BattleWeapon', 'BattleWeapon', '', FWeapons, ConvertWeapon);
      processImageList('Frame', 'Frame', 'Frames', FFrames, processImage);
   end;
   processImageList('CharSet', 'MapSprite', 'Character sprites', FMapSprites, convertSprite);
   processImageList('FaceSet', 'Portrait', 'Character portraits', FPortraits, convertPortrait);
   processImageList('Monster', 'Monsters', 'Monsters', FMonsters, processImage);
   processImageList('Music', 'Music', 'Music', FSongs, convertMusic, findSoundF);
   processImageList('Movie', 'Movies', 'Movies', FMovies, convertMovies, findMovieF);
   processImageList('Panorama', 'Backgrounds', 'Backgrounds', FBackgrounds, processImage);
   processImageList('Picture', 'Pictures', 'Images', FPictures, processImage);
   processImageList('Sound', 'SFX', 'Sound Effects', FSounds, convertSFX, findSoundF);
   processImageList('System', 'System', 'System layout', FSysTiles, processImage);
   specialsList := TStringList.Create;
   try
      specialsList.Add(string(FLdb.SystemData.titleScreen));
      processImageList('Title', 'Special Images', '', specialsList, processImage);

      specialsList.Clear;
      specialsList.Add(string(FLdb.SystemData.gameOverScreen));
      processImageList('GameOver', 'Special Images', '', specialsList, processImage);

      if GProjectFormat = pf_2k3 then
      begin
         specialsList.Clear;
         specialsList.Add(string(FLdb.systemData.battleSysGraphic));
         processImageList('System2', 'System2', '', specialsList, processImage);
      end;
   finally
      specialsList.Free;
   end;
   ProcessImage(GArchives[BASE_ARCHIVE], 'System\Glyphs', 'glyphs.png', false);
end;

constructor TConverterThread.Create(report: IConversionReport; fromLoc, toLoc: string; format: TProjectFormat);

   function CreateSortedStringList: TStringList;
   begin
      result := TStringList.Create;
      result.Sorted := true;
      result.Duplicates := dupIgnore;
   end;

begin
   inherited Create(true);
   FReport := report;
   self.FreeOnTerminate := true;
   FFromLoc := fromLoc;
   FToLoc := toLoc;
   FFormat := format;
   FBackgrounds := CreateSortedStringList;
   FBattleBackgrounds := CreateSortedStringList;
   FSongs := CreateSortedStringList;
   FMapSprites := CreateSortedStringList;
   FPortraits := CreateSortedStringList;
   FSounds := CreateSortedStringList;
   FSysTiles := CreateSortedStringList;
   FPictures :=  CreateSortedStringList;
   FMovies := CreateSortedStringList;
   FAnims := CreateSortedStringList;
   FAnims2 := CreateSortedStringList;
   FBattleSprites := CreateSortedStringList;
   FWeapons := CreateSortedStringList;
   FMonsters := CreateSortedStringList;
   FFrames := CreateSortedStringList;
end;

destructor TConverterThread.Destroy;
begin
   FFrames.Free;
   FMonsters.Free;
   FWeapons.Free;
   FBattleSprites.Free;
   FAnims.Free;
   FAnims2.Free;
   FMovies.Free;
   FPictures.Free;
   FSysTiles.Free;
   FSounds.Free;
   FMapSprites.Free;
   FPortraits.Free;
   FBackgrounds.Free;
   FBattleBackgrounds.Free;
   FSongs.Free;
   freeAndNil(FLdb);
   freeAndNil(FLmt);
  inherited;
end;

procedure TConverterThread.GatherResources;
var
  i: Integer;
  bgm: LDB.TBgmTypes;
  sfx: LDB.TSfxTypes;
begin
  //gathering resources
  for i := 1 to FLdb.terrains do
  begin
    FBattleBackgrounds.Add(string(FLdb.terrain[i].battleBg));
    if assigned(FLdb.terrain[i].soundEffect) then
      FSounds.Add(string(FLdb.terrain[i].soundEffect.filename));
    FFrames.Add(string(FLdb.terrain[i].frame));
  end;
  FBattleBackgrounds.Add(string(FLdb.systemData.editorBattleBG));
  FSysTiles.add(string(FLdb.SystemData.systemGraphic));
  for i := 1 to FLdb.monsters do
    FMonsters.Add(string(FLdb.monster[i].filename));
  FFrames.Add(string(FLdb.SystemData.frame));
  for bgm := Low(LDB.TBgmTypes) to High(LDB.TBgmTypes) do
    FSongs.Add(string(FLdb.SystemData.bgm[bgm].filename));
  for sfx := Low(LDB.TSfxTypes) to High(LDB.TSfxTypes) do
    FSounds.Add(string(FLdb.SystemData.sfx[sfx].filename));
end;

type
   TLegacySections = class(TDictionary<word, rawbytestring>);

{$WARN SYMBOL_PLATFORM OFF}
procedure TConverterThread.Execute;
const
	SECTIONS_I_KNOW_HOW_TO_READ = [$0b..$0e, $11..$15, $17..$18, $1e, $20];
var
   datafile: TStream;
   filename: string;
   fromFolder, toFolder: IArchive;
   legacy: TLegacySections;
   legacyPair: TPair<word, rawbytestring>;
   key: byte;
begin
   legacy := nil;
   try
      try
         fromFolder := discInterface.openFolder(FFromLoc);
         try
            conversionArchive := GArchives.Add(fromFolder);
            toFolder := discInterface.openFolder(FToLoc);
            datafile := fromFolder.getFile('RPG_RT.ldb');
            try
               if getString(datafile) <> 'LcfDataBase' then
                  raise EParseMessage.create('RPG_RT.LDB is corrupt!');
               VerifyFormat(datafile);

               FReport.tasks := CONVERSION_TASKS;
               FReport.setCurrentTask('Loading database');
               FLdb := TLcfDataBase.Create(datafile);
               //end of format and database check
               legacy := TLegacySections.Create;
               datafile.rewind;
               getString(datafile);
               while not datafile.eof do
               begin
                  key := peekAhead(datafile);
                  if not (key in SECTIONS_I_KNOW_HOW_TO_READ) then
                        legacy.Add(key, getStrSec(key, datafile, nil))
                  else skipSec(key, datafile);
               end;
            finally
               datafile.Free;
            end;

            FReport.setCurrentTask('Loading map tree');
            datafile := fromFolder.getFile('RPG_RT.lmt');
            try
               if getString(datafile) <> 'LcfMapTree' then
                  raise EParseMessage.create('RPG_RT.LMT is corrupt!');
               FLmt := TFullTree.Create(datafile);
               //done checking the map tree; now let's do some converting
            finally
               datafile.Free;
            end;

            FReport.setCurrentTask('Converting Database', 15);
            GatherResources;

            FreeAndNil(GDatabase);
            dmDatabase := TdmDatabase.Create(nil);
            try
               GDatabase := TRpgDatabase.convert(FLdb, FLmt, FReport, self.ScanPageCommands,
                                                 FMapSprites, FPortraits, FAnims, FAnims2,
                                                 FBattleSprites, FWeapons, FSounds);
               for legacyPair in legacy do
                  GDatabase.AddLegacy('database', 0, legacyPair.key, legacyPair.value);
            except
               on E: EMissingPlugin do
               begin
                  MsgBox(E.Message, 'Missing plugin');
                  GDatabase := nil;
                  Exit;
               end;
               else begin
                  GDatabase := nil;
                  raise;
               end;
            end;

            if terminated then
               Exit;
            FReport.setCurrentTask('Converting maps', fromFolder.countFiles('*.lmu'));
            for filename in fromFolder.allFiles('*.lmu') do
            begin
               FReport.newStep(filename);
               ConvertMap(filename, FLdb, FLmt, GDatabase.mapTree, fromFolder, toFolder);
            end;

            if terminated then
               Exit;
            FReport.setCurrentTask('Copying Resources', 12);
            if locate_files.rtpLocation = '' then
               FReport.makeNotice('RTP not found.  RTP resources will not be copied.')
            else self.copyResources(fromFolder, toFolder);

            if terminated then
               Exit;
            FReport.setCurrentTask('Saving database', ord(high(TRpgDataTypes)) + dmDatabase.tableCount + 3);
            FReport.newStep('Preparing schema');
            GDatabase.Filename := IncludeTrailingPathDelimiter(FToLoc) + DBNAME;
            filename := IncludeTrailingPathDelimiter(FToLoc) + 'turbu.tdb';
            db_create.ExtractDB(filename);
            dmDatabase.BuildDatabase(filename, GDatabase);
            GDatabase.copyToDB(dmDatabase, [], self.setNewStep);
            dmDatabase.SaveAll(self.setNewStep);
            UploadScriptCache;
            FReport.newStep('Finished');
         finally
            conversionArchive := -1
         end;

         FReport.makeReport;
      except
         FReport.fatal(TObject(AcquireExceptionObject) as Exception);
      end;
   finally
      legacy.Free;
      FreeAndNil(dmDatabase);
      FreeAndNil(GDatabase);
   end;
end;

procedure TConverterThread.setNewStep(value: string);
begin
   FReport.newStep(value);
end;

function Scanid(const maps, filename: string): integer;
var
   baseFilename: string;
   stream: TStream;
   id: word;
begin
   if filename = 'globalevents' then
      Exit(0);
   baseFilename := TPath.Combine(maps, filename)+ '.tmf';
   if not FileExists(baseFilename) then
      Exit(-1);

   stream := TFile.OpenRead(baseFilename);
   stream.Read(id, sizeof(word));
   stream.Free;
   result := id;
end;

type
   //UTF8 encoding with no BOM
   TUtf8EncodingN = class(TUtf8Encoding)
   public
      function GetPreamble: TBytes; override;
   end;

procedure LoadScriptCache(const path: string; const report: IConversionReport);
var
   filename, maps: string;
   filenames: TStringDynArray;
   id: integer;
   obj: TEBObject;
   ds: TDataset;
   encoding: TUtf8EncodingN;
begin
   ds := dmDatabase.script_cache;
   ds.Active := true;
   encoding := TUtf8EncodingN.Create;
   try
      maps := TPath.Combine(path, 'maps');
      filenames := TDirectory.GetFiles(TPath.Combine(path, 'scripts'));
      report.setCurrentTask('Preparing script cache', length(filenames));
      for filename in filenames do
      begin
         report.newStep(TPath.GetFileNameWithoutExtension(filename));
         id := ScanID(maps, TPath.GetFileNameWithoutExtension(filename));
         if id = -1 then
            Continue;
         obj := TEBObject.Load(TFile.ReadAllText(filename, encoding));
         try
            try
               ds.appendRecord([id, obj.GetScript(0)]);
            except
               on E: Exception do
                  report.makeError(format('%s: %s', [TPath.GetFileNameWithoutExtension(filename), E.Message]));
            end;
         finally
            obj.Free;
         end;
      end;
   finally
      encoding.Free;
   end;
end;

procedure TConverterThread.UploadScriptCache;
begin
   LoadScriptCache(FToLoc, FReport);
   FReport.setCurrentTask('Saving script cache');
   dmDatabase.script_cache.ApplyUpdates(0);
   dmDatabase.script_cache.Close;
end;

procedure TConverterThread.ScanCommands(list: TList<TEventCommand>);
var
   command: TEventCommand;
   name: string;
begin
   try
      for command in list do
      begin
         name := string(command.name);
         case command.opcode of
            10130, 10640: FPortraits.Add(name);
            10630, 10650: FMapSprites.Add(name);
            10660, 11510: FSongs.Add(name);
            10670, 11550: FSounds.Add(name);
            10680: FSysTiles.Add(name);
            10710: FBattleBackgrounds.Add(name);
            11110: FPictures.Add(name);
            11560: FMovies.Add(name);
            11720: FBackgrounds.Add(name);
            else assert(false);
         end;
      end;
   finally
      list.Free;
   end;
end;

procedure TConverterThread.ScanEventsForResources(block: TEventBlock);
var
   i, j: integer;
begin
   for I := 0 to block.len - 1 do
      for j := 0 to block.events[i].len - 1 do
         ScanPage(block.events[i].page[j]);
end;

procedure TConverterThread.ScanMove(block: TEventMoveBlock);
var
   path: turbu_pathing.TPath;
   step: TMoveStep;
begin
   path := turbu_pathing.TPath.convert(block);
   try
      for step in path.opcodes do
         if (step.opcode = MOVECODE_CHANGE_SPRITE) then
            FMapSprites.Add(copy(step.name, 1, length(step.name) - 2)) //trim off the frame at the end
         else if step.opcode = MOVECODE_PLAY_SFX then
            FSounds.Add(step.name);
   finally
      path.Free;
   end;
end;

procedure TConverterThread.ScanPageCommands(list: TEventCommandList);
const OPCODES: array [1..13] of integer =
   (10130, 10630, 10640, 10650, 10660, 10670, 10680, 10710, 11110, 11510, 11550,
    11560, 11720);
begin
   scanCommands(list.where(
      function(arg1: TEventCommand): boolean
      var
         dummy: integer;
      begin
         result := TArray.BinarySearch<integer>(OPCODES, arg1.opcode, dummy);
      end));
end;

procedure TConverterThread.ScanPage(page: TEventPage);
begin
   if page.filename <> '' then
      FMapSprites.Add(string(page.filename));
   if assigned(page.moveBlock.moveBlock) and (page.moveBlock.moveBlock.base <> '') then
      scanMove(page.moveBlock);//do something
   ScanPageCommands(page.commands);
end;

procedure TConverterThread.VerifyFormat(datafile: TStream);
begin
   GProjectFormat := scanRmFormat(datafile);
   FFormat := GProjectFormat;
   case FFormat of
      pf_2k: rtpLocation := GetRegistryValue('\Software\ASCII\RPG2000', 'RuntimePackagePath');
      pf_2k3: rtpLocation := GetRegistryValue('\Software\Enterbrain\RPG2003', 'RUNTIMEPACKAGEPATH');
      else assert(false);
   end;
end;

{ Classless }

procedure convertImage(image: PSdlSurface; id: integer; frame, sprite, sheet: TSgPoint; name, style: string; dirty: boolean = false);
var
   blitSurface: PSdlSurface;
   framesPerSprite: integer;
   startingPoint: TSgPoint;
   i: integer;
   srcrect, dstrect: TSDLRect;
   convertedImage: TStream;
   writename: string;
begin
   image.ColorKey := 0;
   framesPerSprite := sprite.X * sprite.Y;
   blitSurface := TSdlSurface.Create(frame.X, frame.Y * framesPerSprite, 8, 0, 0, 0, 0);
   try
      if (not assigned(image.format.palette)) or
         (not blitSurface.SetPalette(image.format.palette.colors, 0, image.format.palette.count)) then
         raise EInvalidImage.CreateFmt('Unable to convert sprite %s due to colorkey failure!', [name]);
      blitSurface.ColorKey := image.ColorKey;

      if id = -1 then
         startingPoint := ORIGIN
      else
         startingPoint := SgPoint((id mod sheet.X) * frame.X * sprite.X, (id div sheet.X) * frame.Y * sprite.Y);
      blitSurface.Fill(nil, blitSurface.ColorKey);
      for i := 0 to framesPerSprite - 1 do
      begin
         srcrect := rect(SgPoint(frame.X * (i mod sprite.X), frame.Y * (i div sprite.X)), frame);
         inc(srcrect.left, startingPoint.X);
         inc(srcrect.top, startingPoint.Y);
         dstrect := rect(SgPoint(0, i * frame.Y), frame);
         SDL_BlitSurface(image, @srcrect, blitSurface, @dstrect);
      end;
      convertedImage := saveToTBI(blitSurface, frame, dirty);
      convertedImage.Seek(0, soFromBeginning);
      try
         writename := style + '\' + ChangeFileExt(name, '');
         if id <> -1 then
            writename := writename + ' ' + intToStr(id);
         writename := writename + '.png';
         GArchives[IMAGE_ARCHIVE].writeFile(writename, convertedImage);
      finally
         convertedImage.free;
      end;
   finally
      blitSurface.free;
   end;
end;

function ConvertWholeImage(input: TStream; dirty: boolean): TStream;
var
   surface: PSdlSurface;
   rw: PSdl_RWops;
begin
   surface := nil;
   rw := SDLStreamSetup(input);
   try
      surface := PSdlSurface(IMG_Load_RW(rw, 0));
      if surface = nil then
         Exit(nil);
      result := saveToTBI(surface, SgPoint(surface.Width, surface.Height), dirty);
   finally
      SDLStreamCloseRWops(rw);
      surface.Free;
   end;
end;

function convertSprite(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; overload;
var i: integer;
begin
   result := true;
   for I := 0 to 7 do
      result := result and convertSprite(filename, i, dirty);
end;

function convertSprite(name: string; id: integer; dirty: boolean): boolean; overload;
const
   FRAME: TSgPoint = (X: 24; Y: 32);
   SPRITE: TSgPoint = (X: 3; Y: 4);
   SHEET: TSgPoint = (X: 4; Y: 2);
var
   oname: string;
   image: PSdlSurface;
begin
   name := extractFileName(name);
   oname := name;
   findGraphic(name, 'charset');
   if name = '' then
      Exit(false);
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(name))));
   if image = nil then
      Exit(false);
   try
      convertImage(image, id, FRAME, SPRITE, SHEET, oname, 'mapsprite', dirty);
   finally
      image.free;
   end;
   result := true;
end;

function convertPortrait(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
const
   FRAME: TSgPoint = (X: 48; Y: 48);
   SPRITE: TSgPoint = (X: 4; Y: 4);
   SHEET: TSgPoint = (X: 1; Y: 1);
var
   oname: string;
   image: PSdlSurface;
begin
   filename := extractFileName(filename);
   oname := filename;
   findGraphic(filename, 'faceset');
   if filename = '' then
      Exit(false);
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(filename))));
   if image = nil then
      Exit(false);
   try
      convertImage(image, -1, FRAME, SPRITE, SHEET, oname, 'portrait', dirty);
   finally
      image.free;
   end;
   result := true;
end;

function convertBattleChar(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
const
   FRAME: TSgPoint = (X: 48; Y: 48);
   SPRITE: TSgPoint = (X: 3; Y: 8);
   SHEET: TSgPoint = (X: 1; Y: 1);
var
   oname: string;
   image: PSdlSurface;
begin
   filename := extractFileName(filename);
   oname := filename;
   findGraphic(filename, 'BattleCharSet');
   if filename = '' then
      Exit(false);
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(filename))));
   if image = nil then
      Exit(false);
   try
      convertImage(image, -1, FRAME, SPRITE, SHEET, oname, outFolderName, dirty);
   finally
      image.free;
   end;
   result := true;
end;

function convertWeapon(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
const
   FRAME: TSgPoint = (X: 64; Y: 64);
   SPRITE: TSgPoint = (X: 3; Y: 8);
   SHEET: TSgPoint = (X: 1; Y: 1);
var
   oname: string;
   image: PSdlSurface;
begin
   filename := extractFileName(filename);
   oname := filename;
   findGraphic(filename, 'BattleWeapon');
   if filename = '' then
      Exit(false);
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(filename))));
   if image = nil then
      Exit(false);
   try
      convertImage(image, -1, FRAME, SPRITE, SHEET, oname, outFolderName, dirty);
   finally
      image.free;
   end;
   result := true;
end;

function convertMusic(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
var
   oname: string;
   stream: TStream;
begin
   filename := extractFileName(filename);
   oname := filename;
   findMusic(filename);
   if filename = '' then
      Exit(false);
   stream := TFileStream.Create(filename, fmOpenRead);
   try
      GArchives[MUSIC_ARCHIVE].writeFile(oname, stream);
   finally
      stream.Free;
   end;
   result := true;
end;

function convertMovies(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
var
   oname: string;
   stream: TStream;
begin
   filename := extractFileName(filename);
   oname := filename;
   findMovie(filename);
   if filename = '' then
      Exit(false);
   stream := TFileStream.Create(filename, fmOpenRead);
   try
      GArchives[VIDEO_ARCHIVE].writeFile(oname, stream);
   finally
      stream.Free;
   end;
   result := true;
end;

function convertSFX(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
var
   oname: string;
   stream: TStream;
begin
   filename := extractFileName(filename);
   oname := filename;
   findSfx(filename);
   if filename = '' then
      Exit(false);
   stream := TFileStream.Create(filename, fmOpenRead);
   try
      GArchives[SFX_ARCHIVE].writeFile(oname, stream);
   finally
      stream.Free;
   end;
   result := true;
end;

function convertAnim(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
const
   FRAME: TSgPoint = (X: 96; Y: 96);
   SHEET: TSgPoint = (X: 1; Y: 1);
var
   oname: string;
   image: PSdlSurface;
   sprite: TSgPoint;
begin
   filename := extractFileName(filename);
   oname := filename;
   findGraphic(filename, 'battle');
   if filename = '' then
      Exit(false);
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(filename))));
   if image = nil then
      Exit(false);
   sprite := SgPoint(5, image.width div 96);
   try
      convertImage(image, -1, FRAME, sprite, SHEET, oname, 'animation', dirty);
   finally
      image.free;
   end;
   result := true;
end;

function convertAnim2(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
const
   FRAME: TSgPoint = (X: 128; Y: 128);
   SHEET: TSgPoint = (X: 1; Y: 1);
var
   oname: string;
   image: PSdlSurface;
   sprite: TSgPoint;
begin
   filename := extractFileName(filename);
   oname := filename;
   findGraphic(filename, 'battle2');
   if filename = '' then
      Exit(false);
   image := PSdlSurface(IMG_Load(PAnsiChar(Utf8String(filename))));
   if image = nil then
      Exit(false);
   sprite := SgPoint(5, image.width div 128);
   try
      convertImage(image, -1, FRAME, sprite, SHEET, oname, 'animation', dirty);
   finally
      image.free;
   end;
   result := true;
end;

function ProcessImage(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;
var
   aFile, tbi: TStream;
begin
   result := archive.fileExists(filename);
   if not result then
      Exit;

   aFile := archive.getFile(filename);
   try
      if strUtils.RightStr(filename, 4) <> '.png' then
      begin
         tbi := ConvertWholeImage(aFile, dirty);
         if not assigned(tbi) then
            Exit(false);
         filename := ChangeFileExt(filename, '.png');
         aFile.Free;
         aFile := tbi;
      end;
      GArchives[IMAGE_ARCHIVE].writeFile(format('%s\%s', [outFolderName, extractFileName(filename)]), aFile)
   finally
      aFile.Free;
   end;
end;

{ TUtf8EncodingN }

function TUtf8EncodingN.GetPreamble: TBytes;
begin
   SetLength(result, 0);
end;

end.
