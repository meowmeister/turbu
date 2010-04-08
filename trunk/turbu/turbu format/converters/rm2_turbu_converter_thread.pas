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
   classes, windows, Generics.Collections,
   commons, conversion_report, formats, turbu_map_metadata, archiveInterface,
   LDB, LMT, LMU, events;

type
   TConverterThread = class(TRpgThread)
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
      FAnims: TStringList;
      FBattleSprites: TStringList;
      procedure VerifyFormat(datafile: TStream);
      procedure ConvertMap(const filename: string; database: TLcfDataBase; mapTree: TFullTree;
                     metadata: TMapTree; input, output: IArchive);
      procedure CopyResources(input, output: IArchive);
      procedure ScanCommands(list: TList<TEventCommand>);
      procedure ScanEventsForResources(block: TEventBlock);
      procedure ScanPage(page: TEventPage);
      procedure ScanMove(block: TEventMoveBlock);
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
   SysUtils, StrUtils,
   fileIO, discInterface, logs, locate_files,
   turbu_constants, turbu_database, turbu_unit_dictionary, turbu_engines,
   turbu_functional, turbu_maps, turbu_classes, turbu_pathing, turbu_tbi_lib,
   rm2_turbu_database, rm2_turbu_maps, rm2_turbu_map_metadata, rm2_turbu_map_objects,
   sdl, sdl_13, sdlStreams, sdl_image, sg_defs;

const
   CONVERSION_TASKS = 7;

type
   TImageProcessor = function (archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean;

function convertSprite(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward; overload;
function convertPortrait(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function convertAnim(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;
function extLength(filename: string): integer; forward; inline;
function ProcessImage(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; forward;

{ TConverterThread }

procedure TConverterThread.ConvertMap(const filename: string;
  database: TLcfDataBase; mapTree: TFullTree; metadata: TMapTree; input,
  output: IArchive);

   function GetIdFromFilename: integer;
   var
      dummy: string;
   begin
      dummy := StringReplace(filename, 'map', '', [rfIgnoreCase]);
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
      if (id = -1) or (id > mapTree.GetMax) then
         Exit;
      map := TMapUnit.Create(mapFile, database, mapTree, id);
      cMap := TRpgMap.Convert(map, mapTree.getMapData(id), database, id);
      outFile := TMemoryStream.Create;
      cMap.save(outFile);
      outFile.Seek(0, soFromBeginning);

      FBackgrounds.Add(cmap.bgName);
      FSongs.Add(metadata[id].bgmData.name);
      FBattleBackgrounds.Add(metadata[id].battleBgName);

      scanEventsForResources(map.eventBlock);

      output.currentFolder := 'maps';
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
   index: integer;
   rtpInput: IArchive;

   procedure processImageList(const folderName, outFolderName, stepname: string;
                              fileList: TStringList; processor: TImageProcessor);
   const NOT_FOUND = 'File %s from %s not found!';
   var
      filename: string;
      lFilename: string;
   begin
      if fileList.Find('', index) then
         filelist.Delete(index);
      rtpInput.currentFolder := folderName;
      FReport.newStep(stepname);
      for filename in input.allFiles(folderName) do
      begin
//         lFilename := extractFilename(leftStr(filename, length(filename) - extLength(filename)));
         if processor(input, outFolderName, filename, false) then
         begin
            if fileList.Find(lFilename, index) then
               fileList.Delete(index);
         end
         else FReport.makeError(format(NOT_FOUND, [filename, folderName]));
      end;
      for filename in fileList do
      begin
         lFilename := ExtractFilename(locate_files.findGraphicF(filename, folderName));
         if processor(rtpInput, outFolderName, format('%s\%s', [folderName, lFilename]), true) then
            FReport.makeNotice(format('Copied RTP resource %s.', ['Panorama\'+ filename]))
         else FReport.makeError(format(NOT_FOUND, [filename, folderName]));
      end;
   end;

begin
   rtpInput := OpenFolder(locate_files.rtpLocation);
   processImageList('Panorama', 'Backgrounds', 'Backgrounds', FBackgrounds, processImage);
   processImageList('System', 'System', 'System layout', FSysTiles, processImage);
   processImageList('Picture', 'Pictures', 'Images', FPictures, processImage);
   processImageList('CharSet', 'Mapsprite', 'Character sprites', FMapSprites, convertSprite);
   processImageList('FaceSet', 'Portrait', 'Character portraits', FPortraits, convertPortrait);
   processImageList('Battle', 'Animation', 'Battle animations', FAnims, convertAnim);
   if FFormat = pf_2k3 then
      processImageList('CharSet', 'Mapsprite', 'Character sprites', FMapSprites, convertSprite);
   //FSongs FSounds FMovies
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
   FBattleSprites := CreateSortedStringList;
end;

destructor TConverterThread.Destroy;
begin
   FBattleSprites.Free;
   FAnims.Free;
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

procedure TConverterThread.Execute;
const
	SECTIONS_I_KNOW_HOW_TO_READ = [$0b..$0d, $11..$14, $17..$18, $1e];
var
   datafile: TStream;
   stream: TStream;
   outFile: TFileStream;
   savefile: TMemoryStream;
   filename, uFilename: string;
   dic: TUnitDictionary;
   fromFolder, toFolder: IArchive;
   legacy: TLegacySections;
   key: byte;
begin
   legacy := nil; //to silence a compiler warning
   try
      inherited Execute;
      outFile := nil;
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
               if not key in SECTIONS_I_KNOW_HOW_TO_READ then
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

         FReport.setCurrentTask('Preparing scripts');
         GDatabase.Free;
         dic := TUnitDictionary.Create(10);
         for filename in GArchives[0].allFiles('scripts\general\') do
         begin
            stream := GArchives[0].getFile(filename);
            uFilename := StringReplace(filename, 'scripts\general\', '', []);
            uFilename := StringReplace(uFilename, '.trs', '', []);
            try
               dic.Add(uFilename, TStringList.Create);
               dic[uFilename].LoadFromStream(stream);
            finally
               stream.Free;
            end;
         end;

         try
            FReport.setCurrentTask('Converting Database', 12);
            GDatabase := TRpgDatabase.convert(FLdb, FLmt, dic, legacy, FReport,
                                              FMapSprites, FPortraits, FAnims, FBattleSprites);
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

         FReport.setCurrentTask('Converting maps', fromFolder.countFiles('*.lmu'));
         TFunctional.Map<string>(fromFolder.allFiles,
            procedure(const filename: string)
            begin
               FReport.newStep(filename);
               ConvertMap(filename, FLdb, FLmt, GDatabase.mapTree, fromFolder, toFolder);
            end);

         FReport.setCurrentTask('Copying Resources', 3);
         if locate_files.rtpLocation = '' then
            FReport.makeNotice('RTP not found.  RTP resources will not be copied.')
         else self.copyResources(fromFolder, toFolder);

         savefile := TMemoryStream.Create;
         try
            GDatabase.save(savefile);
            FReport.setCurrentTask('Saving database');
            filename := IncludeTrailingPathDelimiter(FToLoc) + DBNAME;
            if FileExists(filename) then
               deleteFile(PChar(filename));
            outFile := TFileStream.Create(filename, fmCreate);
            outfile.CopyFrom(savefile, 0);
         finally
            savefile.free;
            outFile.Free;
         end;
      finally
         conversionArchive := -1
      end;

      FReport.makeReport;
   except
      FReport.fatal(TObject(AcquireExceptionObject) as Exception);
   end;
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
            10660, 11550: FSongs.Add(name);
            10670: FSounds.Add(name);
            10680: FSysTiles.Add(name);
            11110: FPictures.Add(name);
            11560: FMovies.Add(name);
            11720: FBackgrounds.Add(name);
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
   path: TPath;
   step: TMoveStep;
begin
   path := TPath.convert(block);
   try
      for step in path.opcodes do
         if (step.opcode = MOVECODE_CHANGE_SPRITE) then
            FMapSprites.Add(step.name)
         else if step.opcode = MOVECODE_PLAY_SFX then
            FSounds.Add(step.name);
   finally
      path.Free;
   end;
end;

procedure TConverterThread.ScanPage(page: TEventPage);
const OPCODES: array [1..12] of integer =
   (10130, 10630, 10640, 10650, 10660, 10670, 10680, 11110, 11510, 11550, 11560,
    11720);
begin
   if page.filename <> '' then
      FMapSprites.Add(string(page.filename));
   if assigned(page.moveBlock.moveBlock) and (page.moveBlock.moveBlock.base <> '') then
      scanMove(page.moveBlock);//do something
   scanCommands(page.commands.where(
      function(arg1: TEventCommand): boolean
      var
         dummy: integer;
      begin
         result := TArray.BinarySearch<integer>(OPCODES, arg1.opcode, dummy);
      end));
end;

procedure TConverterThread.VerifyFormat(datafile: TStream);
begin
   GProjectFormat := scanRmFormat(datafile);
   if GProjectFormat <> FFormat then
      FFormat := GProjectFormat;
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
   framesPerSprite := sprite.X * sprite.Y;
   blitSurface := TSdlSurface.Create(frame.X, frame.Y * framesPerSprite, 8, 0, 0, 0, 0);
   try
      if not blitSurface.SetPalette(image.format.palette.colors, 0, image.format.palette.count) then
         raise EInvalidImage.CreateFmt('Unable to convert sprite %s due to colorkey failure!', [name]);
      blitSurface.ColorKey := image.ColorKey;

      if id = -1 then
         startingPoint := SgPoint(0, 0)
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
   try
      convertImage(image, id, FRAME, SPRITE, SHEET, oname, 'mapsprite', dirty);
   finally
      image.free;
   end;
   result := true;
end;

function convertSprite(archive: IArchive; outFolderName, filename: string; dirty: boolean): boolean; overload;
var i: integer;
begin
   result := true;
   for I := 0 to 7 do
      result := result and convertSprite(filename, i, dirty);
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
   try
      convertImage(image, -1, FRAME, SPRITE, SHEET, oname, 'portrait', dirty);
   finally
      image.free;
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
   sprite := SgPoint(5, image.width div 96);
   try
      convertImage(image, -1, FRAME, sprite, SHEET, oname, 'animation', dirty);
   finally
      image.free;
   end;
   result := true;
end;

function extLength(filename: string): integer;
begin
   result := length(ExtractFileExt(filename));
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
            Exit;
         filename := ChangeFileExt(filename, '.png');
         aFile.Free;
         aFile := tbi;
      end;
      GArchives[IMAGE_ARCHIVE].writeFile(format('%s\%s', [outFolderName, extractFileName(filename)]), aFile)
   finally
      aFile.Free;
   end;
end;

end.
