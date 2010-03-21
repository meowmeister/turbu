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
   classes, windows,
   commons, conversion_report, formats, turbu_map_metadata, archiveInterface,
   LDB, LMT, LMU;

type
   TConverterThread = class(TRpgThread)
   private
      FReport: IConversionReport;
      FLdb: TLcfDataBase;
      FLmt: TFullTree;
      FFromLoc, FToLoc: string;
      FFormat: TProjectFormat;
      FBackgrounds: TStringList;
      FBattleBackgrounds: TStringList;
      FSongs: TStringList;
      procedure VerifyFormat(datafile: TStream);
      procedure ConvertMap(const filename: string; database: TLcfDataBase; mapTree: TFullTree;
                     metadata: TMapTree; input, output: IArchive);
      procedure CopyResources(input, output: IArchive);
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
   Generics.Collections, SysUtils, StrUtils,
   fileIO, discInterface, logs, locate_files,
   turbu_constants, turbu_database, turbu_unit_dictionary, turbu_engines,
   turbu_functional, turbu_maps, turbu_classes,
   rm2_turbu_database, rm2_turbu_maps, rm2_turbu_map_metadata;

const
   CONVERSION_TASKS = 7;

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

   function extLength(filename: string): integer;
   begin
      result := length(ExtractFileExt(filename));
   end;

   procedure ProcessImage(archive: IArchive; filename: string);
   var
      aFile: TStream;
   begin
      aFile := archive.getFile(filename);
      if strUtils.RightStr(filename, 4) = '.png' then
      begin
         output.writeFile(format('Backgrounds\%s', [filename]), aFile);
         aFile.Free;
      end
      else begin
         aFile.Free;
      end;
   end;

var
   index: integer;
   filename: string;
   lFilename: string;
   rtpInput: IArchive;
begin
   rtpInput := OpenFolder(locate_files.rtpLocation);
   rtpInput.currentFolder := 'Panorama';
   FReport.newStep('Backgrounds');
   for filename in input.allFiles('Panorama') do
   begin
      processImage(input, filename);
      lFilename := extractFilename(leftStr(filename, length(filename) - extLength(filename)));
      if FBackgrounds.Find(lFilename, index) then
         FBackgrounds.Delete(index);
   end;
   for filename in FBackgrounds do
   begin
      if filename = '' then
         Continue;
      processImage(rtpInput, 'Panorama\' + ExtractFilename(locate_files.findGraphicF(filename, 'Panorama')));
      FReport.makeNotice(format('Copied RTP resource %s.', ['Panorama\'+ filename]));
   end;
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
end;

destructor TConverterThread.Destroy;
begin
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
            GDatabase := TRpgDatabase.convert(FLdb, FLmt, dic, legacy, FReport);
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

procedure TConverterThread.VerifyFormat(datafile: TStream);
begin
   GProjectFormat := scanRmFormat(datafile);
   if GProjectFormat <> FFormat then
      FFormat := GProjectFormat;
end;

{ Classless }

end.