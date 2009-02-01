unit preloader;
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
   classes, contnrs, syncObjs,
   commons;

type
   TPreloaderItem = (pl_map, pl_image, pl_charset, pl_animation, pl_monster);

   TSimpleCallback = procedure();

   TPreloaderStackFrame = class(TObject)
   private
      FItem: TPreloaderItem;
      FName: string;
      FNumber: integer;
      FCallback: TSimpleCallback;
   public
      constructor Create(item: TPreloaderItem; name: string; number: integer; callback: TSimpleCallback);

      property item: TPreloaderItem read FItem;
      property name: string read FName;
      property number: integer read FNumber;
      property callback: TSimpleCallback read FCallback;
   end;

   TPreloaderThread = class(TRpgThread)
   private
      FHasted: boolean;
      FPreloaderQueue: TObjectQueue;
      FQueueLock: TCriticalSection;

      procedure loadStackframeImage;
      procedure loadStackframeCharset;
      procedure loadStackframeMap;
      procedure loadStackframeAnim;
      procedure pause; inline;
   public
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
      procedure push(item: TPreloaderStackFrame);
      procedure clear;
      procedure haste;
   end;

   function totalMemoryAllocated: integer;
   procedure scanMap;

const
   PRELOADING = pointer($DEADBEEF);

var
   GPreloader: TPreloaderThread;

implementation
uses
   windows, psAPI, forms, sysUtils,
   fileIO, LMU, chipset, chipset_data, locate_files, map_unit, chipset_graphics,
   script_engine, events, tiles,
   {AsphyreSprite} SDL_sprite;

var
   LPreloaderCurrent: TPreloaderStackFrame;
   LWaitingEngine: TGameMap;

{ TPreloaderStackFrame }

constructor TPreloaderStackFrame.Create(item: TPreloaderItem; name: string; number: integer; callback: TSimpleCallback);
begin
   FItem := item;
   FName := name;
   FNumber := number;
   FCallback := callback;
end;

{ TPreloaderThread }

procedure TPreloaderThread.clear;
begin
   while FPreloaderQueue.Count > 0 do
      FPreloaderQueue.Pop.Free;
   //end WHILE
end;

constructor TPreloaderThread.Create;
begin
   inherited create(true);
   FPreloaderQueue := TObjectQueue.Create;
   FQueueLock := TCriticalSection.Create;
end;

destructor TPreloaderThread.Destroy;
begin
   self.clear;
   FPreloaderQueue.Free;
   FQueueLock.Free;
   inherited;
end;

procedure TPreloaderThread.Execute;
begin
   inherited;
   self.FreeOnTerminate := false;
{   GCurrentThread := self;
   LPreloaderCurrent := nil;
   self.Priority := tpIdle;
   LWaitingEngine.Image := GGameEngine.Image;

   repeat
      try
         if FPreloaderQueue.Count > 0 then
         begin
            LPreloaderCurrent := FPreloaderQueue.pop as TPreloaderStackFrame;
            //interpret and preload
            if assigned(LPreloaderCurrent.callback) then
               LPreloaderCurrent.callback
            else case LPreloaderCurrent.FItem of
               pl_map: self.loadStackframeMap;
               pl_image: loadStackframeImage;
               pl_charset: loadStackframeCharset;
               pl_monster:;
               pl_animation: loadStackframeAnim;
            end;

            FreeAndNil(LPreloaderCurrent);
            if FHasted then
            begin
               FHasted := false;
               self.priority := tpIdle;
            end;
            sleep(100);
         end else begin
            if FHasted then
            begin
               FHasted := false;
               self.priority := tpIdle;
            end;
            sleep(500);
         end;
      except
         on Exception do assert(false);
      end;
   until self.Terminated;}
end;

procedure TPreloaderThread.haste;
begin
   FHasted := true;
   self.priority := tpHighest;
end;

procedure TPreloaderThread.loadStackframeCharset;
var
   filename: string;
begin
   assert(LPreloaderCurrent.item = pl_charset);
   filename := LPreloaderCurrent.FName;
   locate_files.findGraphic(filename, 'charset');
   if self.terminated then
      Exit;
   GGameEngine.loadCharset(LPreloaderCurrent.FName, filename);
end;

procedure TPreloaderThread.loadStackframeImage;
begin
   assert(LPreloaderCurrent.item = pl_image);
   if self.terminated then
      Exit;
   GGameEngine.loadRpgImage(LPreloaderCurrent.FName, boolean(LPreloaderCurrent.FNumber));
end;

procedure TPreloaderThread.loadStackframeAnim;
begin
   assert(LPreloaderCurrent.item = pl_animation);
   if self.Terminated then
      Exit;
   GGameEngine.loadAnim(GDatabase.anim[LPreloaderCurrent.number].filename);
end;

{******************************************************************************
* The pride and joy of the preloader thread, this loads up a map while the
* player's not looking.  It periodically sleeps to allow higher-priority threads
* to do their thing.
*******************************************************************************}
procedure TPreloaderThread.loadStackframeMap;
var
   newmap: TRpgMap;
   mapfile: TFileStream;
   template: TMapUnit;
   currentChipset: TChipSet;
   filename: string;
   mapname: string;
   i, j: integer;
   tileIndex: integer;
begin
   assert(LPreloaderCurrent.item = pl_map);
   if assigned(GGameEngine.map[LPreloaderCurrent.number]) then
      Exit;
   if self.terminated then
      Exit;

   GGameEngine.map[LPreloaderCurrent.number] := TRpgMap(PRELOADING);
   i := 0;
   j := LPreloaderCurrent.number;
   repeat
      j := j div 10;
      inc(i)
   until j = 0;
   mapname := GCurrentFolder + '/map';
   for j := 1 to 4 - i do
      mapname := mapname + '0';
   mapname := mapname + intToStr(LPreloaderCurrent.number) + '.lmu';
   mapfile := commons.openFile(mapname);
   assert(getString(mapfile) = 'LcfMapUnit');
   template := TMapUnit.Create(mapfile, GDatabase, GGameEngine.mapTree, LPreloaderCurrent.number);
   newmap := TRpgMap.Create(template, LWaitingEngine);
   template := newMap.template;
   newmap.spriteList := LWaitingEngine.spriteList;
   template.eventBlock.compiler := GCurrentEngine.compiler;
   LWaitingEngine.assignCurrentMap(newMap);
   pause;
   fileName := GDatabase.getChipset(template.terrain).filename;
   if filename <> '' then
      GGameEngine.loadChipset(fileName, GGameEngine.images);
   pause;
   if (template.usesPano) and (template.panoName <> '') then
   begin
      filename := template.panoName;
      findGraphic(filename, 'panorama');
      if filename = '' then
         raise EParseMessage.create('Panorama graphic file "' + fileName + '" not found!');
      GGameEngine.loadBG(filename, template.panoName);
      newMap.setBG;
   end;
   pause;
   for i := 0 to template.eventCount - 1 do
   begin
      filename := template.events[i].page[0].filename;
      if (template.events[i].page[0].filename <> '') then
      begin
         findGraphic(filename, 'charset');
         if filename <> '' then
            GGameEngine.loadCharset(template.events[i].page[0].filename, filename);
      end;
   end;
   pause;
   currentChipset := GDatabase.getChipset(template.terrain);
   tileIndex := 0;
   for j := 0 to template.height - 1 do
   begin
      for i := 0 to template.width - 1 do
      begin
         newMap[lower, i, j].place(i, j, lower, template.lowChip[tileIndex], currentChipset);
         if template.highChip[tileIndex] <> 10000 then
            TLowerTile(newMap[lower, i, j]).placeUpper(i, j, upper, template.highChip[tileIndex], currentChipset);
         inc(tileIndex);
      end;
      if not FHasted then
         pause;
   end;
   newMap.scanSquare;
   pause;
   LWaitingEngine.adjustOwnership;
   newMap.spriteList := LWaitingEngine.swapOutSpriteList(TSpriteList.Create);
   GGameEngine.map[LPreloaderCurrent.number] := newMap;
   newMap.engine := GGameEngine;
end;

procedure TPreloaderThread.pause;
begin
   sleep(10);
   if self.terminated then
      raise Exception.Create('');
end;

procedure TPreloaderThread.push(item: TPreloaderStackFrame);
begin
   FQueueLock.Enter;
   try
      FPreloaderQueue.Push(item);
   finally
      FQueueLock.Leave;
   end;
end;

{ Classless }

{**************************************************************************
* Thanks to Huehnerschaender at PascalGameDevelopment.com for this routine
***************************************************************************}
function totalMemoryAllocated: integer;
const
   KILOBYTE = 1024;
var
  pmc: PPROCESS_MEMORY_COUNTERS;
  cb: Integer;
begin
  cb := SizeOf(_PROCESS_MEMORY_COUNTERS);
  GetMem(pmc, cb);
  pmc^.cb := cb;
  assert(GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb));
  result := pmc^.WorkingSetSize div KILOBYTE;
  FreeMem(pmc);
end;

procedure scanMap;
var
   theMap: TRpgMap;
   eventBlock: TEventBlock;
   eventCommand: TEventCommand;
   i, j, k: integer;
begin
   assert(assigned(LPreloaderCurrent) and (LPreloaderCurrent.item = pl_map));
   theMap := GGameEngine.map[LPreloaderCurrent.number];
   eventBlock := theMap.template.eventBlock;
   for I := 0 to eventBlock.len - 1 do
   begin
      for j := 0 to eventBlock[i].len - 1 do
      begin
         if eventBlock[i][j].hasScript then
            for k := 0 to eventBlock[i][j].len do
            begin
               eventCommand := eventBlock[i][j].opcode[k];
               case eventCommand.opcode of
                  10810: //fixed teleport code
                     GPreloader.Push(TPreloaderStackFrame.Create(pl_map, '', eventCommand.data[0], nil));
                  11110: //show image code
                     GPreloader.Push(TPreloaderStackFrame.Create(pl_image, eventCommand.name, eventCommand.data[7], nil));
                  10630, 10650: //charset codes
                     GPreloader.Push(TPreloaderStackFrame.Create(pl_charset, eventCommand.name, 0, nil));
                  11210:
                     GPreloader.push(TPreloaderStackFrame.Create(pl_animation, '', eventCommand.data[0], nil));
                  else ;
               end;
            end;
         //end if
      end;
   end;
end;

initialization
begin
   LWaitingEngine := TGameMap.Create(nil);
   GPreloader := TPreloaderThread.Create;
end;

finalization
begin
   LWaitingEngine.Free;
   GPreloader.clear;
   GPreloader.Free;
end;

end.
