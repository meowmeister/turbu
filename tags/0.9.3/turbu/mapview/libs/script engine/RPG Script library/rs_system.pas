unit rs_system;
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
   classes,
   commons, LDB;

   function heldItems(const index: word; const equipped: boolean): word;
   procedure removeItems(const value: word; const count: byte);
   procedure addItems(const value: word; const count: byte);
   procedure heroLeave(const id: byte);
   procedure heroJoin(const id: byte);
   procedure setSystemMusic(const which: TBgmTypes; const newSong: string);
   procedure wait(time: cardinal);
   procedure playMusic(name: string; time, volume, tempo, balance: word);
   procedure fadeOutMusic(time: word);
   procedure memorizeBgm;
   procedure playMemorizedBgm;
   procedure playSound(name: string; volume, tempo, balance: word);
   function keyScan(mask: TSet16; wait: boolean): byte;
   procedure callGlobalEvent(id: word);
   procedure callEvent(event, page: word);

implementation
uses
   sysUtils,
   formats, script_engine, script_interface, chipset_graphics, rm_sound,
   mapview; //turbu libs

var
   GDatabase: TLcfDataBase; //temporary hack to get this to compile

function heldItems(const index: word; const equipped: boolean): word;
var
  I: Integer;
begin
   result := 0;
   if between(index, 0, GDatabase.items) <> index then
      Exit;

   case equipped of
      false: result := GParty.inventory.quantityOf(index);
      true:
      begin
         result := 0;
         for I := 1 to MAXPARTYSIZE do
            if (GParty[i] <> GScriptEngine.hero[0]) and (GParty[i].equipped(index)) then
               inc(result);
            //end if
         //end for
      end;
   end;
end;

procedure removeItems(const value: word; const count: byte);
begin
   GParty.removeItem(value, count);
end;

procedure addItems(const value: word; const count: byte);
begin
   GParty.addItem(value, count);
end;

procedure heroJoin(const id: byte);
var i: byte;
begin
   if (not id in [1..GScriptEngine.heroes])
   or (GRsys.partySize = MAXPARTYSIZE) then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GParty[i] = GScriptEngine.hero[id] then
         Exit;
      //end if
   //end if

   i := GParty.openSlot;
   if i <> 0 then
      GParty[i] := GScriptEngine.hero[id];
   //end if
end;

procedure heroLeave(const id: byte);
var i: byte;
begin
   if (id = 0) or (id > GScriptEngine.heroes) then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GParty[i] = GScriptEngine.hero[id] then
         GParty[i] := nil;
      //end if
   //end if
end;

procedure setSystemMusic(const which: TBgmTypes; const newSong: string);
begin
   GDatabase.SystemData.bgm[which].filename := newSong;
end;

procedure wait(time: cardinal);
begin
   TEventThread(GCurrentThread).threadsleep(time);
end;

function waitForMusicPlaying: boolean;
begin
   result := GScriptEngine.mediaPlayer.syncWait = false;
end;

procedure playMusic(name: string; time, volume, tempo, balance: word);
var
   song: TRmMusic;
begin
   song := nil;
   try
      if name = '(OFF)' then
         GScriptEngine.mediaPlayer.stopMusic
      else begin
         song := TRmMusic.Create(name, time, volume, tempo, balance);
         GScriptEngine.mediaPlayer.playMusic(song);
         GWaiting := @waitForMusicPlaying;
      end;
   finally
      song.free;
   end;
end;

procedure playSound(name: string; volume, tempo, balance: word);
var
   sound: TRmSound;
begin
   if name = '(OFF)' then
      Exit;
   sound := TRmSound.Create(name, 0, volume, tempo, balance);
   GScriptEngine.mediaPlayer.playSfx(sound);
   sound.free;
end;

procedure fadeOutMusic(time: word);
begin
   GScriptEngine.mediaPlayer.fadeOut(time);
end;

procedure memorizeBgm;
begin
   GScriptEngine.mediaPlayer.memorizeBgm;
end;

procedure playMemorizedBgm;
begin
   //this must be run from the main thread to prevent a race
   //condition that keeps the song from playing the first time
   //if it's a MIDI
   GCurrentThread.syncRun(GScriptEngine.mediaPlayer.playMemorizedBgm);
end;

{$WARN NO_RETVAL OFF}
function readKeyScan(mask: TSet16): byte;
var
   bitset, bitmask: TSet16;
   i: integer;
const LIMIT_MASK: TSet16 = [0..5];
begin
   bitset := GInputReader.scan;
   if bitset = [] then
      result := 0
   else begin
      bitmask := mask;
      if GGameEngine.enterLock then
         bitset := bitset - [5, 6];
      if 6 in bitset then
         include(bitset, 5);
      if 8 in bitset then
         include(bitset, 7);
      bitset := bitset * bitmask * LIMIT_MASK;
      if bitset = [] then
         result := 0
      else for I in bitset do
         result := i;
      //end for
   end;
end;
{$WARN NO_RETVAL ON}

function keyScan(mask: TSet16; wait: boolean): byte;
begin
   if mask = [] then
   begin
      result := 0;
      Exit;
   end;

   GGameEngine.noEnterLock := true;
   result := readKeyScan(mask);
   //make sure to sleep the thread at least once, to avoid wasteful
   //spin-looping in certain keyboard-scanning event scripts
   if not wait then
      TEventThread(GCurrentThread).threadsleep(10)
   else while result = 0 do
   begin
      TEventThread(GCurrentThread).threadsleep(10);
      result := readKeyScan(mask);
   end;
   GGameEngine.noEnterLock := false;
end;

procedure callGlobalEvent(id: word);
begin
   if id <= high(GGlobalEvent) then
   begin
      case GProjectFormat of
         pf_2k:
         begin
            GScriptEngine.executeEvent(GGlobalEvent[id].event, nil);
            GCurrentThread.Terminate;
         end;
         pf_2k3:
         begin
            GScriptEngine.executeEvent(GGlobalEvent[id].event, nil, GCurrentThread as TEventThread);
            GCurrentThread.Suspend;
         end;
         else assert(false, 'Project format variable is corrupt!');
      end;
   end;
end;

procedure callEvent(event, page: word);
begin
   case GProjectFormat of
      pf_2k:
         begin
            GScriptEngine.executeEventPage(GRsys.event[event].base, page);
            GCurrentThread.Terminate;
         end;
      pf_2k3:
         begin
            GScriptEngine.executeEventPage(GRsys.event[event].base, page, GCurrentThread as TEventThread);
            GCurrentThread.Suspend;
         end;
      else assert(false, 'Project format variable is corrupt!');
   end;
end;

end.
