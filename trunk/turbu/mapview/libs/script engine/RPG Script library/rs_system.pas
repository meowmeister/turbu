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
   LDB;

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
   function keyScan(mask: word; wait: boolean): byte;
   procedure callGlobalEvent(id: word);
   procedure callEvent(event, page: word);
   
implementation
uses
   sysUtils,
   commons, formats, script_engine, script_interface, chipset_graphics, rm_sound,
   mapview; //turbu libs

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
            if (GParty[i] <> GCurrentEngine.hero[0]) and (GParty[i].equipped(index)) then
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
   if (not id in [1..GCurrentEngine.heroes])
   or (GRsys.partySize = MAXPARTYSIZE) then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GParty[i] = GCurrentEngine.hero[id] then
         Exit;
      //end if
   //end if

   i := GParty.openSlot;
   if i <> 0 then
      GParty[i] := GCurrentEngine.hero[id];
   //end if
end;

procedure heroLeave(const id: byte);
var i: byte;
begin
   if (id = 0) or (id > GCurrentEngine.heroes) then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GParty[i] = GCurrentEngine.hero[id] then
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
   result := GCurrentEngine.mediaPlayer.syncWait = false;
end;

procedure playMusic(name: string; time, volume, tempo, balance: word);
var
   song: TRmMusic;
begin
   song := nil;
   try
      if name = '(OFF)' then
         GCurrentEngine.mediaPlayer.stopMusic
      else begin
         song := TRmMusic.Create(name, time, volume, tempo, balance);
         GCurrentEngine.mediaPlayer.playMusic(song);
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
   GCurrentEngine.mediaPlayer.playSfx(sound);
   sound.free;
end;

procedure fadeOutMusic(time: word);
begin
   GCurrentEngine.mediaPlayer.fadeOut(time);
end;

procedure memorizeBgm;
begin
   GCurrentEngine.mediaPlayer.memorizeBgm;
end;

procedure playMemorizedBgm;
begin
   //this must be run from the main thread to prevent a race
   //condition that keeps the song from playing the first time
   //if it's a MIDI
   GCurrentThread.syncRun(GCurrentEngine.mediaPlayer.playMemorizedBgm);
end;

{$WARN NO_RETVAL OFF}
function readKeyScan(mask: word): byte;
var
   bitset, bitmask: TRpgBitset16;
   i: integer;
const LIMIT_MASK = $3F;
begin
   bitset := GInputReader.scan;
   if bitset = 0 then
      result := 0
   else begin
      bitmask := mask;
      bitset.bits[5] := bitset.bits[5] and not GGameEngine.enterLock;
      bitset.bits[6] := bitset.bits[6] and not GGameEngine.enterLock;
      bitset.bits[5] := bitset.bits[5] or bitset.bits[6];
      bitset.bits[6] := bitset.bits[7] or bitset.bits[8];
      bitset := bitset and bitmask and LIMIT_MASK;
      if bitset = 0 then
         result := 0
      else for I := 1 to 16 do
         if bitset.bits[i] then
            result := i;
         //end if
      //end for
   end;
end;
{$WARN NO_RETVAL ON}

function keyScan(mask: word; wait: boolean): byte;
begin
   if mask = 0 then
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
            GCurrentEngine.executeEvent(GGlobalEvent[id].event, nil);
            GCurrentThread.Terminate;
         end;
         pf_2k3:
         begin
            GCurrentEngine.executeEvent(GGlobalEvent[id].event, nil, GCurrentThread as TEventThread);
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
            GCurrentEngine.executeEventPage(GRsys.event[event].base, page);
            GCurrentThread.Terminate;
         end;
      pf_2k3:
         begin
            GCurrentEngine.executeEventPage(GRsys.event[event].base, page, GCurrentThread as TEventThread);
            GCurrentThread.Suspend;
         end;
      else assert(false, 'Project format variable is corrupt!');
   end;
end;

end.
