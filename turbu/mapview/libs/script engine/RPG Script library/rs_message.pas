unit rs_message;
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
uses classes, frames;

   procedure showMessage(const msg: string; sender: TThread);
   procedure setMessageBoxPosition(const position: TMboxLocation);
   procedure setMessageBoxVisible(const value: boolean);
   procedure setMessageModal(const value: boolean);
   procedure prepareMbox(out oldValue: TMboxLocation);
   procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero, modal: boolean);
   procedure clearPortrait;
   procedure setPortrait(const filename: string; const index: byte; const rightside, flipped: boolean);
   function choiceBox(const msg: string; const acceptCancel: boolean; sender: TThread): integer;
   function inputNumber(const digits: byte; sender: TThread): integer;
   function inn(messageStyle: byte; cost: integer; sender: TThread): boolean;

implementation
uses
   windows,
   commons, LDB, script_engine, chipset_graphics, locate_files;

var
   FMboxCautious: boolean;
   FMboxModal: boolean;

const
   MAXINNTIME = 5000;

procedure prepareMbox(out oldValue: TMboxLocation);
var newValue: TMboxLocation;
begin
   oldValue := GGameEngine.currentMBox.position;
   newValue := oldValue;
   if FMboxCautious and GGameEngine.heroIn(oldValue) then
   begin
      case oldValue of
         mb_top: newValue := mb_bottom;
         mb_middle:
            if GGameEngine.heroIn(mb_top) then
               newValue := mb_bottom
            else
               newValue := mb_top;
         mb_bottom: newValue := mb_top;
      end;
   end;
   setMessageBoxPosition(newValue);
end;

procedure showMessage(const msg: string; sender: TThread);
var
   continuing: boolean;
   oldValue: TMboxLocation;
begin
   prepareMbox(oldValue);
   continuing := GGameEngine.messageBox(msg);
   GCurrentEngine.waitForMboxReturn(sender as TEventThread);
   while continuing do
   begin
      continuing := GGameEngine.continueMessage;
      GCurrentEngine.waitForMboxReturn(sender as TEventThread);
   end;
   setMessageBoxPosition(oldValue);
end;

procedure setMessageBoxCaution(const value: boolean);
begin
   FMBoxCautious := value;
end;

procedure setMessageBoxPosition(const position: TMboxLocation);
begin
   GGameEngine.currentMBox.position := position;
end;

procedure setMessageModal(const value: boolean);
begin
   FMboxModal := value;
end;

procedure setMessageBoxVisible(const value: boolean);
begin
   GGameEngine.currentMBox.boxVisible := value;
end;

procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero, modal: boolean);
begin
   setMessageBoxVisible(transparent);
   setMessageBoxPosition(position);
   setMessageBoxCaution(dontHideHero);
   setMessageModal(modal);
end;

procedure setFlipped(const value: boolean);
begin
   GGameEngine.currentMBox.portrait.MirrorX := value;
end;

procedure clearPortrait;
begin
   GGameEngine.currentMBox.portrait.Visible := false;
end;

procedure setRightside(const value: boolean);
begin
   GGameEngine.currentMBox.rightside := value;
end;

procedure setPortrait(const filename: string; const index: byte; const rightside, flipped: boolean);
var
   dummy: string;
begin
   if (not index in [1..16]) then
      Exit;
   dummy := filename;
   findGraphic(dummy, 'faceset');
   if dummy = '' then
      Exit;

   setRightside(rightside);
   GGameEngine.currentMBox.setPortrait(filename, index);
   setFlipped(flipped);
end;

function choiceBox(const msg: string; const acceptCancel: boolean; sender: TThread): integer;
var
   oldValue: TMboxLocation;
begin
   prepareMbox(oldValue);
   GGameEngine.choiceBox(msg, acceptCancel);
   GCurrentEngine.waitForMboxReturn(sender as TEventThread);
   setMessageBoxPosition(oldValue);
   result := GGameEngine.menuInt;
end;

function inputNumber(const digits: byte; sender: TThread): integer;
var
   oldValue: TMboxLocation;
begin
   prepareMbox(oldValue);
   GGameEngine.inputNumber(digits);
   GCurrentEngine.waitForMboxReturn(sender as TEventThread);
   setMessageBoxPosition(oldValue);
   result := GGameEngine.menuInt;
end;

function inn(messageStyle: byte; cost: integer; sender: TThread): boolean;
var
   oldValue: TMboxLocation;
   I: Integer;
begin
   prepareMbox(oldValue);
   GGameEngine.inn(messageStyle, cost);
   GCurrentEngine.waitForMboxReturn(sender as TEventThread);
   setMessageBoxPosition(oldValue);
   if GGameEngine.menuInt = 2 then
   begin
      GGameEngine.cutscene := GGameEngine.cutscene + 1;
      result := true;
      for I := 1 to high(GCurrentEngine.heroes) do
         GCurrentEngine.Hero[i].fullheal;
      assert(GParty.money >= cost);
      GParty.money := GParty.money - cost;
      GGameEngine.fadeOut(1500);
      GCurrentEngine.mediaPlayer.fadeOut(1500);
      TEventThread(GCurrentThread).threadSleep(1750);
      GCurrentEngine.mediaPlayer.playSystemMusic(bgmInn);
      TEventThread(GCurrentThread).threadSleep(MAXINNTIME);
      GGameEngine.fadeIn(1500);
      GCurrentEngine.mediaPlayer.fadeIn(1500);
      TEventThread(GCurrentThread).threadSleep(1500);
      GGameEngine.cutscene := GGameEngine.cutscene - 1;
   end
   else result := false;
end;

initialization
begin
   FMboxCautious := false;
   FMboxModal := false;
end;

end.
