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
uses
   classes,
   turbu_defs, turbu_script_engine;

   procedure showMessage(const msg: string);
   procedure setMessageBoxPosition(const position: TMboxLocation);
   procedure setMessageBoxVisible(const value: boolean);
   procedure setMessageModal(const value: boolean);
   procedure prepareMbox(out oldValue: TMboxLocation);
   procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero, modal: boolean);
   procedure clearPortrait;
   procedure setPortrait(filename: string; const index: integer; const rightside, flipped: boolean);
   function choiceBox(const msg: string; const handler: integer): integer;
   function inputNumber(const digits: integer): integer;
   function inn(messageStyle, cost: integer): boolean;

   procedure RegisterScriptUnit(engine: TScriptEngine);

implementation
uses
   SysUtils, SyncObjs,
   commons, turbu_constants, turbu_2k_sprite_engine, turbu_2k_frames,
   rsCompiler, rsExec,
   ArchiveUtils;

var
   FMboxCautious: boolean;
   FMboxModal: boolean;
   FMessageLock: TCriticalSection;

const
   MAXINNTIME = 5000;

procedure prepareMbox(out oldValue: TMboxLocation);
var newValue: TMboxLocation;
begin
   oldValue := GMenuEngine.position;
   newValue := oldValue;
   if FMboxCautious and GSpriteEngine.heroIn(oldValue) then
   begin
      case oldValue of
         mb_top: newValue := mb_bottom;
         mb_middle:
            if GSpriteEngine.heroIn(mb_top) then
               newValue := mb_bottom
            else newValue := mb_top;
         mb_bottom: newValue := mb_top;
      end;
   end;
   setMessageBoxPosition(newValue);
end;

procedure showMessage(const msg: string);
var
   oldValue: TMboxLocation;
begin
   FMessageLock.Enter;
   try
      prepareMbox(oldValue);
      GMenuEngine.ShowMessage(msg, FMboxModal);
      setMessageBoxPosition(oldValue);
   finally
      FMessageLock.Leave;
   end;
end;

procedure setMessageBoxCaution(const value: boolean);
begin
   FMBoxCautious := value;
end;

procedure setMessageBoxPosition(const position: TMboxLocation);
begin
   GMenuEngine.position := position;
end;

procedure setMessageModal(const value: boolean);
begin
   FMboxModal := value;
end;

procedure setMessageBoxVisible(const value: boolean);
begin
   GMenuEngine.boxVisible := value;
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
{$MESSAGE WARN 'Commented out code in live unit'}
//   GMenuEngine.MessageBox.portrait.MirrorX := value;
end;

procedure clearPortrait;
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   GMenuEngine.MessageBox.portrait.Visible := false;
end;

procedure setRightside(const value: boolean);
begin
{$MESSAGE WARN 'Commented out code in live unit'}
//   GMenuEngine.MessageBox.rightside := value;
end;

procedure setPortrait(filename: string; const index: integer; const rightside, flipped: boolean);
var
   valid: boolean;
begin
   if not ArchiveUtils.GraphicExists(filename, 'portrait') then
   begin
      filename := filename + '.png';
      if not ArchiveUtils.GraphicExists(filename, 'portrait') then
         Exit;
   end;
   commons.runThreadsafe(
      procedure
      var path: string;
      begin
         path := format('portrait\%s', [filename]);
         valid := (clamp(index, 0, GSpriteEngine.Images.EnsureImage(path, filename, PORTRAIT_SIZE).count - 1) = index)
      end, true);
   if not valid then
      Exit;

   setRightside(rightside);
{$MESSAGE WARN 'Commented out code in live unit'}
//   GMenuEngine.MessageBox.setPortrait(filename, index);
   setFlipped(flipped);
end;

function choiceBox(const msg: string; const handler: integer): integer;
var
   oldValue: TMboxLocation;
begin
   prepareMbox(oldValue);
   asm int 3 end;
{$MESSAGE WARN 'Commented out code in live unit'}
//   GMenuEngine.choiceBox(msg, acceptCancel);
   setMessageBoxPosition(oldValue);
   result := GMenuEngine.menuInt;
end;

function inputNumber(const digits: integer): integer;
var
   oldValue: TMboxLocation;
begin
   prepareMbox(oldValue);
   asm int 3 end;
{$MESSAGE WARN 'Commented out code in live unit'}
//   GMenuEngine.inputNumber(digits);
   setMessageBoxPosition(oldValue);
   result := GMenuEngine.menuInt;
end;

function inn(messageStyle, cost: integer): boolean;
var
   oldValue: TMboxLocation;
//   I: Integer;
begin
   prepareMbox(oldValue);
   GMenuEngine.inn(messageStyle, cost);
   setMessageBoxPosition(oldValue);
   if GMenuEngine.menuInt = 1 then
   begin
      result := true;
{$MESSAGE WARN 'Commented out code in live unit'}
{      GMenuEngine.cutscene := GMenuEngine.cutscene + 1;
      for I := 1 to high(GScriptEngine.heroes) do
         GScriptEngine.Hero[i].fullheal;
      assert(GParty.money >= cost);
      GParty.money := GParty.money - cost;
      GMenuEngine.fadeOut(1500);
      GScriptEngine.mediaPlayer.fadeOut(1500);
      TEventThread(GCurrentThread).threadSleep(1750);
      GScriptEngine.mediaPlayer.playSystemMusic(bgmInn);
      TEventThread(GCurrentThread).threadSleep(MAXINNTIME);
      GMenuEngine.fadeIn(1500);
      GScriptEngine.mediaPlayer.fadeIn(1500);
      TEventThread(GCurrentThread).threadSleep(1500);
      GMenuEngine.cutscene := GMenuEngine.cutscene - 1;    }
   end
   else result := false;
end;

procedure RegisterMessagesC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TMboxLocation));
   input.ImportFunction('procedure showMessage(const msg: string)');
   input.ImportFunction('procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero, continueEvents: boolean);');
   input.ImportFunction('procedure clearPortrait');
   input.ImportFunction('procedure setPortrait(filename: string; const index: integer; const rightside, flipped: boolean);');
   input.ImportFunction('function showChoice(const input: string; handler: integer): integer;');
   input.ImportFunction('function inputNumber(const digits: integer): integer');
   input.ImportFunction('function inn(messageStyle, cost: integer): boolean;');

   input.ImportFunction('function inputText(const base: string; portrait: integer): string');
   input.ImportFunction('procedure OpenMenu;');
   input.ImportFunction('procedure EnableMenu;');
   input.ImportFunction('procedure DisableMenu;');
   input.ImportFunction('procedure SaveMenu;');
end;

procedure RegisterMessagesE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('showMessage', @showMessage);
   RegisterFunction('messageOptions', @messageoptions);
   RegisterFunction('clearPortrait', @clearPortrait);
   RegisterFunction('setPortrait', @setPortrait);
   RegisterFunction('showChoice', @ChoiceBox);
   RegisterFunction('inputNumber', @inputNumber);
   RegisterFunction('inn', @inn);
   RegisterFunction('inputText', nil);
   RegisterFunction('OpenMenu', nil);
   RegisterFunction('EnableMenu', nil);
   RegisterFunction('DisableMenu', nil);
   RegisterFunction('SaveMenu', nil);
end;

procedure RegisterScriptUnit(engine: TScriptEngine);
begin
   engine.RegisterUnit('messages', RegisterMessagesC, RegisterMessagesE);
end;

initialization
   FMessageLock := TCriticalSection.Create;
finalization
   FMessageLock.Free;
end.
