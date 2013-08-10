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
   turbu_defs, turbu_script_engine,
   dwsJSON;

   procedure showMessage(const msg: string);
   procedure setMessageBoxPosition(const position: TMboxLocation);
   procedure setMessageBoxVisible(const value: boolean);
   procedure setMessageModal(const value: boolean);
   procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero, modal: boolean);
   procedure clearPortrait;
   procedure setPortrait(filename: string; const index: integer; const rightside, flipped: boolean);
   function choiceBox(const msg: string; const choices: TArray<string>; const handler: integer): integer;
   function inputNumber(const msg: string; digits: integer): integer;
   function inn(messageStyle, cost: integer): boolean;
   procedure setSkin(const name: string; tiled: boolean);

   procedure RegisterScriptUnit(engine: TScriptEngine);
   procedure SerializeMessageState(writer: TdwsJSONWriter);
   procedure DeserializeMessageState(obj: TdwsJSONObject);

implementation
uses
   SysUtils, SyncObjs,
   commons, turbu_constants, turbu_2k_sprite_engine, turbu_2k_frames, rs_media,
   turbu_2k_environment, turbu_2k_map_engine, turbu_classes,
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
   GMenuEngine.portrait.MirrorX := value;
end;

procedure clearPortrait;
begin
   GMenuEngine.portrait.Visible := false;
end;

procedure setRightside(const value: boolean);
begin
   GMenuEngine.SetRightside(value);
end;

procedure setPortrait(filename: string; const index: integer; const rightside, flipped: boolean);
var
   valid: boolean;
begin
   if not ArchiveUtils.GraphicExists(filename, 'portrait') then
      Exit;
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
   GMenuEngine.setPortrait(filename, index);
   setFlipped(flipped);
end;

function choiceBox(const msg: string; const choices: TArray<string>; const handler: integer): integer;
var
   oldValue: TMboxLocation;
begin
   prepareMbox(oldValue);
   GMenuEngine.choiceBox(msg, choices, handler > 0);
   setMessageBoxPosition(oldValue);
   result := GMenuEngine.menuInt;
end;

function inputNumber(const msg: string; digits: integer): integer;
var
   oldValue: TMboxLocation;
begin
   prepareMbox(oldValue);
   GMenuEngine.inputNumber(msg, digits);
   setMessageBoxPosition(oldValue);
   result := GMenuEngine.menuInt;
end;

function inn(messageStyle, cost: integer): boolean;
var
   oldValue: TMboxLocation;
   I: Integer;
begin
   prepareMbox(oldValue);
   GMenuEngine.inn(messageStyle, cost);
   setMessageBoxPosition(oldValue);
   if GMenuEngine.menuInt = 1 then
   begin
      result := true;
      GGameEngine.enterCutscene;
      try
         for I := 1 to GEnvironment.HeroCount do
            GEnvironment.Heroes[i].fullheal;
         assert(GEnvironment.money >= cost);
         GEnvironment.money := GEnvironment.money - cost;
         GSpriteEngine.fadeOut(1500);
         rs_media.fadeOutMusic(1500);
         GScriptEngine.threadSleep(1750, true);
         rs_media.PlaySystemMusic(bgmInn, true);
         GScriptEngine.threadWait();
         GSpriteEngine.fadeIn(1500);
         rs_media.fadeInLastMusic(1500);
         GScriptEngine.threadSleep(1500, true);
      finally
         GGameEngine.LeaveCutscene;
      end;
   end
   else result := false;
end;

procedure setSkin(const name: string; tiled: boolean);
begin
   GMenuEngine.SetSkin(name, not tiled);
end;

procedure RegisterMessagesC(input: TrsTypeImporter);
begin
   input.ImportType(TypeInfo(TMboxLocation));
   input.ImportFunction('procedure showMessage(const msg: string)');
   input.ImportFunction('procedure messageOptions(const transparent: boolean; const position: TMboxLocation; const dontHideHero, continueEvents: boolean);');
   input.ImportFunction('procedure clearPortrait');
   input.ImportFunction('procedure setPortrait(filename: string; const index: integer; const rightside, flipped: boolean);');
   input.ImportFunction('function showChoice(const input: string; const choices: TStringArray; handler: integer): integer;');
   input.ImportFunction('function inputNumber(const msg: string; digits: integer): integer');
   input.ImportFunction('function inn(messageStyle, cost: integer): boolean;');

   input.ImportFunction('function inputText(const base: string; portrait: integer): string');
   input.ImportFunction('procedure OpenMenu;');
   input.ImportFunction('procedure EnableMenu;');
   input.ImportFunction('procedure DisableMenu;');
   input.ImportFunction('procedure SaveMenu;');
   input.ImportFunction('procedure setSkin(const name: string; tiled: boolean);');
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
   RegisterFunction('setSkin', @setSkin);
end;

procedure RegisterScriptUnit(engine: TScriptEngine);
begin
   engine.RegisterUnit('messages', RegisterMessagesC, RegisterMessagesE);
end;

procedure SerializeMessageState(writer: TdwsJSONWriter);
begin
   writer.BeginObject;
      writer.WriteName('Visible'); writer.WriteBoolean(GMenuEngine.boxVisible);
      writer.WriteName('Position'); writer.WriteInteger(ord(GMenuEngine.position));
      writer.WriteName('Cautious'); writer.WriteBoolean(FMboxCautious);
      writer.WriteName('Modal'); writer.WriteBoolean(FMboxModal);
      writer.WriteName('Portrait');
      writer.BeginObject;
         GMenuEngine.SerializePortrait(writer);
      writer.EndObject;
   writer.EndObject;
end;

procedure DeserializeMessageState(obj: TdwsJSONObject);
var
   portrait: TdwsJSONObject;
begin
   GMenuEngine.boxVisible := obj.Items['Visible'].AsBoolean;
   setMessageBoxPosition(TMboxLocation(obj.Items['Position'].AsInteger));
   obj.Items['Visible'].Free;
   obj.Items['Position'].Free;
   obj.CheckRead('Cautious', FMboxCautious);
   obj.CheckRead('Modal', FMboxModal);
   portrait := obj.Items['Portrait'] as TdwsJSONObject;
   GMenuEngine.DeserializePortrait(portrait);
   portrait.Free;
   obj.CheckEmpty;
end;

initialization
   FMessageLock := TCriticalSection.Create;
finalization
   FMessageLock.Free;
end.
