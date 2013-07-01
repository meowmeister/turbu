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
unit rs_characters;

interface
uses
   turbu_script_engine;

   procedure AddItem(id, quantity: integer);
   procedure RemoveItem(id, quantity: integer);
   procedure heroJoin(id: integer);
   procedure heroLeave(id: integer);
   procedure addExp(id, number: integer; notify: boolean);
   procedure RemoveExp(id, number: integer);
   procedure AddLevels(id, number: integer; showMessage: boolean);
   procedure RemoveLevels(hero, count: integer);

   procedure RegisterScriptUnit(engine: TScriptEngine);

implementation
uses
   turbu_2k_environment, turbu_constants,
   rsCompiler, rsExec;

procedure AddItem(id, quantity: integer);
begin
   GEnvironment.Party.addItem(id, quantity);
end;

procedure RemoveItem(id, quantity: integer);
begin
   GEnvironment.Party.removeItem(id, quantity);
end;

procedure heroJoin(id: integer);
var
   i: integer;
begin
   if (not id in [1..GEnvironment.HeroCount])
   or (GEnvironment.partySize = MAXPARTYSIZE) then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GEnvironment.Party[i] = GEnvironment.Heroes[id] then
         Exit;

   i := GEnvironment.Party.openSlot;
   if i <> 0 then
      GEnvironment.Party[i] := GEnvironment.Heroes[id];
end;

procedure heroLeave(id: integer);
var
   i: integer;
begin
   if (id = 0) or (id > GEnvironment.HeroCount) then
      Exit;

   for I := 1 to MAXPARTYSIZE do
      if GEnvironment.Party[i] = GEnvironment.Heroes[id] then
         GEnvironment.Party[i] := nil;
end;

procedure addExp(id, number: integer; notify: boolean);
begin
   GEnvironment.Party.levelNotify := notify;
   GEnvironment.party.addExp(id, number);
end;

procedure RemoveExp(id, number: integer);
begin
   GEnvironment.party.removeExp(id, number);
end;

procedure AddLevels(id, number: integer; showMessage: boolean);
begin
   GEnvironment.Party.levelNotify := showMessage;
   GEnvironment.party.addLevels(id, number);
end;

procedure RemoveLevels(hero, count: integer);
begin
   GEnvironment.party.removeLevels(hero, count);
end;

procedure RegisterMessagesC(input: TrsTypeImporter);
begin
   input.ImportFunction('procedure AddItem(id, quantity: integer);');
   input.ImportFunction('procedure RemoveItem(id, quantity: integer);');
   input.ImportFunction('procedure heroJoin(id: integer);');
   input.ImportFunction('procedure heroLeave(id: integer);');
   input.ImportFunction('procedure addExp(id, number: integer; notify: boolean)');
   input.ImportFunction('procedure RemoveExp(id, number: integer);');
   input.ImportFunction('procedure AddLevels(id, number: integer; showMessage: boolean);');
   input.ImportFunction('procedure RemoveLevels(hero, count: integer);');
end;

procedure RegisterMessagesE(RegisterFunction: TExecImportCall; RegisterArrayProp: TArrayPropImport);
begin
   RegisterFunction('AddItem', @AddItem);
   RegisterFunction('RemoveItem', @RemoveItem);
   RegisterFunction('heroJoin', @heroJoin);
   RegisterFunction('heroLeave', @heroLeave);
   RegisterFunction('addExp', @addExp);
   RegisterFunction('RemoveExp', @RemoveExp);
   RegisterFunction('AddLevels', @AddLevels);
   RegisterFunction('RemoveLevels', @RemoveLevels);
end;

procedure RegisterScriptUnit(engine: TScriptEngine);
begin
   engine.RegisterUnit('characters', RegisterMessagesC, RegisterMessagesE);
end;

end.
