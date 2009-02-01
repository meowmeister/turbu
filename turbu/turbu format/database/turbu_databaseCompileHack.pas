unit turbu_databaseCompileHack;
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
*****************************************************************************
*
* This is an ugly hack to work around a compiler bug in Delphi 2009. It
* appears to be Generics-related, like a handful of other compiler bugs.
* Hopefully this won't be necessary in the next version.
*****************************************************************************}

interface

   function getSkillFunc(index: integer): TObject;
   function skillFuncIndexOf(value: TObject): integer;

implementation
uses
   turbu_database, turbu_skills;

function getSkillFunc(index: integer): TObject;
begin
   result := GDatabase.skillFunc[index];
end;

function skillFuncIndexOf(value: TObject): integer;
begin
   result := GDatabase.skillFunc.indexOf(value as TSkillGainRecord);
end;

end.
