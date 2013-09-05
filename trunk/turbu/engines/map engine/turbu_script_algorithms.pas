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

unit turbu_script_algorithms;
interface
uses
   turbu_heroes;

//this sutff should all be supported script-side, not in Delphi code, but until
//the script revamp, that won't be possible

implementation
uses
   Math, SysUtils;

const
   MAXEXP2K  =  1000000;
   MAXEXP2K3 = 10000000;

type
   DesignNameAttribute = class(TCustomAttribute)
      constructor Create(const name: string);
   end;


[DesignName('RM2K level algorithm')]
function calcExp2k(currentLevel: integer; stdIncrease, addIncrease, correction: integer; dummy: integer): integer;
var
   standard, additional: real;
   i: integer;
begin
   result := 0;
   standard := stdIncrease;
   additional := 1.5 + (addIncrease * 0.01);
   for i := currentLevel - 1 downto 1 do
   begin
      result := result + correction + trunc(standard);
      standard := standard * additional;
      additional := (((currentLevel * 0.002) + 0.8) * (additional - 1) + 1);
   end; //end FOR
   result := min(result, MAXEXP2K);
end;

[DesignName('RM2K3 level algorithm')]
function calcExp2k3(level: integer; primary, secondary, tertiary: integer; dummy: integer): integer;
var
   i: integer;
begin
   result := 0;
   for I := 1 to level - 1 do
      result := result + i;
   result := (result * secondary) + ((level - 1) * (primary + tertiary));
   result := min(result, MAXEXP2k3);
end;

function skillSelectByLevel_display(Level, unused2, unused3, unused4: integer): string;
begin
   result := 'Lv. ' + intToStr(Level);
end;

function skillSelectByEq_display(Item1, Item2, Item3, Item4: integer): string;
begin
   result := 'Eq.';
end;

function skillSelectByBoolean_display(which, unused2, unused3, unused4: integer): string;
begin
   result := 'Switch ' + intToStr(which);
   if which > 0 then
      result := result + ' ON'
   else result := result + ' OFF';
end;

function skillSelectByVar_display(which, value, unused3, unused4: integer): string;
var
   whichvar: integer;
begin
   whichvar := trunc(abs(which));
   result := 'Var ' + intToStr(whichvar);
   if which > 0 then
      result := result + ' >='
   else result := result + ' <=';
   result := result + intToStr(value);
end;

{ DesignNameAttribute }

constructor DesignNameAttribute.Create(const name: string);
begin
   //
end;

initialization
   TRpgHero.RegisterExpFunc('calcExp2k', calcExp2k);
   TRpgHero.RegisterExpFunc('calcExp2k3', calcExp2k3);
end.
