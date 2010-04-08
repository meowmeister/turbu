unit strtok;
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
* Delphi String Tokenizer written by Zarko Gajic.  The original code can be
* found at http://delphi.about.com/cs/adptips2002/a/bltip0902_2.htm
*
*****************************************************************************
*
* This file was last edited by Mason Wheeler.  He can be reached for support
* at www.turbu-rpg.com.
*****************************************************************************}

interface
   uses Classes;

{Returns the next token (substring)
from string S, starting at index
StartPos and ending 1 character
before the next occurrence of
Separator (or at the end of S,
whichever comes first).}
{StartPos returns the starting
position for the next token, 1
more than the position in S of
the end of this token}
function GetNextToken(Const S: string; Separator: char; var StartPos: integer): String;

{Splits a string containing designated
separators into tokens and adds
them to MyStringList NOTE: MyStringList
must be Created before being passed to this
procedure and Freed after use}
procedure Split(const S: String; Separator: Char; MyStringList: TStringList);

{Used to join 2 strings with a
separator character between them and
can be used in a Join function}
{The StringLimit parameter prevents
the length of the Result String
from exceeding a preset maximum}
function AddToken(const aToken, S: String; Separator: Char; StringLimit: integer): String;

function inString(const input: string; const character: char; const start: cardinal = 1): integer;
function inStringA(const input: ansiString; const character: ansiChar; const start: cardinal = 1): integer;
procedure strInsert(var input: string; const character: char; const position: integer);
procedure strInsertAnsi(var input: ansiString; const character: ansiChar; const position: integer);
function getLastToken(const S: String; Separator: Char): string;
function stripLastToken(const s: string; separator: char): string;

implementation
Uses Sysutils, strUtils;

function stripLastToken(const s: string; separator: char): string;
var
   dummy: string;
begin
   dummy := getLastToken(s, separator);
   if s <> dummy then   
      result := LeftStr(s, length(s) - (length(dummy) + 1))
   else result := '';
end;

function GetNextToken(Const S: string; Separator: char; var StartPos: integer): String;
var Index: integer;
begin
   Result := '';

{Step over repeated separators}
   While ((StartPos <= length(S)) and (S[StartPos] = Separator)) do
    StartPos := StartPos + 1;

   if StartPos > length(S) then Exit;

{Set Index to StartPos}
   Index := StartPos;

{Find the next Separator}
   While (Index <= length(S))
   and (S[Index] <> Separator) do
    Index := Index + 1;

{Copy the token to the Result}
   Result := Copy(S, StartPos, Index - StartPos) ;

{SetStartPos to next Character after the Separator}
   StartPos := Index + 1;
end;

procedure Split(const S: String; Separator: Char; MyStringList: TStringList);
var Start: integer;
begin
   Start := 1;
   While Start <= Length(S) do
     MyStringList.Add(GetNextToken(S, Separator, Start)) ;
end;

function getLastToken(const S: String; Separator: Char): string;
var
   start: integer;
begin
   start := 1;
   while start <= length(S) do
      result := GetNextToken(S, Separator, Start);
end;

function AddToken(const aToken, S: String; Separator: Char; StringLimit: integer): String;
begin
   if Length(aToken) + Length(S) < StringLimit then
     begin
       {Add a separator unless the
        Result string is empty}
       if S = '' then
         Result := ''
       else Result := S + Separator;

       {Add the token}
       Result := Result + aToken;
     end
   else
   {if the StringLimit would be
   exceeded, raise an exception}
     Raise Exception.Create('Cannot add token') ;
end;

function inString(const input: string; const character: char; const start: cardinal = 1): integer;
var position: integer;
begin
   result := -1;
   for position := start to length(input) do
      if input[position] = character then
      begin
         result := position;
         Exit;
      end;
end;

function inStringA(const input: ansiString; const character: ansiChar; const start: cardinal = 1): integer;
var position: integer;
begin
   result := -1;
   for position := start to length(input) do
      if input[position] = character then
      begin
         result := position;
         Exit;
      end;
end;

procedure strInsertAnsi(var input: ansiString; const character: ansiChar; const position: integer);
var
   i: integer;
begin
   if position > length(input) then
      raise Exception.Create('StrInsert position out of bounds!');
   setLength(input, length(input) + 1);
   for i := length(input) - 1 downto position do
      input[i + 1] := input[i];
   input[position] := character;
end;

procedure strInsert(var input: string; const character: char; const position: integer);
var
   i: integer;
begin
   if position > length(input) then
      raise Exception.Create('StrInsert position out of bounds!');
   setLength(input, length(input) + 1);
   for i := length(input) - 1 downto position do
      input[i + 1] := input[i];
   input[position] := character;
end;

end.
