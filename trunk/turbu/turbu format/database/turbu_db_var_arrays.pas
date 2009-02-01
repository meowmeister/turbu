unit turbu_db_var_arrays;
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
   classes;

type
   TVarSection = class(TObject)
   private
      FLength: word;
{$IFNDEF PRO}
      FVarNames: array of string;

      function getName(x: word): string;
      procedure setName(x: word; data: string);
{$ENDIF}
   public
      constructor create(input: TStream; size: word);
      property len: word read FLength;
{$IFNDEF PRO}
      destructor Destroy; override;
      property name[x: word]: string read getName write setname;
{$ENDIF}
   end;

   TSwitchSection = class(TObject)
   private
      FLength: word;
{$IFNDEF PRO}
      FSwitchNames: array of string;

      function getName(x: word): string;
      procedure setName(x: word; data: string);
{$ENDIF}
   public
      constructor create(input: TStream; size: word);
      property len: word read FLength;
{$IFNDEF PRO}
      destructor Destroy; override;
      property name[x: word]: string read getName write setname;
{$ENDIF}
   end;

implementation
uses
   windows, sysUtils,
   commons, BER, fileIO;

   procedure fillInSwitchStr(const expected: byte; out theResult: ansiString); forward;

{ TVarSection }
{$IFNDEF PRO}
constructor TVarSection.create(input: TStream; size: word);
var
  I: Integer;
  dummy: string;
  converter: intX80;
begin
   FLength := size;
   setLength(FVarNames, size + 1);
   for I := 1 to size do
   begin
      converter := intX80.Create(input);
      if converter.getData <> i then
         raise EParseMessage.create('Switch value x' + intToHex(i, 2) + ' not found!');
      dummy := string(getStrSec(1, input, fillInSwitchStr));
      while pos('=', dummy) <> 0 do
         dummy[pos('=', dummy)] := ':';
      FVarNames[i] := dummy;
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB string x' + intToHex(i, 2) + '!');
      converter.free;
   end;
end;

destructor TVarSection.Destroy;
begin
   finalize(FVarNames);
   inherited;
end;

function TVarSection.getName(x: word): string;
begin
   result := FVarNames[x];
end;

procedure TVarSection.setName(x: word; data: string);
begin
   FVarNames[x] := data;
end;
{$ELSE}

constructor TVarSection.create(input: TStream; size: word);
var
   I: Integer;
   converter: intX80;
begin
   FLength := size;
   for I := 1 to size do
   begin
      converter := intX80.create(input);
      if converter.getData <> i then
         raise EParseMessage.create('Var value x' + intToHex(i, 2) + ' not found!');
      skipSec(1, input);
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB string x' + intToHex(i, 2) + '!');
   end;

end;
{$ENDIF}

{ TSwitchSection }
{$IFNDEF PRO}
constructor TSwitchSection.create(input: TStream; size: word);
var
  I: Integer;
  dummy: string;
  converter: intX80;
begin
   FLength := size;
   setLength(FSwitchNames, size + 1);
   for I := 1 to size do
   begin
      converter := intX80.Create(input);
      if converter.getData <> i then
         raise EParseMessage.create('Switch value x' + intToHex(i, 2) + ' not found!');
      dummy := string(getStrSec(1, input, fillInSwitchStr));
      while pos('=', dummy) <> 0 do
         dummy[pos('=', dummy)] := ':';
      FSwitchNames[i] := dummy;
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB string x' + intToHex(i, 2) + '!');
      converter.free;
   end;
end;

destructor TSwitchSection.Destroy;
begin
   finalize(FSwitchNames);
   inherited;
end;

function TSwitchSection.getName(x: word): string;
begin
   result := FSwitchNames[x];
end;

procedure TSwitchSection.setName(x: word; data: string);
begin
   FSwitchNames[x] := data;
end;

{$ELSE}

constructor TSwitchSection.create(input: TStream; size: word);
var
   I: Integer;
   converter: intX80;
begin
   FLength := size;
   for I := 1 to size do
   begin
      converter := intX80.Create(input);
      if converter.getData <> i then
         raise EParseMessage.create('Switch value x' + intToHex(i, 2) + ' not found!');
      skipSec(1, input);
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB string x' + intToHex(i, 2) + '!');
   end;
end;
{$ENDIF}

{ Classless }

procedure fillInSwitchStr(const expected: byte; out theResult: ansiString);
begin
   msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInSwitchStr says:', MB_OK);
   raise EMessageAbort.Create
end;

end.
