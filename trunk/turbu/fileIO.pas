unit fileIO;
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
  Windows, SysUtils, Classes, //system libraries
  BER, commons; //modules

type
   radioSet = (first, second, third);
   PIntHandler = procedure(const expected: byte; out theResult: integer);
   PStrHandler = procedure(const expected: byte; out theResult: ansiString);

procedure skipSec (expected: byte; theFile: TStream; alreadyRead: boolean = false);
function unknownCheck (x, y, z: byte; theFile: TStream): boolean; deprecated;
function getString(theFile: TStream) : ansiString;
function getStrSec (expected: word; theFile: TStream; handleUnex: PStrHandler) : ansiString;
function getRSetSec (expected: byte; theFile: TStream; handleUnex: PIntHandler) : radioSet;
function getNumSec (expected: byte; theFile: TStream; handleUnex: PIntHandler) : integer;
function getChboxSec (expected: byte; theFile: TStream; handleUnex: PIntHandler): boolean;
function getArraySec (expected: byte; theFile: TStream; out theArray): integer;
function getNext(theFile: TStream): byte;
function loadString(inFile: TStream): string;
function peekAhead(thefile: TStream; const expected: byte): boolean; overload;
function peekAhead(thefile: TStream): byte; overload;
procedure saveString(const theString: string; outFile: TStream);
procedure fillInBlankStr(const expected: byte; out theResult: ansiString);
procedure fillInZeroInt(const expected: byte; out theResult: integer);

implementation

procedure foundWrongSection(callername: string; expected, actual: integer); forward;

function unknownCheck (x, y, z: byte; theFile: TStream): boolean;
var
   trueX, trueY, trueZ: byte;
begin
   with theFile do
   begin
      read(trueX, 1);
      read(trueY, 1);
      read(trueZ, 1);
   end;
   if trueX > x then
   begin
      msgBox ('Error: Expected section ' + intToHex(x, 2) + ' not found!', 'UnknownCheck says:', MB_OK);
      result := true;
      theFile.seek (-3, soFromCurrent)
   end
   else if trueX < x then
   begin
      msgBox ('Error: Expected section ' + intToHex(x, 2) + ', but found section ' + intToHex(trueX, 2) + ' stuck in there first.', 'UnknownCheck says:', MB_OK);
      result := true;
      theFile.seek (-3, soFromCurrent)
   end
   else
      result := ((trueY = y) and (trueZ = z))
end;

{This procedure skips a section of a file.  If backUp is passed as TRUE, this
means that the section number has already been read and the "expected" parameter
is assumed to be correct already.}
procedure skipSec (expected: byte; theFile: TStream; alreadyRead: boolean = false);
var dummy: word;
    converter: intX80;
begin
//the next section won't do anything unless alreadyRead is true.  This section
//sets it to true if the section header is the expected one; otherwise it
//SEEKs back one and jumps to the end.
   if alreadyRead = false then
   begin
      converter := TBerConverter.Create(theFile);
      dummy := converter.getData;
      if dummy = expected then
         alreadyRead := true
      else if (dummy > expected) or (dummy = 0) then
         if (dummy > 127) then
            theFile.seek(-2, soFromCurrent)
         else begin
            theFile.Seek(-1, soFromCurrent)
         end
      else
         raise EParseMessage.create('Attempted to skip section x' + intToHex(expected, 2)
               + ', but found section x' + intToHex(dummy, 2) + ' unexpectedly.');
      //end if
   end;

   if alreadyRead = true then
   begin
      converter := TBerConverter.Create(theFile);
      theFile.Seek(converter.getData, soFromCurrent);
   end;
end;

{Reads the next section of the file, expecting an ASCII
string as the resulting data type}
function getString(theFile: TStream): ansiString;
var
   recordLen: integer;
   converter: intX80;
begin
   converter := TBerConverter.create(theFile);
   recordLen := converter.getData;
   setLength (result, recordLen);
   if recordLen > 0 then
      theFile.read(result[1], recordLen);
end;

function getArray (theFile: TStream; out theArray): integer;
var
   recordLen: integer;
   converter: intX80;
begin
   with theFile do
   begin
      converter := TBerConverter.create(theFile);
      recordLen := converter.getData;
      read(theArray, recordLen);
   end; //end of WITH block
   result := recordLen;
end;

function getArraySec (expected: byte; theFile: TStream; out theArray): integer;
const
   callername = 'getArraySec';
var
   i: byte;
begin
   i := 0;
   with theFile do
   begin
      Read(i, 1);
      if i = expected then
         result := getArray(theFile, theArray)
      else
      begin
         foundWrongSection(callername, expected, i);
         Exit(-1);
      end;
   end;
end;

function getStrSec (expected: word; theFile: TStream; handleUnex: PStrHandler) : ansiString;
const
   callername = 'getStrSec';
var
   i: word;
   converter: intX80;
begin
   with theFile do
   begin
      converter := TBerConverter.Create(theFile);
      i := converter.getData;
      if i = expected then
         result := getString(theFile)
      else if i > expected then
      begin
         Seek(-1 * converter.size, soFromCurrent);
         handleUnex(expected, result);
      end
      else if i = 0 then
      begin
         result := '';
         seek(-1, soFromCurrent);
      end
      else
      begin
         foundWrongSection(callername, expected, i);
         Exit;
      end;
   end;
end;

{Reads the next section of the LMU and returns an integer.  If
it's not what's expected, use HandleUnex to fix this.}
function getNumSec (expected: byte; theFile: TStream; handleUnex: PIntHandler) : integer;
const
   callername = 'getNumSec';
var
   recordLen: byte;
   converter: intX80;
   wasExpected: boolean;
begin
   result := 0;
   with theFile do
   begin
      Read(result, 1);
      wasExpected := result = expected;
      if wasExpected then
      begin
         read(recordLen, 1);
         read(result, recordlen);
      end
      else if (result > expected) or (result = 0) then
      begin
         Seek(-1, soFromCurrent);
         handleUnex(expected, result);
      end
      else
      begin
         foundWrongSection(callername, expected, result);
         Exit;
      end;
      if (((result > 128) or (result < 0)) and wasExpected) then
      begin
         Seek(-recordLen, soFromCurrent);
         converter := TBerConverter.Create(theFile);
         result := converter.getData;
      end;
   end;
end;

function getChboxSec (expected: byte; theFile: TStream; handleUnex: PIntHandler): boolean;
const
   callername = 'getChboxSec';
var
   recordLen: byte;
   dummy: integer;
begin
   dummy := 0;
   with theFile do
   begin
      Read(dummy, 1);
      if dummy = expected then
      begin
         read(recordLen, 1);
         read(dummy, recordlen);
         if recordLen > 1 then
            raise EParseMessage.Create('Expected recordLen of 1 for section x' + IntToHex(expected,2) + ' but found ' + IntToStr(recordlen) + ' instead!');
      end
      else if (dummy > expected) or (dummy = 0) then
      begin
         Seek(-1, soFromCurrent);
         handleUnex(expected, dummy);
      end
      else
      begin
         foundWrongSection(callername, expected, dummy);
         Exit(false);
      end;
   end;
   result := (dummy = 1);
end;

function getRsetSec (expected: byte; theFile: TStream; handleUnex: PIntHandler): radioSet;
const
   callername = 'getRsetSec';
var
   recordLen: byte;
   dummy: integer;
begin
   dummy := 0;
   with theFile do
   begin
      Read(dummy, 1);
      if dummy = expected then
      begin
         read(recordLen, 1);
         read(dummy, recordlen);
         if recordLen > 1 then
            raise EParseMessage.Create('Expected recordLen of 1 for section x' + IntToHex(expected,2) + ' but found ' + IntToStr(recordlen) + ' instead!');
      end
      else if dummy > expected then
      begin
         Seek(-1, soFromCurrent);
         handleUnex(expected, dummy);
      end
      else
      begin
         foundWrongSection(callername, expected, dummy);
         Exit(radioSet(-1));
      end;
   end;
   result := radioSet(dummy);
end;

function getNext(theFile: TStream): byte;
begin
   theFile.Read(result, 1);
end;

procedure saveString(const theString: string; outFile: TStream);
var
   len: word;
begin
   len := length(theString);
   outFile.Write(len, 2);
   if len > 0 then
      outFile.write(theString[1], len);
end;

function loadString(inFile: TStream): string;
var
   len: word;
begin
   inFile.read(len, 2);
   if len > 0 then
   begin
      setLength(result, len);
      inFile.read(result[1], len);
   end
   else result := '';
end;

function peekAhead(thefile: TStream; const expected: byte): boolean;
var
   dummy: byte;
begin
   theFile.Read(dummy, 1);
   result := dummy = expected;
   if not result then
      theFile.seek(-1, soFromCurrent);
end;

function peekAhead(thefile: TStream): byte;
var dummy: byte;
begin
   theFile.Read(dummy, 1);
   theFile.seek(-1, soFromCurrent);
   result := dummy;
end;

procedure foundWrongSection(callername: string; expected, actual: integer);
begin
   MsgBox('Expected to find section x' + IntToHex(expected, 2) + ', but found x'
   + IntToHex(actual, 2) + ' hidden away in there!', callername + 'says:');
   raise EMessageAbort.create;
end;

procedure fillInBlankStr(const expected: byte; out theResult: ansiString);
begin
   theResult := '';
end;

procedure fillInZeroInt(const expected: byte; out theResult: integer);
begin
   theResult := 0;
end;

end.
