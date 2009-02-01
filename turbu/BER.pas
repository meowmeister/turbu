unit BER;
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
{$D+}
interface
uses
   classes, windows,
   commons;

type
   tX80 = array[1..10] of byte;

   intX80 = class (TObject)
   private
      value: tX80;
      len: byte;
   public
      constructor Create (theFile: TStream); overload;
      constructor Create (theNum: tX80; length: byte); overload; deprecated;
      constructor write (data: integer; theFile: TStream; writelen: boolean = true);
      procedure read (theFile: TStream);
      procedure setData (theNum: tX80; length: byte);
      function getData: integer;
      function size: byte;
   end;

implementation

constructor intX80.Create (theFile: TStream);
begin
   inherited Create;
   read(theFile);
end;

constructor intX80.Create (theNum: tX80; length: byte);
begin
   inherited Create;
   setData (theNum, length);
end;

constructor intX80.write (data: integer; theFile: TStream; writelen: boolean = true);
var i, dummy: byte;
begin
   inherited Create;
   len := 0;
   repeat
      inc(len);
      value[len] := data mod 128;
      data := data div 128;
   until data = 0;
   if writelen then
      theFile.Write(len, 1);
   for i := len downto 2 do
   begin
      dummy := value[i] + 128;
      theFile.Write(dummy, 1);
   end;
   theFile.Write(value[1], 1);
end;

procedure intX80.setData (theNum: tX80; length: byte);
begin
   value := theNum;
   len := length;
end;

function intX80.getData: integer; {$q-} {$r-}
var
   runningTotal: cardinal;
   exponent: byte;
begin
try
   runningTotal := value[1];
   assert(value[1] < 128, 'BER processing error');
   begin
      for exponent := 2 to len do
      begin
         assert(value[exponent] >= 128, 'BER processing error');
         runningTotal := cardinal(integer(runningTotal) +  (powerWrap(128, exponent - 1) * (value[exponent] - 128)))
      end;
   end;
   result := runningTotal;
except
   on E: EParseMessage do
   begin
      msgBox(E.Message, 'intX80.getData says:', MB_OK);
      raise EMessageAbort.Create;
   end;
end; // end of TRY block
end;

procedure intX80.read(theFile: TStream);
var i, dummy: byte;
begin
   i := 0;
   repeat
   begin
      i := i + 1;
      theFile.Read(value[i], 1)
   end
   until value[i] < $80;
   len := i;
   for i := 1 to trunc(len / 2) do
   begin
      dummy := value[len + 1 - i];
      value[len + 1 - i] := value[i];
      value[i] := dummy
   end
end;

function intX80.size: byte;
begin
   result := len;
end;

end.
