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

interface
uses
   classes;

type
   tX80 = array[1..10] of byte;

   intX80 = ^TBerConverter;
   TBerConverter = record
   private
      value: tX80;
      len: shortint;
   public
      class function Create(theFile: TStream): intX80; overload; static;

      procedure read (theFile: TStream);
      function getData: integer;
      property size: shortint read len;
   end;

implementation

threadvar
   singleton: TBerConverter;

class function TBerConverter.Create(theFile: TStream): intX80;
begin
   singleton.read(theFile);
   result := @singleton;
end;

{$Q-} {$R-}
function TBerConverter.getData: integer;
var
   exponent: byte;
begin
   result := 0;
   for exponent := 1 to len do
      result :=  (result shl 7) +  (value[exponent] mod 128);
end;

procedure TBerConverter.read(theFile: TStream);
var
   i: byte;
begin
   i := 0;
   repeat
      inc(i);
      theFile.Read(value[i], 1)
   until value[i] < $80;
   len := i;
end;

end.
