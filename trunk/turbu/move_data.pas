unit move_data;
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
   turbu_defs;

type
   TMoveRecord = record
      opcode: byte;
      name: string;
      data: array[1..3] of word;
   end;

   TMoveOrder = class(TObject)
   private
      FOpcodes: array of TMoveRecord;
      FString: ansiString;
      FPointer: word;
      FLoop: boolean;
      FLooped: boolean;

      procedure addOpcode(input: TMoveRecord);
      function getCommand(index: word): TMoveRecord;
      function getLast: smallint;
   public
      constructor Create(data: ansiString; loop: boolean); overload;
      constructor Create(direction: TFacing); overload;
      constructor Assign(copy: TMoveOrder);
      function nextCommand: TMoveRecord;
      procedure setDirection(direction: TFacing);

      property base: ansiString read FString;
      property command[index: word]: TMoveRecord read getCommand;
      property index: word read FPointer write FPointer;
      property last: smallint read getLast;
      property loop: boolean read FLoop;
      property looped: boolean read FLooped write FLooped;
   end;

   TRouteSet = array of TMoveOrder;

implementation
uses
   classes, sysUtils,
   BER, fileIO;

{ TMoveOrder }

procedure TMoveOrder.addOpcode(input: TMoveRecord);
begin
   setLength(FOpcodes, length(FOpcodes) + 1);
   FOpcodes[high(FOpcodes)] := input;
end;

function TMoveOrder.getCommand(index: word): TMoveRecord;
begin
   result := FOpcodes[index];
end;

function TMoveOrder.getLast: smallint;
begin
   result := high(FOpcodes);
end;

function TMoveOrder.nextCommand: TMoveRecord;
begin
   if FPointer > high(FOpcodes) then
      if loop then
      begin
         FPointer := 0;
         FLooped := true;
      end else begin
         result.opcode := $30;
         Exit;
      end;
   result := self.command[FPointer];
   inc(FPointer);
end;

constructor TMoveOrder.Assign(copy: TMoveOrder);
var
   i: Integer;
begin
   inherited Create;
   setLength(FOpcodes, length(copy.FOpcodes));
   for i := 0 to high(FOpcodes) do
      FOpcodes[i] := copy.FOpcodes[i];
   FLoop := copy.FLoop;
end;

constructor TMoveOrder.Create(direction: TFacing);
begin
   inherited Create;
   setDirection(direction);
end;

procedure TMoveOrder.setDirection(direction: TFacing);
begin
   FLoop := false;
   SetLength(FOpcodes, 1);
   FOpcodes[0].opcode := Ord(direction);
   FPointer := 0;
end;

constructor TMoveOrder.Create(data: ansiString; loop: boolean);
var
   i: word;
   converter: intX80;
   dummy: TMoveRecord;
   numStream: TStream;
begin
   inherited Create;
   numStream := TMemoryStream.Create;
   try
      numStream.Write(data[1], length(data));
      numStream.Seek(0, soFromBeginning);
      FString := data;
      FLoop := loop;
      while numStream.Position < numStream.Size do
      begin
         dummy.name := ''; //this doesn't make any real difference in terms of
                           //program execution, but it makes it easier to read
                           //in the debugger
         numStream.Read(dummy.opcode, 1);
         assert(dummy.opcode in [0..$29]);
         if dummy.opcode in [$20..$23] then
         begin
            if dummy.opcode in [$20, $21] then
            begin
               converter := TBerConverter.Create(numStream);
               dummy.data[1] := converter.getData;
            end
            else if dummy.opcode = $22 then
            begin
               dummy.name := string(getString(numStream));
               converter := TBerConverter.Create(numStream);
               dummy.data[1] := converter.getData;
            end
            else if dummy.opcode = $23 then
            begin
               dummy.name := string(getString(numStream));
               for i := 1 to 3 do
               begin
                  converter := TBerConverter.Create(numStream);
                  dummy.data[i] := converter.getData;
               end;
            end;
         end;
         addOpcode(dummy);
      end;
   finally
      numStream.Free;
   end;
end;

end.
