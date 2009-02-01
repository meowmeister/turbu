unit battle_anims;
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
   commons, rm_sound;

type
   TAnimYTarget = (at_top, at_center, at_bottom);
   TFlashTarget = (fl_none, fl_target, fl_screen);

   TAnimEffects = class(TObject)
   private
      FFrame: word;
      FSound: TRmSound;
      FFlashWhere: TFlashTarget;
      FRed: byte;
      FGreen: byte;
      FBlue: byte;
      FPower: byte;
   public
      constructor Create(input: TStream; const id: word);
      destructor Destroy; override;

      property frame: word read FFrame;
      property sound: TRmSound read FSound;
      property flashWhere: TFlashTarget read FFlashWhere;
      property r: byte read FRed;
      property g: byte read FGreen;
      property b: byte read FBlue;
      property a: byte read FPower;
   end;

   TAnimCell = class(TObject)
   private
      FUnknown: integer;
      FIndex: word;
      FX: smallint;
      FY: smallint;
      FZoom: word;
      FColor: TRpgColor;
      FSaturation: byte;
   public
      constructor Create(input: TStream; const id: word);

      property index: word read FIndex;
      property x: smallint read FX;
      property y: smallint read FY;
      property zoom: word read FZoom;
      property color: TRpgColor read FColor;
      property saturation: byte read FSaturation;
   end;

   TAnimFrame = array of TAnimCell;

   TBattleAnim = class(TObject)
   private
      FName: ansiString;
      FFilename: ansiString;
      FTimingSec: array of TAnimEffects;
      FHitsAll: boolean;
      FYTarget: TAnimYTarget;
      FFrameSec: array of TAnimFrame;

      function countEffects: word; inline;
      function getEffect(x: word): TAnimEffects; inline;
      function getFrame(x: word): TAnimFrame; inline;
      function countFrames: word; inline;
   public
      constructor Create(input: TStream; const id: word);
      destructor Destroy; override;

      property name: ansiString read FName;
      property filename: ansiString read FFilename write FFilename;
      property hitsAll: boolean read FHitsAll;
      property yTarget: TAnimYTarget read FYTarget;
      property effect[x: word]: TAnimEffects read getEffect;
      property effects: word read countEffects;
      property frame[x: word]: TAnimFrame read getFrame;
      property frames: word read countFrames;
   end;

implementation
uses sysUtils, windows,
     fileIO, BER, formats;

procedure fillInAnimInt(const expected: byte; out theResult: integer); forward;
procedure fillInAnimFxInt(const expected: byte; out theResult: integer); forward;
procedure fillInCellInt(const expected: byte; out theResult: integer); forward;

{ TAnimEffects }

constructor TAnimEffects.Create(input: TStream; const id: word);
var
   converter: intX80;
begin
   inherited create;
   converter := intX80.Create(input);
   try
      if converter.getData <> id then
         raise EParseMessage.create('Battle Animation FX record ' + intToStr(id) + ' of RPG_RT.LDB not found!');
      FFrame := getNumSec(1, input, fillInAnimFxInt);
      FSound := TRmSound.Create(2, input);
      FFlashWhere := TFlashTarget(getNumSec(3, input, fillInZeroInt));
      FRed := getNumSec(4, input, fillInAnimFxInt);
      FGreen := getNumSec(5, input, fillInAnimFxInt);
      FBlue := getNumSec(6, input, fillInAnimFxInt);
      FPower := getNumSec(7, input, fillInAnimFxInt);
      skipSec(8, input); //no idea what this does.  It showed up in Love and War's database
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB anim fx x' + intToHex(id, 2) + '!');
   //end if
   finally
      converter.free;
   end;
end;

destructor TAnimEffects.Destroy;
begin
   FSound.free;
   inherited;
end;

{ TAnimCell }

constructor TAnimCell.Create(input: TStream; const id: word);
var
   converter: intX80;
begin
   inherited create;
   converter := intX80.Create(input);
   if converter.getData <> id then
      raise EParseMessage.create('Battle Animation FX record ' + intToStr(id) + ' of RPG_RT.LDB not found!');
   converter.free;
   FUnknown := getNumSec(1, input, fillInZeroInt);
   assert(FUnknown = 0);
   FIndex := getNumSec(2, input, fillInZeroInt);
   FX := getNumSec(3, input, fillInZeroInt);
   FY := getNumSec(4, input, fillInZeroInt);
   FZoom := getNumSec(5, input, fillInCellInt);
   FColor.rgba[1] := getNumSec(6, input, fillInCellInt);
   FColor.rgba[2] := getNumSec(7, input, fillInCellInt);
   FColor.rgba[3] := getNumSec(8, input, fillInCellInt);
   FSaturation := getNumSec(9, input, fillInCellInt);
   FColor.rgba[4] := getNumSec($A, input, fillInCellInt);
   if not peekAhead(input, 0) then
      raise EParseMessage.create('Exceptional case found at LDB anim cell x' + intToHex(id, 2) + '!');
   //end if
end;

{ TBattleAnim }

function TBattleAnim.countEffects: word;
begin
   result := high(FTimingSec);
end;

function TBattleAnim.countFrames: word;
begin
   result := high(FFrameSec);
end;

constructor TBattleAnim.Create(input: TStream; const id: word);
var
   converter: intX80;
   i, j: word;
begin
   inherited create;
   converter := intX80.Create(input);
   if converter.getData <> id then
      raise EParseMessage.create('Battle Animation record ' + intToStr(id) + ' of RPG_RT.LDB not found!');
   FName := getStrSec(1, input, fillInBlankStr);
   FFilename := getStrSec(2, input, fillInBlankStr);
   if GProjectFormat = pf_2k3 then
   begin
      for i := 3 to 5 do         
         skipSec(i, input);
   end;

   if not peekAhead(input, 6) then //FX section
      raise EParseMessage.create('Animation Effects section of RPG_RT.LDB not found!');
   converter.read(input); //length statement
   converter.read(input); //quantity
   setLength(FTimingSec, converter.getData + 1);
   FTimingSec[0] := nil;
   for I := 1 to high(FTimingSec) do
      FTimingSec[i] := TAnimEffects.Create(input, i);

   FHitsAll := getChboxSec(9, input, fillInAnimInt);
   FYTarget := TAnimYTarget(getNumSec($A, input, fillInAnimInt));

   if not peekAhead(input, $C) then //Cells section
      raise EParseMessage.create('Animation Cells section of RPG_RT.LDB not found!');
   converter.read(input); //length statement
   converter.read(input); //quantity
   setLength(FFrameSec, converter.getData + 1);
   FFrameSec[0] := nil;
   for I := 1 to high(FFrameSec) do
   begin
      converter.read(input);
      if converter.getData <> i then
         raise EParseMessage.create('Animation Cell section of RPG_RT.LDB not found!');
      if not peekAhead(input, 1) then //Cells section
         raise EParseMessage.create('Animation Cell section of RPG_RT.LDB not found!');
      converter.read(input); //length statement
      converter.read(input); //quantity
      setLength(FFrameSec[i], converter.getData + 1);
      FFrameSec[i, 0] := nil;
      for j := 1 to high(FFrameSec[i]) do
         FFrameSec[i, j] := TAnimCell.create(input, j);
      if not peekAhead(input, 0) then
         raise EParseMessage.create('Exceptional case found at LDB anim cell x' + intToHex(id, 2) + '!');
      //end if
   end;

   //   FFrameSec := getStrSec($C, input, fillInAnimStr);
   if not peekAhead(input, 0) then
      raise EParseMessage.create('Exceptional case found at LDB anim x' + intToHex(id, 2) + '!');
   //end if
   converter.free;
end;

destructor TBattleAnim.Destroy;
var
   i, j: integer;
begin
   for I := low(FTimingSec) to high(FTimingSec) do
      FTimingSec[i].free;
   finalize(FTimingSec);
   for j := low(FFrameSec) to high(FFrameSec) do
   begin
      for I := low(FFrameSec[j]) to high(FFrameSec[j]) do
         FFrameSec[j, i].free;
      finalize(FFrameSec[j]);
   end;
   finalize(FFrameSec);
   inherited;
end;

function TBattleAnim.getEffect(x: word): TAnimEffects;
begin
   result := FTimingSec[x];
end;

function TBattleAnim.getFrame(x: word): TAnimFrame;
begin
   result := FFrameSec[x];
end;

{ Classless }

procedure fillInAnimInt(const expected: byte; out theResult: integer);
begin
{   case expected of
      9: result :=
   end;}
   msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInAnimStr says:', MB_OK);
   raise EMessageAbort.Create
end;

procedure fillInAnimFxInt(const expected: byte; out theResult: integer);
begin
   case expected of
      4..7: theResult := 31;
   else begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInAnimFxInt says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

procedure fillInCellInt(const expected: byte; out theResult: integer);
begin
   case expected of
      5..$A: theResult := 100;
   else begin
      msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'fillInCellInt says:', MB_OK);
      raise EMessageAbort.Create
   end;
   end;
end;

end.
