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
      FShakeWhere: TFlashTarget;
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
      property shakeWhere: TFlashTarget read FShakeWhere;
   end;

   TAnimCell = class(TObject)
   private
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
      FLargeAnim: boolean;

      function countEffects: word; inline;
      function getEffect(x: word): TAnimEffects;
      function getFrame(x: word): TAnimFrame;
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
      property largeAnim: boolean read FLargeAnim;
   end;

   TBattle2Data = class
   private
      FName: ansiString;
      FFilename: ansiString;
      FFrame: integer;
      FUnk04: integer;
      FUnk05: integer;
   public
      constructor Create(input: TStream; const id: word);
      property name: ansiString read FName;
      property filename: ansiString read FFilename;
      property frame: integer read FFrame;
      property unk04: integer read FUnk04;
      property unk05: integer read FUnk05;
   end;

   TBattleAnim2 = class
   private
      FName: ansiString;
      FSpeed: integer;
      FPoses: TArray<TBattle2Data>;
      FWeapons: TArray<TBattle2Data>;
   public
      constructor Create(input: TStream; const id: word);
      destructor Destroy; override;

      property name: ansiString read FName;
      property speed: integer read FSpeed;
      property poses: TArray<TBattle2Data> read FPoses;
      property weapons: TArray<TBattle2Data> read FWeapons;
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
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.createFmt('Battle Animation FX record %d of RPG_RT.LDB not found!', [id]);
   FFrame := getNumSec(1, input, fillInAnimFxInt);
   FSound := TRmSound.Create(2, input);
   FFlashWhere := TFlashTarget(getNumSec(3, input, fillInZeroInt));
   FRed := getNumSec(4, input, fillInAnimFxInt);
   FGreen := getNumSec(5, input, fillInAnimFxInt);
   FBlue := getNumSec(6, input, fillInAnimFxInt);
   FPower := getNumSec(7, input, fillInAnimFxInt);
   FShakeWhere := TFlashTarget(getNumSec(8, input, fillInZeroInt));
   if not peekAhead(input, 0) then
      raise EParseMessage.createFmt('Exceptional case found at LDB anim fx x%s!', [intToHex(id, 2)]);
   //end if
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
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.createFmt('Battle Animation FX record %d of RPG_RT.LDB not found!', [id]);
   assert(getNumSec(1, input, fillInZeroInt) = 0);
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
      raise EParseMessage.createFmt('Exceptional case found at LDB anim cell x%s!', [intToHex(id, 2)]);
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
   dummy: rawByteString;
   i, j: word;
begin
   inherited create;
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.createFmt('Battle Animation record %d of RPG_RT.LDB not found!', [id]);
   FName := getStrSec(1, input, fillInBlankStr);
   FFilename := getStrSec(2, input, fillInBlankStr);
   if GProjectFormat = pf_2k3 then
   begin
      FLargeAnim := GetChboxSec(3, input, FillInZeroInt);
      for i := 4 to 5 do
      begin
         dummy := getStrSec(i, input, fillInBlankStr);
         if dummy <> '' then
            raise EParseMessage.createFmt('Unexpected content in section %d, Battle Animation record %d!', [i, id]);
      end;
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
         raise EParseMessage.createFmt('Exceptional case found at LDB anim cell x%s!', [intToHex(id, 2)]);
      //end if
   end;

   if not peekAhead(input, 0) then
      raise EParseMessage.createFmt('Exceptional case found at LDB anim x%s!', [intToHex(id, 2)]);
end;

destructor TBattleAnim.Destroy;
var
   i, j: integer;
begin
   for I := low(FTimingSec) to high(FTimingSec) do
      FTimingSec[i].free;
   for j := low(FFrameSec) to high(FFrameSec) do
   begin
      for I := low(FFrameSec[j]) to high(FFrameSec[j]) do
         FFrameSec[j, i].free;
   end;
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

{ TBattle2Data }

constructor TBattle2Data.Create(input: TStream; const id: word);
var
   converter: intX80;
begin
   inherited create;
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.createFmt('Battle 2 Data record %d not found!', [id]);
   if peekAhead(input, 0) then
      Exit;

   FName := getStrSec(1, input, fillInBlankStr);
   FFilename := getStrSec(2, input, fillInBlankStr);
   FFrame := getNumSec(3, input, fillInZeroInt);
   FUnk04 := getNumSec(4, input, fillInZeroInt);
   FUnk05 := getNumSec(5, input, fillInZeroInt);
   assert(peekAhead(input, 0));
end;

{ TBattleAnim2 }

constructor TBattleAnim2.Create(input: TStream; const id: word);
var
   converter: intX80;
   i: integer;
begin
   inherited create;
   converter := TBerConverter.Create(input);
   if converter.getData <> id then
      raise EParseMessage.createFmt('Battle 2 record %d not found!', [id]);
   FName := getStrSec(1, input, fillInBlankStr);
   FSpeed := getNumSec(2, input, fillInZeroInt);
   if FSpeed = 0 then
      FSpeed := 20;

   if not peekAhead(input, $A) then //Poses section
      raise EParseMessage.createFmt('Poses section of Anim2 %d not found!', [id]);
   converter.read(input); //length statement
   converter.read(input); //quantity
   setLength(FPoses, converter.getData + 1);
   FPoses[0] := nil;
   for I := 1 to high(FPoses) do
      FPoses[i] := TBattle2Data.Create(input, i);

   if not peekAhead(input, $B) then //Weapons section
      raise EParseMessage.createFmt('Weapons section of Anim2 %d not found!', [id]);
   converter.read(input); //length statement
   converter.read(input); //quantity
   setLength(FWeapons, converter.getData + 1);
   FWeapons[0] := nil;
   for I := 1 to high(FWeapons) do
      FWeapons[i] := TBattle2Data.Create(input, i);

   assert(peekAhead(input, 0));
end;

destructor TBattleAnim2.Destroy;
var
   i: integer;
begin
   for i := 1 to High(FPoses) do
      FPoses[i].Free;
   for i := 1 to High(FWeapons) do
      FWeapons[i].Free;
   inherited Destroy;
end;

{ Classless }

procedure fillInAnimInt(const expected: byte; out theResult: integer);
begin
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
