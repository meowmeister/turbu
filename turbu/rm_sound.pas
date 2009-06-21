unit rm_sound;
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
   classes; //windows libs

type
   TRmMusic = class(TObject)
   private
      FFilename: ansiString;
      FFadein: integer;
      FTempo: integer;
      FVolume: byte;
      FLeftBalance: byte;
      FRightBalance: byte;

      procedure setFilename(value: ansiString); virtual;
   public
      constructor Create(id: word; input: TStream); overload;
      constructor Create(name: string; time, volume, tempo, balance: word); overload;
      constructor assign(const source: TRmMusic);

      property filename: ansiString read FFilename write setFilename;
      property fadeIn: integer read FFadein;
      property tempo: integer read FTempo;
      property left: byte read FLeftBalance;
      property right: byte read FRightBalance;
      property volume: byte read FVolume;
   end;

   TRmSound = class(TRmMusic)
   private
      procedure setFilename(value: ansiString); override;
   public
//      constructor Create(id: word; input: TStream); overload;
      constructor Create(filename: string); overload;
   end;

implementation
uses
   windows, sysUtils, //windows libs
   fileIO, commons, BER, locate_files; //turbu libs

procedure fillInRmSoundInt(const expected: byte; out theResult: integer); forward;

{ TRmSound }

constructor TRmMusic.assign(const source: TRmMusic);
begin
   inherited create;
   FFilename := source.FFilename;
   FFadein := source.FFadein;
   FTempo := source.FTempo;
   FLeftBalance := source.FLeftBalance;
   FRightBalance := source.FRightBalance;
end;

constructor TRmMusic.Create(id: word; input: TStream);
var
   dummy: integer;
begin
   inherited Create;
   if not peekAhead(input, id) then
      Exit;

   TBerConverter.Create(input); //get size block and discard
   filename := getStrSec(1, input, fillInBlankStr);
   FFadein := getNumSec(2, input, fillInRmSoundInt);
   FVolume := getNumSec(3, input, fillInRmSoundInt);
   FTempo := getNumSec(4, input, fillInRmSoundInt);
   dummy := getNumSec(5, input, fillInRmSoundInt);
   FRightBalance := round(254 * (dummy / 100));
   FLeftBalance := 254 - FRightBalance;
   FRightBalance := commons.round(FRightBalance * (FVolume / 100));
   FLeftBalance := commons.round(FLeftBalance * (FVolume / 100));
   assert(peekAhead(input, 0));
end;

constructor TRmMusic.Create(name: string; time, volume, tempo, balance: word);
var
   dummy: integer;
begin
   inherited Create;
   filename := ansiString(name);
   FFadeIn := time;
   FVolume := volume;
   FTempo := tempo;
   dummy := balance;
   FRightBalance := round(254 * (dummy / 100));
   FLeftBalance := 254 - FRightBalance;
   FRightBalance := commons.round(FRightBalance * (FVolume / 100));
   FLeftBalance := commons.round(FLeftBalance * (FVolume / 100));
end;

procedure TRmMusic.setFilename(value: ansiString);
var
   dummy: string;
begin
   dummy := unicodeString(value);
   if (value <> '') and (value <> '(OFF)') then
      findMusic(dummy)
   else dummy := '';
   FFilename := ansiString(dummy);
end;

procedure fillInRmSoundInt(const expected: byte; out theResult: integer);
begin
   case expected of
      2: theResult := 0;
      3, 4: theResult := 100;
      5: theResult := 50;
   else
      begin
         msgBox ('No case implemented for x' + IntToHex(expected, 2) + '!', 'FillInLmtEndInt says:', MB_OK);
         raise EMessageAbort.Create
      end
   end;
end;

{ TRmSound }
{$R-}
{constructor TRmSound.Create(id: word; input: TStream);
var
   converter: intX80;
   volume, dummy: integer;
begin
   if not peekAhead(input, id) then
      Exit;

   converter := intX80.Create(input); //get size block and discard
try
   filename := getStrSec(1, input, fillInBlankStr);
   volume := getNumSec(2, input, fillInRmSoundInt);
   FTempo := getNumSec(3, input, fillInRmSoundInt);
   dummy := getNumSec(4, input, fillInRmSoundInt);
   FRightBalance := round(254 * (dummy / 100));
   FLeftBalance := 254 - FRightBalance;
   FRightBalance := round(FRightBalance * (volume / 100));
   FLeftBalance := round(FLeftBalance * (volume / 100));
   assert(peekAhead(input, 0));
finally
   converter.Free;
end;
end;}
{$R+}

constructor TRmSound.Create(filename: string);
begin
   self.filename := ansiString(filename);
   self.FTempo := 100;
   self.FLeftBalance := 127;
   self.FRightBalance := 127;
end;

procedure TRmSound.setFilename(value: ansiString);
var
   dummy: string;
begin
   dummy := unicodeString(value);
   if (value <> '') and (value <> '(OFF)') then
      findSfx(dummy)
   else dummy := '';
   FFilename := ansiString(dummy);
end;

end.
