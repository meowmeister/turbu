unit locate_files;
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

procedure findMusic(var filename: string);
procedure findSfx(var filename: string);
procedure findGraphic(var filename: string; const folder: string);

var
   rtpLocation: string;
   GXyzHack: boolean;

implementation
uses sysUtils,
     commons, xyz_lib;

const
   AUDIO_EXTENSION: array[1..8] of string = ('.mid', '.wav', '.ogg', '.mp3', '.it', '.xm', '.s3m', '.mod');
   GRAPHIC_EXTENSION: array[1..3] of string = ('.png', '.bmp', '.xyz');

procedure findAudio(var filename: string; const folder: string);
var
   i: word;
   found: boolean;
begin
   found := false;
   i := 0;
   while (not found) and (i < high(AUDIO_EXTENSION)) do
   begin
      inc(i);
      if FileExists(GCurrentFolder + '\' + folder + '\' + fileName + AUDIO_EXTENSION[i]) then
      begin
         fileName := GCurrentFolder + '\' + folder + '\' + fileName + AUDIO_EXTENSION[i];
         found := true;
      end;
   end;

   if not found then
   begin
      i := 0;
      while (not found) and (i < high(AUDIO_EXTENSION)) do
      begin
         inc(i);
         if FileExists(rtpLocation + '\' + folder + '\' + fileName + AUDIO_EXTENSION[i]) then
         begin
            fileName := rtpLocation + '\' + folder + '\' + fileName + AUDIO_EXTENSION[i];
            found := true;
         end;
      end;
   end;

   if not found then
      filename := '';
   //end if
end;

procedure findMusic(var filename: string);
begin
   findAudio(filename, 'Music');
end;

procedure findSfx(var filename: string);
begin
   findAudio(filename, 'Sound');
end;

procedure findGraphic(var filename: string; const folder: string);
var
   i: word;
   found: boolean;
begin
   found := false;
   i := 0;
   while (not found) and (i < high(GRAPHIC_EXTENSION)) do
   begin
      inc(i);
      if FileExists(GCurrentFolder + '\' + folder + '\' + fileName + GRAPHIC_EXTENSION[i]) then
      begin
         fileName := GCurrentFolder + '\' + folder + '\' + fileName + GRAPHIC_EXTENSION[i];
         found := true;
      end;
   end;

   if not found then
   begin
      i := 0;
      while (not found) and (i < high(GRAPHIC_EXTENSION)) do
      begin
         inc(i);
         if FileExists(rtpLocation + '\' + folder + '\' + fileName + GRAPHIC_EXTENSION[i]) then
         begin
            fileName := rtpLocation + '\' + folder + '\' + fileName + GRAPHIC_EXTENSION[i];
            found := true;
         end;
      end;
   end;
{$IFDEF ENGINE}
{
   if ExtractFileExt(filename) = '.bmp' then
      filename := bmpToPng(filename)
   else if ExtractFileExt(filename) = '.xyz' then
   begin
      GXyzHack := true;
      filename := xyzToPng(filename);
   end;}

   //code that's no longer necessary with SDL_Image available
{$ENDIF}

   if not found then
      filename := '';
   //end if
end;

initialization
begin
   GXyzHack := false;
end;

end.
