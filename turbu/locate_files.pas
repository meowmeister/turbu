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

implementation
uses sysUtils,
     commons, xyz_lib;

const
   AUDIO_EXTENSION: array[1..8] of string = ('.mid', '.wav', '.ogg', '.mp3', '.it', '.xm', '.s3m', '.mod');
   GRAPHIC_EXTENSION: array[1..4] of string = ('.png', '.bmp', '.xyz', '.gif');

function scanFile(var filename: string; const folder: string; list: array of string): boolean;
var
   lFilename: string;
   extension: string;
begin
   for extension in list do
   begin
      lFileName := folder + fileName + extension;
      if FileExists(lFilename) then
      begin
         filename := lFilename;
         Exit(true);
      end;
   end;
   result := false;
end;


procedure findAudio(var filename: string; const folder: string);
begin
   if scanFile(filename, IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GCurrentFolder) + folder), AUDIO_EXTENSION) then
      Exit;
   if scanFile(filename, IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(rtpLocation) + folder), AUDIO_EXTENSION) then
      Exit;

   filename := '';
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
begin
   if scanFile(filename, IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GCurrentFolder) + folder), GRAPHIC_EXTENSION) then
      Exit;
   if scanFile(filename, IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(rtpLocation) + folder), GRAPHIC_EXTENSION) then
      Exit;

   filename := '';
end;

end.
