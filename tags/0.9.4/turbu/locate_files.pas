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
procedure findMovie(var filename: string);
procedure findGraphic(var filename: string; const folder: string);
function findGraphicF(filename: string; const folder: string): string;
function findSoundF(filename: string; const folder: string): string;
function findMovieF(filename: string; const folder: string): string;

type
   TLocateFileFunc = function(filename: string; const folder: string): string;

var
   rtpLocation: string;

implementation
uses
   sysUtils,
   commons, xyz_lib, ArchiveInterface;

const
   AUDIO_EXTENSION: array[1..8] of string = ('.mid', '.wav', '.ogg', '.mp3', '.it', '.xm', '.s3m', '.mod');
   GRAPHIC_EXTENSION: array[1..4] of string = ('.png', '.bmp', '.xyz', '.gif');
   MOVIE_EXTENSION: array[1..1] of string = ('.avi');

function scanFile(var filename: string; const folder: string; list: array of string): boolean;
var
   lFilename: string;
   extension: string;
begin
   if pos('.', filename) = 0 then
   begin
      for extension in list do
      begin
         lFilename := folder + fileName + extension;
         if FileExists(lFilename) then
         begin
            filename := lFilename;
            Exit(true);
         end;
      end;
      result := false;
   end
   else begin
      lFilename := folder + filename;
      result := FileExists(lFilename);
      if result then
         filename := lFilename;
   end;
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

procedure findMovie(var filename: string);
const FOLDER = 'Movie';
begin
   if scanFile(filename, IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GCurrentFolder) + FOLDER), AUDIO_EXTENSION) then
      Exit;
   if scanFile(filename, IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(rtpLocation) + FOLDER), AUDIO_EXTENSION) then
      Exit;

   filename := '';
end;

function findGraphicF(filename: string; const folder: string): string;
begin
   findGraphic(filename, folder);
   result := filename;
end;

function findSoundF(filename: string; const folder: string): string;
begin
   findAudio(filename, folder);
   result := filename;
end;

function findMovieF(filename: string; const folder: string): string;
begin
   findMovie(filename);
   result := filename;
end;

end.
