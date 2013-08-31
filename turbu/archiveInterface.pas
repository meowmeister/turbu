unit archiveInterface;
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
   classes, sysUtils, Generics.Collections;

type
   EArchiveError = class(Exception);

   TFilenameData = record
      name: string;
      duplicates: integer;
   end;

   IArchive = interface(IInterface)
      function fileExists(name: string): boolean;
      function getFile(key: string): TStream;
      procedure writeFile(key: string; theFile: TStream);
      function allFiles(folder: string = ''): TArray<string>;
      function countFiles(filter: string): integer;
      function makeValidFilename(const path, value: string; expectedNumber: integer = 1): TFilenameData;
      function getRoot: string;
      procedure deleteFile(name: string);
      procedure createFolder(name: string);
      property root: string read getRoot;
   end;

   TArchiveList = class(TList<IArchive>)
   public
      procedure clearFrom(value: cardinal);
   end;

var
   GArchives: TArchiveList;

const
   //archive constants
   BASE_ARCHIVE      = 0;
   MAP_ARCHIVE       = 1;
   IMAGE_ARCHIVE     = 2;
   SCRIPT_ARCHIVE    = 3;
   MUSIC_ARCHIVE     = 4;
   SFX_ARCHIVE       = 5;
   VIDEO_ARCHIVE     = 6;

implementation

{ TArchiveList }

procedure TArchiveList.clearFrom(value: cardinal);
var
   i: integer;
begin
   for I := self.Count - 1 downto value do
      self.Delete(i);
end;

initialization
   GArchives := TArchiveList.Create;
finalization
   FreeAndNil(GArchives);
end.
