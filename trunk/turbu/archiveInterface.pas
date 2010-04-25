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
      function allFiles(folder: string = ''): TEnumerable<string>;
      function countFiles(filter: string): integer;
      function makeValidFilename(const value: string; expectedNumber: integer = 1): TFilenameData;
      procedure setCurrentFolder(const value: string);
      function getCurrentFolder: string;
      procedure deleteFile(name: string);
      property currentFolder: string read getCurrentFolder write setCurrentFolder;
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
   DATABASE_ARCHIVE  = 1;
   MAP_ARCHIVE       = 2;
   IMAGE_ARCHIVE     = 3;
   SCRIPT_ARCHIVE    = 4;
   MUSIC_ARCHIVE     = 5;

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
begin
   GArchives := TArchiveList.Create;
end;

finalization
begin
   FreeAndNil(GArchives);
end;

end.
