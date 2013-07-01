unit asdbInterface;
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
   archiveInterface,
   AsphyreDb;

   function openAsdb(filename: string; password: string  = ''): IArchive;
   function newAsdb(filename: string): IArchive;

implementation
uses
   classes, sysUtils, Generics.Collections;

type
   TAsdbCollection = class(TEnumerable<string>)
   private
      FArchive: TASDb;
      FPath: string;

      type TEnumerator = class(TEnumerator<string>)
      private
         FArchive: TASDb;
         FPath: string;
         FIndex: integer;
      protected
         function DoGetCurrent: string; override;
         function DoMoveNext: Boolean; override;
      public
         constructor Create(archive: TASDb; path: string);
      end;
   protected
      function DoGetEnumerator: TEnumerator<string>; override;
   public
      constructor Create(archive: TASDb);
      property path: string write FPath;
   end;

   TAsdbArchive = class(TInterfacedObject, IArchive)
   private
      FArchive: TASDb;
      FCollection: TAsdbCollection;
   public
      constructor Create(archive: TASDb);
      destructor Destroy; override;

      function getFile(key: string): TStream;
      procedure writeFile(key: string; theFile: TStream);
      function allFiles(folder: string): TEnumerable<string>;
   end;

constructor TAsdbArchive.Create(archive: TASDb);
begin
   inherited Create;
   FArchive := archive;
   FCollection := TAsdbCollection.Create(FArchive);
end;

destructor TAsdbArchive.Destroy;
begin
   FArchive.Free;
   FCollection.Free;
   inherited Destroy;
end;

function TAsdbArchive.getFile(key: string): TStream;
begin
   result := TMemoryStream.Create;
   if not FArchive.ReadStream(key, result) then
      raise EArchiveError.Create('Unable to read ' + key + ' from the ASDB archive!');
   result.Seek(0, soFromBeginning);
end;

procedure TAsdbArchive.writeFile(key: string; theFile: TStream);
var
   pos: int64;
begin
   pos := theFile.Position;
   theFile.Seek(0, soFromBeginning);
   if not FArchive.WriteStream(key, theFile, 0) then
      raise EArchiveError.Create('Unable to write ' + key + ' to the ASDB archive!');
   theFile.Seek(pos, soFromBeginning);
end;

function TAsdbArchive.allFiles(folder: string): TEnumerable<string>;
begin
   FCollection.path := folder;
   result := FCollection;
end;

function openAsdb(filename: string; password: string  = ''): IArchive;
var
   database: TASDb;
begin
   if not fileExists(filename) then
      raise EArchiveError.Create('Unable to open ASDb file ' + filename);
   database := TASDb.Create;
   database.FileName := filename;
   if password <> '' then
      database.SetPassword(shortString(password));
   database.OpenMode := opUpdate;
   if not database.Update then
      raise EArchiveError.Create('Unable to open ASDb file ' + filename);
   result := TAsdbArchive.Create(database);
end;

function newAsdb(filename: string): IArchive;
var
   database: TASDb;
begin
   database := TASDb.Create;
   database.FileName := filename;
   database.OpenMode := opOverwrite;
   if not database.Update then
      raise EArchiveError.Create('Unable to open ASDb file ' + filename);
   result := TAsdbArchive.Create(database);
end;

{ TAsdbEnumerator }

constructor TAsdbCollection.Create(archive: TASDb);
begin
   inherited Create;
   FArchive := archive;
end;

function TAsdbCollection.DoGetEnumerator: TEnumerator<string>;
begin
   result := TEnumerator.Create(FArchive, FPath);
end;

{ TAsdbCollection.TEnumerator }

constructor TAsdbCollection.TEnumerator.Create(archive: TASDb; path: string);
begin
   inherited Create;
   FArchive := archive;
   FPath := path;
   FIndex := -1;
end;

function TAsdbCollection.TEnumerator.DoGetCurrent: string;
begin
   result := FArchive.RecordKey[FIndex];
end;

function TAsdbCollection.TEnumerator.DoMoveNext: Boolean;
var
   count: integer;
begin
   count := FArchive.RecordCount;
   if FIndex >= count then
      Exit(False);
   repeat
      Inc(FIndex);
   until (FIndex >= count) or (FPath = '') or (pos(FPath, FArchive.RecordKey[FIndex]) = 1);
   result := FIndex < count;
end;

end.
