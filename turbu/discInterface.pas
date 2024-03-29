unit discInterface;
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
   archiveInterface;

   function openFolder(filename: string): IArchive;
   function newFolder(filename: string): IArchive;

implementation
uses
   Types, classes, sysUtils, Windows, Generics.Collections, IOUtils;

type
   TDiscArchive = class(TInterfacedObject, IArchive)
   private
      FRoot: string;
      function adjustFilename(name: string): string;
   private //IArchive implementation
      function getFile(key: string): TStream;
      procedure writeFile(key: string; theFile: TStream);
      function allFiles(folder: string = ''): TArray<string>;
      function countFiles(filter: string): integer;
      function MakeValidFilename(const path, value: string; expectedNumber: integer = 1): TFilenameData;
      procedure deleteFile(name: string);
      function FileExists(key: string): boolean;
      procedure createFolder(name: string);
      function getRoot: string;
   public
      constructor Create(root: string);
      destructor Destroy; override;
   end;

{$WARN SYMBOL_DEPRECATED OFF} {$WARN SYMBOL_PLATFORM OFF}
procedure DelTree(const Directory: TFileName);
var
   DrivesPathsBuff: array[0..1024] of char;
   DrivesPaths: string;
   len: longword;
   ShortPath: array[0..MAX_PATH] of char;
   dir: TFileName;

   procedure rDelTree(const Directory: TFileName);
   // Recursively deletes all files and directories
   // inside the directory passed as parameter.
   var
      SearchRec: TSearchRec;
      Attributes: LongWord;
      ShortName, FullName: TFileName;
      pname: pchar;
   begin
      if FindFirst(Directory + '*', faAnyFile and not faVolumeID, SearchRec) = 0 then
      begin
         try
            repeat // Processes all files and directories
               if SearchRec.FindData.cAlternateFileName[0] = #0 then
                  ShortName := SearchRec.Name
               else
                  ShortName := SearchRec.FindData.cAlternateFileName;
               FullName := Directory + ShortName;
               if (SearchRec.Attr and faDirectory) <> 0 then
               begin
                  // It's a directory
                  if (ShortName <> '.') and (ShortName <> '..') then
                     rDelTree(FullName + '\');
               end else begin
                  // It's a file
                  pname := PChar(FullName);
                  Attributes := GetFileAttributes(pname);
                  if Attributes = $FFFFFFFF then
                     raise EInOutError.Create(SysErrorMessage(GetLastError));
                  if (Attributes and FILE_ATTRIBUTE_READONLY) <> 0 then
                     SetFileAttributes(pname, Attributes and not FILE_ATTRIBUTE_READONLY);
                  if not Windows.DeleteFile(pname) then
                     raise EInOutError.Create(SysErrorMessage(GetLastError));
               end;
            until FindNext(SearchRec) <> 0;
         except
            sysUtils.FindClose(SearchRec);
            raise;
         end;
         sysUtils.FindClose(SearchRec);
      end;
      if Pos(#0 + Directory + #0, DrivesPaths) = 0 then
      begin
         // if not a root directory, remove it
         pname := PChar(Directory);
         Attributes := GetFileAttributes(pname);
         if Attributes = $FFFFFFFF then
            raise EInOutError.Create(SysErrorMessage(GetLastError));
         if (Attributes and FILE_ATTRIBUTE_READONLY) <> 0 then
           SetFileAttributes(pname, Attributes and not FILE_ATTRIBUTE_READONLY);
         if not Windows.RemoveDirectory(pname) then
            raise EInOutError.Create(SysErrorMessage(GetLastError));
      end;
   end;

begin
   DrivesPathsBuff[0] := #0;
   len := GetLogicalDriveStrings(1022, @DrivesPathsBuff[1]);
   if len = 0 then
      raise EInOutError.Create(SysErrorMessage(GetLastError));
   SetString(DrivesPaths, DrivesPathsBuff, len + 1);
   DrivesPaths := Uppercase(DrivesPaths);
   len := GetShortPathName(PChar(Directory), ShortPath, MAX_PATH);
   if len = 0 then
      raise EInOutError.Create(SysErrorMessage(GetLastError));
   SetString(dir, ShortPath, len);
   dir := Uppercase(dir);
   rDelTree(IncludeTrailingPathDelimiter(dir));
end;
{$WARN SYMBOL_DEPRECATED ON} {$WARN SYMBOL_PLATFORM ON}

constructor TDiscArchive.Create(root: string);
begin
   inherited Create;
   FRoot := root;
end;

procedure TDiscArchive.createFolder(name: string);
begin
   ForceDirectories(IncludeTrailingPathDelimiter(FRoot) + name);
end;

destructor TDiscArchive.Destroy;
begin
   inherited;
end;

function TDiscArchive.allFiles(folder: string = ''): TArray<string>;
var
   filter, path: string;
   strings: TStringDynArray;
   i: integer;
begin
   if pos('.', folder) > 0 then
   begin
      filter := ExtractFileName(folder);
      folder := ExtractFilePath(folder);
   end
   else filter := '*.*';
   path := IncludeTrailingPathDelimiter(TPath.Combine(FRoot, folder));
   strings := TDirectory.GetFiles(path, filter);
   SetLength(result, length(strings));
   for i := 0 to high(strings) do
      result[i] := copy(strings[i], length(path) + 1);
end;

function TDiscArchive.countFiles(filter: string): integer;
begin
   result := length(TDirectory.GetFiles(TPath.Combine(FRoot, ExtractFilePath(filter)), ExtractFileName(filter)));
end;

function TDiscArchive.adjustFilename(name: string): string;
var
   path: string;
begin
   path := FRoot;
   if pos(path, name) <> 1 then
   begin
      if pos(path, name) = 1 then
         result := FRoot + name
      else result := path + name;
   end
   else result := name;
end;

function TDiscArchive.FileExists(key: string): boolean;
begin
   key := adjustFilename(key);
   result := sysUtils.FileExists(key);
end;

function TDiscArchive.getFile(key: string): TStream;
var
   filestream: TFileStream;
begin
   try
      key := adjustFilename(key);
      fileStream := TFileStream.Create(key, fmOpenRead);
      try
         result := TMemoryStream.Create;
         TMemoryStream(result).SetSize(filestream.Size);
         result.CopyFrom(fileStream, fileStream.Size);
         result.seek(0, soFromBeginning);
      finally
         fileStream.Free;
      end;
   except
      on E: Exception do
         raise EArchiveError.Create(E.Message);
   end;
end;

function TDiscArchive.GetRoot: string;
begin
   result := FRoot;
end;

procedure TDiscArchive.deleteFile(name: string);
begin
   if pos(FRoot, name) <> 1 then
      name := FRoot + name;
   if not sysUtils.DeleteFile(name) then
      raise EArchiveError.CreateFmt('Could not delete file %s: file not found.', [name]);
end;

function TDiscArchive.MakeValidFilename(const path, value: string; expectedNumber: integer = 1): TFilenameData;

   function FindInvalidCharacters(const fileName : string): TSysCharset;
   const
      InvalidCharacters: TSysCharset = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
   var
      c: char;
   begin
      result := [];

      if fileName <> '' then
      begin
         for c in fileName do
         begin
            if CharInSet(c, InvalidCharacters) then
               include(result, ansiChar(c));
         end;
      end;
   end;

var
   rogue: ansichar;
   rogueChars: TSysCharset;
   checkname, basename, ext: string;
   duplicates: integer;
   baseFolder: string;
begin
   checkname := value;
   rogueChars := FindInvalidCharacters(checkname);
   for rogue in rogueChars do
      checkname := StringReplace(checkname, char(rogue), '', [rfReplaceAll]);
   ext := ExtractFileExt(checkname);
   basename := Copy(checkname, 1, length(checkname) - length(ext));
   checkname := basename;
   if (expectedNumber = 1) or (fileExists(IncludeTrailingPathDelimiter(FRoot)
                                + checkname + format(' (%d)', [expectedNumber]) + ext)) then
   begin
      duplicates := 1;
      if path = '' then
         baseFolder := FRoot
      else baseFolder := TPath.Combine(FRoot, path);
      baseFolder := IncludeTrailingPathDelimiter(baseFolder);
      while fileExists(baseFolder + checkname + ext) do
      begin
         inc(duplicates);
         checkname := basename + format(' (%d)', [duplicates]);
      end;
   end
   else duplicates := expectedNumber;
   result.name := checkName + ext;
   result.duplicates := duplicates;
end;

procedure TDiscArchive.writeFile(key: string; theFile: TStream);
var
   pos: int64;
   dummy: TFileStream;
   filename, foldername: string;
begin
   pos := theFile.Position;
   filename := FRoot + key;
   if system.pos(key, PathDelim) <> -1 then
   begin
      foldername := ExtractFilePath(filename);
      if not ForceDirectories(foldername) then
         raise EArchiveError.Create('Unable to create folder ' + foldername);
   end;
   if sysUtils.fileExists(filename) then
      sysUtils.DeleteFile(filename);
   dummy := TFileStream.Create(filename, fmCreate);
   try
      theFile.Seek(0, soFromBeginning);
      dummy.CopyFrom(theFile, theFile.Size);
   finally
      dummy.free;
      theFile.Seek(pos, soFromBeginning);
   end;
end;

function openFolder(filename: string): IArchive;
begin
   if not directoryExists(filename) then
      raise EArchiveError.CreateFmt('Unable to open project folder %s', [filename]);
   result := TDiscArchive.Create(IncludeTrailingPathDelimiter(filename));
end;

function newFolder(filename: string): IArchive;
begin
   if directoryExists(filename) then
   try
      deltree(filename);
   except
      on E: EOSError do
         raise EArchiveError.CreateFmt('Unable to delete folder %s: %s', [filename, E.Message]);
   end;

   try
      if not CreateDir(filename) then
         RaiseLastOSError;
   except
      on E: EOSError do
         raise EArchiveError.CreateFmt('Unable to create folder %s: %s', [filename, E.Message]);
   end;
   result := TDiscArchive.Create(IncludeTrailingPathDelimiter(filename));
end;

end.
