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
   classes, sysUtils, Windows, Generics.Collections,
   findfile, strtok;

type
   TFileCollection = class(TEnumerable<string>)
   private
      FRoot: string;
      FPath: string;
      FFinder: TFindFile;
      FList: TList<string>;

      procedure setPath(const Value: string);
   protected
      function DoGetEnumerator: TEnumerator<string>; override;
   public
      constructor Create(root: string);
      destructor Destroy; override;

      property path: string write setPath;
   end;

   TDiscArchive = class(TInterfacedObject, IArchive)
   private
      FRoot: string;
      FCollection: TFileCollection;
   public
      constructor Create(root: string);
      destructor Destroy; override;

      function getFile(key: string): TStream;
      procedure writeFile(key: string; theFile: TStream);
      function allFiles(folder: string): TEnumerable<string>;
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
   FCollection := TFileCollection.Create(FRoot);
end;

destructor TDiscArchive.Destroy;
begin
   FCollection.Free;
   inherited;
end;

function TDiscArchive.allFiles(folder: string): TEnumerable<string>;
begin
   FCollection.path := folder;
   result := FCollection;
end;

function TDiscArchive.getFile(key: string): TStream;
begin
   try
      if pos(FRoot, key) <> 1 then
         result := TFileStream.Create(FRoot + key, fmOpenRead)
      else result := TFileStream.Create(key, fmOpenRead);
   except
      on E: Exception do
         raise EArchiveError.Create(E.Message);
   end;
end;

procedure TDiscArchive.writeFile(key: string; theFile: TStream);
var
   pos: int64;
   dummy: TFileStream;
   filename, foldername: string;
begin
   pos := theFile.Position;
   filename := FRoot + key;
   if inString(key, PathDelim) <> -1 then
   begin
      foldername := strtok.stripLastToken(filename, PathDelim);
      if not ForceDirectories(foldername) then
         raise EArchiveError.Create('Unable to create folder ' + foldername);
   end;
   if fileExists(filename) then
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
      raise EArchiveError.Create('Unable to open project folder ' + filename);
   result := TDiscArchive.Create(IncludeTrailingPathDelimiter(filename));
end;

function newFolder(filename: string): IArchive;
begin
   if directoryExists(filename) then
   try
      deltree(filename);
   except
      on E: EOSError do
         raise EArchiveError.Create('Unable to delete folder ' + filename + ':  ' + E.Message);
   end;

   try
      if not CreateDir(filename) then
         RaiseLastOSError;
   except
      on E: EOSError do
         raise EArchiveError.Create('Unable to create folder ' + filename + ':  ' + E.Message);
   end;
   result := TDiscArchive.Create(IncludeTrailingPathDelimiter(filename));
end;

{ TFileCollection }

constructor TFileCollection.Create(root: string);
begin
   inherited Create;
   FRoot := IncludeTrailingPathDelimiter(root);
   FList := TList<string>.create;
   FFinder := TFindFile.Create(FRoot);
end;

destructor TFileCollection.Destroy;
begin
   FList.Free;
   FFinder.Free;
   inherited Destroy;
end;

function TFileCollection.DoGetEnumerator: TEnumerator<string>;
begin
   result := FList.GetEnumerator;
end;

procedure TFileCollection.setPath(const Value: string);
var
   dummy: string;
begin
   FPath := Value;
   FFinder.Path := FRoot + FPath;
   FList.clear;
   for dummy in FFinder.SearchForFiles do
      FList.Add(dummy);
end;

end.
