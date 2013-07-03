unit ArchiveUtils;

interface

   function GraphicExists(var filename: string; const folder: string): boolean;
   function SoundExists(var filename: string): boolean;
   function MusicExists(var filename: string): boolean;

implementation
uses
   SysUtils, Types, IOUtils,
   ArchiveInterface;

function ArchiveFileExists(archive: integer; const filename, folder: string): boolean;
var
   current: string;
begin
   current := GArchives[archive].currentFolder;
   try
      GArchives[archive].currentFolder := folder;
      result := GArchives[archive].fileExists(filename);
   finally
      GArchives[archive].currentFolder := current;
   end;
end;

function GraphicExists(var filename: string; const folder: string): boolean;
begin
   result := ArchiveFileExists(IMAGE_ARCHIVE, filename, folder);
   if (result = false) and (ExtractFileExt(filename) = '') then
   begin
      if ArchiveFileExists(IMAGE_ARCHIVE, filename + '.png', folder) then
      begin
         filename := filename + '.png';
         Exit(true);
      end
      else result := false;
   end;
end;

function SoundExists(var filename: string): boolean;
var
   files: TStringDynArray;
begin
   result := ArchiveFileExists(SFX_ARCHIVE, filename, '');
   if (result = false) and (ExtractFileExt(filename) = '') then
   begin
      files := TDirectory.GetFiles(GArchives[SFX_ARCHIVE].root, filename + '.*');
      if length(files) = 1 then
      begin
         filename := ExtractFileName(files[0]);
         Exit(true);
      end
      else result := false;
   end;
end;

function MusicExists(var filename: string): boolean;
var
   files: TStringDynArray;
begin
   result := ArchiveFileExists(MUSIC_ARCHIVE, filename, '');
   if (result = false) and (ExtractFileExt(filename) = '') then
   begin
      files := TDirectory.GetFiles(GArchives[MUSIC_ARCHIVE].root, filename + '.*');
      if length(files) = 1 then
      begin
         filename := ExtractFileName(files[0]);
         Exit(true);
      end
      else result := false;
   end;
end;

end.