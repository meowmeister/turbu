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
begin
   result := GArchives[archive].fileExists(format('%s\%s', [folder, filename]));
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
   if (result = false) then
      if (ExtractFileExt(filename) = '') then
      begin
         files := TDirectory.GetFiles(GArchives[SFX_ARCHIVE].root, filename + '.*');
         if length(files) = 1 then
         begin
            filename := ExtractFileName(files[0]);
            Exit(true);
         end
         else result := false;
      end
      else if ArchiveFileExists(SFX_ARCHIVE, filename + '.wav', '') then
      begin
         result := true;
         filename := filename + '.wav';
      end;
end;

function MusicExists(var filename: string): boolean;
var
   files: TStringDynArray;
begin
   result := ArchiveFileExists(MUSIC_ARCHIVE, filename, '');
   if (result = false) and (ExtractFileExt(filename) = '') then
   begin
      if pos('[', filename) > 0 then
         filename := StringReplace(filename, '[', '[[]', [rfReplaceAll]);
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
