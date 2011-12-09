unit ArchiveUtils;

interface

   function GraphicExists(const filename, folder: string): boolean;

implementation
uses
   SysUtils,
   ArchiveInterface;

function GraphicExists(const filename, folder: string): boolean;
var
   current: string;
begin
   current := GArchives[IMAGE_ARCHIVE].currentFolder;
   try
      GArchives[IMAGE_ARCHIVE].currentFolder := folder;
      result := GArchives[IMAGE_ARCHIVE].fileExists(filename);
   finally
      GArchives[IMAGE_ARCHIVE].currentFolder := current;
   end;
end;

end.
