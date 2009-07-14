unit image_callback_hack;

interface
uses
   SDL_ImageManager;

procedure assignCallbacks(images: TSdlImages);

implementation
uses
   classes,
   archiveInterface, turbu_constants,
   sdl, sdlstreams;

function ALoader(filename, keyname: string): PSDL_RWops;
var
   stream: TStream;
begin
   stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
   result := sdlstreams.SDLStreamSetup(stream);
end;

procedure ACallback(var rw: PSdl_RWops);
begin
   (TObject(rw.unknown) as TStream).Free;
   SDL_FreeRW(rw);
end;

procedure assignCallbacks(images: TSdlImages);
begin
   Images.ArchiveCallback := aCallback;
   Images.ArchiveLoader := aLoader;
end;

end.
