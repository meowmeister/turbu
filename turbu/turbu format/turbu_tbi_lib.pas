unit turbu_tbi_lib;
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
   classes,
   sdl, SDL_13, sg_defs;

type
   PTbiHeader = ^TTbiHeader;
   TTbiHeader = packed record
      name: array[0..3] of AnsiChar;
      size: integer;
      version: word;
      dirty: wordbool;
      width: word;
      height: word;
   constructor Create(size: TSgPoint; dirty: boolean);
   end;

function saveToTBI(image: PSdlSurface; size: TSgPoint; dirty: boolean): TMemoryStream;
function loadFromTBI(stream: TStream): PSdlSurface;

implementation
uses
   sysUtils,
   turbu_constants,
   sdl_image, sdlstreams;

type
   ETbiError = class(Exception);

const
   TBI_VERSION = 1;

function saveToTBI(image: PSdlSurface; size: TSgPoint; dirty: boolean): TMemoryStream;
var
   header: TTbiHeader;
   rw: PSdl_RWops;
begin
   assert(image.Tag = nil);
   header := TTbiHeader.Create(size, dirty);
   image.Tag := @header;
   result := TMemoryStream.Create;
   rw := SDLStreamSetup(result);
   IMG_SavePNG_RW(pointer(image), rw);
   SDL_FreeRW(rw);
end;

function loadFromTBI(stream: TStream): PSdlSurface;
var
   rw: PSdl_RWops;
begin
   rw := SDLStreamSetup(stream);
   pointer(result) := sdl_image.IMG_LoadPNG_RW(rw);
   SDL_FreeRW(rw);
end;

{ TTbiHeader }

constructor TTbiHeader.Create(size: TSgPoint; dirty: boolean);
begin
   name := 'RPG'#0;
   self.size := sizeof(TTbiHeader);
   version := TBI_VERSION;
   width := size.x;
   height := size.y;
   self.dirty := dirty;
end;

end.
