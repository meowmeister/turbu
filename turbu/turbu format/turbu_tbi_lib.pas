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
   SDL;

function saveToTBI(image: PSdl_Surface): TMemoryStream;
function loadFromTBI(image: TStream): PSdl_Surface;

implementation
uses
   sysUtils,
   turbu_constants;

type
   ETbiError = class(Exception);

const
   TBI_HEADER = $2A494254;

function saveToTBI(image: PSdl_Surface): TMemoryStream;
var
   xBorder, yBorder: array of boolean;
   yProgress, xSkip, ySkip: array of word;
   x, y: cardinal;
   i, j: cardinal;
   i2: integer;
   current: ^byte;
   flag: boolean;
   header: longint;
   sizer: word;
begin
   //check invariants
   assert(image.format.BitsPerPixel = 8);
   assert(image.w > 0);
   assert(image.h > 0);

   //initialization
   x := image.w;
   y := image.h;
   setLength(xBorder, x);
   setLength(yBorder, y);
   setLength(yProgress, x);
   setLength(xSkip, 1);
   setLength(ySkip, 1);
   result := TMemoryStream.Create;

   //build border tables
   try
      for j := 0 to y - 1 do
      begin
         current := pointer(cardinal(image.pixels) + (j * image.pitch));
         i := 0;
         while (current^ = 0) and (i < x) do
         begin
            inc(yProgress[i]);
            inc(i);
            inc(current);
         end;
         yBorder[j] := i = x;
      end;

      i := 0;
      while (i < x) and (yProgress[i] = y) do
      begin
         xBorder[i] := true;
         inc(i);
      end;
      for i := i to x - 1 do
      begin
         current := pointer(cardinal(image.pixels) + i);
         j := 0;
         while (current^ = 0) and (j < y) do
         begin
            inc(j);
            inc(current, image.pitch);
         end;
         xBorder[i] := j = y;
      end;

      flag := xBorder[0];
      xSkip[0] := 0;
      if flag = false then
         setLength(xSkip, 2);
      for I := 0 to x - 1 do
      begin
         if xBorder[i] = flag then
            inc(xSkip[high(xSkip)])
         else begin
            setLength(xSkip, length(xSkip) + 1);
            xSkip[high(xSkip)] := 1;
            flag := not flag;
         end;
      end;

      flag := yBorder[0];
      ySkip[0] := 0;
      if flag = false then
         setLength(ySkip, 2);
      for I := 1 to y - 1 do
         begin
            if yBorder[i] = flag then
               inc(ySkip[high(ySkip)])
            else begin
               setLength(ySkip, length(ySkip) + 1);
               ySkip[high(ySkip)] := 1;
               flag := not flag;
            end;
         end;

      //now that the laborious process of setting up the border tables is
      //finished, let's save the file!
      header := TBI_HEADER;
      result.Write(header, 4);
      sizer := length(xSkip) * 2;
      result.Write(sizer, 2);
      result.write(xSkip[0], sizer);
      sizer := length(ySkip) * 2;
      result.Write(sizer, 2);
      result.write(ySkip[0], sizer);
      assert(image.format.palette.ncolors <= 256);
      result.write(image.format.palette.ncolors, 2);
      for I := 0 to image.format.palette.ncolors - 1 do
      begin
         result.write(image.format.palette.colors[i].r, 1);
         result.write(image.format.palette.colors[i].g, 1);
         result.write(image.format.palette.colors[i].b, 1);
      end;
      for j := ySkip[0] to y - 1 do
      begin
         if yBorder[j] = false then
         begin
            i := xSkip[0];
            i2 := 1;
            current := pointer(cardinal(image.pixels) + (j * image.pitch) + i);
            while i2 <= high(xSkip) do
            begin
               if i2 mod 2 = 1 then
                  result.Write(current^, xSkip[i2]);
               inc(i, xSkip[i2]);
               inc(i2);
            end;
            assert(i = x);
         end;
      end;

      //end with a 0
      sizer := 0;
      result.write(sizer, 1);
   except
      on Exception do
      begin
         result.Free;
         raise;
      end;
   end;
end;

function loadFromTBI(image: TStream): PSdl_Surface;
var
   buffer4: integer;
   buffer2: word;
   xSkip, ySkip: array of word;
   yBorder: array of boolean;
   flag: boolean;
   x, y: cardinal;
   i, j: cardinal;
   i2: integer;
   colors: TSDL_Palette;
   current: ^byte;
   intermediate: PSdl_Surface;
begin
   image.Seek(0, soFromBeginning);
   image.Read(buffer4, 4);
   if buffer4 <> TBI_HEADER then
      raise ETbiError.Create('TBI image format is corrupt');
   image.read(buffer2, 2);
   setLength(xSkip, buffer2 div 2);
   image.Read(xSkip[0], buffer2);
   image.read(buffer2, 2);
   setLength(ySkip, buffer2 div 2);
   image.Read(ySkip[0], buffer2);
   x := 0;
   y := 1;
   for I := 0 to high(xSkip) do
      inc(x, xSkip[i]);
   for I := 0 to high(ySkip) do
      inc(y, ySkip[i]);
   intermediate := SDL_CreateRGBSurface(SDL_SWSURFACE, x, y, 8, 0, 0, 0, 0);

   //build yBorder table
   setLength(yBorder, y);
   flag := true;
   i := 0;
   for j := 0 to high(ySkip) do
   begin
      fillChar(yBorder[i], ySkip[j], integer(flag));
      inc(i, ySkip[j]);
      flag := not flag;
   end;

   colors.ncolors := 0;
   new(colors.colors);
   try
      image.Read(colors.ncolors, 2);
      for I := 0 to colors.ncolors - 1 do
      begin
         image.Read(colors.colors[i].r, 1);
         image.Read(colors.colors[i].g, 1);
         image.Read(colors.colors[i].b, 1);
      end;
      SDL_SetPalette(intermediate, SDL_LOGPAL, @colors.colors[0], 0, colors.ncolors);
   finally
      dispose(colors.colors);
   end;
   with intermediate.format.palette.colors[0] do
      SDL_FillRect(intermediate, nil, SDL_MapRGB(intermediate.format, r, g, b));
   if SDL_MustLock(intermediate) then
      SDL_LockSurface(intermediate);

   for j := ySkip[0] to y - 1 do
   begin
      if yBorder[j] = false then
      begin
         i := xSkip[0];
         i2 := 1;
         current := pointer(cardinal(intermediate.pixels) + (j * intermediate.pitch) + i);
            while i2 <= high(xSkip) do
            begin
               if i2 mod 2 = 1 then
                  image.read(current^, xSkip[i2]);
               inc(i, xSkip[i2]);
               inc(i2);
            end;
            assert(i = x);
         end;
      end;
   if SDL_MustLock(intermediate) then
      SDL_UnlockSurface(intermediate);
   //end with a 0
   buffer2 := 0;
   image.read(buffer2, 1);
   assert(buffer2 = 0);
   result := SDL_CreateRGBSurface(IMAGE_FORMAT, x, y, 8, 0, 0, 0, 0);
   SDL_SetPalette(result, SDL_LOGPAL, @intermediate.format.palette.colors[0], 0, intermediate.format.palette.ncolors);
   with result.format.palette.colors[0] do
      assert(SDL_SetColorKey(intermediate, SDL_SRCCOLORKEY or SDL_RLEACCEL, SDL_MapRGB(result.format, r, g, b)) = 0);
   SDL_BlitSurface(intermediate, nil, result, nil);
   SDL_FreeSurface(intermediate);
   image.Seek(0, soFromBeginning);
end;

end.
