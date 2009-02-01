unit png_routines;
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
uses windows, contnrs, graphics, //system libs
     chipset_data, charset_data, //turbu libs
     pngimage, tempfile; //3rd party libs

function getPNGRect(filename: string; sourceRect: TRect): TTempFileStream; overload;
function getPNGRect(const image: TPNGObject; sourceRect: TRect): TTempFileStream; overload;
function stitchPNGFiles(first, second: string): TTempFileStream;
procedure SlicePNGVert(JoinedPNG: TPNGObject; Columns, Rows: Integer;
                   out SlicedPNGs: TObjectList);
procedure SlicePNGHoriz(JoinedPNG: TPNGObject; Columns, Rows: Integer;
                   out SlicedPNGs: TObjectList);
function divideChipset(const filename: string; out fileList: TThirteenTemps): TColor;
function getPaletteColor(var image:TPngObject; const color: byte; var changed: boolean): TColor; overload;
function getPaletteColor(var image:TPngObject; const color: byte): TColor; overload;
function getBGColor(const filename: string): TColor;
function colorToTriple(Color: TColor): TRgbTriple;
procedure grayscale(Graphic: TPNGObject);
function bmpToPng(filename: string): string;
procedure bmpCleanup;
function xyzToPng(filename: string): string;

implementation
uses sysUtils, classes, //system libs
     xyz_lib, commons; //turbu libs

var
   bmpList: TStringList;

function ColorToTriple(Color: TColor): TRGBTriple;
begin
   Color := ColorToRGB(Color);
   Result.rgbtBlue := Color shr 16 and $FF;
   Result.rgbtGreen := Color shr 8 and $FF;
   Result.rgbtRed := Color and $FF;
end;

function copyRectPNG(source: TPngObject; sourceRect: TRect): TPngObject;
var
   Bitmap: TBitmap;
   X, Y: integer;
   BitmapLine: PRGBLine;
   AlphaLineA, AlphaLineB: pngimage.PByteArray;
begin
   result := nil;
try
   Bitmap := TBitmap.Create;
   try
      Bitmap.Width := sourceRect.right;
      Bitmap.Height := sourceRect.bottom;
      Bitmap.PixelFormat := pf24bit;

      //Copy the color information into a temporary bitmap. We can't use TPNGObject.Draw
      //here, because that would combine the color and alpha values.
      for Y := 0 to Bitmap.Height - 1 do
      begin
         BitmapLine := Bitmap.Scanline[Y];
         for X := 0 to Bitmap.Width - 1 do
            BitmapLine^[X] := ColorToTriple(source.Pixels[X + sourceRect.Left, Y + sourceRect.top]);
      end;

      result := TPNGObject.Create;
      result.Assign(Bitmap);

      if source.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA] then
      begin
         //Copy the alpha channel
         result.CreateAlpha;
         for Y := 0 to result.Height - 1 do
         begin
            AlphaLineA := source.AlphaScanline[Y + sourceRect.top];
            AlphaLineB := result.AlphaScanline[Y];
            for X := 0 to result.Width - 1 do
               AlphaLineB^[X] := AlphaLineA^[X + sourceRect.Left];
            //end nested for
         end; //end for
      end; //end if

   finally
      Bitmap.Free;
   end;
   except
      assert(false);
   end;
end;

function stitchPNG(first, second: TPngObject): TPngObject;
var
   Bitmap: TBitmap;
   X, Y: integer;
   BitmapLine: PRGBLine;
   AlphaLineA, AlphaLineB, AlphaLineC: pngimage.PByteArray;
begin
   Bitmap := TBitmap.Create;
   try
      Bitmap.Width := first.width + second.width;
      Bitmap.Height := greaterOf(first.height, second.Height);
      Bitmap.PixelFormat := pf24bit;

      //Copy the color information into a temporary bitmap. We can't use TPNGObject.Draw
      //here, because that would combine the color and alpha values.
      for Y := 0 to first.Height - 1 do
      begin
         BitmapLine := Bitmap.Scanline[Y];
         for X := 0 to first.Width - 1 do
            BitmapLine^[X] := ColorToTriple(first.Pixels[X, Y]);
      end;
      for Y := 0 to second.Height - 1 do
      begin
         BitmapLine := Bitmap.Scanline[Y];
         for X := 0 to second.Width - 1 do
            BitmapLine^[X + first.width] := ColorToTriple(second.Pixels[X, Y]);
      end;

      result := TPNGObject.Create;
      result.Assign(Bitmap);

      if (first.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA]) or
      (second.Header.ColorType in [COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA]) then
      begin
         //Copy the alpha channel
         result.CreateAlpha;
         for Y := 0 to result.Height - 1 do
         begin
            if y < first.Height then
               AlphaLineA := first.AlphaScanline[Y]
            else alphaLineA := nil;
            if y < second.Height then
               AlphaLineB := second.AlphaScanline[Y]
            else alphaLineB := nil;
            AlphaLineC := result.AlphaScanline[Y];
            for X := 0 to result.Width - 1 do
            begin
               if x < first.width then
                  if alphaLineA <> nil then
                     AlphaLineC^[X] := AlphaLineA^[X]
                  else alphaLineC^[x] := $FF
                  //end if
               else
                  if alphaLineB <> nil then
                     AlphaLineC^[X] := AlphaLineB^[X - first.Width]
                  else alphaLineC^[x] := $FF;
                  //end nested if
               //end if
            end; //end nested for
         end; //end for
      end; //end if

   finally
      Bitmap.Free;
   end;
end;

procedure SlicePNGVert(JoinedPNG: TPNGObject; Columns, Rows: Integer;
                   out SlicedPNGs: TObjectList);
var
   ImageX, ImageY, OffsetX, OffsetY: Integer;
   Width, Height: Integer;
begin
   //This function slices a large PNG file (e.g. an image with all images for a
   //toolbar) into smaller, equally-sized pictures.
   SlicedPNGs := TObjectList.Create(false);
   Width := JoinedPNG.Width div Columns;
   Height := JoinedPNG.Height div Rows;

   //Loop through the columns and rows to create each individual image
   for ImageX := 0 to Columns - 1 do
   begin
      for ImageY := 0 to Rows - 1 do
      begin
         OffsetX := ImageX * Width;
         OffsetY := ImageY * Height;
         slicedPNGs.Add(copyRectPNG(joinedPNG, rect(offsetX, offsetY, width, height)));
      end;
   end;
end;

procedure SlicePNGHoriz(JoinedPNG: TPNGObject; Columns, Rows: Integer;
                   out SlicedPNGs: TObjectList);
var
   ImageX, ImageY, OffsetX, OffsetY: Integer;
   Width, Height: Integer;
begin
   //This function slices a large PNG file (e.g. an image with all images for a
   //toolbar) into smaller, equally-sized pictures.
   SlicedPNGs := TObjectList.Create;
   Width := JoinedPNG.Width div Columns;
   Height := JoinedPNG.Height div Rows;

   //Loop through the columns and rows to create each individual image
   for ImageY := 0 to Rows - 1 do
   begin
      for ImageX := 0 to Columns - 1 do
      begin
         OffsetX := ImageX * Width;
         OffsetY := ImageY * Height;
        slicedPNGs.Add(copyRectPNG(joinedPNG, rect(offsetX, offsetY, width, height)));
      end;
   end;
end;

function divideChipset(const filename: string; out fileList: TThirteenTemps ): TColor;
var
   imageFile: TPngObject;
   temp: TPngObject;
   pngSet, tempPngSet: TObjectList;
   i: byte;
begin
result := 0;
imageFile := TPngObject.Create;
pngSet := nil;
tempPngSet := nil;
try
   try
      imageFile.LoadFromFile(filename);
   except
      on Exception do raise EParseMessage.create('Unable to open ' + filename + ' for processing!');
   end;
   slicePngVert(imageFile, 5, 2, pngSet);
   assert(pngSet.count = 10);
   result := getPaletteColor(imageFile, 0);
   temp := TPngObject(pngSet[0]);
   slicePngVert(TPngObject(temp), 2, 2, tempPngSet);
   assert (tempPngSet.count= 4);
   pngSet.Delete(0);
   for i := 0 to 3 do
      pngSet.Insert(i, tempPngSet[i]);
   for I := 1 to 13 do
   begin
      fileList[i] := TTempFileStream.Create('png', false);
      TPngObject(pngSet[i - 1]).SaveToStream(fileList[i]);
   end;
finally
   freeAndNil(imageFile);
   try
      try
         pngSet.clear;
         tempPngSet.clear;
      except on EInvalidPointer do
      end;
   finally
      pngSet.Free;
      tempPngSet.Free;
   end;
end; //end of finally block
end;

function identicalEntry(const first, second: TPaletteEntry): boolean;
begin
   result := ((first.peRed = second.peRed) and (first.peGreen = second.peGreen)
              and (first.peBlue = second.peBlue) and (first.peFlags = second.peFlags));
end;

function paletteScan(var image: TPngObject): boolean;
var
   thePalette: array[0..255] of TPaletteEntry;
   I: Integer;
begin
   result := false;
   windows.GetPaletteEntries(image.Palette, 0, 256, thePalette);
   for I := 1 to 255 do
      if identicalEntry(thePalette[i], thePalette[0]) then
      begin
      result := true;
      with thePalette[i] do
         if greaterOf(peRed, peBlue) = peRed then
            if peRed > 0 then
               dec(peRed, 40)
            else inc(peRed)
         else if greaterOf(peGreen, peBlue) = peGreen then
            if peGreen > 0 then
               dec(peGreen)
            else inc(peGreen);
         //end if
      //end WITH
      windows.SetPaletteEntries(image.Palette, i, 1, thePalette[i]);
      end;
   //end FOR
end;

function getPaletteColor(var image:TPngObject; const color: byte; var changed: boolean): TColor;
var
   theColor: array[0..0] of TPaletteEntry;
begin
   windows.GetPaletteEntries(image.Palette, color, 1, theColor);
   result := theColor[0].peRed;
   inc(result, (theColor[0].peGreen * $100));
   inc(result, (theColor[0].peBlue * $10000));

   changed := (color = 0) and paletteScan(image);
end;

function getPaletteColor(var image:TPngObject; const color: byte): TColor;
var dummy: boolean;
begin
   result := getPaletteColor(image, color, dummy);
end;

function getPNGRect(const image: TPNGObject; sourceRect: TRect): TTempFileStream;
var
   destImage: TPngObject;
begin
   destImage := copyRectPNG(image, sourceRect);
   try
      result := TTempFileStream.Create('png', false);
      destImage.SaveToStream(result);
   finally
      destImage.Free;
   end;
end;

function getPNGRect(filename: string; sourceRect: TRect): TTempFileStream;
var
   imageFile: TPngObject;
begin
   result := nil;
   imageFile := TPngObject.Create;
   try
      try
         imageFile.LoadFromFile(filename);
      except
         on Exception do raise EParseMessage.create('Unable to open ' + filename + ' for processing!');
      end;
      result := getPNGRect(imageFile, sourceRect);
   finally
      imageFile.Free;
   end;
end;

function stitchPNGFiles(first, second: string): TTempFileStream;
var
   dummy, dummy2: TPngObject;
begin
   dummy := TPNGObject.Create;
   dummy2 := TPNGObject.Create;
   try
      dummy.LoadFromFile(first);
      dummy2.LoadFromFile(second);

      dummy := stitchPNG(dummy, dummy2);
      result := TTempFileStream.Create('png', false);
      dummy.SaveToStream(result);
   finally
      dummy.free;
      dummy2.free;
   end;
end;

function getBGColor(const filename: string): TColor;
var
   imageFile: TPngObject;
begin
   result := 0;
   imageFile := TPngObject.Create;
   try
      try
         imageFile.LoadFromFile(filename);
      except
         on Exception do raise EParseMessage.create('Unable to open ' + filename + ' for processing!');
      end;
      result := getPaletteColor(imageFile, 0);
   finally
      imageFile.Free;
   end;
end;

{*******************************************************************************
* This routine is a slightly modified version of an algorithm found at
* http://delphi.about.com/cs/adptips2003/a/bltip1003_3.htm to turn a color
* bitmap into a grayscale image.  Originally written by Zarko Gajic.
*******************************************************************************}
procedure grayscale(Graphic: TPNGObject);
type
   TRGBColor = record
   case Integer of
     0 : (Color : TColor) ;
     1 : (Red, Green, Blue, Intensity : Byte) ;
     2 : (Colors : array[0..3] of Byte) ;
end;
var
   bmp, mask : TBitmap;
   x, y, Avg : Integer;
   RGBColor : TRGBColor;
begin
   if not Assigned(Graphic) then
      Exit;

   bmp := TBitmap.Create;
   mask := TBitmap.Create;

try
   bmp.Height := Graphic.Height;
   bmp.Width := Graphic.Width;
   with bmp.Canvas do
   begin
      Brush.Color := Graphic.TransparentColor;
      FillRect(ClipRect);
      Draw(0,0,Graphic);
   end;
   mask.assign(bmp);
   mask.mask(bmp.TransparentColor);
   with bmp.Canvas do
   begin
      for y := 0 to ClipRect.Bottom do
      begin
         for x := 0 to ClipRect.Right do
         begin
            if mask.Canvas.Pixels[x,y] = clBlack then
            begin
               RGBColor.Color := Pixels[x,y];
               with RGBColor do
               begin
                  Avg := ((Red+Green+Blue) div 3);
//                Inc(Avg, Contrast);
                  if Avg > 255 then
                     Avg := 255
                  else if Avg < 0 then
                     Avg := 0;
                  Red := Avg;
                  Green := Avg;
                  Blue := Avg;
               end;
               Pixels[x,y] := RGBColor.Color;
            end;
         end;
      end;
   end;
   Graphic.Assign(bmp);
finally
   bmp.Free;
   mask.Free;
end; //end of TRY...FINALLY block
end;

function bmpToPng(filename: string): string;
var
   temp: TPNGObject;
   bitmap: TBitmap;
   tempfile: TTempFileStream;
begin
   temp := TPNGObject.Create;
   try
      bitmap := TBitmap.Create;
      try
         bitmap.LoadFromFile(filename);
         temp.Assign(bitmap);
         tempfile := TTempFileStream.Create('png', false);
         try
            temp.SaveToStream(tempfile);
            result := tempfile.FileName;
         finally
         tempfile.Free;
         end;
      finally
         bitmap.free;
      end;
   finally
      temp.free;
      bmpList.Add(result);
   end;
end;

function xyzToPng(filename: string): string;
var
   temp: TPNGObject;
   input: TXyzImage;
   tempfile: TTempFileStream;
begin
   temp := TPNGObject.Create;
   try
      input := TXyzImage.Create(filename);
      try
//         input.saveToPng(temp);
         tempfile := TTempFileStream.Create('png', false);
         try
            temp.SaveToStream(tempfile);
            result := tempfile.FileName;
         finally
         tempfile.Free;
         end;
      finally
         input.free;
      end;
   finally
      temp.free;
      bmpList.Add(result);
   end;
end;

procedure bmpCleanup;
var
   I: Integer;
begin
   if bmpList.count > 0 then
   begin
      for I := 0 to bmpList.count - 1 do
         DeleteFile(bmpList[i]);
      bmpList.Clear;
   end;
end;

initialization
begin
   bmpList := TStringList.Create;
end;

finalization
begin
   bmpCleanup;
   bmpList.Free;
end;

end.
