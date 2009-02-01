unit xyz_lib;
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
   windows,
   {pngimage,} zlib;

type
   TXyzImage = class(TObject)
   private
      FWidth: word;
      FHeight: word;
      FPalette: array[0..255] of TRgbTriple;
      FData: array of byte;
   public
      constructor Create(filename: string);
//      procedure saveToPng(png: TPngObject);
   end;

implementation
uses
   classes, graphics,
   commons;

{ TXyzImage }

constructor TXyzImage.Create(filename: string);
var
   inFile: TFileStream;
   decStream: TDecompressionStream;
   dummy: string;
   i: cardinal;
begin
   inFile := nil;
   decStream := nil;
   try
      inFile := TFileStream.Create(filename, OPEN_READ);
      setLength(dummy, 4);
      inFile.Read(dummy[1], 4);
      if dummy <> 'XYZ1' then
         raise EParseMessage.create('Invalid XYZ image.');
      inFile.read(FWidth, 2);
      inFile.Read(FHeight, 2);
      setLength(FData, FWidth * FHeight);
      decStream := TDecompressionStream.Create(inFile);
      for I := 0 to 255 do
         decStream.Read(FPalette[i], 3);
      assert(decStream.read(FData[0], length(FData)) = length(FData)) ;
      assert(inFile.position = inFile.size);
   finally
      inFile.Free;
      decStream.free;
   end;
end;

(*
procedure TXyzImage.saveToPng(png: TPngObject);
var
   i, j: word;
   thePalette: array[0..255] of TPaletteEntry;
   dummy: TRgbTriple;
   bitmap: TBitmap;
begin
   bitmap := TBitmap.Create;
   bitmap.SetSize(FWidth, FHeight);
   bitmap.PixelFormat := pf8bit;
   for I := 0 to 255 do
   begin
      thePalette[i].peRed := FPalette[i].rgbtBlue;
      thePalette[i].peGreen := FPalette[i].rgbtGreen;
      thePalette[i].peBlue := FPalette[i].rgbtRed;
      thePalette[i].peFlags := 0;
   end;
   png.assign(bitmap);
//   windows.SetPaletteEntries(png.Palette, 0, 256, thePalette[0]);
   for j := 0 to FHeight - 1 do
   begin
      for i := 0 to FWidth - 1 do
      begin
         dummy := FPalette[FData[(FWidth * j) + i]];
         //yes, this looks backwards.  It's because TRgbTriple actually orders
         //its bytes as GBR.
         png.Pixels[i, j] := rgb(dummy.rgbtBlue, dummy.rgbtGreen, dummy.rgbtRed);
      end;
      //end FOR
   end;
//   windows.SetPaletteEntries(png.Palette, 0, 256, thePalette[0]);
{   for I := 0 to 255 do
      with TChunkPLTE(png.Chunks.ItemFromClass(TChunkPLTE)).Item[i] do
      begin
         rgbRed := thePalette[i].peRed;
         rgbBlue := thePalette[i].peBlue;
         rgbGreen := thePalette[i].peGreen;
      end;}
   png.SaveToFile('c:\test.png');
end;*)

end.
