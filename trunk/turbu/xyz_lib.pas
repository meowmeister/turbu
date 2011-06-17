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
   windows;

type
   TXyzImage = class(TObject)
   private
      FWidth: word;
      FHeight: word;
      FPalette: array[0..255] of TRgbTriple;
      FData: array of byte;
   public
      constructor Create(filename: string);
   end;

implementation
uses
   classes, graphics, SysUtils, zlib;

{ TXyzImage }

constructor TXyzImage.Create(filename: string);
var
   inFile: TFileStream;
   decStream: TDecompressionStream;
   header: AnsiString;
   i: cardinal;
begin
   inFile := nil;
   decStream := nil;
   try
      inFile := TFileStream.Create(filename, fmOpenRead);
      setLength(header, 4);
      inFile.Read(header[1], 4);
      if header <> 'XYZ1' then
         raise EInvalidGraphic.create('Invalid XYZ image.');
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

end.
