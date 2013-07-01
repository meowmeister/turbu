unit SDL_ImageManager;
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
******************************************************************************
*
* The TSdlImages class is based on the previously-existing TAsphyreImages
* class. The Original Code is found in AsphyreImages.pas, version 2.1, and can
* be found at www.afterwarp.net.
*
* The Initial Developer of the Original Code is M. Sc. Yuriy Kotsarenko.
* Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007,
* Afterwarp Interactive. All Rights Reserved.
*
*****************************************************************************
*
* This file was created by Mason Wheeler.  He can be reached for support at
* www.turbu-rpg.com.
*****************************************************************************}

interface
uses
   classes, sysUtils, Generics.Collections,
   SG_Defs,
   SDL, sdl_13;

const EMPTY: TSgPoint = (X: 0; Y: 0);

type
   TDrawMode = (dmFull, dmSprite);

   ESdlImageException = class(Exception);

   {***************************************************************************
   * Custom routine interface for loading images from archives.
   *
   * TArchiveLoader provides a filename for the archive and a keyname for the
   * file within the archive, and expects a valid PSDL_RWops to a readible
   * (decompressed and decrypted) version of the file.  
   *
   * TArchiveCallback provides an opportunity to do something else with the
   * RWops once the file has been read.
   ***************************************************************************}
   TArchiveLoader = function(filename: string): PSDL_RWops;
   TArchiveCallback = procedure(var rw: PSdl_RWops);

   TSdlImages = class;

   {***************************************************************************
   * Encapsulates an SDL_Surface for holding images
   ***************************************************************************}
   TSdlImage = class(TObject)
   private
   class var
      FRw: PSDL_RWops;
   protected
      FSurface: TSdlTexture; //the actual SDL_Surface
      FName: string; //Name for the image.  Must be unique!
      FTextureSize: TSgPoint;
      FTexturesPerRow: integer;
      FTextureRows: integer;
      FColorkey: TSDL_Color;

      function getSpriteRect(index: integer): TRect;
      procedure setup(renderer: TSdlRenderer; filename, imagename: string;
        container: TSdlImages; spriteSize: TSgPoint; lSurface: PSdlSurface); virtual;
      procedure SetTextureSize(size: TSgPoint);
      function GetCount: integer; inline;
      procedure processImage(image: PSdlSurface); virtual;
   public
      {************************************************************************
      * Creates a TSdlImage.  Pass in the full filename and a logical name for
      * the image.  The image will automatically add itself to container if the
      * argument is not nil.
      ************************************************************************}
      constructor Create(renderer: TSdlRenderer; filename, imagename: string; container: TSdlImages); overload;

      {************************************************************************
      * Like the previous constructor, except that it creates a TSdlImage from
      * a pre-existing SDL_Surface.
      ************************************************************************}
      constructor Create(renderer: TSdlRenderer; surface: PSdlSurface; imagename: string; container: TSdlImages); overload;

      {************************************************************************
      * Like the previous constructor, except that it creates a TSdlImage from
      * an SDL_RWops.  Pass in the filetype extension instead of the full
      * filename.
      ************************************************************************}
      constructor Create(renderer: TSdlRenderer; rw: PSDL_RWops; extension, imagename: string; container: TSdlImages); overload;

      {************************************************************************
      * These three are like the last three, but with an additional spriteSize
      * parameter. This defines the image as a sprite sheet and sets the size
      * of the individual sprites on the sheet.
      *
      * The image's X and Y dimensions must be evenly divisible by the X and Y
      * values of the SpriteSize parameters, or creation will fail and raise an
      * exception.
      ************************************************************************}
      constructor CreateSprite(renderer: TSdlRenderer; filename, imagename: string; container: TSdlImages; spriteSize: TSgPoint); overload;
      constructor CreateSprite(renderer: TSdlRenderer; rw: PSDL_RWops; extension, imagename: string; container: TSdlImages; spriteSize: TSgPoint); overload; virtual;
      constructor CreateSprite(renderer: TSdlRenderer; surface: PSdlSurface; imagename: string; container: TSdlImages; spriteSize: TSgPoint); overload;

      constructor CreateBlankSprite(renderer: TSdlRenderer; imagename: string; container: TSdlImages; spriteSize: TSgPoint; count: integer);

      destructor Destroy; override;

      {************************************************************************
      * Drawing routines.  Draw draws the image to the current canvas.
      * The Dest parameter represents the screen position of the top-left
      * corner of the rectangle where the image will be drawn to.
      *
      * DrawRect and DrawSprite are methods to draw only a part of the image to
      * the screen canvas.  DrawRect draws only a certain rectangle from the
      * image, as defined by the Source parameter.  DrawSprite draws a sprite
      * from the image based on the Index parameter and the image's textureSize
      * property.
      *
      * Invalid values for DrawSprite result in nothing being drawn.
      ************************************************************************}
      procedure Draw; overload; virtual;
      procedure Draw(dest: TSgPoint); overload;
      procedure DrawRect(dest: TSgPoint; source: TRect);
      procedure DrawSprite(dest: TSgPoint; index: integer);

      procedure DrawTo(dest: TRect);
      procedure DrawRectTo(dest: TRect; source: TRect);
      procedure DrawSpriteTo(dest: TRect; index: integer);

      property name: string read FName write FName;
      property surface: TSdlTexture read FSurface;
      property textureSize: TSgPoint read FTextureSize write SetTextureSize;
      property texPerRow: integer read FTexturesPerRow;
      property texRows: integer read FTextureRows;
      property count: integer read GetCount;
      property spriteRect[index: integer]: TRect read getSpriteRect;
      property Colorkey: TSDL_Color read FColorkey;
   end;

   TSdlImageClass = class of TSdlImage;

   {***************************************************************************
   * Adaptation of TAsphyreImages for SDL images.  This class is a specialized
   * container class, much like a TList, for storing and easily accessing
   * multiple images.
   ***************************************************************************}
   TSdlImages = class(TObject)
   private
      FRenderer: TSdlRenderer;
      FData: array of TSdlImage;
      FFreeOnClear: Boolean;
      FArchiveLoader: TArchiveLoader;
      FArchiveCallback: TArchiveCallback;
      FUpdateMutex: PSDL_Mutex;
      FHash: TDictionary<string, integer>;
    FSpriteClass: TSdlImageClass;

      function GetCount: Integer; inline;
      function GetItem(Num: Integer): TSdlImage;
      function GetImage(const Name: string): TSdlImage;
      function FindEmptySlot: Integer;
      function Insert(Element: TSdlImage): Integer;
   public
      {************************************************************************
      * Sets up the image list.  The FreeOnClear argument controls whether
      * images held in the list will be freed when the Clear method is called.
      ************************************************************************}
      constructor Create(renderer: TSdlRenderer; FreeOnClear: boolean = true; loader: TArchiveLoader = nil; callback: TArchiveCallback = nil);
      destructor Destroy; override;

      function Contains(const name: string): boolean; inline;
      function IndexOf(Element: TSdlImage): Integer; overload;
      function IndexOf(const Name: string): Integer; overload;

      {************************************************************************
      * Adds a TSdlImage to the list and returns its index position.  If the
      * image is already in the list, it returns the current position.  (Will
      * not add duplicates.)
      ************************************************************************}
      function Add(Element: TSdlImage): Integer;

      {************************************************************************
      * Creates a TSdlImage from the file specified in filename with the name
      * given in imagename, adds it to the list, and returns its index position.
      ************************************************************************}
      function AddFromFile(filename, imagename: string): integer; overload;
      function AddFromFile(filename, imagename: string; imgClass: TSdlImageClass): integer; overload;

      {************************************************************************
      * Like AddFromFile, but loads the image file specified in keyname from
      * the archive named filename.  The loader parameter allows you to specify
      * a custom TArchiveLoader; otherwise it uses the one already assigned to
      * the imagelist's ArchiveLoader property.  If both loaders are nil, the
      * loading will fail and raise an exception.
      *
      * If the imagelist's ArchiveCallback property is set, it will call the
      * callback when the loading is finished, to allow the program to do
      * something else with the RWops.  If so, the RWops's current position will
      * be set to 0 (the start of the file.)  If the callback is unassigned,
      * AddFromArchive will automatically free the RWops.
      ************************************************************************}
      function AddFromArchive(filename, imagename: string; loader: TArchiveLoader = nil): integer;
      function AddSpriteFromArchive(filename, imagename: string; spritesize: TSgPoint;
        imgClass: TSdlImageClass; loader: TArchiveLoader = nil): integer; overload;
      function AddSpriteFromArchive(filename, imagename: string; spritesize: TSgPoint;
        loader: TArchiveLoader = nil): integer; overload;
      function EnsureImage(filename, imagename: string): TSdlImage; overload;
      function EnsureImage(filename, imagename: string; spritesize: TSgPoint): TSdlImage; overload;
      function EnsureBGImage(filename, imagename: string): TSdlImage;

      {************************************************************************
      * Frees the TSdlImage at the current index and removes it from the list.
      ************************************************************************}
      procedure Remove(Num: Integer);

      {************************************************************************
      * Removes the TSdlImage at the current index from the list without
      * freeing it.  Returns the image.
      ************************************************************************}
      function Extract(Num: integer): TSdlImage;

      {************************************************************************
      * Removes all images from the list.  If FreeOnClear is true, all images
      * will be freed.
      ************************************************************************}
      procedure Clear;

      {************************************************************************
      * Removes all nil pointers from the list.  This is not a "stable sort"
      * and may change the indices and index orders of several images.
      ************************************************************************}
      procedure Pack;

      procedure SetRenderer(renderer: TSdlRenderer);

      property Count: Integer read GetCount;
      property Items[Num: Integer]: TSdlImage read GetItem; default;
      property Image[const Name: string]: TSdlImage read GetImage;
      property FreeOnClear: boolean read FFreeOnClear write FFreeOnClear;
      property ArchiveLoader: TArchiveLoader read FArchiveLoader write FArchiveLoader;
      property ArchiveCallback: TArchiveCallback read FArchiveCallback write FArchiveCallback;
      property SpriteClass: TSdlImageClass read FSpriteClass write FSpriteClass;
   end;

   TSdlBackgroundImage = class(TSdlImage)
   protected
      procedure processImage(image: PSdlSurface); override;
   end;

   {***************************************************************************
   * To set up routines to load images not covered by SDL_Image.  For advanced
   * users only!
   ***************************************************************************}
   TImgLoadMethod = reference to function(inFile: TStream): PSdlSurface;

   procedure registerImageLoader(extension: string; loader: TImgLoadMethod);

implementation
uses
   Math,
   SDL_rwStream, sdl_canvas,
   sdl_image;

var
   loaders: TDictionary<string, TImgLoadMethod>;
   rwMutex: PSDL_Mutex;

{ TSdlImages }
{$REGION TSdlImages}
//---------------------------------------------------------------------------
constructor TSdlImages.Create(renderer: TSdlRenderer; FreeOnClear: boolean = true; loader: TArchiveLoader = nil; callback: TArchiveCallback = nil);
begin
   inherited Create;
   FRenderer := renderer;
   FFreeOnClear := FreeOnClear;
   FArchiveLoader := loader;
   FArchiveCallback := callback;
   FUpdateMutex := SDL_CreateMutex;
   FHash := TDictionary<string, integer>.Create;
   FSpriteClass := TSdlImage;
end;

//---------------------------------------------------------------------------
destructor TSdlImages.Destroy;
begin
   self.Clear;
   FHash.Free;
   SDL_DestroyMutex(FUpdateMutex);
   inherited Destroy;
end;

//---------------------------------------------------------------------------
function TSdlImages.GetCount: Integer;
begin
   Result := Length(FData);
end;

//---------------------------------------------------------------------------
function TSdlImages.GetItem(Num: Integer): TSdlImage;
begin
   if (Num >= 0) and (Num < Length(FData)) then
      Result := FData[Num]
   else Result := nil;
end;

//---------------------------------------------------------------------------
function TSdlImages.GetImage(const Name: string): TSdlImage;
var
   index: integer;
begin
   if FHash.TryGetValue(name, index) then
      result := FData[index]
   else result := nil;
end;

//---------------------------------------------------------------------------
function TSdlImages.IndexOf(Element: TSdlImage): Integer;
begin
   result := IndexOf(element.name);
end;

//---------------------------------------------------------------------------
function TSdlImages.IndexOf(const Name: string): Integer;
begin
   if not FHash.TryGetValue(name, result) then
      result := -1;
end;

//---------------------------------------------------------------------------
function TSdlImages.Contains(const name: string): boolean;
begin
   result := self.IndexOf(name) <> -1;
end;

//---------------------------------------------------------------------------
procedure TSdlImages.Pack;
var
   Lo, Hi: integer;
  I: Integer;
begin
   Lo := -1;
   Hi := high(FData);
   while Lo < Hi do
   begin
      inc(Lo);
      if FData[Lo] = nil then
      begin
         while (FData[Hi] = nil) and (Hi > Lo) do
            dec(Hi);
         if Hi > Lo then
         begin
            FData[Lo] := FData[Hi];
            dec(Hi);
         end;
      end;
   end;

   if FData[Hi] = nil then
      dec(Hi);
   setLength(FData, Hi * 2);
   FHash.Clear;
   for I := 0 to Hi do
      FHash.Add(FData[i].name, i);
end;

//---------------------------------------------------------------------------
function TSdlImages.FindEmptySlot: Integer;
var
   i: Integer;
begin
   Result := -1;
   for i := 0 to Length(FData) - 1 do
      if FData[i] = nil then
      begin
         Result := i;
         Break;
      end;
end;

//---------------------------------------------------------------------------
function TSdlImages.Insert(Element: TSdlImage): Integer;
var
   Slot: Integer;
begin
   Slot := FindEmptySlot;
   if Slot = -1 then
   begin
      Slot := Length(FData);
      SetLength(FData, max(Slot * 2, 16));
   end;
   FData[Slot] := Element;
   Result := Slot;
   FHash.Add(element.name, result);
end;

//---------------------------------------------------------------------------
function TSdlImages.Add(Element: TSdlImage): Integer;
begin
   SDL_LockMutex(FUpdateMutex);
   try
      if FHash.ContainsKey(element.name) then
         Result := IndexOf(Element)
      else Result := Insert(Element);
   finally
      SDL_UnlockMutex(FUpdateMutex);
   end;
end;

//---------------------------------------------------------------------------
function TSdlImages.EnsureImage(filename, imagename: string): TSdlImage;
begin
   result := EnsureImage(filename, imagename, EMPTY);
end;

//---------------------------------------------------------------------------
function TSdlImages.EnsureBGImage(filename, imagename: string): TSdlImage;
var
   index: integer;
begin
   if self.Contains(imagename) then
      result := GetImage(imagename)
   else begin
      if FileExists(filename) then
         index := AddFromFile(filename, imagename, TSdlBackgroundImage)
      else index := AddSpriteFromArchive(filename, imagename, EMPTY, TSdlBackgroundImage);
      result := Self[index];
   end;
end;

//---------------------------------------------------------------------------
function TSdlImages.EnsureImage(filename, imagename: string; spritesize: TSgPoint): TSdlImage;
var
   index: integer;
begin
   if self.Contains(imagename) then
      result := GetImage(imagename)
   else begin
      if FileExists(filename) then
         index := AddFromFile(filename, imagename)
      else index := AddSpriteFromArchive(filename, imagename, spritesize);
      result := Self[index];
   end;
end;

function TSdlImages.Extract(Num: integer): TSdlImage;
begin
   SDL_LockMutex(FUpdateMutex);
   try
      result := nil;
      if (Num < 0) or (Num >= Length(FData)) then
         Exit;
      result := FData[num];
      FData[num] := nil;
      FHash.Remove(result.name);
   finally
      SDL_UnlockMutex(FUpdateMutex);
   end;
end;

//---------------------------------------------------------------------------
procedure TSdlImages.Remove(Num: Integer);
begin
   SDL_LockMutex(FUpdateMutex);
   try
      if (Num < 0) or (Num >= Length(FData)) then
         Exit;
      FHash.Remove(FData[Num].name);
      freeAndNil(FData[Num]);
   finally
      SDL_UnlockMutex(FUpdateMutex);
   end;
end;

//---------------------------------------------------------------------------
procedure TSdlImages.SetRenderer(renderer: TSdlRenderer);
begin
   if FRenderer.ptr <> nil then
      raise Exception.Create('Cannot call TSdlImages.SetRenderer twice!');
   FRenderer := renderer;
end;

//---------------------------------------------------------------------------
function TSdlImages.AddFromArchive(filename, imagename: string; loader: TArchiveLoader = nil): integer;
var
   dummy: PSDL_RWops;
begin
   if assigned(loader) then
      dummy := loader(filename)
   else if assigned(FArchiveLoader) then
      dummy := FArchiveLoader(filename)
   else raise ESdlImageException.Create('No archive loader available!');
   if dummy = nil then
      raise ESdlImageException.CreateFmt('Archive loader failed to extract "%s" from the archive.', [filename]);
   result := self.Add(TSdlImage.Create(FRenderer, dummy, ExtractFileExt(filename), imagename, nil));
   if assigned(FArchiveCallback) then
      FArchiveCallback(dummy)
   else SDL_FreeRW(dummy);
end;

//---------------------------------------------------------------------------
function TSdlImages.AddSpriteFromArchive(filename, imagename: string;
  spritesize: TSgPoint; imgClass: TSdlImageClass;
  loader: TArchiveLoader): integer;
var
   dummy: PSDL_RWops;
begin
   if assigned(loader) then
      dummy := loader(filename)
   else if assigned(FArchiveLoader) then
      dummy := FArchiveLoader(filename)
   else raise ESdlImageException.Create('No archive loader available!');
   if dummy = nil then
      raise ESdlImageException.CreateFmt('Archive loader failed to extract "%s" from the archive.', [filename]);

   result := self.Add(imgClass.CreateSprite(FRenderer, dummy, ExtractFileExt(filename), imagename, nil, spriteSize));
   if assigned(FArchiveCallback) then
      FArchiveCallback(dummy)
   else SDL_FreeRW(dummy);
end;

//---------------------------------------------------------------------------
function TSdlImages.AddSpriteFromArchive(filename, imagename: string;
  spritesize: TSgPoint; loader: TArchiveLoader = nil): integer;
begin
   result := AddSpriteFromArchive(filename, imagename, spritesize, FSpriteClass, loader);
end;

//---------------------------------------------------------------------------
function TSdlImages.AddFromFile(filename, imagename: string): integer;
begin
   result := AddFromFile(filename, filename, TSdlImage);
end;

//---------------------------------------------------------------------------
function TSdlImages.AddFromFile(filename, imagename: string; imgClass: TSdlImageClass): integer;
begin
   result := self.Add(imgClass.Create(FRenderer, filename, filename, nil));
end;

//---------------------------------------------------------------------------
procedure TSdlImages.Clear;
var
   i: Integer;
begin
   SDL_LockMutex(FUpdateMutex);
   try
      FHash.Clear;
      if FFreeOnClear then
         for i := 0 to Length(FData) - 1 do
            FData[i].Free;
      SetLength(FData, 0);
   finally
      SDL_LockMutex(FUpdateMutex);
   end;
end;
{$ENDREGION}

{ TSdlImage }
{$REGION TSdlImage}
//---------------------------------------------------------------------------
constructor TSdlImage.Create(renderer: TSdlRenderer; filename, imagename: string; container: TSdlImages);
begin
   inherited Create;
   setup(renderer, filename, imagename, container, EMPTY, nil);
end;

//---------------------------------------------------------------------------
constructor TSdlImage.Create(renderer: TSdlRenderer; rw: PSDL_RWops; extension, imagename: string; container: TSdlImages);
begin
   SDL_LockMutex(rwMutex);
   try
      FRw := rw;
      setup(renderer, ExtractFileExt(extension), imagename, container, EMPTY, nil);
      FRw := nil;
   finally
      SDL_UnlockMutex(rwMutex);
   end;
end;

//---------------------------------------------------------------------------
constructor TSdlImage.Create(renderer: TSdlRenderer; surface: PSdlSurface; imagename: string; container: TSdlImages);
begin
   inherited Create;
   FSurface := TSdlTexture.Create(renderer, 0, surface);
   setup(renderer, '', imagename, container, EMPTY, surface);
end;

//---------------------------------------------------------------------------
constructor TSdlImage.CreateSprite(renderer: TSdlRenderer; filename, imagename: string; container: TSdlImages; spriteSize: TSgPoint);
begin
   inherited Create;
   setup(renderer, filename, imagename, container, spriteSize, nil);
end;

constructor TSdlImage.CreateSprite(renderer: TSdlRenderer; surface: PSdlSurface; imagename: string; container: TSdlImages; spriteSize: TSgPoint);
begin
   inherited Create;
   setup(renderer, '', imagename, container, spriteSize, surface);
end;

constructor TSdlImage.CreateSprite(renderer: TSdlRenderer; rw: PSDL_RWops; extension, imagename: string; container: TSdlImages; spriteSize: TSgPoint);
begin
   inherited Create;
   SDL_LockMutex(rwMutex);
   FRw := rw;
   setup(renderer, extension, imagename, container, spriteSize, nil);
   FRw := nil;
   SDL_UnlockMutex(rwMutex);
end;

constructor TSdlImage.CreateBlankSprite(renderer: TSdlRenderer; imagename: string; container: TSdlImages; spriteSize: TSgPoint; count: integer);
begin
   inherited Create;
   spriteSize.Y := spriteSize.Y * count;
   setup(renderer, '', imagename, container, spriteSize, nil);
   spriteSize.Y := spriteSize.Y div count;
   self.textureSize := spriteSize;
end;

destructor TSdlImage.Destroy;
begin
   FSurface.Free;
   inherited Destroy;
end;

//---------------------------------------------------------------------------
procedure TSdlImage.setup(renderer: TSdlRenderer; filename, imagename: string;
  container: TSdlImages; spriteSize: TSgPoint; lSurface: PSdlSurface);
var
   loader: TImgLoadMethod;
   loadStream: TStream;
   intFilename: PAnsiChar; //internal version of the filename
begin
   FName := imagename;
   if (lSurface = nil) and (FSurface.ptr = nil) then
   begin
      if filename <> '' then
      begin
         if not loaders.TryGetValue(ExtractFileExt(filename), loader) then
            loader := nil;
         if FRw = nil then
         begin
            intFilename := PAnsiChar(UTF8String(filename));
            if not assigned(loader) then
               LSurface := PSdlSurface(IMG_Load(intFilename))
            else begin
               loadStream := TFileStream.Create(filename, fmOpenRead);
               try
                  LSurface := loader(loadStream);
               finally
                  loadStream.Free;
               end;
            end;
         end
         else begin
            if not assigned(loader) then
            begin
               LSurface := PSdlSurface(IMG_LoadTyped_RW(FRw, 0, PAnsiChar(ansiString(filename))));
            end
            else begin
               loadStream := TRWStream.Create(FRw, false);
               try
                  LSurface := loader(loadStream);
               finally
                  loadStream.Free;
               end;
            end;
         end;
      end
      else
         LSurface := TSdlSurface.Create(spritesize.x, spritesize.y, 32, 0, 0, 0, 0);
   end;

   if LSurface = nil then
      raise ESdlImageException.Create(string(IMG_GetError));
   if assigned(LSurface.format.palette) then
   begin
      FColorkey := LSurface.format.palette.colors^[0];
      LSurface.colorkey := SDL_MapRGB(LSurface.format, FColorkey.r, FColorkey.g, FColorkey.b)
   end;
   //Allow descendant classes to fix up the image, if desired.
   processImage(LSurface);
   if FSurface.ptr = nil then
      FSurface := TSdlTexture.Create(renderer, 0, LSurface);

   if (spriteSize.X = EMPTY.X) and (spriteSize.Y = EMPTY.Y) then
      self.textureSize := point(LSurface.width, LSurface.height)
   else self.textureSize := spriteSize;
   LSurface.Free;
   if assigned(container) then
      container.add(self);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.draw(dest: TSgPoint);
begin
   currentRenderTarget.parent.draw(self, dest);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.Draw;
begin
   self.Draw(EMPTY);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.DrawTo(dest: TRect);
begin
   currentRenderTarget.parent.drawTo(self, dest);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.drawRect(dest: TSgPoint; source: TRect);
begin
   currentRenderTarget.parent.drawRect(self, dest, source);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.DrawRectTo(dest, source: TRect);
begin
   currentRenderTarget.parent.drawRectTo(self, dest, source);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.DrawSprite(dest: TSgPoint; index: integer);
begin
   if index >= count then
      Exit;

   currentRenderTarget.parent.drawRect(self, dest, self.spriteRect[index]);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.DrawSpriteTo(dest: TRect; index: integer);
begin
   if index >= count then
      Exit;

   currentRenderTarget.parent.drawRectTo(self, dest, self.spriteRect[index]);
end;

//---------------------------------------------------------------------------
function TSdlImage.GetCount: integer;
begin
   result := FTexturesPerRow * FTextureRows;
end;

function TSdlImage.getSpriteRect(index: integer): TRect;
var
   x, y: integer;
begin
   x := index mod FTexturesPerRow;
   y := index div FTexturesPerRow;
   result := rect(point(x * FTextureSize.X, y * FTextureSize.y), FTextureSize);
end;

procedure TSdlImage.processImage(image: PSdlSurface);
begin
   //this virtual method intentionally left blank
end;

//---------------------------------------------------------------------------
procedure TSdlImage.SetTextureSize(size: TSgPoint);
var
   lSize: TSgPoint;
begin
   lSize := FSurface.size;
   if (lSize.X mod size.X > 0) or (lSize.Y mod size.Y > 0) then
      raise ESdlImageException.Create('Texture size is not evenly divisible into base image size.');

   FTextureSize := size;
   FTexturesPerRow := lSize.X div size.X;
   FTextureRows := lSize.Y div size.Y;
end;
{$ENDREGION}

{ Classless }

procedure registerImageLoader(extension: string; loader: TImgLoadMethod);
begin
   if extension[1] <> '.' then
      extension := '.' + extension;
   loaders.Add(extension, loader);
end;

{ TSdlBackgroundImage }

procedure TSdlBackgroundImage.processImage(image: PSdlSurface);
begin
  SDL_SetColorKey(image, false, 0);
  integer(FColorKey) := 0;
end;

initialization
begin
   loaders := TDictionary<string, TImgLoadMethod>.Create;
   rwMutex := SDL_CreateMutex;
   assert(IMG_Init([imgPng, imgXyz]) = [imgPng, imgXyz]);
end;

finalization
begin
   IMG_Quit;
   loaders.Free;
   SDL_DestroyMutex(rwMutex);
end;

end.
