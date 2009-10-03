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
   classes, sysUtils,
   SG_Defs,
   SDL, sdl_13;

const EMPTY: TPoint = (X: 0; Y: 0);

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
   TArchiveLoader = function(filename, keyname: string): PSDL_RWops;
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

      function getSpriteRect(index: integer): TRect;
      procedure setup(filename, imagename: string; container: TSdlImages; spriteSize: TSgPoint; lSurface: PSdlSurface); virtual;
      procedure SetTextureSize(size: TSgPoint);
      function GetCount: integer; inline;
      procedure processImage(image: PSdlSurface); virtual;
   public
      {************************************************************************
      * Creates a TSdlImage.  Pass in the full filename and a logical name for
      * the image.  The image will automatically add itself to container if the
      * argument is not nil.
      ************************************************************************}
      constructor Create(filename, imagename: string; container: TSdlImages); overload;

      {************************************************************************
      * Like the previous constructor, except that it creates a TSdlImage from
      * a pre-existing SDL_Surface.
      ************************************************************************}
      constructor Create(surface: PSdlSurface; imagename: string; container: TSdlImages); overload;

      {************************************************************************
      * Like the previous constructor, except that it creates a TSdlImage from
      * an SDL_RWops.  Pass in the filetype extension instead of the full
      * filename.
      ************************************************************************}
      constructor Create(rw: PSDL_RWops; extension, imagename: string; container: TSdlImages); overload;

      {************************************************************************
      * These three are like the last three, but with an additional spriteSize
      * parameter. This defines the image as a sprite sheet and sets the size
      * of the individual sprites on the sheet.
      *
      * The image's X and Y dimensions must be evenly divisible by the X and Y
      * values of the SpriteSize parameters, or creation will fail and raise an
      * exception.
      ************************************************************************}
      constructor CreateSprite(filename, imagename: string; container: TSdlImages; spriteSize: TSgPoint); overload;
      constructor CreateSprite(rw: PSDL_RWops; extension, imagename: string; container: TSdlImages; spriteSize: TSgPoint); overload;
      constructor CreateSprite(surface: PSdlSurface; imagename: string; container: TSdlImages; spriteSize: TSgPoint); overload;

      constructor CreateBlankSprite(imagename: string; container: TSdlImages; spriteSize: TSgPoint; count: integer);

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

      property name: string read FName write FName;
      property surface: TSdlTexture read FSurface;
      property textureSize: TSgPoint read FTextureSize write SetTextureSize;
      property texPerRow: integer read FTexturesPerRow;
      property texRows: integer read FTextureRows;
      property count: integer read GetCount;
      property spriteRect[index: integer]: TRect read getSpriteRect;
   end;

   {***************************************************************************
   * Adaptation of TAsphyreImages for SDL images.  This class is a specialized
   * container class, much like a TList, for storing and easily accessing
   * multiple images.
   ***************************************************************************}
   TSdlImages = class(TObject)
   private
      FData: array of TSdlImage;
      FSearchObjects: array of Integer;
      FSearchDirty: Boolean;
      FFreeOnClear: Boolean;
      FArchiveLoader: TArchiveLoader;
      FArchiveCallback: TArchiveCallback;
      FUpdateMutex: PSDL_Mutex;

      function GetCount: Integer; inline;
      function GetItem(Num: Integer): TSdlImage;
      function CountSearchObjects: Integer;
      procedure FillSearchObjects(Amount: Integer);
      procedure SortSearchObjects(Left, Right: Integer);
      procedure PrepareSearchObjects;
      function GetImage(const Name: string): TSdlImage;
      function FindEmptySlot: Integer;
      function Insert(Element: TSdlImage): Integer;
   public
      {************************************************************************
      * Sets up the image list.  The FreeOnClear argument controls whether
      * images held in the list will be freed when the Clear method is called.
      ************************************************************************}
      constructor Create(FreeOnClear: boolean = true; loader: TArchiveLoader = nil; callback: TArchiveCallback = nil);
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
      function AddFromFile(filename, imagename: string): integer;

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
      function AddFromArchive(filename, keyname, imagename: string; loader: TArchiveLoader = nil): integer;
      function AddSpriteFromArchive(filename, keyname, imagename: string; spritesize: TSgPoint; loader: TArchiveLoader = nil): integer;

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

      property Count: Integer read GetCount;
      property Items[Num: Integer]: TSdlImage read GetItem; default;
      property Image[const Name: string]: TSdlImage read GetImage;
      property FreeOnClear: boolean read FFreeOnClear write FFreeOnClear;
      property ArchiveLoader: TArchiveLoader read FArchiveLoader write FArchiveLoader;
      property ArchiveCallback: TArchiveCallback read FArchiveCallback write FArchiveCallback;
   end;

   {***************************************************************************
   * To set up routines to load images not covered by SDL_Image.  For advanced
   * users only!
   ***************************************************************************}
   TImgLoadMethod = function(inFile: TStream): PSdlSurface;

   procedure registerImageLoader(extension: string; loader: TImgLoadMethod);

implementation
uses
   SDL_rwStream, sdl_canvas,
   sdl_image;

var
   loaders: TStringList;
   rwMutex: PSDL_Mutex;

{ TSdlImages }
{$REGION TSdlImages}
//---------------------------------------------------------------------------
constructor TSdlImages.Create(FreeOnClear: boolean = true; loader: TArchiveLoader = nil; callback: TArchiveCallback = nil);
begin
   inherited Create;
   FSearchDirty := False;
   FFreeOnClear := FreeOnClear;
   FArchiveLoader := loader;
   FArchiveCallback := callback;
   FUpdateMutex := SDL_CreateMutex;
end;

//---------------------------------------------------------------------------
destructor TSdlImages.Destroy;
begin
   self.Clear;
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
   Index: Integer;
begin
   Index := IndexOf(Name);
   if Index <> -1 then
      Result := FData[Index]
   else Result:= nil;
end;

//---------------------------------------------------------------------------
function TSdlImages.IndexOf(Element: TSdlImage): Integer;
var
   i: Integer;
begin
   Result := -1;
   for i := 0 to Length(FData) - 1 do
      if FData[i] = Element then
      begin
         Result := i;
         Break;
      end;
end;

//---------------------------------------------------------------------------
function TSdlImages.IndexOf(const Name: string): Integer;
var
   Lo, Hi, Mid: Integer;
begin
   Result := -1;
   if FSearchDirty then
      PrepareSearchObjects;
   Lo := 0;
   Hi := Length(FSearchObjects) - 1;
   while Lo <= Hi do
   begin
      Mid := (Lo + Hi) div 2;
      if FData[FSearchObjects[Mid]].Name = Name then
      begin
         Result := FSearchObjects[Mid];
         Break;
      end;
      if FData[FSearchObjects[Mid]].Name > Name then
         Hi := Mid - 1
      else Lo:= Mid + 1;
   end;
end;

//---------------------------------------------------------------------------
function TSdlImages.Contains(const name: string): boolean;
begin
   result := self.IndexOf(name) <> -1;
end;

//---------------------------------------------------------------------------
function TSdlImages.CountSearchObjects: Integer;
var
   i: Integer;
begin
   Result:= 0;
   for i:= 0 to Length(FData) - 1 do
      if FData[i] <> nil then
         Inc(Result);
end;

//---------------------------------------------------------------------------
procedure TSdlImages.FillSearchObjects(Amount: Integer);
var
   i, DestIndex: Integer;
begin
   SetLength(FSearchObjects, Amount);
   DestIndex := 0;
   for i := 0 to Length(FData) - 1 do
   if FData[i] <> nil then
   begin
      FSearchObjects[DestIndex]:= i;
      Inc(DestIndex);
   end;
end;

//---------------------------------------------------------------------------
procedure TSdlImages.SortSearchObjects(Left, Right: Integer);
var
   Lo, Hi: Integer;
   TempIndex: Integer;
   MidValue: string;
begin
   Lo := Left;
   Hi := Right;
   MidValue := FData[FSearchObjects[(Left + Right) div 2]].Name;
   repeat
      while FData[FSearchObjects[Lo]].Name < MidValue do
         Inc(Lo);
      while MidValue < FData[FSearchObjects[Hi]].Name do
         Dec(Hi);
      if Lo <= Hi then
      begin
         TempIndex := FSearchObjects[Lo];
         FSearchObjects[Lo] := FSearchObjects[Hi];
         FSearchObjects[Hi] := TempIndex;
         Inc(Lo);
         Dec(Hi);
      end;
   until (Lo > Hi);
   if Left < Hi then
      SortSearchObjects(Left, Hi);
   if Lo < Right then
      SortSearchObjects(Lo, Right);
end;

//---------------------------------------------------------------------------
procedure TSdlImages.PrepareSearchObjects;
var
   Amount: Integer;
begin
   Amount := CountSearchObjects;
   FillSearchObjects(Amount);
   if Amount > 0 then
      SortSearchObjects(0, Amount - 1);
   FSearchDirty := False;
end;

//---------------------------------------------------------------------------
procedure TSdlImages.Pack;
var
   Lo, Hi: integer;
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
   setLength(FData, Hi + 1);
   FSearchDirty := true;
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
      SetLength(FData, Slot + 1);
   end;
   FData[Slot] := Element;
   FSearchDirty := True;
   Result := Slot;
end;

//---------------------------------------------------------------------------
function TSdlImages.Add(Element: TSdlImage): Integer;
begin
   SDL_LockMutex(FUpdateMutex);
   try
      Result := IndexOf(Element);
      if Result = -1 then
         Result := Insert(Element);
   finally
      SDL_UnlockMutex(FUpdateMutex);
   end;
end;

//---------------------------------------------------------------------------
function TSdlImages.Extract(Num: integer): TSdlImage;
begin
   SDL_LockMutex(FUpdateMutex);
   try
      result := nil;
      if (Num < 0) or (Num >= Length(FData)) then
         Exit;
      result := FData[num];
      FData[num] := nil;
      FSearchDirty := true;
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
      freeAndNil(FData[Num]);
      FSearchDirty := True;
   finally
      SDL_UnlockMutex(FUpdateMutex);
   end;
end;

//---------------------------------------------------------------------------
function TSdlImages.AddFromArchive(filename, keyname, imagename: string; loader: TArchiveLoader = nil): integer;
var
   dummy: PSDL_RWops;
begin
   if assigned(loader) then
      dummy := loader(filename, keyname)
   else if assigned(FArchiveLoader) then
      dummy := FArchiveLoader(filename, keyname)
   else raise ESdlImageException.Create('No archive loader available!');
   if dummy = nil then
      raise ESdlImageException.CreateFmt('Archive loader failed to extract "%s" from the archive "%s".', [keyname, filename]);
   result := self.Add(TSdlImage.Create(dummy, ExtractFileExt(filename), imagename, nil));
   if assigned(FArchiveCallback) then
      FArchiveCallback(dummy)
   else SDL_FreeRW(dummy);
end;

//---------------------------------------------------------------------------
function TSdlImages.AddSpriteFromArchive(filename, keyname, imagename: string;
  spritesize: TSgPoint; loader: TArchiveLoader = nil): integer;
var
   dummy: PSDL_RWops;
begin
   if assigned(loader) then
      dummy := loader(filename, keyname)
   else if assigned(FArchiveLoader) then
      dummy := FArchiveLoader(filename, keyname)
   else raise ESdlImageException.Create('No archive loader available!');
   if dummy = nil then
      raise ESdlImageException.CreateFmt('Archive loader failed to extract "%s" from the archive "%s".', [keyname, filename]);

   result := self.Add(TSdlImage.CreateSprite(dummy, ExtractFileExt(filename), imagename, nil, spriteSize));
   if assigned(FArchiveCallback) then
      FArchiveCallback(dummy)
   else SDL_FreeRW(dummy);
end;

//---------------------------------------------------------------------------
function TSdlImages.AddFromFile(filename, imagename: string): integer;
begin
   result := self.Add(TSdlImage.Create(filename, filename, nil));
end;

//---------------------------------------------------------------------------
procedure TSdlImages.Clear;
var
   i: Integer;
begin
   SDL_LockMutex(FUpdateMutex);
   try
      if FFreeOnClear then
         for i := 0 to Length(FData) - 1 do
            FData[i].Free;

      SetLength(FData, 0);
      SetLength(FSearchObjects, 0);
      FSearchDirty := False;
   finally
      SDL_LockMutex(FUpdateMutex);
   end;
end;
{$ENDREGION}

{ TSdlImage }
{$REGION TSdlImage}
//---------------------------------------------------------------------------
constructor TSdlImage.Create(filename, imagename: string; container: TSdlImages);
begin
   inherited Create;
   setup(filename, imagename, container, EMPTY, nil);
end;

//---------------------------------------------------------------------------
constructor TSdlImage.Create(rw: PSDL_RWops; extension, imagename: string; container: TSdlImages);
begin
   SDL_LockMutex(rwMutex);
   try
      FRw := rw;
      setup(ExtractFileExt(extension), imagename, container, EMPTY, nil);
      FRw := nil;
   finally
      SDL_UnlockMutex(rwMutex);
   end;
end;

//---------------------------------------------------------------------------
constructor TSdlImage.Create(surface: PSdlSurface; imagename: string; container: TSdlImages);
begin
   inherited Create;
   FSurface := TSdlTexture.Create(0, surface);
   setup('', imagename, container, EMPTY, surface);
end;

//---------------------------------------------------------------------------
constructor TSdlImage.CreateSprite(filename, imagename: string; container: TSdlImages; spriteSize: TSgPoint);
begin
   inherited Create;
   setup(filename, imagename, container, spriteSize, nil);
end;

constructor TSdlImage.CreateSprite(surface: PSdlSurface; imagename: string; container: TSdlImages; spriteSize: TSgPoint);
begin
   inherited Create;
   FSurface := TSdlTexture.Create(0, surface);
   setup('', imagename, container, spriteSize, surface);
end;

constructor TSdlImage.CreateSprite(rw: PSDL_RWops; extension, imagename: string; container: TSdlImages; spriteSize: TSgPoint);
begin
   inherited Create;
   SDL_LockMutex(rwMutex);
   FRw := rw;
   setup(extension, imagename, container, spriteSize, nil);
   FRw := nil;
   SDL_UnlockMutex(rwMutex);
end;

constructor TSdlImage.CreateBlankSprite(imagename: string; container: TSdlImages; spriteSize: TSgPoint; count: integer);
begin
   inherited Create;
   spriteSize.Y := spriteSize.Y * count;
   setup('', imagename, container, spriteSize, nil);
   spriteSize.Y := spriteSize.Y div count;
   self.textureSize := spriteSize;
end;

destructor TSdlImage.Destroy;
begin
   FSurface.Free;
   inherited Destroy;
end;

//---------------------------------------------------------------------------
procedure TSdlImage.setup(filename, imagename: string; container: TSdlImages; spriteSize: TSgPoint; lSurface: PSdlSurface);
var
   colorkey: TSDL_Color;
   dummy: integer;
   loader: TImgLoadMethod;
   loadStream: TStream;
   intFilename: PAnsiChar; //internal version of the filename
begin
   FName := imagename;
   if FSurface.ID = 0 then
   begin
      if filename <> '' then
      begin
         dummy := loaders.IndexOf(ExtractFileExt(filename));
         if FRw = nil then
         begin
            {$IFDEF UNICODE}
            intFilename := PAnsiChar(UTF8String(filename));
            {$ELSE}
            intFilename := PChar(filename);
            {$ENDIF}
            if dummy = -1 then
               LSurface := PSdlSurface(IMG_Load(intFilename))
            else begin
               loader := TImgLoadMethod(loaders.Objects[dummy]);
               loadStream := TFileStream.Create(filename, fmOpenRead);
               try
                  LSurface := loader(loadStream);
               finally
                  loadStream.Free;
               end;
            end;
         end
         else begin
            if dummy = -1 then
            begin
               LSurface := PSdlSurface(IMG_LoadTyped_RW(FRw, 0, PAnsiChar(ansiString(filename))));
            end
            else begin
               loader := TImgLoadMethod(loaders.Objects[dummy]);
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
      colorkey := LSurface.format.palette.colors^[0];
      LSurface.ColorKey := SDL_MapRGB(LSurface.format, colorkey.r, colorkey.g, colorkey.b)
   end;
   //Allow descendant classes to fix up the image, if desired.
   processImage(LSurface);
   if FSurface.ID = 0 then
      FSurface := TSdlTexture.Create(0, LSurface);

   LSurface.Free;
   if (spriteSize.X = EMPTY.X) and (spriteSize.Y = EMPTY.Y) then
      self.textureSize := point(LSurface.width, LSurface.height)
   else self.textureSize := spriteSize;
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
procedure TSdlImage.drawRect(dest: TSgPoint; source: TRect);
begin
   currentRenderTarget.parent.drawRect(self, dest, source);
end;

//---------------------------------------------------------------------------
procedure TSdlImage.DrawSprite(dest: TSgPoint; index: integer);
begin
   if index >= count then
      Exit;

   currentRenderTarget.parent.drawRect(self, dest, self.spriteRect[index]);
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
   loaders.AddObject(extension, TObject(@loader));
end;

initialization
begin
   loaders := TStringList.Create;
   loaders.Sorted := true;
   loaders.Duplicates := dupError;
   loaders.CaseSensitive := false;
   rwMutex := SDL_CreateMutex;
   assert(IMG_Init([imgPng]) = [imgPng]);
end;

finalization
begin
   IMG_Quit;
   loaders.Free;
   SDL_DestroyMutex(rwMutex);
end;
end.
