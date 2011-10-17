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

unit portrait_selector;

interface

uses
   StdCtrls, Controls, Types, Classes, ExtCtrls,
   base_selector, sdl_frame;

type
   TfrmPortraitSelector = class(TfrmBaseSelector)
      ScrollBar1: TScrollBar;
      procedure lstFilenamesClick(Sender: TObject); override;
   private
      { Private declarations }
      procedure LoadPortrait(filename: string);
      class procedure SelectPortrait(var current: string; var frame: integer);
   public
      class procedure SelectPortraitInto(sdlFrame: TSdlFrame; var current: string;
        var frame: integer; flipped: boolean);
   end;

implementation
uses
   SysUtils, Generics.Collections,
   ArchiveInterface, sdl_frame_helper,
   sdl_ImageManager, sg_Defs;

{$R *.dfm}

procedure TfrmPortraitSelector.LoadPortrait(filename: string);
var
   oFilename: string;
   image: TSdlImage;
   size: TSgPoint;
   tSize: integer;
begin
   oFilename := filename;
   if not imgSelector.ContainsName(oFilename) then
   begin
      filename := format('portrait\%s.png', [oFilename]);
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      AddNewImage(filename, oFilename);
   end;

   EnsureCanvas;
   image := imgSelector.Images.Image[oFilename];
   tSize := image.textureSize.x * 4;
   size := sgPoint(tSize, round(tSize * (imgSelector.Height / imgSelector.Width)));
   finishLoading(image, size, size,
      procedure
         var
            i: integer;
            position: TSgPoint;
         begin
            position := ORIGIN;
            imgSelector.FillColor(FBackground, $FF);
            for I := 0 to image.count do
            begin
               image.DrawSpriteTo(rect(position, image.textureSize), i);
               if position.x + (2 * image.textureSize.x) <= imgSelector.LogicalWidth then
                  inc(position.x, image.textureSize.x)
               else begin
                  position.x := 0;
                  inc(position.y, image.textureSize.y);
               end;
            end;
         end);
end;

procedure TfrmPortraitSelector.lstFilenamesClick(Sender: TObject);
begin
   FFilename := lstFilenames.Items[lstFilenames.ItemIndex];
   loadPortrait(FFilename);
   inherited lstFilenamesClick(sender);
end;

class procedure TfrmPortraitSelector.SelectPortrait(var current: string; var frame: integer);
begin
   doSelection(current, 'portrait', frame, self);
end;

class procedure TfrmPortraitSelector.SelectPortraitInto(sdlFrame: TSdlFrame;
  var current: string; var frame: integer; flipped: boolean);
begin
   SelectPortrait(current, frame);
   sdlFrame.SetPortrait(current, frame, flipped);
end;

end.
