unit portrait_selector;

interface

uses
   StdCtrls, Controls, Classes, ExtCtrls,
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
begin
   oFilename := filename;
   if not imgSelector.ContainsName(oFilename) then
   begin
      filename := format('portrait\%s.png', [oFilename]);
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      AddNewImage(filename, oFilename);
   end;

   size := sgPoint(imgSelector.width, imgSelector.height);
   image := imgSelector.Images.Image[oFilename];
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
