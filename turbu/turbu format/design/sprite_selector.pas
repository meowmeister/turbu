unit sprite_selector;

interface

uses
   SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Messages,
   sdl_frame,
   turbu_tilesets, turbu_constants,
   sdl_13, sg_defs;

type
   TfrmSpriteSelector = class(TForm)
      Panel1: TPanel;
      lstFilenames: TListBox;
      btnOK: TButton;
      btnCancel: TButton;
      imgSelector: TSdlFrame;
      procedure FormShow(Sender: TObject);
      procedure lstFilenamesClick(Sender: TObject);
      procedure imgSelectorAvailable(Sender: TObject);
      procedure imgSelectorMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
      procedure imgSelectorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
   private
      procedure WMRender(var message: TMessage); message WM_RENDER;
   private
      FFrame: integer;
      FFileList: TStringList;
      FFilename: string;
      FTileset: TTileset;
      FTextureSize: TSgPoint;
      FRedrawProc: TProc;
      FGridLoc: TSgPoint;
      FBackground: SDL_Color;
      procedure loadTileGroups;
      procedure loadTileset(grouprec: TTileGroupRecord);
      procedure loadSprite(filename: string);
      procedure AddNewImage(const filename, imagename: string);
      procedure BindCursor(x, y: integer); overload;
      procedure BindCursor; overload;
      function GetFrame: integer;
      procedure SetFrame(const Value: integer);
      property CurrentFrame: integer read GetFrame write SetFrame;
   public
      class procedure SelectSprite(tileset: TTileset; var current: string; var frame: integer);
   end;

var
   frmSpriteSelector: TfrmSpriteSelector;

implementation
uses
   Windows, Generics.Collections, OpenGL,
   ArchiveInterface, turbu_containers, turbu_tbi_lib, turbu_sdl_image,
   sdl_ImageManager, sg_utils;

const
   SCALE = 2;

{$R *.dfm}

{ TfrmSpriteSelector }

procedure TfrmSpriteSelector.loadTileGroups;
var
   tilegroups: TRpgObjectList<TTileGroupRecord>;
   groupRec: TTileGroupRecord;
begin
   tilegroups := FTileset.Records.where(turbu_tilesets.upperLayerFilter);
   try
      for grouprec in tilegroups do
         lstFilenames.AddItem('*Tile group ' + intToStr(grouprec.id), grouprec);
   finally
      tilegroups.Free;
   end;
end;

procedure TfrmSpriteSelector.BindCursor;
var
   cursorRect: TRect;
begin
   cursorRect.TopLeft := FGridLoc;
   cursorRect.Right := cursorRect.Left + 1;
   cursorRect.Bottom := cursorRect.Top + 1;
   cursorRect := multiplyRect(cursorRect, FTextureSize);
   FRedrawProc;
   imgSelector.DrawBox(TRectToSDLRect(constrictRect(cursorRect, 2)), SDL_BLACK, $90);
   imgSelector.DrawBox(TRectToSDLRect(constrictRect(cursorRect, 1)), SDL_WHITE, $A0);
   imgSelector.DrawBox(TRectToSDLRect(cursorRect), SDL_BLACK, $B0);
   imgSelector.Flip;
end;

procedure TfrmSpriteSelector.BindCursor(x, y: integer);
var
   gridLoc: TSgPoint;
begin
   gridLoc:= pointToGridLoc(imgSelector.LogicalCoordinates(x, y), FTextureSize, 0, 0, 1);
   if gridLoc = FGridLoc then
      Exit //prevent flicker
   else FGridLoc := gridLoc;
   BindCursor;
end;

procedure TfrmSpriteSelector.FormCreate(Sender: TObject);
begin
   FGridLoc := sgPoint(-1, -1);
end;

procedure TfrmSpriteSelector.FormDestroy(Sender: TObject);
begin
   FFileList.Free;
end;

procedure TfrmSpriteSelector.FormShow(Sender: TObject);
var
   filename: string;
   index: integer;
begin
   assert(assigned(FFileList));
   assert(FFilename <> '');
   if assigned(FTileset) then
      loadTileGroups;
   for filename in FFileList do
      lstFilenames.AddItem(ChangeFileExt(ExtractFileName(filename), ''), nil);
   index := lstFilenames.Items.IndexOf(FFilename);
   if index <> -1 then
      lstFilenames.ItemIndex := index
   else lstFilenames.ItemIndex := 0;
end;

function TfrmSpriteSelector.GetFrame: integer;
begin
   result := (FGridLoc.y * (imgSelector.LogicalWidth div FTextureSize.x)) + FGridLoc.x;
end;

procedure TfrmSpriteSelector.SetFrame(const Value: integer);
var
   gridWidth: integer;
begin
   gridWidth := imgSelector.LogicalWidth div FTextureSize.x;
   FGridLoc := sgPoint(value mod gridWidth, value div gridWidth);
end;

procedure TfrmSpriteSelector.AddNewImage(const filename, imagename: string);
var
   stream: TStream;
begin
   stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
   try
      imgSelector.AddImage(TRpgSdlImage.CreateSprite(loadFromTBI(stream), imagename, nil));
   finally
      stream.Free;
   end;
end;

procedure TfrmSpriteSelector.loadTileset(grouprec: TTileGroupRecord);
var
   filename, oFilename: string;
   image: TSdlImage;
begin
   oFilename := groupRec.group.filename;
   if not imgSelector.ContainsName(oFilename) then
   begin
      filename := format('tileset\%s.png', [oFilename]);
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      addNewImage(filename, oFilename);
   end;

   image := imgSelector.images.Image[oFilename];
   FTextureSize := Image.Texturesize;
   FBackground := image.Colorkey;
   imgSelector.Width := image.surface.size.X * SCALE;
   imgSelector.Height := image.surface.size.Y * SCALE;
   imgSelector.BringToFront;
   imgSelector.Clear;
   imgSelector.Update;
   imgSelector.LogicalWidth := image.surface.size.X;
   imgSelector.LogicalHeight := image.surface.size.Y;
   glLineWidth(SCALE);
   FRedrawProc :=
      procedure
      begin
         imgSelector.FillColor(FBackground, $FF);
         imgSelector.DrawTexture(oFilename);
      end;
end;

procedure TfrmSpriteSelector.loadSprite(filename: string);
var
   oFilename: string;
   image: TSdlImage;
   logicalSize: TSgPoint;
begin
   oFilename := filename;
   if not imgSelector.ContainsName(oFilename) then
   begin
      filename := format('mapsprite\%s.png', [oFilename]);
      assert(GArchives[IMAGE_ARCHIVE].fileExists(filename));
      AddNewImage(filename, oFilename);
   end;

   image := imgSelector.Images.Image[oFilename];
   FTextureSize := image.textureSize;
   FBackground := image.Colorkey;
   logicalSize.x := FTextureSize.x * 3;
   logicalSize.y := FTextureSize.y * 4;
   imgSelector.Width := logicalSize.x * SCALE;
   imgSelector.Height := logicalSize.y * SCALE;
   imgSelector.Clear;
   imgSelector.Update;
   imgSelector.LogicalWidth := logicalSize.x;
   imgSelector.LogicalHeight := logicalSize.y;
   glLineWidth(SCALE);
   FRedrawProc :=
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
         end;
end;

procedure TfrmSpriteSelector.lstFilenamesClick(Sender: TObject);
var
   filename: string;
begin
   filename := lstFilenames.Items[lstFilenames.ItemIndex];
   if filename[1] = '*' then
   begin
      loadTileset(lstFilenames.Items.Objects[lstFilenames.ItemIndex] as TTileGroupRecord);
      FFilename := stringReplace(filename, '*Tile group ', '*', []);
   end
   else begin
      loadSprite(filename);
      FFilename := filename;
   end;
   FRedrawProc;
   imgSelector.Flip;
   if FFrame <> -1 then
   begin
      self.CurrentFrame := FFrame;
      FFrame := -1;
   end
   else self.CurrentFrame := 0;
   BindCursor;
end;

procedure TfrmSpriteSelector.WMRender(var message: TMessage);
begin
   SDL_SetRenderDrawBlendMode([sdlbBlend]);
   lstFilenamesClick(self);
end;

procedure TfrmSpriteSelector.imgSelectorAvailable(Sender: TObject);
begin
   PostMessage(self.Handle, WM_RENDER, 0, 0);
end;

procedure TfrmSpriteSelector.imgSelectorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   imgSelectorMouseMove(sender, Shift, X, Y);
end;

procedure TfrmSpriteSelector.imgSelectorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if not (ssLeft in shift) then
      Exit;
   if (x < 0) or (y < 0) or (x >= imgSelector.Width) or (y >= imgSelector.Height) then
      Exit;

   BindCursor(x, y);
end;

class procedure TfrmSpriteSelector.SelectSprite(tileset: TTileset; var current: string;
  var frame: integer);
var
   form: TfrmSpriteSelector;
   enum: TEnumerable<string>;
   filename: string;
begin
   form := TfrmSpriteSelector.Create(nil);
   try
      form.FFileList := TStringList.Create;
      enum := GArchives[IMAGE_ARCHIVE].allFiles('mapsprite');
      for filename in  enum do
        form.FFileList.Add(filename);
      form.FFilename := current;
      form.FFrame := frame;
      form.FTileset := tileset;
      GArchives[IMAGE_ARCHIVE].currentFolder := '';
      if form.ShowModal = mrOK then
      begin
         current := form.FFilename;
         frame := form.CurrentFrame;
      end;
   finally
      form.Release;
   end;
end;

end.
