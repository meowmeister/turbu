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

unit base_selector;

interface

uses
   SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Messages,
   sdl_frame, sdl_canvas,
   turbu_constants,
   sdl_ImageManager, sdl_13, sg_defs;

type
   TfrmBaseSelector = class;
   TSelectorClass = class of TfrmBaseSelector;
   TSelectorSetupProc = reference to procedure(form: TfrmBaseSelector);

   TfrmBaseSelector = class(TForm)
      Panel1: TPanel;
      lstFilenames: TListBox;
      btnOK: TButton;
      btnCancel: TButton;
      imgSelector: TSdlFrame;
      procedure FormShow(Sender: TObject);
      procedure lstFilenamesClick(Sender: TObject); virtual;
      procedure imgSelectorAvailable(Sender: TObject);
      procedure imgSelectorMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
      procedure imgSelectorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
   private
      FCanvas: TSdlCanvas;
      FOldCanvas: TSdlRenderSurface;

      procedure WMRender(var message: TMessage); message WM_RENDER;
   protected
      FBackground: SDL_Color;
      FFilename: string;
      FFrame: integer;
      FFileList: TStringList;
      procedure FinishLoading(image: TSdlImage; size, logicalSize: TSgPoint; redrawProc: TProc);
      procedure ShowLoad; virtual;
      procedure AddNewImage(const filename, imagename: string);
      function GetFrame: integer;
      procedure SetFrame(const Value: integer);
      property CurrentFrame: integer read GetFrame write SetFrame;
      class procedure DoSelection(var current: string; const folder: string;
        var frame: integer; selector: TSelectorClass; extra: TSelectorSetupProc = nil);
      procedure EnsureCanvas;
   private
      FTextureSize: TSgPoint;
      FRedrawProc: TProc;
      FGridLoc: TSgPoint;
      procedure BindCursor(x, y: integer); overload;
      procedure BindCursor; overload;
   end;

const
   SELECTOR_SCALE = 2;

implementation
uses
   Windows, Generics.Collections, OpenGL,
   ArchiveInterface, turbu_containers, turbu_tbi_lib, turbu_sdl_image,
   sg_utils;

{$R *.dfm}

{ TfrmSpriteSelector }

procedure TfrmBaseSelector.BindCursor;
var
   cursorRect: TRect;
begin
   cursorRect.TopLeft := FGridLoc;
   cursorRect.Right := cursorRect.Left + 1;
   cursorRect.Bottom := cursorRect.Top + 1;
   cursorRect := multiplyRect(cursorRect, FTextureSize);
   FRedrawProc;
   GlLineWidth(SELECTOR_SCALE);
   imgSelector.DrawBox(TRectToSDLRect(constrictRect(cursorRect, 2)), SDL_BLACK, $90);
   imgSelector.DrawBox(TRectToSDLRect(constrictRect(cursorRect, 1)), SDL_WHITE, $A0);
   imgSelector.DrawBox(TRectToSDLRect(cursorRect), SDL_BLACK, $B0);
   imgSelector.Flip;
end;

class procedure TfrmBaseSelector.DoSelection(var current: string; const folder: string;
  var frame: integer; selector: TSelectorClass; extra: TSelectorSetupProc);
var
   form: TfrmBaseSelector;
   enum: TEnumerable<string>;
   filename: string;
begin
   form := selector.Create(nil);
   try
      form.FFileList := TStringList.Create;
      enum := GArchives[IMAGE_ARCHIVE].allFiles(folder);
      for filename in enum do
        form.FFileList.Add(filename);
      if current = '' then
         current := form.FFileList[0];
      form.FFilename := current;
      form.FFrame := frame;
      if assigned(extra) then
         extra(form);
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

procedure TfrmBaseSelector.EnsureCanvas;
begin
   if FCanvas = nil then
   begin
      FOldCanvas := currentRenderTarget;
      FCanvas := TSdlCanvas.CreateFrom(imgSelector.SdlWindow);
      FCanvas.SetRenderer;
   end;
end;

procedure TfrmBaseSelector.ShowLoad;
begin
   //this virtual method intentionally left blank
end;

procedure TfrmBaseSelector.BindCursor(x, y: integer);
var
   gridLoc: TSgPoint;
begin
   gridLoc:= pointToGridLoc(imgSelector.LogicalCoordinates(x, y), FTextureSize, 0, 0, 1);
   if gridLoc <> FGridLoc then
   begin
      FGridLoc := gridLoc;
      BindCursor;
   end;
end;

procedure TfrmBaseSelector.FinishLoading(image: TSdlImage; size,
  logicalSize: TSgPoint; redrawProc: TProc);
begin
   FTextureSize := image.textureSize;
   FBackground := image.Colorkey;
   if (imgSelector.width <> logicalSize.x) or (imgSelector.height <> logicalSize.y) then
   begin
      imgSelector.Width := logicalSize.x;
      imgSelector.Height := logicalSize.y;
      imgSelector.Clear;
      imgSelector.Update;
      GlLineWidth(SELECTOR_SCALE);
   end;
   imgSelector.LogicalWidth := size.x;
   imgSelector.LogicalHeight := size.y;
   FRedrawProc := redrawProc;
end;

procedure TfrmBaseSelector.FormCreate(Sender: TObject);
begin
   FGridLoc := sgPoint(-1, -1);
end;

procedure TfrmBaseSelector.FormDestroy(Sender: TObject);
begin
   if assigned(FCanvas) then
   begin
      FCanvas.Free;
      FOldCanvas.parent.RenderTarget := FOldCanvas;
   end;
   FFileList.Free;
end;

procedure TfrmBaseSelector.FormShow(Sender: TObject);
var
   filename: string;
   index: integer;
begin
   assert(assigned(FFileList));
   assert(FFilename <> '');
   ShowLoad;
   for filename in FFileList do
      lstFilenames.AddItem(ChangeFileExt(ExtractFileName(filename), ''), nil);
   index := lstFilenames.Items.IndexOf(FFilename);
   if index <> -1 then
      lstFilenames.ItemIndex := index
   else lstFilenames.ItemIndex := 0;
end;

function TfrmBaseSelector.GetFrame: integer;
begin
   result := (FGridLoc.y * (imgSelector.LogicalWidth div FTextureSize.x)) + FGridLoc.x;
end;

procedure TfrmBaseSelector.SetFrame(const Value: integer);
var
   gridWidth: integer;
begin
   gridWidth := imgSelector.LogicalWidth div FTextureSize.x;
   FGridLoc := sgPoint(value mod gridWidth, value div gridWidth);
end;

procedure TfrmBaseSelector.AddNewImage(const filename, imagename: string);
var
   stream: TStream;
begin
   stream := GArchives[IMAGE_ARCHIVE].getFile(filename);
   try
      imgSelector.AddImage(TRpgSdlImage.CreateSprite(imgSelector.renderer, loadFromTBI(stream), imagename, nil));
   finally
      stream.Free;
   end;
end;

procedure TfrmBaseSelector.lstFilenamesClick(Sender: TObject);
begin
{   FRedrawProc;
   imgSelector.Flip;}
   if FFrame <> -1 then
   begin
      self.CurrentFrame := FFrame;
      FFrame := -1;
   end
   else self.CurrentFrame := 0;
   BindCursor;
end;

procedure TfrmBaseSelector.WMRender(var message: TMessage);
begin
   SDL_SetRenderDrawBlendMode(imgSelector.renderer, [sdlbBlend]);
   lstFilenamesClick(self);
end;

procedure TfrmBaseSelector.imgSelectorAvailable(Sender: TObject);
begin
   PostMessage(self.Handle, WM_RENDER, 0, 0);
end;

procedure TfrmBaseSelector.imgSelectorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   imgSelectorMouseMove(sender, Shift, X, Y);
end;

procedure TfrmBaseSelector.imgSelectorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if not (ssLeft in shift) then
      Exit;
   if (x < 0) or (y < 0) or (x >= imgSelector.Width) or (y >= imgSelector.Height) then
      Exit;

   BindCursor(x, y);
end;

end.
