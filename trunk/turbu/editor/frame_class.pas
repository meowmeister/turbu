unit frame_class;
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
   SysUtils, Classes, Controls, Forms, ComCtrls, StdCtrls, ExtCtrls, Graphics,
   DBGrids, DB, Mask, DBCtrls, Grids,
   commons, frame_commands, turbu_sprites, turbu_characters, turbu_defs,
   conversion_table, dm_database,
   SDL_ImageManager,
   JvListBox, JvExStdCtrls, JvExMask, JvSpin, JvDBSpinEdit, turbu_listGrid;

type
   TframeClass = class(TFrame)
      pnlClass: TPanel;
      grpClassName: TGroupBox;
      grpClassStats: TGroupBox;
      imgStats: TImage;
      cbxBaseStats: TComboBox;
      grpClassOptions: TGroupBox;
      grpClassExp: TGroupBox;
      lblExpVal1: TLabel;
      lblExpVal2: TLabel;
      lblExpVal3: TLabel;
      lblExpVal4: TLabel;
      btnExpCurveEditor: TButton;
      grpGraphics: TGroupBox;
      tabGraphics: TTabControl;
      grpRepertoire: TGroupBox;
      pageRepertoire: TPageControl;
      tshEquipment: TTabSheet;
      lblEquip1: TLabel;
      lblEquip2: TLabel;
      lblEquip3: TLabel;
      lblEquip4: TLabel;
      lblEquip5: TLabel;
      tshSkills: TTabSheet;
      frameHeroCommands: TframeHeroCommands;
      tshRepertoire: TTabSheet;
      grpScriptEvents: TGroupBox;
      lstScripts: TListView;
      grpUnarmed: TGroupBox;
      grpResistVuln: TGroupBox;
      lblClasses: TLabel;
      pageResists: TPageControl;
      tshAttributes: TTabSheet;
      tshConditions: TTabSheet;
      btnSetGfx: TButton;
      tmrAnim: TTimer;
      imgMapSprite: TImage;

      dsCharClass: TDataSource;
      dsSkillset: TDataSource;
      dsResist: TDataSource;
      dsCondition: TDataSource;
      dbTxtName: TDBEdit;
      spnExpVal1: TJvDBSpinEdit;
      spnExpVal2: TJvDBSpinEdit;
      spnExpVal3: TJvDBSpinEdit;
      spnExpVal4: TJvDBSpinEdit;
      radWeaponStyle: TDBRadioGroup;
      chkEqLocked: TDBCheckBox;
      chkStrongDef: TDBCheckBox;
      cbxUnarmedAnim: TDBLookupComboBox;
      grdClasses: TRpgListGrid;
      cbxEquip1: TDBLookupComboBox;
      cbxEquip2: TDBLookupComboBox;
      cbxEquip3: TDBLookupComboBox;
      cbxEquip4: TDBLookupComboBox;
      cbxEquip5: TDBLookupComboBox;
      lstSkills: TRpgListGrid;
      lstAttributes: TRpgListGrid;
      navAdd: TDBNavigator;
      btnEditAttributes: TButton;
      navDel: TDBNavigator;
      RpgListGrid1: TRpgListGrid;
    cbxExpAlgorithm: TDBLookupComboBox;

      procedure tabGraphicsChange(Sender: TObject);
      procedure tmrAnimTimer(Sender: TObject);
      procedure cbxBaseStatsChange(Sender: TObject);
      procedure radWeaponStyleClick(Sender: TObject);
      procedure grdClassesRowEnter(Sender: TObject; FromIndex, ToIndex: Integer);
      procedure lstSkillsDblClick(Sender: TObject);
      procedure checkFor2Handed(Sender: TObject);
      procedure linkNav(Sender: TObject);
      procedure btnEditAttributesClick(Sender: TObject);
   private
      FLoading: boolean;
      FLoaded: boolean;
      FImageList: TSdlImages;
      FNameList: TStringList;
      FId: integer;
      FTemplate: TClassTemplate;
      FSpriteData: TSpriteData;
      FMatrixPosition: TRpgPoint;

      procedure loadPortrait(id: integer);
      procedure loadMapSprite(id: integer; frame: byte);
   public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure initClasses;
      procedure loadClass(data: word);
      procedure onShow;
      procedure onHide;
   end;

implementation
uses
   zlib, math, contnrs, generics.Collections, Generics.Defaults,
   turbu_database, archiveInterface, turbu_constants, turbu_items,
   turbu_tbi_lib, turbu_sdl_image, turbu_decl_utils, turbu_classes,
   generic_algorithm_editor, design_script_engine, skill_settings,
   attributes_editor,
   SDL, sdlstreams, sdl_gfx;

{$R *.dfm}

procedure TframeClass.checkFor2Handed(Sender: TObject);
var
   base: TDataSet;
   field: TField;
begin
   base := cbxEquip1.DataSource.DataSet;
   field := base.FindField(cbxEquip1.DataField);
   if assigned(field) then
   begin
      base := field.LookupDataSet;
      field := base.FindField('twoHanded');
      if assigned(field) and (field.AsBoolean = true) then
      begin
         cbxEquip2.Enabled := false;
         lblEquip2.Enabled := false;
         Exit;
      end;
   end;

   base := cbxEquip2.DataSource.DataSet;
   field := base.FindField(cbxEquip2.DataField);
   if assigned(field) then
   begin
      field := base.FindField('twoHanded');
      if assigned(field) and (field.AsBoolean = true) then
      begin
         cbxEquip1.Enabled := false;
         lblEquip1.Enabled := false;
         Exit;
      end;
   end;

   cbxEquip1.Enabled := true;
   lblEquip1.Enabled := true;
   cbxEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);
   lblEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);;
end;

constructor TframeClass.Create(AOwner: TComponent);
var
   i: byte;
begin
   inherited Create(AOwner);
   FMatrixPosition := point(2, 0);
   for i := 0 to radWeaponStyle.Items.Count - 1 do
      radWeaponStyle.Values.Add(intToStr(i));
   tshAttributes.Tag := integer(dsResist);
   tshConditions.Tag := integer(dsCondition);
end;

destructor TframeClass.Destroy;
var
   i: integer;
begin
   FImageList.Free;
   if assigned(FNameList) then
   begin
      for I := 0 to FNameList.Count - 1 do
         FNameList.Objects[i].Free;
      FNameList.Free;
   end;

   pnlClass.Parent := self;
   DestroyComponents;
   inherited Destroy;
end;

procedure TframeClass.grdClassesRowEnter(Sender: TObject; FromIndex,
  ToIndex: Integer);
begin
   FLoading := true;
   try
      loadClass(dsCharClass.DataSet.FieldByName('id').AsInteger);
   finally
      FLoading := false;
   end;
end;

procedure TframeClass.initClasses;
begin
   FLoading := true;
   try
      if GDatabase.charClasses > 0 then
         loadClass(0);
   finally
      FLoading := false;
   end;
end;

procedure TframeClass.linkNav(Sender: TObject);
begin
   navAdd.DataSource := TObject(TControl(sender).Tag) as TDataSource;
   navDel.DataSource := TObject(TControl(sender).Tag) as TDataSource;
end;

procedure TframeClass.loadClass(data: word);
var
   iterator: TDataSet;
begin
   if data = 0 then
      Exit;

   FId := data;
   FTemplate := GDatabase.charClass[FId];
   tabGraphicsChange(tabGraphics); //load portrait or sprite
   frameHeroCommands.size := FTemplate.commands;
   cbxBaseStatsChange(cbxBaseStats);
   radWeaponStyle.ItemIndex := ord(FTemplate.dualWield);
   chkEqLocked.Checked := FTemplate.staticEq;
   chkStrongDef.Checked := FTemplate.strongDef;
   self.radWeaponStyleClick(self); //adjust EQ box 2
   for iterator in dmDatabase.views do
   begin
      iterator.Filtered := false;
      iterator.Filtered := true;
   end;
end;

procedure TframeClass.loadPortrait(id: integer);
var
   filename: string;
   data: ansiString;
   size, dummy: integer;
   fileStream: TStream;
   outStream: TStringStream;
   decompressor: TDecompressionStream;
   smallSurface, zoomedSurface: PSdl_Surface;
   image: TRpgSdlImage;
   spriteRect: TSdl_Rect;
   bitmap: TBitmap;
begin
   assert(assigned(GDatabase.portraitList));
//   smallSurface := nil;
   if not assigned(FImageList) then
   begin
      FImageList := TSdlImages.Create;
      FNameList := TStringList.Create;
   end;

   dummy := id div 16;
   filename := GDatabase.portraitList[dummy] + '.tbi';
   if FImageList.IndexOf(filename) = -1 then
   begin
      try
         fileStream := GArchives[IMAGE_ARCHIVE].getFile(filename);
         try
            decompressor := TDecompressionStream.Create(fileStream);
            try
               decompressor.Read(size, 4);
               setLength(data, size);
               assert(decompressor.Read(data[1], size) = size);
            finally
               decompressor.free;
            end;
         finally
            fileStream.free;
         end;
         outStream := TStringStream.Create(data);
         try
            image := TRpgSdlImage.CreateSprite(loadFromTBI(outStream), filename, FImageList, PORTRAIT_SIZE);
         finally
            outStream.Free;
         end;
      except
         on EArchiveError do
            image := TRpgSdlImage.CreateBlankSprite(filename, FImageList, SPRITE_SIZE, 18);
      end;
   end
   else image := FImageList.image[filename] as TRpgSdlImage;

   data := ansiString(filename) + ' ' + ansiString(intToStr(id mod 16));
   dummy := FNameList.IndexOf(string(data));
   if dummy = -1 then
   begin
      smallSurface := SDL_CreateRGBSurface(IMAGE_FORMAT, PORTRAIT_SIZE.x, PORTRAIT_SIZE.y, 8, 0,0,0,0);
      try
         with image.surface.format^ do
         begin
            if assigned(palette) then
               SDL_SetPalette(smallSurface, SDL_LOGPAL, @palette.colors[0], 0, palette.ncolors);
         end;
         spriteRect := image.spriteRect[id mod 16];
         SDL_BlitSurface(image.surface, @spriteRect, smallSurface, nil);
         filestream := TMemoryStream.Create;
         zoomedSurface := zoomSurface(smallSurface, 2, 2, 0);
         try
            SaveSDLBMPToStream(zoomedSurface, filestream);
            bitmap := TBitmap.Create;
            filestream.Seek(0, soFromBeginning);
            bitmap.LoadFromStream(filestream);
            FNameList.AddObject(string(data), bitmap);
         finally
            filestream.free;
            SDL_FreeSurface(zoomedSurface);
         end;
      finally
         SDL_FreeSurface(smallSurface);
      end;
   end
   else bitmap := FNameList.Objects[dummy] as TBitmap;

   imgMapSprite.Picture.Bitmap := bitmap;
end;

procedure TframeClass.lstSkillsDblClick(Sender: TObject);
begin
   frmSkillLearning.ShowModal;
end;

procedure TframeClass.onShow;
var
   dset: TDataset;
   dummy: string;
begin
   tmrAnim.Enabled := true;
   if not FLoaded then
   begin
      frameHeroCommands.dataSet := dsCharClass.DataSet;

      {2 ugly hacks to make everything set up and display properly}
      dset := dsCharClass.DataSet;
      //1
      dset.Last;
      dset.First;

      //2
      dset.Edit;
      dummy := dset.FieldByName('expFunc').AsString;
      dset.FieldByName('expFunc').AsString := '';
      dset.FieldByName('expFunc').AsString := dummy;
      dset.Post;

      loadClass(1);
      FLoaded := true;
   end;
end;

procedure TframeClass.onHide;
begin
   tmrAnim.Enabled := false;
end;

procedure TframeClass.loadMapSprite(id: integer; frame: byte);
var
   filename: string;
   data: ansiString;
   size, dummy: integer;
   fileStream: TStream;
   outStream: TStringStream;
   decompressor: TDecompressionStream;
   smallSurface, zoomedSurface: PSdl_Surface;
   image: TRpgSdlImage;
   spriteRect: TSdl_Rect;
   bitmap: TBitmap;
begin
   assert(assigned(GDatabase.spriteList));
//   smallSurface := nil;
   if not assigned(FImageList) then
   begin
      FImageList := TSdlImages.Create;
      FNameList := TStringList.Create;
   end;

   dummy := id;
   FSpriteData := extractSpriteData(GDatabase.spriteList[dummy]);
   filename := FSpriteData.name + '.tbi';
   if FImageList.IndexOf(filename) = -1 then
   begin
      try
         try
            fileStream := GArchives[IMAGE_ARCHIVE].getFile(filename);
            decompressor := TDecompressionStream.Create(fileStream);
            try
               decompressor.Read(size, 4);
               setLength(data, size);
               assert(decompressor.Read(data[1], size) = size);
            finally
               decompressor.free;
            end;
         finally
            fileStream.free;
         end;
         outStream := TStringStream.Create(data);
         try
            image := TRpgSdlImage.CreateSprite(loadFromTBI(outStream), filename, FImageList, SPRITE_SIZE);
         finally
            outStream.Free;
         end;
      except
         on EArchiveError do
            image := TRpgSdlImage.CreateBlankSprite(filename, FImageList, SPRITE_SIZE, 12);
      end;
      assert(frame in [0..image.count]);
   end
   else image := FImageList.image[filename] as TRpgSdlImage;

   data := ansiString(filename) + ' ' + ansiString(intToStr(frame));
   dummy := FNameList.IndexOf(string(data));
   if dummy = -1 then
   begin
      smallSurface := SDL_CreateRGBSurface(IMAGE_FORMAT, SPRITE_SIZE.x, SPRITE_SIZE.y, 8, 0,0,0,0);
      try
         with image.surface.format^ do
         begin
            if assigned(palette) then
               SDL_SetPalette(smallSurface, SDL_LOGPAL, @palette.colors[0], 0, palette.ncolors);
         end;
         spriteRect := image.spriteRect[frame];
         SDL_BlitSurface(image.surface, @spriteRect, smallSurface, nil);
         filestream := TMemoryStream.Create;
         zoomedSurface := zoomSurface(smallSurface, 2, 2, 0);
         try
            SaveSDLBMPToStream(zoomedSurface, filestream);
            bitmap := TBitmap.Create;
            filestream.Seek(0, soFromBeginning);
            bitmap.LoadFromStream(filestream);
            FNameList.AddObject(string(data), bitmap);
         finally
            filestream.free;
            SDL_FreeSurface(zoomedSurface);
         end;
      finally
         SDL_FreeSurface(smallSurface);
      end;
   end
   else bitmap := FNameList.Objects[dummy] as TBitmap;

   imgMapSprite.Picture.Bitmap := bitmap;
end;

procedure TframeClass.radWeaponStyleClick(Sender: TObject);
begin
   cbxEquip2.DataField := '';
   case TWeaponStyle(radWeaponStyle.ItemIndex) of
      ws_single: cbxEquip2.DataField := '';
      ws_shield:
      begin
         cbxEquip2.DataField := 'shieldname';
         lblEquip2.Caption := 'Shield:';
      end;
      ws_dual:
      begin
         cbxEquip2.DataField := 'weapon2Name';
         lblEquip2.Caption := 'Weapon:';
      end;
      ws_all:
      begin
         cbxEquip2.DataField := 'offhandName';
         lblEquip2.Caption := 'Offhand:';
      end;
   end;
   cbxEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);
   lblEquip2.Enabled := radWeaponStyle.ItemIndex <> ord(ws_single);;
   checkFor2Handed(sender);
end;

procedure TframeClass.tabGraphicsChange(Sender: TObject);
begin
   case (sender as TTabControl).TabIndex of
      0: loadPortrait(dsCharClass.DataSet.FieldByName('portrait').AsInteger);
      1: loadMapSprite(max(0, (dsCharClass.DataSet.FieldByName('mapSprite').AsInteger)),
                       nextPosition(GDatabase.moveMatrix[FSpriteData.moveMatrix], FMatrixPosition));
      2: {fix this later};
      else assert(false);
   end;
end;

procedure TframeClass.tmrAnimTimer(Sender: TObject);
var
   matrix: TMoveMatrix;
const
   REPS = 3;
begin
   if tabGraphics.TabIndex = 0 then
      Exit;

   matrix := GDatabase.moveMatrix[FSpriteData.moveMatrix];
   case tabGraphics.TabIndex of
      1: loadMapSprite(max(0, (dsCharClass.DataSet.FieldByName('mapSprite').AsInteger)), nextPosition(matrix, FMatrixPosition));
      2: {fix this later};
      else assert(false);
   end;
   if FMatrixPosition.y = 0 then
   begin
      tmrAnim.Tag := (tmrAnim.Tag + 1) mod REPS;
      if tmrAnim.Tag = 0 then
         FMatrixPosition.x := (FMatrixPosition.x + 1) mod length(matrix);
   end;
end;

procedure TframeClass.btnEditAttributesClick(Sender: TObject);
begin
   frmAttributesEditor.ShowModal;
end;

procedure TframeClass.cbxBaseStatsChange(Sender: TObject);
var
   box: TComboBox;
begin
   box := sender as TComboBox;
   //come back to this later
end;

end.
